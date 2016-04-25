#lang racket

;; ---------------------------------------------------------------
;; Defines a compiler pass which:
;;  (1) Adds type annotations (whereever missing) for all binders.
;;  (2) Throws an exception localized to the right syntax object
;;      for any type errors.
;; This does not work as a macro but instead takes in a syntax object
;; and works on it at the syntax phase level
;; ---------------------------------------------------------------

(provide
 (contract-out
  [typecheck-expr (-> (listof (cons/c identifier? acc-syn-entry?)) syntax?
                      (values acc-type? syntax?))]
  [unify-types (-> string-tree? (or/c syntax? #f)
                   instantiated-type? instantiated-type? instantiated-type?)]
  [collapse (-> instantiated-type? acc-type?)]
  [freshen-type-vars (-> acc-type? acc-type?)]
  [instantiate (-> acc-type? instantiated-type?)])
 )

(require (for-syntax racket/base
                     )
         syntax/parse
         ; syntax/parse
         racket/trace
         syntax/to-string
         rackunit rackunit/text-ui
         (only-in accelerack/private/utils pass-output-chatter)
         accelerack/private/types
         accelerack/private/parse
         (only-in accelerack/private/syntax acc-array : use acc-primop-types acc-primop
                  acc-lambda-param acc-type acc-element-literal acc-let-bind)
         (only-in accelerack/private/wrappers acc-array-ref acc-array-flatref
                  zipwith fold stencil3x3 generate replicate until)

         (for-template (except-in racket/base map))
         (for-template (only-in accelerack/private/wrappers acc-array-ref
                                map zipwith fold stencil3x3 generate replicate until))
         (for-template (only-in accelerack/private/syntax acc-array : use
                                acc-lambda-param acc-type))
         )

;; For error messages:
(define pass-name 'accelerack-typecheck)

;; ---------------- Persistent variables and typevariable related stuff ---------------------
(define var-cnt 0)
(define (reset-var-cnt) (set! var-cnt 0))

;; Create a fresh, non-numeric (Num class, i.e. Int/Double) tyvar.
(define fresh-tyvar 
  (case-lambda
    [() (fresh-tyvar 'a)]
    [(sym)
     (set! var-cnt (+ var-cnt 1))
     (define newsym
       (string->symbol
        (string-append (symbol->string sym)
                       (number->string var-cnt))))
     (make-tyvar #f newsym
                 (numeric-type-var? sym))]))


;; -------------------------------------------------------------------------------------------

;; Data type definitions and predicates:

;; TypeEnv:
;; ---------
;; Immutable dict of type:  syntax? (TermVariable) -> type-schema?

(define empty-tenv
  (make-immutable-custom-hash
   free-identifier=?
   #; ;; Dumb comparison:
   (lambda (id1 id2) (equal? (syntax->datum id1)
                             (syntax->datum id2)))
   ))

(define empty-set (list->seteq '()))
(define (make-mono-schema mono) (make-type-schema empty-set mono))

;; Instantiated Tyes:
;; ------------------
;; This includes everything in acc-type? plus the tyvar struct.
(define (instantiated-type? t)
  (match t
    ;; Because this is not yet monomorphic we cannot determine for
    ;; sure whether the "n" and "elt" meet their constraints here:
    [`(Array ,n ,elt) (and (instantiated-type? n)
                           (instantiated-type? elt))]
    ;; Likewise, we may end up with constraints that reference type
    ;; level numbers:
    [(? exact-nonnegative-integer?) #t]
    [`#( ,t* ...)     (andmap instantiated-type? t*)]
    [`(-> ,t* ...)    (andmap instantiated-type? t*)]
    [(? tyvar?)
     (if (tyvar-ptr t)
         (instantiated-type? (tyvar-ptr t))
         #t)]
    [(? symbol?) #:when (type-var-symbol? t) #t]
    ['SExp   #t]
    ['Int    #t]
    ['Bool   #t]
    ['Double #t]
    [_       #f]))


(define (make-array-type dim elt)
  (match elt
    [`(Array ,_ ,_) (error 'make-array-type "cannot build array type containing array elements ~a"
                           `(Array ,dim ,elt))]
    [else `(Array ,dim ,elt)]))

;; TyVars are mutable pointers to another type
(define-struct tyvar
  ([ptr #:mutable] ;; #f or a instantiated-type?
    name           ;; symbol?, For nicer printing
    [numeric #:mutable]  ;; boolean?, can change through unification
   )
  ; #:transparent
  #:methods gen:custom-write
  [(define (write-proc v prt mode)
     (display ; (if mode write print)
      (if (tyvar-ptr v)
          (format "~a=~a" (tyvar-name v) (tyvar-ptr v))
          (format "<~a>" (tyvar-name v)))
      prt))]
  )

;; ------------------------------------------------------------

;; Type Enviroment reference with good errors:

(define (tenv-ref d var)
  (define res (dict-ref d var #f))
  (or res
      (let ([syms (map (lambda (k) (cons (syntax->datum k) k))
                       (dict-keys d))])
        (match (assoc (syntax->datum var) syms)
          [`(,symkey . ,idkey)
           (raise-syntax-error
            #f
            (format (string-append
                     "Attempt to reference variable ~a inside accelerack code.\n"
                     "This variable should have been imported from the accelerack module.\n"
                     "Instead, a different copy of the variable was used, from:\n"
                     "~a\n"
                     "Whereas we expected this identifier:\n"
                     "~a\n")
                     (syntax->datum var)
                     var
                     idkey))]
          [#f
           (raise-syntax-error
            #f
            (format "Attempt to reference unbound variable (in type environment), '~a', inside accelerack code.\n"
                    (syntax->datum var)))]))))

(define (tenv-set te k v)
  ;; These are type-schemas:
  ; (-> dict? identifier? type-schema? dict?)
  (dict-set te k v))

(define (tenv-free-vars te)
  (define ls (for/list ([(_ sch) (in-dict te)])
               (schema-free-vars sch)))
  (if (null? ls) empty-set
      (apply set-union ls)))
            
;; Type Utilities
;; ------------------------------------------------------------

;; Take a tyvar used during unification and make it visible to the
;; user.  In the future could remap the suffix renaming.
(define (export-tyvar t)
  (if (tyvar-numeric t)
      (if (numeric-type-var? (tyvar-name t))
          (tyvar-name t)
          (symbol-append 'num_ (tyvar-name t)))
      (tyvar-name t)))

(define (symbol-append a b)
  (string->symbol (string-append (symbol->string a) (symbol->string b))))

;; Remove type variables:
;; instantiated-type? -> acc-type?
(define (export-type ty)
  (match ty
    [(? tyvar?)     
     (if (tyvar-ptr ty)
         (export-type (tyvar-ptr ty))
         (export-tyvar ty))]
    [(? integer?) ty] ;; type-level lits
    [(? symbol?) ty]
    [`(Array ,n ,elt) (make-array-type (export-type n) (export-type elt))]
    [`#( ,t* ...)     (list->vector (map export-type t*))]
    [`(-> ,t* ...)   `(-> ,@(map export-type t*))]
    [(? acc-scalar-type?)           ty]
    [(? exact-nonnegative-integer?) ty]))

;; Short-circuit paths through type-variables.
;; Leave free type variables alone.
;; instantiated-type? -> instantiated-type?
(define (collapse ty)
  (match ty
    [(? tyvar?)     
     (if (tyvar-ptr ty)
         ;; policy: if its a chain of variables, which name to use?
         (collapse (tyvar-ptr ty))
         ty)]
    [(? integer?) ty] ;; type-level lits
    [(? symbol?) ty]
    [`(Array ,n ,elt) (make-array-type (collapse n) (collapse elt))]
    [`#( ,t* ...)     (list->vector (map collapse t*))]
    [`(-> ,t* ...)   `(-> ,@(map collapse t*))]
    [(? acc-scalar-type?)           ty]
    [(? exact-nonnegative-integer?) ty]
    ))

(define (schema-free-vars x)
  (match-define (type-schema quantified ty) x)
  (set-subtract (free-vars ty) quantified))

;; free variables: instantiated-type? -> (seteq? of symbol?)
;; Fetches all the variables in the input given
(define (free-vars ty)
  (match ty
    [(? tyvar?)
     (if (tyvar-ptr ty)
         (free-vars (tyvar-ptr ty)) ;; Not free, bound.
         (set-add empty-set (tyvar-name ty)))]
    [elt #:when (acc-element-type? elt)            empty-set]
    [num #:when (exact-nonnegative-integer? num)   empty-set]
    [`(Array ,n ,elt) (set-union (free-vars n) (free-vars elt))]
    [`(-> ,a ,bs ...) (apply set-union (free-vars a) (map free-vars bs))]
    [`#(,vs ...) (apply set-union (map free-vars vs))]
    [sym #:when (symbol? sym) (list->seteq (list sym))]))


;; ------------------------------------------------------------


;; The full type-checking pass.
;; Returns two values:
;;   (1) principal type of expression
;;   (2) fully annotated expression
(define (typecheck-expr syn-table e)
  #;(-> (listof (cons/c identifier? acc-syn-entry?)) syntax?
      (values acc-type? syntax?))
  (reset-var-cnt)

  (define env0
    (for/fold ([env empty-tenv])
              ([(id ty) (in-dict acc-primop-types)])
      ;; TODO: could store type schemas a priori:
      (tenv-set env id (generalize empty-tenv ty))))
  (define env1
    (for/fold ([env env0])
              ([pr syn-table])
      (match pr
        [`(,v . ,(acc-syn-entry ty expr))
         ;; TODO: could store type schemas a priori:
         (tenv-set env v (generalize empty-tenv ty))])))
  ;; Rip out the type variable stuff:
  (define-values (ty e2) (infer e env1))
  (define ty2 (export-type ty))
  (pass-output-chatter 'typecheck-expr
                       (list 'expr: (syntax->datum e2)
                             'type: ty
                             'exported/collapsed: ty2))
  (values ty2 e2))


;; Put a type into a human-readable form for error messages.
(define (show-type ty)
  (match (export-type ty)
    [(? type-var-symbol? s)
     (format "type variable '~a'" s)]
    [oth (format "~a" oth)]))

(define (is-numeric? t)
  (if (tyvar? t)
      (or (tyvar-numeric t)
          (is-numeric? (tyvar-ptr t)) ;; This should be redundant.
          )
      (acc-num-type? t)))

(define (make-numeric! t)
  (when (and t (tyvar? t))
    (set-tyvar-numeric! t #t)
    (make-numeric! (tyvar-ptr t))))

;; Safely set a type variable while respecting whether or not it is a
;; numeric-only (num_) type variable.
(define (set-tyvar! msg ctxt t1 rhs)
  (define rhs-coll (collapse rhs))
;  (printf " unify to tyvar:  ~a(~a)  -> ~a " (tyvar-name t1) (tyvar-numeric t1) rhs)
  (when (and (tyvar-numeric t1)
             (not (or (acc-num-type? rhs-coll)
                      ;; It is OK to equate with a non-numeric type var..
                      ;; We defer judgement.
                      (tyvar? rhs-coll)
                      (numeric-type-var? rhs-coll))))
    (raise-syntax-error
     'unify-types
     (format "error\n  Expected a numeric type, instead found ~a\nContext notes:\n~a\n"
             (show-type rhs) (tree->string msg))
     ctxt))
  ;; Propagate numeric-ness in both directions:
  (when (or (is-numeric? rhs) (tyvar-numeric t1))
    (make-numeric! t1))
  (set-tyvar-ptr! t1 rhs))

(define (string-tree? x)
  (cond
    [(string? x) #t]
    [(null? x) #t]
    [(pair? x) (and (string-tree? (car x))
                    (string-tree? (cdr x)))]
    [else #f]))

(define (tree->string x)
  (with-output-to-string
    (lambda ()
      (let loop ((x x))
        (cond
          [(null? x) (void)]
          [(string? x) (display x)]
          [(pair? x) (loop (car x)) (loop (cdr x))]
          [else (error 'tree->string "unexpected input: ~a" x)])))))

(define (short-show syn)
  ;; FINISHME: crop to a max length and include ellipses:
  (with-output-to-string
    (lambda ()
      (pretty-print
       (if (syntax? syn)
           (syntax->datum syn)
           syn)))))

;; instantiated-type? instantiated-type? -> instantiated-type?
;; If one of the two is "expected", it should be the latter.
(define/contract (unify-types msg ctxt t1 t2)
  ;; DEBUGGING:
  (-> string-tree? (or/c syntax? #f) instantiated-type? instantiated-type? instantiated-type?)
  (match/values (values t1 t2)
    ;; Variables trivially unify with themselves:
    [((? tyvar?) (? tyvar?))
     #:when (eq? (tyvar-name t1) (tyvar-name t2))
     t1]
     
    [((? tyvar?) _)
     (occurs-check ctxt (tyvar-name t1) t2)
     (set-tyvar! msg ctxt t1 (if (tyvar-ptr t1)                         
                             (unify-types msg ctxt (tyvar-ptr t1) t2)
                             t2))
     t1]
    [(_ (? tyvar?))
     (occurs-check ctxt (tyvar-name t2) t1)
     (set-tyvar! msg ctxt t2 (if (tyvar-ptr t2)
                             (unify-types msg ctxt t1 (tyvar-ptr t2))
                             t1))
     t2]

    [(`(Array ,n1 ,e1) `(Array ,n2 ,e2))
     (make-array-type (unify-types (cons "Dimensions of arrays must match.\n" msg)
                                   ctxt n1 n2)
                      (unify-types (cons "Element types of arrays must match.\n" msg)
                                   ctxt e1 e2))
     #; (match/values (values (collapse n1) (collapse n2))
       ;; Attempt to improve error messages here:
       [((? number?) (? number?))
        (raise-syntax-error #f "")]
       [(_ _)  ...])]
       
    [(`(-> ,as ...) `(-> ,bs ...))
     #:when (= (length as) (length bs))
     (define blen (length bs))
     `(-> ,@(for/list ([a as] [b bs] [ix (in-naturals)])
              (unify-types (cons (if (= ix (sub1 blen))
                                     "Return values of function types must match.\n"
                                     (format "Argument position ~a must match ~a"
                                             ix "between arrow types (starting with 0)\n"))
                                 msg)
                           ctxt a b)))]
    
    [((vector as ...) (vector bs ...))
     #:when (= (length as) (length bs))
     (for/vector ([a as] [b bs] [ix (in-naturals)])
       (unify-types (cons (format "Position ~a within two vectors must match.\n" ix) msg)
                    ctxt a b))]
    
    [((? acc-element-type?) (? acc-element-type?))
     #:when (equal? t1 t2)
     t1]    
    [((? exact-nonnegative-integer?) (? exact-nonnegative-integer?))
     #:when (equal? t1 t2)
     t1]
    ;; Rigid (uninstantiated) type variables:
    [((? type-var-symbol?) (? type-var-symbol?))
     #:when (equal? t1 t2)
     t1]
    
    [((? type-var-symbol?) _) #:when (not (equal? t1 t2))
     (raise-syntax-error
      'unify-types
      (format "Found a rigid type variable ~a, whereas expected type ~a\n"
              (export-type t1) (export-type t2))
      ctxt)]

    [(_ (? type-var-symbol?)) #:when (not (equal? t1 t2))
     (raise-syntax-error
      'unify-types
      (format "Couldn't match type ~a against expected type, which was a rigid type variable ~a\n"
              (export-type t1) (export-type t2))
      ctxt)]
    
    [(_ _)
     (raise-syntax-error
      'unify-types
      (format (string-append "Conflicting types.\n"
                             "Found: ~a\n"
                             "Expected: ~a\n"
                             "Context notes:\n"
                             (tree->string msg))
              (show-type t1) (show-type t2))
      ctxt)]))

;; A method of unifying an arrow type that yields better-localized
;; error messages.  Specifically, it is better to highlight the
;; argument of the wrong type than to highlight the whole application.
;;
(define/contract (gentle-unify-arrow msg0 fnty fnstx args ret)
  (-> string-tree? instantiated-type? syntax?
      (listof (cons/c instantiated-type? (or/c syntax? #f)))
      (cons/c instantiated-type? (or/c syntax? #f))
      instantiated-type?)
  (define target `(-> ,@(map car args) ,(car ret)))
  ;; Option (1), SIMPLEST implementation, with worse errors:
  ; (unify-types msg0 fnstx target fnty)
  ;; Option (2):
  (begin
    (define (helper msg)
      ;; FIXME: Pass more context info / extra messages to unify-types:
      (lambda (expected pr)
        ;; One option is to print and rethrow, but better to pass to unify.
        #; (with-handlers ([exn:fail?
                            (lambda (exn)
                              (fprintf (current-error-port) msg)
                              (raise exn))]) ...)
        (match-let ([ (cons rcvd stx) pr])
          (unify-types
           (cons msg msg0)
           (or stx fnstx) rcvd expected))))
   (match fnty
     [`(-> ,e* ... ,en)
      (for ([e e*] [arg args] [ix (in-naturals)])        
        ((helper (format "Function argument ~a, in position ~a, did not have expected type: ~a.\n"
                         (syntax->datum (cdr arg)) ix (export-type e)))
         e arg))
     ((helper "Function return value did not have expected type.\n") en ret)]
    ;; Here, there won't be any argument-level errors:
    [(? tyvar?) (unify-types msg0 fnstx target fnty)]
    [else
     (raise-syntax-error
      'unify-types
      (format "Expected a function type here, found: ~a" fnty)
      fnstx)])))
  

;; Var instantiated-type? -> boolean?
(define/contract (occurs-check stx var type)
  (-> syntax? symbol? any/c void?)
  (when (set-member? (free-vars type) var)
    ;This is an infinite type. Send an error back
    (raise-syntax-error 'occurs-check
                        (format "Occurs check failed, ~a occurs in ~a\n" var type)
                        stx)))

;; Returns two values:
;;   (1) principal type of expression
;;   (2) fully annotated expression
(define (infer stx tenv)

  ;; Recur on a whole list in the same tenv.
  ;; Handles multiple-value boilerplate:
  (define (infer-list ls)
   (define-values [ls1 ls2]
     (for/fold ([tys '()] [xs '()])
               ([x ls])
      (define-values [xty xnew] (infer x tenv))
      (values (cons xty tys)
              (cons xnew xs))))
   (values (reverse ls1) (reverse ls2)))
    
  (syntax-parse stx
    #:literals (use acc-array acc-array-ref :
                ;; FIXME: some of these can just be removed when they go to the prim table:
                map zipwith fold stencil3x3 generate replicate until
                lambda let if vector vector-ref)

    ;; Literal data:
    ;[n:number  (values (acc-element->type (syntax->datum #'n)) #'n)]
    ;[b:boolean (values 'Bool #'n)]
    ; [#(e* ...)]
    
    [n:acc-element-literal (values (acc-element->type (syntax->datum #'n)) #'n)]

    [x:identifier
     (values (instantiate-scheme (tenv-ref tenv #'x))
             #'x)
     #;
     (match (dict-ref tenv #'x #f)
       [#f (raise-syntax-error
            'typecheck-expr
            (apply string-append
                   "Internal error, unbound variable should not be found at this point in the compiler.\n"
                   (format "Unbound variable: ~a\n" (syntax->datum #'x))
                   (format "Full variable syntax: ~a\n" #'x)
                   "Full type environment:\n"
                   (for/list ([(k v) (in-dict tenv)])
                     (format "  ~a : ~a\n" k v)))
            #'x)]
       [ity
        (values (instantiate-scheme ity)
                #'x)])]
    
    ;; Other features, Coming soon:
    [(: e t:acc-type)
     (define-values (ty e2) (infer #'e tenv))
     (values (unify-types (format "Ascription expression: ~a\n" (short-show stx))
                          stx ty (instantiate (syntax->datum #'t)))
             e2)]
    [(use x:id t:acc-type)
     (values (syntax->datum #'t)
             stx)]
    [(use x:id)            (raise-syntax-error pass-name "use form without type not yet supported." stx)]

    [(acc-array dat) (values (syntax->datum (infer-array-type #'dat))
                             #'(acc-array dat))]

    [(acc-array-ref e1 e2s ...)
     (define-values (ty1 e1b) (infer #'e1 tenv))
     (define e2ls (syntax->list #'(e2s ...)))
     (define e2slen (length e2ls))
     
     ;; Here we KNOW what dimension to expect, so we put it in.
     ;; We could alternatively do this unification in stages and try
     ;; to optimize the errors that come out:     
     (let ((arrty (unify-types "Input array to acc-array-ref must be of the proper type.\n"
                               #'e1 ty1 (make-array-type e2slen (fresh-tyvar 'elt)))))
       (match (collapse arrty) ;; AUDIT ME
         #;
         [(? tyvar?)
          (if (tyvar-ptr arrty)
              (loop (tyvar-ptr arrty))
              (error 'typecheck "Internal error in accelerack typecheck pass. Please report this."))]
         [`(Array ,dim ,elt)
          (cond
            #;
            [(symbol? dim)
             (raise-syntax-error 'acc-array-ref
              (format "\n  Expected an array with known-dimension ~a, instead found type variable ~a"
                      e2slen dim)
              #'e1)]
            [(exact-nonnegative-integer? dim)
             (if (equal? dim e2slen)
                 (values elt ;; Collapsed!
                   (let ([e2news
                        (for/list ([e2 e2ls])
                          (define-values (ty enew) (infer e2 tenv))
                          ;; Side effect only, but it should be safe here:
                          (unify-types "Index argument to acc-array-ref must be an Int."
                                       e2 ty 'Int) ;; SAFE
                          enew
                          #;
                          (if (eq? ty 'Int) enew
                             (raise-syntax-error 'acc-array-ref
                               (format "expected all index expressions to have type Int, instead found ~a" ty)
                               #'e2))
                         )])
                    #`(acc-array-ref #,e1b #,@e2news)))
                 (raise-syntax-error 'acc-array-ref
                                     (format "this array has dimension ~a, but ~a indices were provided"
                                             dim e2slen)
                                     #'e1))])]
         [else
          (raise-syntax-error 'acc-array-ref
            (format "array reference of non-array type, ~a" (show-type ty1))
            #'e1)]))]

    ;; Method one, don't match bad params:
    [(lambda (x:acc-lambda-param ...) e)
     (define xs (syntax->list #'(x.name ...)))
     (define freshes (build-list (length xs)
                                 (lambda (_) (fresh-tyvar))))

     ;; Enforce the user type annotations BEFORE normal inference.
     ;; The main reason for this order is that it may prevent an Array
     ;; dimension ambiguity until we fix those constraints to be deferred.
     (define xtys
       (for/list ([x xs] [infrd freshes]
                  [expected (syntax->datum #'(x.type ...))])
         (if expected
             (unify-types (format "Function (lambda) parameter must match its annotated type: ~a\n"
                                  (export-type expected))
                          x infrd
                          (type-schema-monoty (generalize tenv expected)))
             infrd)))

     (define tenv2 (for/fold ([te tenv])
                             ([x xs]
                              [xty xtys])
                     (tenv-set te x (make-mono-schema xty))))
     (define-values (tbod bod) (infer #'e tenv2))
     (define new-params (for/list ([x xs] [xty xtys])
                          #`(#,x : #,(datum->syntax x (export-type xty)))))
     (values `(-> ,@xtys ,tbod)
             ;; TODO: must return annotated here:
             #`(lambda (#,@new-params) #,bod))]
    
    ;; Generate gets its own typing judgement.  It can't go in the prim table.
    [(generate f e* ...)
     (define es (syntax->list #'(e* ...)))
     (define-values (etys news) (infer-list es))
     (define etys2 (for/list ([e es] [ety etys])
                     (unify-types "Size arguments to generate must be of type Int.\n"
                                  e ety 'Int)))
     (define-values (fty fnew) (infer #'f tenv))
     (define res (fresh-tyvar 'res))
     (define ufty `(-> ,@etys2 ,res))
     (gentle-unify-arrow "Function argument to generate must have the right type."
                         fty #'f 
                         (map cons etys2 es)
                         (cons res #f))
     (values (make-array-type (length es) res)
	     #`(generate #,fnew #,@news))]

    [(until (var init pred) bod)
     (define-values (stateTy newinit) (infer #'init tenv))
     (define tenv2 (tenv-set tenv #'var (make-mono-schema stateTy)))
     (define-values (predty newpred) (infer #'pred tenv2))
     (define-values (bodty  newbod)  (infer #'bod tenv2))
     (unify-types "Predicate expression in until must have Bool type.\n"
                  #'pred predty 'Bool)
     (define stateTy2 (unify-types (string-append "Return value of initial and update sub-expression"
                                                  " of until must have the same type.\n")
                                   #'bod bodty stateTy))
     (values stateTy
             #`(until (var #,newinit #,newpred) #,newbod))]
    
    ;; Replicate gets its own typing judgement.
    [(replicate (v* ...) (e* ...) arr)
     (define-values (arrty newArr) (infer #'arr tenv))
     (define vs (syntax->list #'(v* ...)))
     (define es (syntax->list #'(e* ...)))
     (define ntv (fresh-tyvar 'n))
     (define elt (fresh-tyvar 'elt))
     ;; This unify side effects ntv and elt.
     (define _arrty2 (unify-types "Argument to replicate must be an array.\n"
                      #'arr arrty `(Array ,ntv ,elt)))
     (if (or (not vs) (not es))
         (raise-syntax-error pass-name
			     (string-append "Malformed syntax for replicate.\n"
					    (format "Pattern 1: ~a\n" (syntax->datum #'(v* ...)))
					    (format "Pattern 2: ~a\n" (syntax->datum #'(e* ...)))))
	 (let ([plus-dim (count not
				(map (lambda (e)
				       (and (identifier? e)
					    (memf (lambda (v) (free-identifier=? v e)) vs)))
				     es))])
	   (match (collapse ntv) ;; Careful when we use this.  Here it's safe.
	     [n #:when (number? n)
		(values `(Array ,(+ plus-dim n) ,elt)
			#`(replicate #,vs #,es #,newArr))]
	     [other (raise-syntax-error pass-name
					(string-append
					 "Replicate is expected to take an array of known dimension.\n"
					 "Expected non-negative integer dimension, instead found "
					 (if (symbol? other)
					     (format "type variable, '~a'\n" (export-type other))
					     (format "unexpected type, '~a'\n" (export-type other)))
					 (format "The input to fold had type: ~a"
                                                 (export-type `(Array ,other ,elt)))
					 )
					stx
					)])))]
		

    ;; Fold gets its own typing judgement.  It can't go in the prim table.
    [(fold f zer arr)
     (define-values (arrty newArr) (infer #'arr tenv))
     (define-values (fty newF)     (infer #'f   tenv))
     (define-values (elt newZer) (infer #'zer tenv))

     (define ntv (fresh-tyvar 'n))

     (gentle-unify-arrow "" fty #'f
                         `((,elt . ,#'zer)
                           (,elt . ,#'zer))
                         `(,elt . ,#'zer))
     (unify-types "Argument to fold must be an array.\n"
      #'arr arrty `(Array ,ntv ,elt)) ;; For side effect on ntv.

     ;; FIXME: We should defer the final check that the dimensions are concrete.
     ;; Otherwise, whether it works can depend on the order of type inference.
     (match (collapse ntv)
       [n #:when (number? n)
          (if (< n 1)
              (raise-syntax-error
               'unify-types "This array is zero dimensional, and cannot be folded."
               #'arr)
              (values `(Array ,(sub1 n) ,elt)
                  #`(fold #,newF #,newZer #,newArr)))]
       [other (raise-syntax-error pass-name
               (string-append 
                "Fold is expected to take an array of known dimension.\n"
                "Expected non-negative integer dimension, instead found "
                (show-type (export-type other))
                (format "\nThe input to fold had type: ~a"
                        (export-type `(Array ,other ,elt)))
                )
               stx
               )])]

    [(if e1 e2 e3)
     (define-values (e1ty newe1) (infer #'e1 tenv))
     (define-values (e2ty newe2) (infer #'e2 tenv))
     (define-values (e3ty newe3) (infer #'e3 tenv))
     (unify-types "The test part of an 'if' must return a Bool.\n" #'e1 e1ty 'Bool)
     (values (unify-types "Branches of an 'if' must have the same type.\n"
                          #'e3 e3ty e2ty)
             #`(if ,newe1 ,newe2 ,newe3))] 

    [(let ( lb:acc-let-bind ...) ebod)
     (define xls (syntax->list #'(lb.name ...)))
     (define els (syntax->list #'(lb.rhs ...)))
     (define-values (etys news) (infer-list els))
     (define xtys
       (for/list ([x xls]
                  [ty etys]
                  [expected (syntax->datum #'(lb.type ...))])
           (if expected 
               (unify-types (format "Let binding for ~a must match its annotated type: ~a\n"
                                    (syntax->datum x)
                                    (export-type expected))
                            x ty expected)
               ty)))
     (define tenv2
       (for/fold ([te tenv])
                 ([x xls] [ty xtys])
         (tenv-set te x (generalize tenv ty))))
     (define-values (finalTy newbod) (infer #'ebod tenv2))
     (values finalTy             
            #`(let ([lb.name #,news] ...)
                #,newbod))]

    [(vector e* ...)
     (define-values (tys news) (infer-list (syntax->list #'(e* ...))))
     (values (list->vector tys)
             #`(vector #,@news))]
    
    ;; Special typing rule for polymorphic vector ref:
    ;; The second argument must be a fixed integer.
    ;;
    ;; FIXME: These constraints should also be deferred, along with
    ;; the fold constraints.
    ;;
    ;; Alternatively, this can be replaced with explicit pattern matching.
    [(vector-ref e1 n:number)
     (define-values (e1ty e1new) (infer #'e1 tenv))
     (define ind (syntax->datum #'n))
     (match (collapse e1ty)
       [(vector ts ... )
        (if (>= ind (length ts))
            (raise-syntax-error
             'typecheck (format "Cannot reference position ~a in vector type of length ~a: ~a"
                                ind (length ts)
                                (list->vector ts))
             stx)
            (values (list-ref ts ind)
                    #`(vector-ref #,e1new n)))]
       [oth
        (raise-syntax-error
         'typecheck
         (string-append "This is expected to have a vector type of known length, "
                        (if (symbol? (export-type oth))
                            "instead found a vector of unknown length."
                            (format "instead found: ~a" (show-type oth))))
         #'e1)])]
    
    [(rator args ...)
     (define argls (syntax->list #'(args ...)))
     (define-values (ratorty newrator) (infer #'rator tenv))
     (define-values (argtys newargs) (infer-list argls))
     (define fresh (fresh-tyvar 'res))
     (gentle-unify-arrow "" ratorty stx 
                         (map cons argtys argls)
                         (cons fresh #f))
     (values fresh
             #`(#,newrator #,@newargs))]
    
     ))

;; generalize: set(type var) -> acc-type? -> type-schema?
(define (generalize tenv mono)
  ;; Tyvars mentioned in the environment are "rigid" and NOT generalized.
  (define free-env (tenv-free-vars tenv))
  (define quant (set-subtract (free-vars mono) free-env))
  (make-type-schema quant
                    (for/fold ([ty (collapse mono)])
                              ([q quant])
                      (subst ty q q))))

;; instantiate: scheme -> type
(define/contract (instantiate-scheme scheme)
  (-> type-schema? instantiated-type?)
  (match-define (type-schema vars monoty) scheme)
  ;; Inefficient: repeated subst:
  (for/fold ([ty monoty])
            ([q  vars])
    (define fresh (fresh-tyvar q))
    (subst ty q fresh)))

;; Takes a normal SExpression mono-type.
(define (instantiate mono)
  (instantiate-scheme
   (make-type-schema (free-vars mono) mono)))

;; Even if we are not instantiating type variabes, we still need to
;; make sure we don't have name collissions between *separate* type
;; annotations provided by the user.
(define (freshen-type-vars ty0)
  (define free (free-vars ty0))
  ;; Inefficient, super-linear:
  (for/fold ([ty ty0])
            ([v (in-set free)])
    (subst ty v (tyvar-name (fresh-tyvar v)))))

(define (subst ty var new)
  (define (go x) (subst x var new))
  (match ty
    [(? symbol? t)
     (if (eq? t var)
         new t)]
    [`(Array ,n ,elt)
     `(Array ,(go n) ,(go elt))]
    [`#( ,t* ...)     (list->vector (map go t*))]
    [`(-> ,t* ...)   `(-> ,@(map go t*))]
    [(? acc-scalar-type?)           ty]
    [(? exact-nonnegative-integer?) ty]
    [(? tyvar?)
     (cond
       ;; Warning we should probably NOT ever subst on a bound var:     
       [(and (tyvar-ptr ty) (eq? (tyvar-name ty) var))
        (error 'subst "internal error, attempt to substitute *bound* tyvar ~a" var)]
       [(eq? (tyvar-name ty) var) new]
       ;; Allow subst to short-circuit for the returned type:
       [(tyvar-ptr ty) (go (tyvar-ptr ty))]
       [else ty])]
    [else (error 'subst "error, invalid type: ~a\n" ty)]
    ))


(module+ test

  (check-equal? (free-vars '(-> a (-> b a)))
                (list->seteq '(a b)))

  (test-case "instantiate"
    (instantiate-scheme (make-type-schema (list->seteq '(a b))
                                          '(-> (-> a b) (Array n a) (Array n b))))
    (void))

  ;; Generalize could take instantiated types:
  (check-true
   (match (generalize empty-tenv
                      (instantiate-scheme (generalize empty-tenv '#(a a))))
     [(type-schema set (vector a a)) #:when (set-member? set a)
      #t]))
  
  )
