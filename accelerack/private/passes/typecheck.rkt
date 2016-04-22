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
  [unify-types (-> (or/c syntax? #f) instantiated-type? instantiated-type? instantiated-type?)])
 ; typecheck-expr ;; If the contract is enforced below.
 ; unify-types
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
                  zipwith fold stencil3x3 generate replicate)

         (for-template (except-in racket/base map))
         (for-template (only-in accelerack/private/wrappers acc-array-ref
                                map zipwith fold stencil3x3 generate replicate))
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
   ; free-identifier=?
   (lambda (id1 id2) (equal? (syntax->datum id1)
                             (syntax->datum id2)))))

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


;; TyVars are mutable pointers to another type
(define-struct tyvar
  ([ptr #:mutable] ;; #f or a instantiated-type?
    name           ;; symbol?, For nicer printing
    numeric?        ;; boolean?
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
            (format "Attempt to reference unbound variable ~a inside accelerack code.\n"
                    (syntax->datum var)))]))))

(define/contract (tenv-set te k v)
  ;; These are type-schemas:
  (-> dict? identifier? type-schema? dict?)
  (dict-set te k v))

;; Type Utilities
;; ------------------------------------------------------------

;; Remove type variables:
;; TyInst -> MonoTy (i.e. acc-type?)
(define (collapse ty)
  (match ty
    [(? tyvar?)     
     (if (tyvar-ptr ty)
         (collapse (tyvar-ptr ty))
         (tyvar-name ty))]
    [(? symbol?) ty]
    [`(Array ,n ,elt)
     `(Array ,(collapse n) ,(collapse elt))]
    [`#( ,t* ...)     (list->vector (map collapse t*))]
    [`(-> ,t* ...)   `(-> ,@(map collapse t*))]
    [(? acc-scalar-type?)           ty]
    [(? exact-nonnegative-integer?) ty]
    ))


;; free variables: instantiated-type? -> (seteq? of symbol?)
;; Fetches all the variables in the input given
(define (free-vars ty)
  (match ty
    [(? tyvar?)
     (set-add (if (tyvar-ptr ty)
                  (free-vars (tyvar-ptr ty))
                  empty-set)
              (tyvar-name ty))]
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
(define/contract (typecheck-expr syn-table e)
  (-> (listof (cons/c identifier? acc-syn-entry?)) syntax?
      (values acc-type? syntax?))
  (pass-output-chatter 'typecheck-expr e)
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
  (values (collapse ty) e2))


;; Put a type into a human-readable form for error messages.
(define (show-type ty)
  (match (collapse ty)
    [(? symbol? s)
     (format "type variable '~a'" s)]
    [oth (format "~a" oth)]))

;; Safely set a type variable while respecting whether or not it is a
;; numeric-only (num_) type variable.
(define (set-tyvar! ctxt t1 t2)
  (define rhs (if (tyvar-ptr t1)                         
                  (unify-types ctxt (tyvar-ptr t1) t2)
                  t2))
  (define rhs2 (collapse rhs))
  (when (and (tyvar-numeric? t1)
             (not (or (acc-num-type? rhs2)
                      ;; It is OK to equate with a non-numeric type var..
                      ;; We defer judgement.
                      (type-var-symbol? rhs2))))
    (raise-syntax-error
     'unify-types
     (format "error\n  Expected a numeric type, instead found ~a" (show-type rhs))
     ctxt))
  (set-tyvar-ptr! t1 rhs))
  
;; instantiated-type? instantiated-type? -> instantiated-type?
;; If one of the two is "expected", it should be the latter.
(define/contract (unify-types ctxt t1 t2)
  ;; DEBUGGING:
  (-> (or/c syntax? #f) instantiated-type? instantiated-type? instantiated-type?)
  (match/values (values t1 t2)
    ;; Variables trivially unify with themselves:
    [((? tyvar?) (? tyvar?))
     #:when (eq? (tyvar-name t1) (tyvar-name t2))
     t1]
     
    [((? tyvar?) _)
     ;(printf "unify:  ~a  -> ~a\n" (tyvar-name t1) t2)
     (occurs-check ctxt (tyvar-name t1) t2)
     (set-tyvar! ctxt t1 t2)
     t1]
    [(_ (? tyvar?)) (unify-types ctxt t2 t1)] ;; Flip!

    [(`(Array ,n1 ,e1) `(Array ,n2 ,e2))

     (match/values (values (collapse n1) (collapse n2))
       ;; Attempt to improve error messages here:
       #;
       [((? number?) (? number?))
        (raise-syntax-error #f "")]
       [(_ _)      
        `(Array ,(unify-types ctxt n1 n2)
                ,(unify-types ctxt e1 e2))])]
       
    [(`(-> ,as ...) `(-> ,bs ...))
     `(-> ,@(for/list ([a as] [b bs])
              (unify-types ctxt a b)))]
    
    [((vector as ...) (vector bs ...))
     (for/vector ([a as] [b bs])
       (unify-types ctxt a b))]
    
    [((? acc-element-type?) (? acc-element-type?))
     #:when (equal? t1 t2)
     t1]
    
    [((? exact-nonnegative-integer?) (? exact-nonnegative-integer?))
     #:when (equal? t1 t2)
     t1]
    
    [(_ _)
     (raise-syntax-error
      'unify-types
      (format (string-append "Conflicting types.\n"
                             "Found: ~a\n"
                             "Expected: ~a\n")
              t1 t2)
      ctxt)]))

;; A method of unifying an arrow type that yields better, more
;; localized error messages.  Specifically, it is better to highlight
;; the argument of the wrong type than to highlight the whole application.
(define (gentle-unify-arrow fnty fnstx ls)
  ;; FINISHME: can do better here.
  (unify-types fnstx fnty `(-> ,@(map car ls)))
  #;
  (match fnty
    [`(-> ,args ... ,res) ...]
    [(? tyvar) (unify-types fnstx fnty `(-> ,@(map car ls)))]
    [else (raise-syntax-error ...)])    
  )
  

;; Var instantiated-type? -> boolean?
(define/contract (occurs-check stx var type)
  (-> syntax? symbol? any/c void?)
  ; (printf "CHECk occurs ~a ~a, free ~a\n" var type (set->list (free-vars type)))
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
                map zipwith fold stencil3x3 generate replicate
                lambda let if vector vector-ref)

    ;; Literal data:
    ;[n:number  (values (acc-element->type (syntax->datum #'n)) #'n)]
    ;[b:boolean (values 'Bool #'n)]
    ; [#(e* ...)]
    
    [n:acc-element-literal (values (acc-element->type (syntax->datum #'n)) #'n)]

    [x:identifier
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
       [ity (values (instantiate-scheme ity)
                    #'x)])]
    
    ;; Other features, Coming soon:
    [(: e t:acc-type)
     (define-values (ty e2) (infer #'e tenv))
     (values (unify-types stx ty (instantiate (syntax->datum #'t)))
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
     (let ((arrty (unify-types #'e1 ty1 `(Array ,e2slen ,(fresh-tyvar 'elt)))))
       (match (collapse arrty)
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
                 (values elt
                   (let ([e2news
                        (for/list ([e2 e2ls])
                          (define-values (ty enew) (infer e2 tenv))
                          (unify-types e2 ty 'Int)
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
            (format "array reference of non-array type ~a" ty1)
            #'e1)]))]

    ;; Method one, don't match bad params:
    [(lambda (x:acc-lambda-param ...) e)
     (define xs (syntax->list #'(x.name ...)))
     (define freshes (build-list (length xs)
                                 (lambda (_) (fresh-tyvar))))

     ;; Enforce the user type annotations BEFORE normal inference.
     ;; The main reason for this order is that it may prevent an Array
     ;; dimension ambiguity until we fix those constraints to be deferred.
     (for ([x xs] [infrd freshes]
           [expected (syntax->datum #'(x.type ...))])
       (when expected
         (unify-types x infrd (instantiate expected))))
     
     (define tenv2 (for/fold ([te tenv])
                             ([x xs]
                              [fresh freshes])
                     (tenv-set te x (make-mono-schema fresh))))
     (define-values (tbod bod) (infer #'e tenv2))     
     (values `(-> ,@freshes ,tbod)
             ;; TODO: must return annotated here:
             #`(lambda (x.name ...) #,bod))]
    
    ;; Generate gets its own typing judgement.  It can't go in the prim table.
    [(generate f e* ...)
     (define es (syntax->list #'(e* ...)))
     (define-values (fty fnew) (infer #'f tenv))
     (define-values (etys news) (infer-list es))
     (define res (fresh-tyvar 'res))
     (define ufty `(-> ,@etys ,res))
     (unify-types #'f fty ufty)
     (for ([e es] [ety etys])
       (unify-types e ety 'Int))
     (values `(Array ,(length es) ,res)
	     #`(generate #,fnew #,@news))]             

    ;; Replicate gets its own typing judgement.
    [(replicate (v* ...) (e* ...) arr)
     (define-values (arrty newArr) (infer #'arr tenv))
     (define vs (syntax->list #'(v* ...)))
     (define es (syntax->list #'(e* ...)))
     (define ntv (fresh-tyvar 'n))
     (define elt (fresh-tyvar))
     (unify-types #'arr arrty `(Array ,ntv ,elt))
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
	   (match (collapse ntv)
	     [n #:when (number? n)
		(values `(Array ,(+ plus-dim n) ,elt)
			#`(replicate #,vs #,es #,newArr))]
	     [other (raise-syntax-error pass-name
					(string-append
					 "Replicate is expected to take an array of known dimension.\n"
					 "Expected non-negative integer dimension, instead found "
					 (if (symbol? other)
					     (format "type variable, '~a'\n" other)
					     (format "unexpected type, '~a'\n" other))
					 (format "The input to fold had type: ~a" `(Array ,other ,(collapse elt)))
					 )
					stx
					)])))]
		

    ;; Fold gets its own typing judgement.  It can't go in the prim table.
    [(fold f zer arr)
     (define-values (arrty newArr) (infer #'arr tenv))
     (define-values (fty newF)     (infer #'f   tenv))
     (define-values (zerty newZer) (infer #'zer tenv))

     (define ntv (fresh-tyvar 'n))
     (define elt (fresh-tyvar))
     
     (gentle-unify-arrow fty #'f
                         `((,zerty . ,#'zer)
                           (,zerty . ,#'zer)
                           (,zerty . ,#'zer)))
     (unify-types #'arr arrty `(Array ,ntv ,elt))

     ;; FIXME: We should defer the final check that the dimensions are concrete.
     ;; Otherwise, whether it works can depend on the order of type inference.
     (match (collapse ntv)
       [n #:when (number? n)
          (values `(Array ,(sub1 n) ,elt)
                  #`(fold #,newF #,newZer #,newArr))]
       [other (raise-syntax-error pass-name
               (string-append 
                "Fold is expected to take an array of known dimension.\n"
                "Expected non-negative integer dimension, instead found "
                (if (symbol? other)
                    (format "type variable, '~a'\n" other)
                    (format "unexpected type, '~a'\n" other))
                (format "The input to fold had type: ~a" `(Array ,other ,(collapse elt)))
                )
               stx
               )])]

    [(if e1 e2 e3)
     (define-values (e1ty newe1) (infer #'e1 tenv))
     (define-values (e2ty newe2) (infer #'e2 tenv))
     (define-values (e3ty newe3) (infer #'e3 tenv))
     (unify-types #'e1 e1ty 'Bool)
     (values (unify-types #'e3 e3ty e2ty)
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
               (unify-types x ty expected)
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
         (format "This is expected to have a vector type of known length, instead found: ~a" oth)
         #'e1)])]
    
    #|
    [(rator e ...)
     ;; It's never a good error message when we treat a keyword like a rator:
     #:when (not (memq (syntax->datum #'rator) acc-keywords-sexp-list))
     #`(#,(loop #'rator) #,@(r:map loop (syntax->list #'(e ...))))]
    |#

    ;; FIXME: this can become the general application case:
    ;; This could be handled through the tenv:
    [(p:acc-primop args ...)
     (define primty (instantiate (tenv-ref acc-primop-types #'p)))
     (define-values (argtys newargs) (infer-list (syntax->list #'(args ...))))
     (define fresh (fresh-tyvar 'res))
     (unify-types stx primty `(-> ,@argtys ,fresh))
     (values fresh
             #`(p #,@newargs))]
    
     ))

;; generalize: set(type var) -> type -> scheme
(define (generalize tenv mono)
  ;; FIXME: this needs to take the environment free vars and set-difference it.
  (make-type-schema (free-vars mono) mono))

(define (tenv-free-vars te)
  'FINISHME)


;; instantiate: scheme -> type
(define/contract (instantiate-scheme scheme)
  (-> type-schema? instantiated-type?)
  (match-define (type-schema vars monoty) scheme)
  (for/fold ([ty monoty])
            ([q  vars])
    (define fresh (fresh-tyvar q))
    (subst ty q fresh)))
  
(define/contract (instantiate mono)
  (-> acc-type? instantiated-type?)
  (instantiate-scheme
   (make-type-schema (free-vars mono) mono)))

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
    [(? tyvar? tv)
     (if (tyvar-ptr tv)
         (set-tyvar-ptr! tv (go (tyvar-ptr tv)))
         tv)]
    ))


(module+ test

  (check-equal? (free-vars '(-> a (-> b a)))
                (list->seteq '(a b)))

  (test-case "instantiate"
    (instantiate-scheme (make-type-schema (list->seteq '(a b))
                                          '(-> (-> a b) (Array n a) (Array n b))))
    (void))
)
