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
 typecheck-expr
  unify-types
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
         (only-in accelerack/private/syntax acc-array : acc-primop-types acc-primop
                  acc-lambda-param acc-type)
         (only-in accelerack/private/wrappers acc-array-ref acc-array-flatref
                  zipwith fold stencil3x3 generate)

         (for-template (except-in racket/base map))
         (for-template (only-in accelerack/private/wrappers acc-array-ref
                                map zipwith fold stencil3x3 generate))
         (for-template (only-in accelerack/private/syntax acc-array : use
                                acc-lambda-param acc-type))
         )

;; For error messages:
(define pass-name 'accelerack-typecheck)

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
    numeric        ;; boolean?
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



;; ------------------------------------------------------------


;; The full type-checking pass.
;; Returns two values:
;;   (1) principal type of expression
;;   (2) fully annotated expression
(define (typecheck-expr syn-table e)
  (pass-output-chatter 'typecheck-expr e)
  (reset-var-cnt)

  (define env0
    (for/fold ([env empty-tenv])
              ([(id ty) (in-dict acc-primop-types)])
      ;; TODO: could store type schemas a priori:
      (tenv-set env id (generalize ty))))
  (define env1
    (for/fold ([env env0])
              ([pr syn-table])
      (match pr
        [`(,v . ,(acc-syn-entry ty expr))
         ;; TODO: could store type schemas a priori:
         (tenv-set env v (generalize ty))])))
  ;; Rip out the type variable stuff:
  (define-values (ty e2) (infer e env1))
  (values (collapse ty) e2))


;; instantiated-type? instantiated-type? -> instantiated-type?
;; If one of the two is "expected", it should be the latter.
(trace-define (unify-types ctxt t1 t2)
  (match/values (values t1 t2)
    ;; Variables trivially unify with themselves:
    [((? tyvar?) (? tyvar?))
     #:when (eq? (tyvar-name t1) (tyvar-name t2))
     t1]
     
    [((? tyvar?) _)
     (printf "unify:  ~a  -> ~a\n" (tyvar-name t1) t2)
     ;; TODO: occurs check here!!
     (check-occurs ctxt (tyvar-name t1) t2)
     (set-tyvar-ptr! t1 
                     (if (tyvar-ptr t1)                         
                         (unify-types ctxt (tyvar-ptr t1) t2)
                         t2))
     t1]
    [(_ (? tyvar?)) (unify-types ctxt t2 t1)] ;; Flip!

    [(`(Array ,n1 ,e1) `(Array ,n2 ,e2))
     `(Array ,(unify-types ctxt n1 n2)
             ,(unify-types ctxt e1 e2))]
    
    [(`(-> ,as ...) `(-> ,bs ...))
     `(-> ,@(for/list ([a as] [b bs])
              (unify-types ctxt a b)))]
    
    [((vector as ...) (vector bs ...))
     (for/vector ([a as] [b bs])
       (unify-types ctxt a b))]
    
    [((? acc-element-type?) (? acc-element-type?))
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
  ;; FINISHME:
  (unify-types fnstx fnty `(-> ,@(map car ls)))
  #;
  (match fnty
    [`(-> ,args ... ,res) ...]
    [(? tyvar) (unify-types fnstx fnty `(-> ,@(map car ls)))]
    [else (raise-syntax-error ...)])    
  )
  

;; Var instantiated-type? -> boolean?
(define/contract (check-occurs stx var type)
  (-> syntax? symbol? any/c void?)
  (printf "CHECk occurs ~a ~a, free ~a\n" var type (set->list (free-vars type)))
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
              (cons xty xs))))
   (values (reverse ls1) (reverse ls2)))
    
  (syntax-parse stx
    #:literals (use acc-array acc-array-ref :
                ;; FIXME: some of these can just be removed when they go to the prim table:
                map zipwith fold stencil3x3 generate
                lambda let if vector vector-ref)

    [n:number  (values (acc-element->type (syntax->datum #'n)) #'n)]
    [b:boolean (values 'Bool #'n)]

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
    [(: e t:acc-type)  (raise-syntax-error pass-name "ascription (:) form not yet supported." stx)]
    [(use x:id t:acc-type) (raise-syntax-error pass-name "use form not yet supported." stx)]
    [(use x:id)            (raise-syntax-error pass-name "use form not yet supported." stx)]
    ; [(use x:id t)          (raise-syntax-error #f "bad type in use form" stx)]

    [(acc-array dat) (values (syntax->datum (infer-array-type #'dat))
                             #'(acc-array dat))]

    [(acc-array-ref e1 e2s ...)
     (define-values (ty1 e1b) (infer #'e1 tenv))
     (define e2ls (syntax->list #'(e2s ...)))
     (define e2slen (length e2ls))
     (match ty1
       [`(Array ,dim ,elt)
        (cond
          [(symbol? dim)
           (raise-syntax-error 'acc-array-ref
            (format "array-ref expected an array with known-dimension ~a, instead found type variable ~a"
                    e2slen dim)
            #'e1)]
          [(exact-nonnegative-integer? dim)
           (if (equal? dim e2slen)
               (values elt
                 (let ([e2news
                      (for/list ([e2 e2ls])
                       (define-values (ty enew) (infer e2 tenv))
                       (if (eq? ty 'Int) enew
                           (raise-syntax-error 'acc-array-ref
                             (format "expected all index expressions to have type Int, instead found ~a" ty)
                             #'e2)))])
                  #`(acc-array-ref #,e1b #,@e2news)))
               (raise-syntax-error 'acc-array-ref
                                   (format "this array has dimension ~a, but ~a indices were provided"
                                           dim e2slen)
                                   #'e1))])]
       [else
        (raise-syntax-error 'acc-array-ref
          (format "array reference of non-array type ~a" ty1)
          #'e1)])]

    ;; FIXME: this can become the general application case:
    ;; This could be handled through the tenv:
    [(p:acc-primop args ...)
     (define primty (instantiate (tenv-ref acc-primop-types #'p)))
     (define-values (argtys newargs) (infer-list (syntax->list #'(args ...))))
     (define fresh (fresh-tyvar 'res))
     (unify-types stx primty `(-> ,@argtys ,fresh))
     (values fresh
             #`(p #,@newargs))]

    ;; Method one, don't match bad params:
    [(lambda (x:acc-lambda-param ...) e)
     (define xs (syntax->list #'(x ...)))
     (define freshes (build-list (length xs)
                                 (lambda (_) (fresh-tyvar))))
     (define tenv2 (for/fold ([te tenv])
                             ([x xs]
                              [fresh freshes])
                     (tenv-set te x (make-mono-schema fresh))))
     (define-values (tbod bod) (infer #'e tenv2))
     (values `(-> ,@freshes ,tbod)
             ;; TODO: must return annotated here:
             #`(lambda (x.name ...) #,bod))]
    
    ;; Generate gets its own typing judgement.  It can't go in the prim table.
;    [(generate f es ...)
;    #`(generate )]

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
    
    #|

    [(map f e) #`(map #,(loop #'f) #,(loop #'e))]
    [(zipwith f e1 e2) #`(zipwith #,(loop #'f) #,(loop #'e1) #,(loop #'e2))]

    [(stencil3x3 f e1 e2) #`(stencil3x3 #,(loop #'f) #,(loop #'e1) #,(loop #'e2))]

    [(if e1 e2 e3) #`(if #,(loop #'e1) #,(loop #'e2) #,(loop #'e3))]

    [#(e* ...)
     (datum->syntax #'(e* ...) (list->vector (r:map loop (syntax->list #'(e* ...)))))]

    [(let ( lb:acc-let-bind ...) ebod)
     (define xls (syntax->list #'(lb.name ...)))
     (define els (syntax->list #'(lb.rhs ...)))
     #`(let ([lb.name #,(r:map loop els)] ...)
         #,(verify-acc-helper
            #'ebod (extend-env xls env)))]

    [(vector e ...)     #`(vector #,@(r:map loop (syntax->list #'(e ...))))]
    [(vector-ref e1 e2) #`(vector-ref #,(loop #'e1) #,(loop #'e2))]

    ;; We have to to be careful with how we influence the back-tracking search performed by
    ;; syntax-parse.
    [(p:acc-primop e ...)
     #`(p #,@(r:map loop (syntax->list #'(e ...))))]

    [(rator e ...)
     ;; It's never a good error message when we treat a keyword like a rator:
     #:when (not (memq (syntax->datum #'rator) acc-keywords-sexp-list))
     ; (printf "RECURRING ON APP, rator ~a ~a \n" #'rator (syntax->datum #'rator))
     #`(#,(loop #'rator) #,@(r:map loop (syntax->list #'(e ...))))]

    [p:acc-primop #'p]

    ;; If we somehow mess up the imports we can end up with one of the keywords UNBOUND:
    [keywd:id
     #:when (and (not (identifier-binding #'x))
                 (memq (syntax->datum #'keywd)
                       acc-keywords-sexp-list))
     (raise-syntax-error
      'error  "Accelerack keyword used but not imported properly.  Try (require accelerack)" #'keywd)]

   [x:identifier ...]
|#
     ))

(check-equal? (let-values ([(ty expr) (infer #'(acc-array-ref (acc-array ((9.9))) 0 0) empty-tenv)])
                ty)
              'Double)


;; FIXME: this needs to take the environment and set-difference it.
;;
;; generalize: set(type var) -> type -> scheme
(define (generalize mono)
  (make-type-schema (free-vars mono) mono))

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

(check-equal? (free-vars '(-> a (-> b a)))
              (list->seteq '(a b)))

;; instantiate: scheme -> type
(define/contract (instantiate-scheme scheme)
  (-> type-schema? instantiated-type?)
  (match-define (type-schema vars monoty) scheme)
  (for/fold ([ty monoty])
            ([q  vars])
    (define fresh (make-tyvar #f q (numeric-type-var? q)))
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

(test-case "instantiate"
  (instantiate-scheme (make-type-schema (list->seteq '(a b))
                                        '(-> (-> a b) (Array n a) (Array n b)))))

#|

;; The full type-checking pass.
;; Returns two values:
;;   (1) principal type of expression
;;   (2) fully annotated expression
(define (typecheck-expr syn-table e)
  (pass-output-chatter 'typecheck-expr e)
  (with-handlers ()
    (infer e syn-table)))


;; ------------------------ SOLVER and UNIFYIER related stuff ------------------------


(define (sub-arr-helper s val)
  (match val
    [(? null?) val]
    [(? pair?) #:when(not (eq? '-> (car val))) (cons (sub-arr-helper s (car val)) (sub-arr-helper s (cdr val)))]
    [else (substitute s val)]))

;; Substitution Type -> Type
(define (substitute s type)
  (cond
    [(type_con? type) type]
    [(type_var? type) (dict-ref s type type)]
    [(type_array? type) `(,(car type) ,(cadr type) ,(sub-arr-helper s (last type)))]
    [(type_fun? type) `(-> ,@(map (curry substitute s) (cdr type)))]
    ;; TODO fix
    [(vector? type) type]
    [else (raise-syntax-error 'substitute (format "Unknown type: ~a" type))]))


;; unify : type type -> ?
(define (unify t1 t2)
  (cond
    [(and (pair? t1) (pair? t2))
     (match-let ((`(-> . ,t1-types) t1)
                 (`(-> . ,t2-types) t2))
       (if (not (eq? (length t1-types) (length t2-types)))
           (error "Types ~a and ~a are incompatible" t1 t2)
           (foldl (lambda (p1 p2 s)
                    (set-union (unify (substitute s p1) (substitute s p2)) s))
                  '() t1-types t2-types)))]
    [(equal? t1 t2) '()]
    [(type_var? t1) (occurs-check t1 t2)]
    [(type_var? t2) (occurs-check t2 t1)]
    [else (raise-syntax-error 'unify (format "Can't Unify t1: ~s and t2: ~s" t1 t2))]))

(define (subs-union subs1 subs2)
  (let ((s (map (lambda (v)
                  (cons (car v) (substitute subs1 (cdr v)))) subs2)))
    (foldl (lambda (v res)
             (when (dict-ref subs2 (car v) #f)
               (raise-syntax-error 'subs-union "Substitutions with same type vars"))
             (set! s (cons v s))) '() subs1) s))
;;  substitution -> constraint -> constraint
(define (sub_constraint s constraint)
  (match constraint
    [`(== ,v1 ,v2) `(== ,(substitute s v1) ,(substitute s v2))]
    [`(implicit ,v1 ,v2 ,v3) `(implicit
                               ,(substitute s v1)
                               ,(substitute s v2)
                               ,(for/set ([var v3])
                                  (dict-ref s var var)))]
    [`(explicit ,v1 ,v2) `(explicit ,(substitute s v1) ,(substitute s v2))]))

(define (solve constraints)
  (cond
    [(empty? constraints) '()]
    [else (let ((constraint (car constraints)))
            (match constraint
              [`(== ,t1 ,t2) (let ((s (unify t1 t2)))
                               (subs-union (solve (map (curry sub_constraint s) (cdr constraints))) s))]
              [`(implicit ,t1 ,t2 ,monos) (if (set-empty? (set-intersect
                                                           (set-subtract (free_vars t2) monos)
                                                           (active_vars (cdr constraints))))
                                              (solve (cons `(explicit ,t1 ,(generalize monos t2))
                                                           (cdr constraints)))
                                              (solve (append (cdr constraints) `(,constraint))))]
              [`(explicit ,t ,s) (solve (cons `(== ,t ,(instantiate s)) (cdr constraints)))]))]))

;; ------------------------ END SOLVER STUFF ----------------------------
(define (str->sym val)
  (match val
    [(? string?) (string->symbol val)]
    [(? list?) (map str->sym val)]
    [else val]))

(define (annotate-type ty subs)
  (match ty
    [`(-> . ,types) `(-> . ,(map (curryr annotate-type subs) types))]
    [else (let ([f (assoc ty subs)])
            (if f (str->sym (cdr f)) (str->sym ty)))]))

(define (annotate-expr type-expr subs)
  (match type-expr
    [x #:when (or (symbol? x) (number? x) (boolean? x) (vector? x)) type-expr]
    [`(,x : ,ty) `(,(annotate-expr x subs) : ,(annotate-type ty subs))]
    [`(lambda ,x ,b) `(lambda ,(foldr (lambda (val res)
                                         (append (annotate-expr val subs) res)) '() x)
                        ,(annotate-expr b subs))]
    [`(let ,vars ,b) `(let ,(foldr (lambda (var res)
                                     (match-let ([`(,x ,e) var])
                                       (append `((,@(annotate-expr x subs)
                                                  ,(annotate-expr e subs))) res))) '() vars)
                        ,(annotate-expr b subs))]
    [`(map ,fun ,arr) `(map ,(annotate-expr fun subs) ,(annotate-expr arr subs))]
    [`(fold ,fun ,res ,arr) `(fold ,(annotate-expr fun subs) ,(annotate-expr res subs) ,(annotate-expr arr subs))]
    [`(,rator . ,rand) `(,(annotate-expr rator subs)
                         ,@(map (curryr annotate-expr subs) rand))]
    [else (raise-syntax-error 'error "Unable to annotate expression for ~a => ~a" type-expr subs)]))


;; syntax -> box list -> type
(define (infer-types e env)
  (match e
    [(? symbol?) (infer-var e env)]
    [`(lambda ,x ,b) (infer-lambda e env)]
    [`(let ,vars ,b) (infer-let e env)]
    [`(fold ,fun ,res ,arr) (infer-fold e env)]
    [`(map ,fun ,arr) (infer-map e env)]
    [`(: ,e ,t0) (infer-asc e t0 env)]
    [`(use ,e ,t0) (infer-use e t0 env)]
    [`(if ,cnd ,thn ,els) (infer-cond e env)]
    [`(acc-array ,ls) (infer-record (mutable-set) (mutable-set) (infer-lit e) e)]
    [(? acc-scalar?) (infer-record (mutable-set) (mutable-set) (infer-lit e) e)]
    [`(,rator . ,rand) (infer-app e env)]
    [else (raise-syntax-error 'infer-types (format "unhandled syntax: ~a" e) #'e)]))

|#



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
     (make-tyvar #f sym #f)]))
