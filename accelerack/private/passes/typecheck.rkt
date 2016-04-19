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
                     syntax/parse)
         ; syntax/parse
         racket/trace
         syntax/to-string
         rackunit rackunit/text-ui
         (only-in accelerack/private/utils pass-output-chatter)
         (only-in accelerack/private/types acc-scalar? acc-int? acc-type? acc-syn-entry)
         )

;; Data type definitions and predicates:

;; TypeEnv:
;; map from:  TermVariable -> type-schema?


;; The full type-checking pass.
;; Returns two values:
;;   (1) principal type of expression
;;   (2) fully annotated expression
(define (typecheck-expr syn-table e)
  (pass-output-chatter 'typecheck-expr e)
  (with-handlers ()
    (infer e syn-table)))

;; RRN: TODO Eliminate syn-table call from infer.  It should take only
;; expression and type-environment.

(define (unify-types ty1 ty2)
  ;; FINISHME
  ;;(unify ty1 ty2)
#t)

;; Typing environment:
(define type-env
  '(
    [map  (-> (-> a b) (Array (Num n) a) (Array (Num n) b))]
    ;; ^ PLUS side condition that a/b don't contain Array
    [fold (-> (-> a a a) a
	      (Array (add1 (Num n)) a)
	      (Array (Num n) b))]

    ;; Shorthands for convenience and simplicity:
    [fold1 (-> (-> a a a) a (Array 1 a) (Array 0 b))]
    [fold2 (-> (-> a a a) a (Array 2 a) (Array 1 b))]

					; (generate (lambda () 99))
					; (generate 3 (lambda (i) i))
					; (generate 3 4 (lambda (x y) (+ x y)))
    ;; Psuedo-syntax for the type:
    [generate (-> Int_1 ... Int_n (-> Int_1 ... Int_n a) (Array n a))]

    ))

(define environment
  '((+ . (-> "t1" "t1" "t1"))
    (add1 . (-> "ta" "ta"))
    (- . (-> "t2" "t2" "t2"))
    (sub1 . (-> "ts" "ts"))
    (* . (-> "t3" "t3" "t3"))
    (/ . (-> "t4" "t4" "t4"))
    (< . (-> "t5" "t5" Bool))
    (= . (-> "t6" "t6" Bool))
    (eq? . (-> "t7" "t7" Bool))))


;; ---------------- Persistent variables and typevariable related stuff ---------------------
(define var-cnt 0)
(define (reset-var-cnt) (set! var-cnt 0))
(define (fresh var)
  (set! var-cnt (+ var-cnt 1))
  (string-append (if (symbol? var)
                     (symbol->string var)
                     (if (syntax? var) (symbol->string (syntax->datum var)) var))
                 (number->string var-cnt)))
;; free variables: type -> set
;; Fetches all the variables in the input given
(define (free_vars t)
  (cond [(or (type_array? t) (type_var? t)) (set t)]
        [(type_fun? t) (let ([in-types (drop-right (cdr t) 1)]
                             [ret-type (last t)])
                         (set-union (list->set (map free_vars in-types))
                                    (free_vars ret-type)))]
        [(type_con? t) (set)]
        [else (raise-syntax-error 'free_vars (format "Unknown type for ~s" t) #`#,t)]))


;; active variables: constraints -> set(type var)
(define (active_vars constraints)
  ;(print constraints)
  (foldl (lambda (constraint res)
           (match constraint
             [`(== ,v1 ,v2) (set-union (free_vars v1) (free_vars v2) res)]
             [`(implicit ,v1 ,v2 ,v3) (set-union (free_vars v1) (set-intersect v3 (free_vars v2)) res)]
             [`(explicit ,v1 ,v2) (set-union (free_vars v1) (free_vars v2) res)]))
         (set) constraints))

;; ---------------- Struct Record ----------------------------
(struct infer-record ([assumptions #:mutable]
                      [contraints #:mutable]
                      type
                      type-expr)
  #:transparent
  #:guard (lambda (as con t te type-name)
            (cond
              [(not (set-mutable? as))
               (raise-syntax-error type-name (format "Assumptions: ~e has to be of the type mutable-set" as))]
              [(not (set-mutable? con))
               (raise-syntax-error type-name (format "Constraints: ~e has to be of the type mutable-set" con))]
              [else (values as con t te)])))

;; ------------------------ Classifiers ------------------------
(define type_var? string?)
(define type_con? symbol?)
(define type_array? (lambda (x)
                      (match x
                        [`(Array ,len ,ls) #t]
                        [else #f])))
(define (type_fun? type)
  (match type
    [`(-> . ,t1) #t]
    [else #f]))
(define (type_scheme? type) (and (pair? type) (eq? (car type) 'scheme)))
;; ------------------------ Actual Type inference ------------------------



; [Var]
; infer-var: Variable -> InferRecord
(define (infer-var x env)
  (let ((simple-type (or (dict-ref environment x #f)
                         (let ([prev-acc (assoc x env)])
                           ;;(displayln env)
                           ;;(displayln prev-acc)
                           (if prev-acc
                               (cdr prev-acc)
                               ;;(displayln ty)
                               ;(acc-syn-entry-type (cdr prev-acc))
                               #f)))))
    (if simple-type
        (infer-record (mutable-set) (mutable-set) simple-type x)
        (let ([var (fresh x)])
          (infer-record (mutable-set (cons x var))
                        (mutable-set)
                        var
                        x)))))

(define (get-acc-array-type x n)
  (cond
    [(null? x) (raise-syntax-error 'infer-lit (format "Empty list in acc-array not permitted " x) #`#,x)]
    [(and (list? x) (null? (cdr x))) (get-acc-array-type (car x) (add1 n))]
    [(list? x) (foldr (lambda (x y) (if (equal? x y) x #f))
                      (get-acc-array-type (car x) (add1 n))
                      (map (lambda(x) (get-acc-array-type x (add1 n))) (cdr x)))]
    [else `(,n ,(infer-lit x))]))

(define (infer-lit exp)
  (match exp
    [(? flonum?) 'Double]
    [(? acc-int?) 'Int]
    [(? boolean?) 'Bool]
    [`(acc-array) (raise-syntax-error 'infer-lit (format "Empty list in acc-array not permitted " exp) #`#,exp)]
    [`(acc-array ,ls)
     (let ((type (get-acc-array-type ls 0)))
       (if type
           `(Array ,@type)
           (raise-syntax-error 'infer-lit (format "All-vars of Acc-array should be of same type: ~a " exp) #`#,exp)))]
    ;; Supporting tuples
    [`#(,x ...)  `,(list->vector (map infer-lit x))]
    [else (raise-syntax-error 'infer-lit (format "~a is not supported yet." exp) #`#,exp)]))

(define (val-fold-fun fun env)
  (match fun
    [`(lambda ,params ,body) (if (eq? 2 (length (car (get-vars params '() '()))))
                                 (infer-types fun env)
                                 (raise-syntax-error 'infer-fold "Function Params cant be more than 2" #`#,fun))]
    [else (infer-types fun env)]))

;;((-> a a a) a (Array (add1 n) a) (Array n a))
(define (infer-fold e env)
  (match-define `(fold ,fun ,res ,arr) e)
  (match-define (infer-record a0 c0 t0 te0) (val-fold-fun fun env))
  (match-define (infer-record a1 c1 t1 te1) (infer-types res env))
  (match-define (infer-record a2 c2 t2 te2) (infer-types arr env))
  (match-define `(-> ,a3 ,a4 ,a5) t0)
  (match-define `(Array ,n ,ty) t2)
  (set-union! a0 a1 a2)
  (set-union! c0 c1 c2 (set `(== ,a3 ,a4)) (set `(== ,a4 ,a5)) (set `(== ,a3 ,ty)))
  (infer-record a0 c0 `(Array ,(sub1 n) ,ty) `(fold ,te0 ,te1 ,te2))
  )


;;((-> a b) (Array n a) (Array n b))
(define (infer-map e env)
  (match-define `(map ,fun ,arr) e)
  (match-define (infer-record a0 c0 t0 te0) (infer-types fun env))
  (match-define (infer-record a1 c1 t1 te1) (infer-types arr env))
  (match t0
     [`(-> ,a ,b) (void)]
     [else (raise-syntax-error 'infer-map (format "The first parameter of map should be a function : ~a" fun) #`#,fun)])
  (match t1
     [`(Array ,n ,ty) (void)]
     [else (raise-syntax-error 'infer-map (format "Invalid array format : ~a. Should be an acc-array." arr) #`#,arr)])
  (match-define `(-> ,a ,b) t0)
  (match-define `(Array ,n ,ty) t1)
  (set-union! a0 a1)
  (set-union! c0 (set `(== ,a ,ty)) c1)
  (infer-record a0 c0 `(Array ,n ,b) `(map ,te0 ,te1)))

(define (infer-use e t0 env)
  (match-define (infer-record a1 c1 t1 te1) (infer-types e env))
  (set-union! c1 (set `(== ,t1 ,t0)))
  (infer-record a1 c1 t1 `(use ,te1 ,t0)))


(define (infer-asc e t0 env)
  (match-define (infer-record a1 c1 t1 te1) (infer-types e env))
  (set-union! c1 (set `(== ,t1 ,t0)))
  (infer-record a1 c1 t1 te1))

;; Infer type for if statement
(define (infer-cond e env)
  (match-define `(if ,cnd ,thn ,els) e)
  (match-define (infer-record ac cc tc tec) (infer-types cnd env))
  (match-define (infer-record at ct tt tet) (infer-types thn env))
  (match-define (infer-record ae ce te tee) (infer-types els env))
  (set-union! ac at ae)
  (set-union! cc ct ce (set `(== ,tt ,te)))
  (infer-record ac cc tt `(if ,tec ,tet ,tee)))

; [App] : Exp Environment -> InferRecord
(define (infer-app exp env)
  (let ((e1 (car exp))
        (args (cdr exp))
        (typevar (fresh "app")))
    (match-define (infer-record a1 c1 t1 te1) (infer-types e1 env))
    (let ([te1 `(,te1)])
      (define argtypes
        (for/list [(arg args)]
          (match-define (infer-record a2 c2 t2 te2) (infer-types arg env))
          ;(set-add! a1 typevar)
          (set-union! a1 a2)
          (set-union! c1 c2)
          (set! te1 (append te1 `(,te2)))
          t2))
      (define ret-c
        (if (and (pair? t1) (eq? (car t1) '->))
          (list->set (map (lambda (v1 v2)
                              `(== ,v1 ,v2)) (cdr t1) `(,@argtypes ,typevar)))
          (set `(== ,t1 (-> ,@argtypes ,typevar)))))
      (set-union! c1 ret-c)
      (infer-record a1 c1 typevar te1))))

; [Let]
(define (infer-let exp env)
  (match-define `(let ,vars ,body) exp)
  (match-define (infer-record a1 c1 t1 te1)
    (foldl (lambda (var res)
             (match var
               [`(,x ,e1)
                (match-define (infer-record a c t te) (infer-types e1 env))
                (match-define (infer-record ares cres tres te-res) res)
                (set-union! ares a)
                (set-union! cres c)
                (infer-record ares cres tres (append `([,(list x ': t) ,te]) te-res))]
               [`(,x : ,t0 ,e1)
                (match-define (infer-record a c t te) (infer-types e1 env))
                (match-define (infer-record ares cres tres te-res) res)
                (set-union! ares a)
                (set-union! cres c (set `(== ,t0 ,t)))
                (infer-record ares cres tres (append `([,(list x ': t) ,te]) te-res))]))
           (infer-record (mutable-set) (mutable-set) 'None '()) vars))
  ;;(match-define (infer-record a1 c1 t1 te1) (infer-types e1 env))
  (match-define (infer-record a2 c2 t2 te2) (infer-types body env))
  ;(displayln body)
  ;(displayln te2)
  ;(set-union! a1 a2)
  (set-union! c1 c2)
  (set-for-each a2 (lambda (a)
                     (let ([aval (assoc (car a) (map (lambda (var)
                                                       (match-let ((`((,x : ,t) ,b) var))
                                                         (list x t))) te1))])
                       (if aval
                           (set-add! c1 `(implicit ,(cdr a) ,(last aval) ,env))
                           (set-add! a1 a)))))
  (infer-record a1 c1 t2 `(let ,te1 ,te2)))


(define (get-vars ls vars env)
  (cond
    [(null? ls) (list (reverse vars) env)]
    [else (match (car ls)
            [`(,x : ,t) (get-vars (cdr ls) (cons x vars)
                                  (cons `(,x . ,t) env))]
            [`,x (get-vars (cdr ls) (cons x vars) env)])]))

(define (infer-lambda exp env)
  (match-define `(lambda ,args ,body) exp)
  (let* ((arg-vals (get-vars args '() '()))
         (arg-env (map (lambda (arg)
                         (let ([env-val (assoc arg (last arg-vals))])
                          (cons arg (if env-val
                                        (cdr env-val)
                                        (fresh "arg"))))) (car arg-vals)))
         (arg-vars (map cdr arg-env))
         (c (mutable-set))
         (a2 (mutable-set)))
    (match-define (infer-record a c t e) (infer-types body (append arg-env env)))
    (set-for-each a (lambda (y)
                      (let ((lkp (assoc (car y) arg-env)))
                        (if lkp
                            (set-add! c `(== ,(cdr y) ,(cdr lkp)))
                            (set-add! a2 y)))))
    ;(displayln c)
    (set-union! a2 a)
    (define ret-type `(-> ,@arg-vars ,t))
    (infer-record a2 c ret-type `(lambda ,(foldr (lambda (x res)
                                                    (append `(,(list (car x) ': (cdr x))) res))
                                                 '() arg-env) ,e))
    ))



;; ------------------------ SOLVER and UNIFYIER related stuff ------------------------
;; Var Type -> ((var . type)...)
(define (occurs-check var type)
  (cond
    [(equal? var type) '()]
    ;This is an infinite type. Send an error back
    [(set-member? (free_vars type) var)
     (raise-syntax-error 'occurs-check (format "Occurs check failed, ~a occurs in ~a\n" var type))]
    [else `(,(cons var type))]))

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

;; generalize: set(type var) -> type -> scheme
(define (generalize monos type)
  (list 'scheme (set-subtract (free_vars type) monos) type))

;; instantiate: scheme -> type
(define (instantiate scheme)
  (match-define `(,_ ,qs ,type) scheme)
  (substitute (for/list ([q qs]) (cons q (fresh "I"))) type))

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


(define (infer e syn-table)
  (reset-var-cnt)
  (define env (set-map syn-table (lambda (x)
                                   (match-define (acc-syn-entry ty code) (cdr x))
                                   `(,(syntax->datum (car x)) . ,ty))))
  (match-define (infer-record assumptions constraints type type-expr)
    (infer-types (if (syntax? e)
                     (syntax->datum e)
                     e)
                 env))
  ;;(displayln  (set-map syn-table (lambda (x)
  ;;                                 `(,(syntax->datum (car x)) . ,(cdr x)))))
  ;;(displayln constraints)
  (define substitutions (solve (set->list constraints)))
  ;(displayln substitutions)
  (values (str->sym (substitute substitutions type))
          #`#,(annotate-expr type-expr substitutions)))
