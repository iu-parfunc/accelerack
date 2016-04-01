#lang racket
(require (for-syntax (except-in racket/base map))
         syntax/parse
         syntax/to-string
         scribble/srcdoc
         racket/trace
         racket/set
         racket/dict
         (only-in accelerack/private/global_utils pass-output-chatter)
         accelerack/private/syntax
         accelerack/private/wrappers
         (prefix-in r: racket/base))

(require ;; We use the identifiers from "wrappers" as our names for map/fold/etc
 (for-template
  accelerack/private/wrappers
  ;; Keyword symbols come from a mix of three places currently:
  (only-in accelerack/private/syntax acc-array)
  (only-in racket/base lambda let #%app if + * - / add1 sub1 vector vector-ref)
  (only-in accelerack/private/keywords : Array Int Bool Double use ->)
  )
 (only-in accelerack/private/types acc-scalar? acc-type?))

(provide p*-infer)


;; Datatype definitions:
;; ------------------------------------------
;; A InferRecord is a triple:
;;  - (variable-renames constraints type)

;; An Environment is:
;;  - a set of bound symbols

;; A Constraint is:
;;  - (== type1 type2)
;;  - ('implicit t1 t2 m)
;;  - ('explicit t1 t2)

;; A Substitution is:
;;  - ...
;; ------------------------------------------

(define var-cnt 0)
(define (reset-var-cnt) (set! var-cnt 0))

;; (define (verify-acc syn-table stx)
;;   (define initial-env (make-env (r:map car syn-table)))
;;   (define res (verify-acc-helper stx initial-env))
;;   (pass-output-chatter 'verify-acc res)
;;   res)


;; (define (verify-type ty)
;;   (syntax-parse ty
;;     [t:acc-type  (void)]
;;     [oth (raise-syntax-error 'Accelerack-type "bad type expression" #'oth)]))

(define (make-env ls)
  (unless (andmap identifier? ls)
    (error 'make-env "list contains non-identifiers: ~a\n" ls))
  ls)

(define (extend-env ls env)
  (unless (andmap identifier? ls)
    (error 'extend-env "list contains non-identifiers: ~a\n" ls))
  (append ls env))

(define acc-keywords-sexp-list
  '(lambda if let : use
    generate map zipwith fold generate stencil3x3 acc-array-ref))


;; types
; 1
; 'int
; (-> (t1 ... tn) tr)
(struct infer-record ([assumptions #:mutable]
                      [contraints #:mutable]
                      type
                      type-expr)
  #:transparent
  #:guard (lambda (as con t te type-name)
            (cond
              [(not (set-mutable? as))
               (raise-syntax-error type-name "Assumptions: ~e has to be of the type mutable-set" as)]
              [(not (set-mutable? con))
               (raise-syntax-error type-name "Constraints: ~e has to be of the type mutable-set" con)]
              [else (values as con t te)])))

(define type_var? string?)
(define type_con? symbol?)
(define type_array? (lambda (x)
                      (match x
                        [`(Array ,n ,el) #t]
                        [else #f])))
(define (type_fun? type)
  (match type
    [`(-> . ,t1) #t]
    [else #f]))
(define (type_scheme? type) (and (pair? type) (eq? (car type) 'scheme)))


;; To create a fresh type variable for inference
;; Symbol -> String
(define (fresh var)
  (set! var-cnt (+ var-cnt 1))
  (string-append (if (symbol? var)
                     (symbol->string var)
                     (if (syntax? var) (symbol->string (syntax->datum var)) var))
                 (number->string var-cnt)))


;; type environment
(define (get_primitives e)
  (cond [(number? e) 'Int]
        [(boolean? e) 'Bool]))


;; constraint collector: Output InferRecord -> (assumptions (mutable-set)
;;                                              constraints (mutable-set)
;;                                              type)
(define (infer-types e env)
  (match e
    [`(lambda ,x ,b) (infer-abs e env)]
    [`(let ,vars ,b) (infer-let e env)]
    [(? symbol?) (infer-var e)]
    [`(: ,e ,t0) (infer-asc e t0 env)]
    [`(use ,e ,t0) (infer-use e t0 env)]
    [`(if ,cnd ,thn ,els) (infer-cond e env)]
    [(? acc-scalar?) (infer-lit e)]
    [`(Array ,n ,el) (infer-lit e)]
    [`(,rator . ,rand) (infer-app e env)]
    [else (raise-syntax-error 'infer-types "unhandled syntax: ~a" e)]))
  ;; (syntax-parse e
  ;;   [(lambda (x:acc-lambda-param ...) body) (infer-abs (syntax->datum e) env)]
  ;;   [(let (lb:acc-let-bind ...) body) (infer-let (syntax->datum e) env)]
  ;;   [(rator rand ...) (infer-app (syntax->datum e) env)]
  ;;   [(p:acc-primop val ...) (infer-var (car e))]
  ;;   [x:identifier (infer-var e)]
  ;;   [(~or n:number b:boolean) (infer-lit e)]
  ;;   )

(define (infer-cond e env)
  (match-define `(if ,cnd ,thn ,els) e)
  (match-define (infer-record ac cc tc tec) (infer-types cnd env))
  (match-define (infer-record at ct tt tet) (infer-types thn env))
  (match-define (infer-record ae ce te tee) (infer-types els env))
  (set-union! ac at ae)
  (set-union! cc ct ce (set `(== ,tt ,te)))
  (infer-record ac cc tt `(if ,tec ,tet ,tee)))

(define (infer-use e t0 env)
  (match-define (infer-record a1 c1 t1 te1) (infer-types e env))
  (set-union! c1 (set `(== ,t1 ,t0)))
  (infer-record a1 c1 t1 `(use ,te1 ,t0)))


(define (infer-asc e t0 env)
  (match-define (infer-record a1 c1 t1 te1) (infer-types e env))
  (set-union! c1 (set `(== ,t1 ,t0)))
  (infer-record a1 c1 t1 te1))

; [Var]
; infer-var: Variable -> InferRecord
(define (infer-var x)
  (let ((simple-type (dict-ref environment x #f)))
    (if simple-type
        (infer-record (mutable-set) (mutable-set) simple-type x)
        (let ([var (fresh x)])
          (infer-record (mutable-set (cons x var))
                        (mutable-set)
                        var
                        x)))))



; [Lit] : Exp -> InferRecord
(define (infer-lit exp)
  (let ([t (match exp
             [(? number?) 'Int]
             [(? boolean?) 'Bool]
             [`(Array ,n ,el) `(Array n ,@(map infer-lit el))]
             [else (raise-syntax-error 'infer-lit "This literal is not supported yet: ~a " exp)])])
    (infer-record (mutable-set)
                  (mutable-set)
                  t
                  exp)))


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
      (set-union! c1 (set `(== ,t1 (-> ,@argtypes ,typevar))))
      (infer-record a1 c1 typevar te1))))


; [Abs]
(define (infer-abs exp env)
  (match-define `(lambda ,args ,body) exp)
  (let* ((arg-env (map (lambda (arg)
                         (cons arg (fresh "arg"))) args))
         (arg-vars (map cdr arg-env))
         (c (mutable-set))
         (a2 (mutable-set)))
    ;(displayln arg-env)
    ;(displayln arg-vars)
    (match-define (infer-record a c t e) (infer-types body (set-union env (list->set args))))
    ;(display a)
    ;(displayln c)
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
                                                  '() arg-env) ,e))))


; [Let]
(define (infer-let exp env)
  (match-define `(let ,vars ,body) exp)
  (match-define (infer-record a1 c1 t1 te1)
    (foldl (lambda (var res)
             (match-let ([`(,x ,e1) var])
               (match-define (infer-record a c t te) (infer-types e1 env))
               (match-define (infer-record ares cres tres te-res) res)
               (set-union! ares a)
               (set-union! cres c)
               (infer-record ares cres tres (append `([,(list x ': t) ,te]) te-res))))
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


;; Solver: list(constraint) -> subsitution
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



(define (subs-union subs1 subs2)
  (let ((s (map (lambda (v)
                  (cons (car v) (substitute subs1 (cdr v)))) subs2)))
    (foldl (lambda (v res)
             (when (dict-ref subs2 (car v) #f)
               (raise-syntax-error 'subs-union "Substitutions with same type vars"))
             (set! s (cons v s))) '() subs1) s))


;; Substitution Type -> Type
(define (substitute s type)
  (cond
    [(or (type_con? type) (type_array? type)) type]
    [(type_var? type) (dict-ref s type type)]
    [(type_fun? type) `(-> ,@(map (curry substitute s) (cdr type)))]
    [else (raise-syntax-error 'substitute (format "unknown type: ~a" type))]))


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


;; free variables: type -> set
;; Fetches all the variables in the input given
(define (free_vars t)
  (cond [(or (type_array? t) (type_var? t)) (set t)]
        [(type_fun? t) (let ([in-types (drop-right (cdr t) 1)]
                             [ret-type (last t)])
                         (set-union (list->set (map free_vars in-types))
                                    (free_vars ret-type)))]
        [(type_con? t) (set)]
        [else (raise-syntax-error 'free_vars (format "Unknown type for ~s" t))]))


;; active variables: constraints -> set(type var)
(define (active_vars constraints)
  ;(print constraints)
  (foldl (lambda (constraint res)
           (match constraint
             [`(== ,v1 ,v2) (set-union (free_vars v1) (free_vars v2) res)]
             [`(implicit ,v1 ,v2 ,v3) (set-union (free_vars v1) (set-intersect v3 (free_vars v2)) res)]
             [`(explicit ,v1 ,v2) (set-union (free_vars v1) (free_vars v2) res)]))
         (set) constraints))

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

;; Var Type -> ((var . type)...)
(define (occurs-check var type)
  (cond
    [(equal? var type) '()]
    ;This is an infinite type. Send an error back
    [(set-member? (free_vars type) var)
     (raise-syntax-error 'occurs-check "Occurs check failed, ~a occurs in ~a\n" var type)]
    [else `(,(cons var type))]))

(define environment
  '((+ . (-> Int Int Int))
    (- . (-> Int Int Int))
    (* . (-> Int Int Int))
    (/ . (-> Int Int Int))
    (< . (-> Int Int Bool))
    (= . (-> Int Int Bool))
    (eq? . (-> Int Int Bool))))

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
    [x #:when (or (symbol? x) (number? x) (boolean? x)) type-expr]
    [`(,x : ,ty) `(,(annotate-expr x subs) : ,(annotate-type ty subs))]
    [`(lambda ,x ,b) `(lambda ,(foldr (lambda (val res)
                                         (append (annotate-expr val subs) res)) '() x)
                        ,(annotate-expr b subs))]
    [`(let ,vars ,b) `(let ,(foldr (lambda (var res)
                                     (match-let ([`(,x ,e) var])
                                       (append `((,@(annotate-expr x subs)
                                                  ,(annotate-expr e subs))) res))) '() vars)
                        ,(annotate-expr b subs))]
    [`(,rator . ,rand) `(,(annotate-expr rator subs)
                         ,@(map (curryr annotate-expr subs) rand))]
    [else (error 'error type-expr)]))

(define (infer e)
  (match-define
    (infer-record assumptions constraints type type-expr)
    (infer-types (if (syntax? e)
                     (syntax->datum e)
                     e)
                 (set)))
  ;;(displayln assumptions)
  ;;(display "FINAL TYPE:") (displayln type)
  ;;(display "CONSTRAINTS: ")(displayln constraints)
  ;;(display "TYPE EXPR: ")(displayln type-expr)
  ;;(displayln type-expr)
  ;; (print "Infer types done")
  (list assumptions constraints type (solve (set->list constraints)) type-expr))

(define (p-infer exp)
  (reset-var-cnt)
  (match-define (list assumptions constraints type substitutions type-expr) (infer exp))
  (displayln "--- Input: ------------------------------------------------------")
  (displayln (list (syntax->datum exp)))
  ;;(displayln "--- Output: -----------------------------------------------------")  
  ;;(displayln (list (syntax->datum exp) ':= (substitute substitutions type)))
  (displayln "--- Principal type of Expression: -------------------------------")
  (displayln (substitute substitutions type))
  (displayln "--- Type Annotated Expression: ----------------------------------")
  (displayln (annotate-expr type-expr substitutions))
  (displayln "-----------------------------------------------------------------")
  ;(annotate-expr type-expr substitutions)
  )

(define (p*-infer exp)
  (reset-var-cnt)
  (match-define (list assumptions constraints type substitutions type-expr) (infer exp))
  ;;(displayln (list assumptions constraints type substitutions))
  (displayln (list (syntax->datum exp) ':= (substitute substitutions type)))
  (for ([s substitutions])
    (displayln s))
  (displayln "-----------------------------------------------------------------"))


;; ========================================================================
;; Test Cases
;; ========================================================================

(define e1 #'x)
(define e2 #'(lambda (x) x))
(define e3 #'(x 2))
(define e4 #'((lambda (x y) (+ x y)) 2 3))
(define e5 #'(let ((x (+ 5 2))) x))
(define e6 #'(let ((x 2) (y 5)) (+ x y)))
(define e7 #'(lambda (x) (let ((x 2)) x)))
(define e8 #'(lambda (x) (let ((x 2)) (+ x x))))
(define e9 #'(lambda (x y) (let ((x 2)) (+ x y))))
(define e10 #'(let ((x (lambda (x y) (+ x y)))) (x 5 2)))
(define e11 #'(let ((f (lambda (x) (lambda (y) (+ x 1)))))
                (let ((g (f 2))) g)))
(define e12 #'(: (+ 5 4) Int))
(define e13 #'(use x Int))
(define e14 #'((lambda(x) (+ (use a Int) x)) 5))
(define e15 #'(if (eq? (use a Int) (use b Int))
                  (+ 3 5)
                  (- 5 3)))
(define e16 #'(: x (Array 1 #t)))

(define e1_ (p-infer e1))

(define e2_ (p-infer e2))
(p-infer e3)
(p-infer e4)
(p-infer e5)
(p-infer e6)
(p-infer e7)
(p-infer e8)
(p-infer e9)
(p-infer e10)
(p-infer e11)
(p-infer e12)
(p-infer e13)
(p-infer e14)
(p-infer e15)
(p-infer e16)

;;(display "Feeding back through:\n")
;; (p-infer e2_)
;; Expected output:
;;  (lambda ((x:t1)) x)
