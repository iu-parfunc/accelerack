#lang racket
(require racket/set)
(require racket/dict)

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


;; types
; 1
; 'int
; (-> (t1 ... tn) tr)
(struct infer-record ([assumptions #:mutable]
                      [contraints #:mutable]
                      type)
  #:guard (lambda (as con t type-name)
            (cond
              [(not (set-mutable? as)) (error type-name "Assumptions: ~e has to be of the type mutable-set" as)]
              [(not (set-mutable? con)) (error type-name "Constraints: ~e has to be of the type mutable-set" con)]
              [else (values as con t)])))
(define type_var? string?)
(define type_con? symbol?)
(define (type_fun? type)
  (match type
    [`(-> . ,t1) #t]
    [else #f]))
(define (type_scheme? type) (and (pair? type) (eq? (car type) 'scheme)))


;; To create a fresh type variable for inference
;; Symbol -> String
(define (fresh var)
  (set! var-cnt (+ var-cnt 1))
   (string-append (if (symbol? var) (symbol->string var) var)
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
	[`(,rator . ,rand) (infer-app e env)]
	[(? symbol?) (infer-var e)]
	[else (infer-lit e)]))


; [Var] 
; infer-var: Variable -> InferRecord
(define (infer-var x)
  (let ((simple-type (dict-ref environment x #f)))
    (if simple-type
        (infer-record (mutable-set) (mutable-set) simple-type)
        (let ([var (fresh x)])
          (infer-record (mutable-set (cons x var)) (mutable-set) var)))))


; [Lit] : Exp -> InferRecord
(define (infer-lit exp)
  (infer-record (mutable-set)
                (mutable-set)
                (cond ((number?  exp) 'Int)
                      ((boolean? exp) 'Bool)
                      (else (error infer-lit "This literal is not supported yet: ~a " exp)))))


; [App] : Exp Environment -> InferRecord
(define (infer-app exp env)
  (let ((e1 (car exp))
        (args (cdr exp))
        (typevar (fresh "app")))
    (match-define (infer-record a1 c1 t1) (infer-types e1 env))
    (define argtypes
      (for/list [(arg args)]
        (match-define (infer-record a2 c2 t2) (infer-types arg env))
        ;(set-add! a1 typevar)
        (set-union! a1 a2)
        (set-union! c1 c2)
        t2))
    (set-union! c1 (set `(== ,t1 (-> ,@argtypes ,typevar))))
    (infer-record a1 c1 typevar)))


; [Abs]
(define (infer-abs exp env)
  (match-define `(lambda ,args ,body) exp)
  (let* ((arg-env (map (curryr cons (fresh "arg")) args))
         (arg-vars (map cdr arg-env))
         (c (mutable-set))
         (a2 (mutable-set)))
    (match-define (infer-record a c t) (infer-types body (set-union env (list->set args))))
    (set-for-each a (lambda (y)
                      (let ((lkp (assoc (car y) arg-env)))
                        (if lkp
                            (set-add! c `(== ,(cdr y) ,(cdr lkp)))
                            (set-add! a2 y)))))
    (infer-record a2 c `(-> ,@arg-vars ,t))))


; [Let]
(define (infer-let exp env)
  (match-define `(let ((,x ,e1)) ,body) exp)
  (match-define (infer-record a1 c1 t1) (infer-types e1 env))
  (match-define (infer-record a2 c2 t2) (infer-types body env))
  (set-union! c1 c2)
  (set-for-each a2 (lambda (a)
                     (if (equal? (car a) x)
                         (set-add! c1 `(implicit ,(cdr a) ,t1 ,env))
                         (set-add! a1 a))))
  (infer-record a1 c1 t2))


;; Solver: list(constraint) -> subsitution
(define (solve constraints)
  (cond
    [(empty? constraints) '()]
    [else (let ((constraint (car constraints)))
            (match constraint
              [`(== ,t1 ,t2) (let ((s (unify t1 t2)))
                               (subs-union (solve (sub_constraints s (cdr constraints))) s))]
              [`(implicit ,t1 ,t2 ,monos) (if (set-empty? (set-intersect
                                                           (set-subtract (free_vars t2) monos)
                                                           (active_vars (cdr constraints))))
                                              (solve (cons `(explicit ,t1 ,(generalize monos t2))
                                                           (cdr constraints)))
                                              (solve (append (cdr constraints) `(,constraint))))]
              [`(explicit ,t ,s) (solve (cons `(== ,t ,(instantiate s)) (cdr constraints)))]))]))


;; dict -> dict -> dict
(define (subs-union subs1 subs2)
  (let ((s (dict-map subs2
                     (lambda (v t)
                       (cons v (substitute subs1 t))))))
    (dict-for-each subs1
                   (lambda (v t)
                     (when (dict-ref subs2 v #f)
                       (error subs-union "Substitutions with same type vars"))
                     (set! s (dict-set s v t)))) s))


;; Substitution Type -> Type
(define (substitute s type)
  (cond
    [(type_con? type) type]
    [(type_var? type) (dict-ref s type type)]
    [(type_fun? type) `(-> ,@(map (curry substitute s) (cdr type)))]
    [else (error substitute "unknown type: ~a" type)]))


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


;; substitution -> list(constraint) -> list(constraint)
(define (sub_constraints s constraints)
  (map (lambda (c) (sub_constraint s c)) constraints))


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
  (cond [(type_var? t) (set t)] 
        [(type_fun? t) (let ([in-types (drop-right (cdr t) 1)]
                             [ret-type (last t)])
                         (set-union (list->set (map free_vars in-types))
                                    (free_vars ret-type)))]
        [(type_con? t) (set)]
        [else (error (format "No match clause for ~s" t))]))

  
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
    [else (error (format "Can't Unify t1: ~s and t2: ~s" t1 t2))]))

;; Var Type -> ((var . type)...)
(define (occurs-check var type)
  (cond
    [(equal? var type) '()]
    ;This is an infinite type. Send an error back
    [(set-member? (free_vars type) var) (error occurs-check "Occurs check failed, ~a occurs in ~a\n" var type)]
    [else `(,(cons var type))]))
    
(define environment
  '((+ . (-> Int Int Int))
    (- . (-> Int Int Int))
    (* . (-> Int Int Int))
    (/ . (-> Int Int Int))
    (< . (-> Int Int Bool))
    (= . (-> Int Int Bool))))

(define (infer e)
  (match-define (infer-record assumptions constraints type) (infer-types e (set)))
  ;; (displayln (list assumptions constraints type))
  ;; (print "Infer types done")
  (list assumptions constraints type (solve (set->list constraints))))

(define (p-infer exp)
  (reset-var-cnt)
  (match-define (list assumptions constraints type substitutions) (infer exp))
  (displayln (list exp ':= (substitute substitutions type)))
  (displayln substitutions)
  (displayln "-----------------------------------------------------------------"))

(define (p*-infer exp)
  (reset-var-cnt)
  (match-define (list assumptions constraints type substitutions) (infer exp))
  ;;(displayln (list assumptions constraints type substitutions))
  (displayln (list exp ':= (substitute substitutions type)))
  (for ([s substitutions])
    (displayln s))
  (displayln "-----------------------------------------------------------------"))


;; ========================================================================
;; Test Cases
;; ========================================================================

(define e1 'x)
(define e2 '(lambda (x) x))
(define e3 '(x 2))
(define e4 '((lambda (x) x) 2))
(define e5 '(let ((x 2)) x))
(define e6 '(let ((x 2)) (+ x x)))
(define e7 '(lambda (x) (let ((x 2)) x)))
(define e8 '(lambda (x) (let ((x 2)) (+ x x))))
(define e9 '(lambda (x y) (let ((x 2)) (+ x x))))
(define e10 '(let ((x (lambda (x y) (let ((x 2)) (+ x x))))) (x 5 2)))
(define e11 '(let ((f (lambda (x) (lambda (y) (+ x 1)))))
               (let ((g (f 2))) g)))


(p*-infer e1)
(p*-infer e2)
(p*-infer e3)
(p*-infer e4)
(p*-infer e5)
(p*-infer e6)
(p*-infer e7)
(p*-infer e8)
(p*-infer e9)
(p*-infer e10)
(p*-infer e11)
