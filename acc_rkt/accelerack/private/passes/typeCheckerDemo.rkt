#lang racket
(require racket/set)
(require racket/dict)

(provide p*-infer)


;; Datatype definitions:
;; ------------------------------------------
;; A InferRecord is a triple:
;;  - ( variable-renames constraints type )

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
(define (fresh var)
  (set! var-cnt (+ var-cnt 1))
   (string-append (if (symbol? var)
                      (symbol->string var)
                      var)
                  (number->string var-cnt)))

;; type environment
(define (get_primitives e)
  (cond ((number? e) 'Int)
        ((boolean? e) 'Bool)))

;; types
; 1
; 'int
; (-> (t1 t2 tn) tr)
(define type_var? string?)
(define type_con? symbol?)
(define (type_fun? type)
  (match type
    [`(-> . ,t1) #t]
    [else #f])
  ;;(and (pair? type) (eq? (car type) '->))
  )
;;(define (type_scheme? type) (and (pair? type) (eq? (car type) 'scheme)))
  
; Bottom up constraint collector: (assumption constraints type)
(define (infer-types e env)
  (match e
	[`(lambda ,x ,b) (infer-abs e env)]
	[`(let ,vars ,b) (infer-let e env)]
	[`(,rator . ,rand) (infer-app e env)]
	[(? symbol?) (infer-var e)]
	[else (infer-lit e)]
    ))


; [Var] 
; infer-var: Variable -> InferRecord
(define (infer-var x)
  (let ((top-type (dict-ref environment x #f)))
    (if top-type
        `(,(mutable-set) ,(mutable-set) ,top-type)
        (let ([var (fresh x)])
          `(,(mutable-set (cons x var)) ,(mutable-set) ,var)))))

; [Lit] : Exp -> InferRecord
(define (infer-lit exp)
  `(,(mutable-set)
    ,(mutable-set)
    ,(cond ((number?  exp) 'Int)
           ((boolean? exp) 'Bool)
           (else (error "infer-lit: this literal is not supported yet: ~a " exp
                 )))))

; [App] : Exp Environment -> InferRecord
(define (infer-app exp env)
  (let ((e1 (car exp))
        (args (cdr exp))
        (typevar (fresh "app")))
    (match-define `(,a1 ,c1 ,t1) (infer-types e1 env))
    (define argtypes
      (for/list [(arg args)]
        (match-define `(,a2 ,c2 ,t2) (infer-types arg env))
        ;(set-add! a1 typevar)
        (set-union! a1 a2)
        (set-union! c1 c2)
        t2))
    (set-union! c1 (set `(== ,t1 (-> ,@argtypes ,typevar))))
    `(,a1 ,c1 ,typevar)))

; [Abs]
(define (infer-abs exp env)
  (let* ((args (cadr exp)) ;; Formal parameters of lambda
         (e (caddr exp))
         (abs (map (lambda (x) (cons x (fresh "arg"))) args))
         (bs (map (lambda (ab) (cdr ab)) abs))
         (c (mutable-set))
         (a2 (mutable-set)))
    (match-define `(,a ,c ,t) (infer-types e (set-union env (list->set args))))
    (set-for-each a (lambda (y)
                      (let ((lkp (assoc (car y) abs)))
                        (if lkp
                            (set-add! c `(== ,(cdr y) ,(cdr lkp)))
                            (set-add! a2 y)))))
    `(,a2 ,c (-> ,@bs ,t))))

; [Let]
(define (infer-let exp env)
  (match-define `(let ((,x ,e1)) ,body) exp)
  (match-define `(,a1 ,c1 ,t1) (infer-types e1 env))
  (match-define `(,a2 ,c2 ,t2) (infer-types body env))
  (set-union! c1 c2)
  (set-for-each a2 (lambda (a)
                     (if (equal? (car a) x)
                         (set-add! c1 `(implicit ,(cdr a) ,t1 ,env))
                         (set-add! a1 a))))
  `(,a1 ,c1 ,t2))


;; Solver: list(constraint) -> subsitution
(define (solve constraints)
  (if (empty? constraints)
      '()
      (let ((constraint (car constraints)))
        (match constraint
          [`(== ,t1 ,t2)
           (let ((s (unify t1 t2)))
             (subs-union (solve (sub_constraints s (cdr constraints))) s))]
          [`(implicit ,t1 ,t2 ,monos)
           ;(print constraint)
           ;(match-define (list _ t1 t2 monos) constraint)
           (if (set-empty? (set-intersect (set-subtract (free_vars t2) monos)
                                          (active_vars (cdr constraints))))
               (solve (cons `(explicit ,t1 ,(generalize monos t2))
                            (cdr constraints)))
               (solve (append (cdr constraints) `(,constraint))))]
          [`(explicit ,t ,s)
           ;(match-define (list _ t s) constraint)
           (solve (cons `(== ,t ,(instantiate s))
                        (cdr constraints)))]))))


;; dict -> dict -> dict
(define (subs-union subs1 subs2)
  (let ((s (dict-map subs2
                      (lambda (v t)
                        (cons v (substitute subs1 t))))))
    (dict-for-each subs1
                   (lambda (v t)
                     (when (dict-ref subs2 v #f)
                       (raise "substitutions with same type vars"))
                     (set! s (dict-set s v t))))
    s))

;; type -> substitution -> type
(define (substitute s type)
  ;; (print type)
  ;; (newline)
  ;; (print s)
  ;; (newline)
  ;; (newline)
  (cond ((type_con? type) type)
        ((type_var? type) (dict-ref s type type))
        ((type_fun? type) (let ([intypes (take (cdr type) (sub1 (length (cdr type))))]
                                [rettype (car (take-right (cdr type) 1))])
                           ;; (print intypes)
                           ;; (print rettype)
                           ;; (newline)
                            `(->
                                  ,@(map (lambda (x) (substitute s x)) intypes)
                                  ,(substitute s rettype))))
        (else (raise (string-append "unknown type: " type)))))

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
(define (free_vars t)
  (cond ((type_var? t) (set t)) 
        ((type_fun? t)
         (let ([intypes (take (cdr t) (length (cdr t)))]
               [rettype (car (take-right (cdr t) 1))])
           (set-union (list->set (map (lambda (x) (free_vars x)) intypes))
                      (free_vars rettype))))
         ((type_con? t) (set))
         (else (error (format "No match clause for ~s" t)))))

  
;; active variables: constraints -> set(type var)
(define (active_vars constraints)
  ;(print constraints)
  (foldl (lambda (constraint nxt)
           (match constraint
             [`(== ,v1 ,v2) (set-union (set-union (free_vars v1) (free_vars v2)) nxt)]
             [`(implicit ,v1 ,v2 ,v3) (set-union
                                       (set-union (free_vars v1)
                                                  (set-intersect v3 (free_vars v2))) nxt)]
             [`(explicit ,v1 ,v2) (set-union
                                   (set-union (free_vars v1)
                                              (free_vars v2)) nxt)]))
         (set) constraints))

;; unify : type type -> constraint set
(define (unify t1 t2)
  (cond ((and (pair? t1) (pair? t2))
      (match-let* ((`(-> . ,t1pars) t1)
                   (`(-> . ,t2pars) t2)
                   )
        (if (not (eq? (length t1pars) (length t2pars)))
            (raise "incompatible arguments")
            (let ((s (foldl (lambda (p1 p2 s)
                              (set-union (unify (substitute s p1) (substitute s p2))
                                         s)) '() t1pars t2pars)))
              s))))
        ((equal? t1 t2) '())
        ((type_var? t1)
         (varbind t1 t2))
        ((type_var? t2)
         (varbind t2 t1))
        (else (error (format "Can't Unify t1: ~s and t2: ~s" t1 t2)))))

;; Var Type -> ( ( var . ty) ...)
(define (varbind var type)
  ;(print type)
  (cond ((equal? var type) '())
        ((set-member? (free_vars type) var)
         ;`(infinite-type ,var ,type)
         (error varbind "occurs check failed, ~a occurs in ~a\n" var type))
        (else `(,(cons var type)))))
      
         
    
(define environment
  '((+ . (-> Int Int Int))
    (- . (-> Int Int Int))
    (* . (-> Int Int Int))
    (/ . (-> Int Int Int))
    (< . (-> Int Int Bool))
    (= . (-> Int Int Bool))))

(define (infer e)
  (match-define `(,assumptions ,constraints ,type) (infer-types e (set)))
  ;;(displayln (list assumptions constraints type))
  ;(print "Infer types done")
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
