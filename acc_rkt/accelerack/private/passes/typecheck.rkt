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
 unify-types
 typecheck-expr
 )
(require (for-syntax racket/base
                     syntax/parse)
         ; syntax/parse
         syntax/to-string
         rackunit rackunit/text-ui
         ;; accelerack/private/passes/typeCheckerDemo
         (only-in accelerack/private/global_utils pass-output-chatter)
         (only-in accelerack/private/types acc-scalar? acc-int? acc-type? acc-syn-entry-type)
         )

;; The full type-checking pass.
;; Returns two values:
;;   (1) principal type of expression
;;   (2) fully annotated expression
(define (typecheck-expr syn-table e)
  (pass-output-chatter 'typecheck-expr e)
  ;; TODO:
  ;; (p-infer e syn-table)
  )

(define (unify-types ty1 ty2)
  ;; FINISHME
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


;; ---------------- Persistent variables and typevariable related stuff ---------------------
(define var-cnt 0)
(define (reset-var-cnt) (set! var-cnt 0))
(define (fresh var)
  (set! var-cnt (+ var-cnt 1))
  (string-append (if (symbol? var)
                     (symbol->string var)
                     (if (syntax? var) (symbol->string (syntax->datum var)) var))
                 (number->string var-cnt)))
;; ---------------- Struct Record ----------------------------
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
(define (infer-lit exp)
  (match exp
    [(? acc-int?) 'Int]
    [(? flonum?) 'Double]
    [(? boolean?) 'Bool]
    [`(acc-array ,ls) (if (null? ls)
                          (raise-syntax-error 'infer-lit "Empty list in acc-array not permitted " exp)
                          (let ((type (foldr (lambda (x y) (if (equal? x y) x #f))
                                             (infer-lit (car ls))
                                             (map infer-lit (cdr ls)))))
                            (if type
                                `(Array ,(length ls) ,type)
                                (raise-syntax-error 'infer-lit "All-vars of Acc-array should be of same type: ~a " exp))))]
    ;; Supporting tuples
    [`(,x ...) (map infer-lit x)]
    [else (raise-syntax-error 'infer-lit "This literal is not supported yet: ~a " exp)]))
;; syntax -> box list -> type
;; This only guesses a type - the final typechecker is the one which throws the actual typechecker
(define (infer e env syn-table)
  (match e
    ;; [`(lambda ,x ,b) (infer-abs e env syn-table)]
    ;; [`(let ,vars ,b) (infer-let e env syn-table)]
    ;; [`(map ,fun ,arr) (infer-map e env syn-table)]
    ;; [`(fold ,fun ,res ,arr) (infer-fold e env syn-table)]
    ;; [(? symbol?) (infer-var e syn-table)]
    ;; [`(: ,e ,t0) (infer-asc e t0 env syn-table)]
    ;; [`(use ,e ,t0) (infer-use e t0 env syn-table)]
    ;; [`(if ,cnd ,thn ,els) (infer-cond e env syn-table)]
    [(? acc-scalar?) (infer-record (mutable-set) (mutable-set) (infer-lit exp) exp)]
    ;[`(Array ,n ,el) (infer-lit e)]
    ;; [`(,rator . ,rand) (infer-app e env syn-table)]
    [else (raise-syntax-error 'infer-types "unhandled syntax: ~a" e)]))





;;;; SOME TESTS FOR typechecker - MOVE to internal once done
;; Test for infer lit
(check-equal? (infer-lit '9) 'Int)
(check-equal? (infer-lit '#t) 'Bool)
(check-equal? (infer-lit '#f) 'Bool)
(check-equal? (infer-lit '9.333) 'Double)
(check-equal? (infer-lit '(acc-array (1.1 2.1 3.1))) '(Array 3 Double))
(check-equal? (infer-lit '(acc-array (1.1 2.1 3.1))) '(Array 3 Double))
(check-equal? (infer-lit '(acc-array ((1 2) (2 3) (2 2)))) '(Array 3 (Int Int)))
(check-equal? (infer-lit '(acc-array ((1 2.1) (2 3.22) (2 2.33)))) '(Array 3 (Int Double)))

;; (check-exn (infer-lit '(acc-array ())) '(Array 0 Int))
;; DUMMY
;; (check-equal? '(1 2 3) '(1 2 3))
