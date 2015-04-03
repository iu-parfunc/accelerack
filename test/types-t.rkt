#lang racket
(require rackunit)
(require "../lib/types.rkt")

;; HELP: Should I test contracts?
;; HELP: circular requires...

;; TODO: if either used, this should be a macro, so errors are tagged to test inputs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (check-pred-all pred ls)
  (letrec ([test ((curry check-pred) pred)]
           [helper (λ (ls) (cond
                             [(empty? ls) (void)]
                             [(and (cons? (rest ls))
                                   (string? (second ls)))
                              (begin (apply test (first ls) (second ls) '())
                                     (helper (rest (rest ls))))]
                             [else (begin (apply test (first ls) '())
                                          (helper (rest ls)))]))])
    (helper ls)))
(define (check-pred/n pred . ls)         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (apply check-pred (negate pred) ls))   ;;
                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-pred-all base? (list 0 0.0 -0 -0.0 1.3 -1 -1.3
                            (sqrt -1) (sqrt (sqrt -1))))

;base?
(check-true (andmap base? (list 0 0.0 -0 -0.0 1 1.3 -1 -1.3
                                (sqrt -1) (sqrt (sqrt -1)))))
;avector?
(check-true (andmap avector? (list (u64vector) (u64vector 4 6)
                                   (f64vector) (f64vector 4.1 6))))

;; Test for Polymorphic Vector Helpers
;; -----------------------------------------------------
;vector?
(check-pred (acc-vector? 'Word64) (u64vector 45 45 45) "a proper vector")
(check-pred (negate (acc-vector? 'Word64)) (f64vector 45 45 45) "AccVector type mismatch")
(check-pred (negate (acc-vector? 'Word64)) (vector 45 45 45) "generic vectors are not AccVectors")
(check-pred (negate (acc-vector? 'Word64)) 45 "nonvector")
(check-exn exn:fail? (thunk ((acc-vector? 'Word43) (u64vector 45 45 45))) "requires valid BaseType")
(check-exn exn:fail? (thunk ((acc-vector? (u64vector 45 45 45)) (u64vector 45 45 45))) "does not take AccVectors")
;vector-length
(check-eqv? (acc-vector-length (f64vector 45 45 45)) 3)
(check-eqv? (acc-vector-length (u64vector 45 45 45)) 3)
(check-eqv? (acc-vector-length (u64vector)) 0)
(check-exn exn:fail? (thunk (acc-vector-length (vector 45 45 45))) "requires AccVectors")
;vector-ref
(check-eqv? (acc-vector-ref (f64vector 45 55 65) 0) 45.0)
(check-eqv? (acc-vector-ref (u64vector 15 25 35) 2) 35)
;list->vector
(test-case "(acc-list->vector 'Word64)"
           (check-true (u64vector? ((acc-list->vector 'Word64) (list 2 3 5))))
           (check-eqv? (u64vector-length ((acc-list->vector 'Word64) (list 2 3 5))) 3))
(test-case "(acc-list->vector 'Float)"
           (check-true (f64vector? ((acc-list->vector 'Float) (list 2 3 5))))
           (check-eqv? (f64vector-length ((acc-list->vector 'Float) (list 2 3 5))) 3))
(test-case "acc-list->vector on empty lists"
           (check-true (f64vector? ((acc-list->vector 'Float) empty)))
           (check-eqv? (f64vector-length ((acc-list->vector 'Float) empty)) 0))
;vector-set!
(check-equal? (u64vector->list (let ([v0 (u64vector 2 3 5)])
                                 (acc-vector-set! v0 1 4)
                                 v0))
              (list 2 4 5))
(check-equal? (f64vector->list (let ([v0 (f64vector 2 3 5)])
                                 (acc-vector-set! v0 1 4)
                                 v0))
              (list 2.0 4.0 5.0))

;payload?
(check-pred payload? 5 "singleton")
(check-pred payload? empty "empty")
(check-pred payload? (list (u64vector) (u64vector)) "higher dimension empty");TODO: valid?
(check-pred payload? (list (u64vector 4 5 6) (u64vector 6 7 8)) "list representation")
(check-pred payload? (vector (u64vector 4 5 6) (u64vector 6 7 8)) "vector representation")
(check-pred payload? (list (u64vector 4 5 6) (f64vector 6 7 8)) "varied AccVector types")
(check-false (payload? (list (u64vector 4 5 6) (f64vector 6 7))) "mismatched lengths")
(check-false (payload? (list (u64vector 4 5 6) (vector 6 7 8))) "not an AccVector")

;payload-val?
(check-pred payload-val? 5 "singleton")
(check-pred payload-val? #(3 4 1) "tuple")
(check-pred/n payload-val? (u64vector 3 4 1) "does not work with ffi/vectors")

;payload-val-valid?

;shape?
(check-true (shape? (Z)))
(check-true (shape? (Z 1)))
(check-true (shape? (Z 3)))
(check-true (shape? (Z 1 4)))
(check-true (shape? (Z 5 7 99)))
(check-true (shape? (Z 0)))
(check-true (shape? (Z 2 0 2)))
;shape-size
(check-eqv? (shape-size (Z)) 1)
(check-eqv? (shape-size (Z 4 5)) 20)
(check-eqv? (shape-size (Z 0)) 0)
(check-eqv? (shape-size (Z 1 0)) 0)
(check-eqv? (shape-size (Z 0 1)) 0)
(check-eqv? (shape-size (Z 2 0 2)) 0)
;shape-dim
(check-eqv? (shape-dim (Z)) 0)
(check-eqv? (shape-dim (Z 1)) 1)
(check-eqv? (shape-dim (Z 6)) 1)
(check-eqv? (shape-dim (Z 6 1 1)) 3)
(check-eqv? (shape-dim (Z 0)) 1)
(check-eqv? (shape-dim (Z 1 0)) 2)
(check-eqv? (shape-dim (Z 0 1)) 2)
(check-eqv? (shape-dim (Z 2 0 2)) 3)
;index-valid?
(check-true (index-valid? (Z) (Z)))
(check-false (index-valid? (Z 1) (Z)))
(check-false (index-valid? (Z) (Z 1)))
(check-true (index-valid? (Z 0) (Z 3)))
(check-false (index-valid? (Z 0 2) (Z 0 5)))
(check-true (index-valid? (Z 1) (Z 3)))
(check-false (index-valid? (Z 3) (Z 3)))
(check-false (index-valid? (Z 4) (Z 3)))
(check-false (index-valid? (Z -1) (Z 3)))
(check-true (index-valid? (Z 0 0 0 0) (Z 3 4 5 6)))
(check-true (index-valid? (Z 1 1 1 1) (Z 3 4 5 6)))
(check-false (index-valid? (Z 1 1 1 1) (Z 3 4 0 6)))
(check-true (index-valid? (Z 2 2 2 2) (Z 3 4 5 6)))
(check-true (index-valid? (Z 2 3 4 5) (Z 3 4 5 6)))
(check-false (index-valid? (Z 3 4 5 6) (Z 3 4 5 6)))
(check-false (index-valid? (Z 6 5 4 3) (Z 3 4 5 6)))
(check-false (index-valid? (Z 9 1 1 1) (Z 3 4 5 6)))
(check-false (index-valid? (Z 1 9 1 1) (Z 3 4 5 6)))
(check-false (index-valid? (Z 1 1 9 1) (Z 3 4 5 6)))
(check-false (index-valid? (Z 1 1 1 9) (Z 3 4 5 6)))
(check-false (index-valid? (Z 1 1 -1 1) (Z 3 4 5 6)))
;flatten-index
(check-eqv? (flatten-index (Z) (Z)) 0)
(check-eqv? (flatten-index (Z 0 0) (Z 3 5)) 0)
(check-eqv? (flatten-index (Z 1 2 1 5) (Z 3 5 2 7)) 110)
(check-exn #rx"Invalid index for given shape" (λ () (flatten-index (Z 1 1 3) (Z 5 5 3)))) ;;FIXME
(check-exn #rx"Invalid index for given shape" (λ () (flatten-index (Z 0) (Z 0))))
;AccArray shape helpers
(check-equal? (arr-shty '(Array (Z) #(Int))) '(Z))
(check-eqv? (arr-shty `(Array ,DIM0 #(Int))) DIM0)
(check-equal? (arr-shty '(Array (Z Int Int) #(Int Int Float))) '(Z Int Int))
;arr-plty
;arr-plty-list
;arr-dim