#lang racket

(require rackunit)
(require "../lib/acc-types.rkt")

;; Types
;; -----------------------------------------------------

(check-true (acc-base-type? 'Int))
(check-true (acc-base-type? 'Word64))
(check-false (acc-base-type? 'int))
(check-false (acc-base-type? 'char))

(check-true (acc-payload-type? '#(Int Float)))
(check-true (acc-payload-type? '#(Float)))
(check-false (acc-payload-type? '#(3 4)))

(check-true (acc-shape-type? '(Z)))
(check-true (acc-shape-type? '(Z Int)))
(check-true (andmap acc-shape-type? (list DIM0 DIM1 DIM2 DIM3)))
(check-true (acc-shape-type? '(Z Int Int Int Int)))
(check-false (acc-shape-type? '(Z 1 4)))

(check-true (acc-array-type? '(Array (Z Int) #(Float))))
(check-true (acc-array-type? '(Array (Z) #(Int))))
(check-true (acc-array-type? `(Array ,DIM0 #(Int))))
(check-true (acc-array-type? '(Array (Z Int Int) #(Int Int Float))))
(check-true (acc-array-type? `(Array ,DIM2 #(Int Int Float))))

(check-false (acc-type? '(Int Float)))
(check-false (acc-type? '(Z Int Float)))


;; Helper functions
;; -----------------------------------------------------

(check-eqv? ((payload-type->vector-length 'Float) (f64vector 45 45 45)) 3)
(check-true ((payload-type->vector-pred 'Word64) (u64vector 45 45 45)))
(check-false ((payload-type->vector-pred 'Word64) (f64vector 45 45 45)))

(check-equal? (arr-shape '(Array (Z) #(Int))) '(Z))
(check-eqv? (arr-shape `(Array ,DIM0 #(Int))) DIM0)
(check-equal? (arr-shape '(Array (Z Int Int) #(Int Int Float))) '(Z Int Int))
;; TODO: add tests for arr-dim and arr-payload

(check-eqv? (shape-size (Z)) 1)
(check-eqv? (shape-size (Z 4 5)) 20)

(check-eqv? (shape-dim (Z)) 0)
(check-eqv? (shape-dim (Z 1)) 1)
(check-eqv? (shape-dim (Z 6)) 1)
(check-eqv? (shape-dim (Z 6 1 1)) 3)

(check-true (acc-index? (Z) (Z)))
(check-false (acc-index? (Z 1) (Z)))
(check-false (acc-index? (Z) (Z 1)))
;
(check-true (acc-index? (Z 0) (Z 3)))
(check-true (acc-index? (Z 1) (Z 3)))
(check-true (acc-index? (Z 3) (Z 3)))
(check-false (acc-index? (Z 4) (Z 3)))
(check-false (acc-index? (Z -1) (Z 3)))
;
(check-true (acc-index? (Z 0 0 0 0) (Z 3 4 5 6)))
(check-true (acc-index? (Z 1 1 1 1) (Z 3 4 5 6)))
(check-true (acc-index? (Z 2 2 2 2) (Z 3 4 5 6)))
(check-true (acc-index? (Z 3 4 5 6) (Z 3 4 5 6)))
(check-false (acc-index? (Z 6 5 4 3) (Z 3 4 5 6)))
(check-false (acc-index? (Z 9 1 1 1) (Z 3 4 5 6)))
(check-false (acc-index? (Z 1 9 1 1) (Z 3 4 5 6)))
(check-false (acc-index? (Z 1 1 9 1) (Z 3 4 5 6)))
(check-false (acc-index? (Z 1 1 1 9) (Z 3 4 5 6)))
(check-false (acc-index? (Z 1 1 -1 1) (Z 3 4 5 6)))
