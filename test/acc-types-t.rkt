#lang racket

(require rackunit)
(require "../lib/acc-types.rkt")
(require ffi/vector)

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

(check-eqv? 3 ((payload-type->vector-length 'Float) (f64vector 45 45 45)))
(check-true ((payload-type->vector-pred 'Word64) (u64vector 45 45 45)))
(check-false ((payload-type->vector-pred 'Word64) (f64vector 45 45 45)))

(check-equal? '(Z) (arr-shape '(Array (Z) #(Int))))
(check-eqv? DIM0 (arr-shape `(Array ,DIM0 #(Int))))
(check-equal? '(Z Int Int) (arr-shape '(Array (Z Int Int) #(Int Int Float))))
;; TODO: add tests for arr-dim and arr-payload

(check-eqv? 1 (shape-size (Z)))
(check-eqv? 20 (shape-size (Z 4 5)))
