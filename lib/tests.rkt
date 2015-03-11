#lang racket

(require rackunit)
(require "accelerack.rkt")
(require ffi/vector)

;; Types
;; -----------------------------------------------------

(check-true (acc-base-type? 'Int))
(check-true (acc-base-type? 'Word64))
(check-false (acc-base-type? 'int))
(check-false (acc-base-type? 'char))

(check-true (acc-payload-type? '#(Int Float)))
(check-true (acc-payload-type? '#(Float)))
(check-true (acc-payload-type? 'Float))
(check-false (acc-payload-type? '#(3 4)))

(check-true (acc-shape-type? '(Z)))
(check-true (acc-shape-type? '(Z Int)))
(check-true (andmap acc-shape-type? (list DIM0 DIM1 DIM2 DIM3)))
(check-true (acc-shape-type? '(Z Int Int Int Int)))
(check-false (acc-shape-type? '(Z 1 4)))


(check-true (acc-array-type? '(Array (Z Int) Float)))
(check-true (acc-array-type? '(Array (Z Int) #(Float))))
(check-true (acc-array-type? '(Array (Z) Int)))
(check-true (acc-array-type? `(Array ,DIM0 Int)))
(check-true (acc-array-type? '(Array (Z Int Int) #(Int Int Float))))
(check-true (acc-array-type? `(Array ,DIM2 #(Int Int Float))))

(check-false (acc-type? '(Int Float)))
(check-false (acc-type? '(Z Int Float)))

;; Basic in-Racket Array operations:
;; -----------------------------------------------------

(check-eqv? 1 (shape-size (Z)))
(check-eqv? 20 (shape-size (Z 4 5)))

(define arr1 (r-arr (Z 4) '(Array (Z Int) Word64)
                    (list (u64vector 0 10 20 30))))

(define arr2 (r-arr (Z 1 5)
                    '(Array (Z Int Int) #(Word64 Word64 Word64))
                    (list (u64vector 0 1 2 3 4)
                          (u64vector 5 6 7 8 9)
                          (u64vector 10 11 12 13 14))))

(define arr3 (r-arr (Z 2 3)
                    '(Array (Z Int Int) #(Word64 Word64 Word64))
                    (list (u64vector 0 1 2 3 4 5)
                          (u64vector 6 7 8 9 10 11)
                          (u64vector 12 13 14 15 16 17))))

(check-eqv? 20 (rget arr1 (Z 2)))

;; FINISHME:
(check-equal? #(0 5 10) (rget arr2 (Z 0 0)))
(check-equal? #(3 8 13) (rget arr2 (Z 0 3)))

