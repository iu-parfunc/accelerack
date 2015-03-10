#lang racket

(require rackunit)
(require "accelerack.rkt")
(require ffi/vector)

;; Types
;; -----------------------------------------------------

(check-true (acc-type? 'Int))

(check-true (acc-type? '(Array (Z Int) Float)))

(check-true (acc-type? '(Z Int Int Int Int)))
(check-true (acc-shape-type? '(Z Int Int Int Int)))

(check-true (acc-type? '#(Int Float)))


(check-false (acc-type? '(Int Float)))
(check-false (acc-type? '(Z Int Float)))


;; Basic in-Racket Array operations:
;; -----------------------------------------------------


(define arr1 (r-arr (Z 4) '(Array (Z Int) Word64)
                    (list (u64vector 0 10 20 30))))

(define arr2 (r-arr (Z 1 5)
                    '(Array (Z Int) #(Word64 Word64 Word64))
                    (list (u64vector 0 1 2 3 4)
                          (u64vector 5 6 7 8 9)
                          (u64vector 10 11 12 13 14))))

(check-eqv? 20 (rget arr1 (Z 2)))

;; FINISHME:
; (check-equal? #(0 5 10) (rget arr2 (Z 0 0)))
