#lang racket

(require rackunit)
(require "../lib/types-hi.rkt")

;; Tests for Higher Level Types
;; -----------------------------------------------------

(check-true (base*? 'Int))
(check-true (base*? 'Word64))
(check-false (base*? 'int))
(check-false (base*? 'char))

(check-true (payload*? '#(Int Float)))
(check-true (payload*? '#(Float)))
(check-true (payload*? 'Float))
(check-true (payload*? '#()))
(check-false (payload*? '#(3 4)))

;(check-true (shape*? '(Z)))
;(check-true (shape*? '(Z Int)))
;(check-true (andmap shape*? (list DIM0 DIM1 DIM2 DIM3)))
;(check-true (shape*? '(Z Int Int Int Int)))
;(check-false (shape*? '(Z 1 4)))
(check-true (shape*? 0))
(check-true (shape*? 1))
(check-true (andmap shape*? (list DIM0 DIM1 DIM2 DIM3)))
(check-true (shape*? 4))
(check-false (shape*? '(Z 1 4)))

(check-false (shape*? '(Z Int Int)))

(check-true (array*? (array* 1 '#(Float))))
(check-true (array*? (array* 0 '#(Int))))
(check-true (array*? (array* DIM0 '#(Int))))
(check-true (array*? (array* 2 '#(Int Int Float))))
(check-true (array*? (array* DIM2 '#(Int Int Float))))

(check-true (fn*? '(Int Int Int -> Int)))
(check-true (fn*? '(Float -> Int)))
(check-true (fn*? '(Int -> #(Int Int))))

(check-false (acc-type*? '(Int Float)))
(check-false (acc-type*? '(Z Int Float)))

;;;untested
;instance-of-base*?
;pl*->lis