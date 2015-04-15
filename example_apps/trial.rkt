#lang htdp/isl+

;; Demonstrate front-most facing, simplest API
(require rackunit)
(require "../lib/accelerack.rkt")

; TODO: add tests for error conditions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Empty arrays
(check-equal? (generate '(0) (λ (_) 4))
              empty)
(check-equal? (generate '(3 4 0 1) (λ (a b c d) 4))
              empty)

; Singleton non-arrays (no 0-ary lambdas allowed in ISL+)
(check-equal? (generate '() +)
              '((0)))
(check-equal? (generate '() *)
              '((1)))

; Array of length one
(check-equal? (generate '(1) (λ (_) 5))
              '((5)))

; Note: The two above examples differ only in dimensionality,
;       and so they are indexed differently, (Z) vs (Z 0), or '() vs '(0).
; This is consistent with Accelerack's internal representation and Accelerate itself.

; Singleton payloads:  two ways of returning singleton payload elements
(check-equal? (generate '(5) sqr)  ;as numbers
              '((0) (1) (4) (9) (16)))
(check-equal? (generate '(5) list) ;as single-element lists
              '((0) (1) (2) (3) (4)))

; Multi-dimensional array:  function arity must match number of dimensions
(check-equal? (generate '(5 5) +)
              '(((0) (1) (2) (3) (4))
                ((1) (2) (3) (4) (5))
                ((2) (3) (4) (5) (6))
                ((3) (4) (5) (6) (7))
                ((4) (5) (6) (7) (8))))
(check-equal? (generate '(2 4 4) *)
              '((((0) (0) (0) (0))
                 ((0) (0) (0) (0))
                 ((0) (0) (0) (0))
                 ((0) (0) (0) (0)))
                
                (((0) (0) (0) (0))
                 ((0) (1) (2) (3))
                 ((0) (2) (4) (6))
                 ((0) (3) (6) (9)))))

; Tuple payloads:  lists for tuples
(check-equal? (generate '(2 3) (λ (i j) (list i j)))
              '(((0 0) (0 1) (0 2))
                ((1 0) (1 1) (1 2))))
(check-equal? (generate '(10) (λ (i) (list i (sqr i))))
              '((0 0) (1 1) (2 4) (3 9) (4 16) (5 25) (6 36) (7 49) (8 64) (9 81)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fold
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional examples from outside student languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Teaching languages do not allow var-arg lambdas
; Full Racket will allow this example
;(require racket)
;(generate '(2 1 1 1 2) (λ ls (length ls)))



;--------------------------------------------------------------------------------------
; Below is old
;--------------------------------------------------------------------------------------

;; dot-product : [ListOf Number] [ListOf Number] -> Number
;; (dot-product p1 p2) produces p1·p2, the algebraic dot product
;(define (dot-product p1 p2)
;  (foldr

