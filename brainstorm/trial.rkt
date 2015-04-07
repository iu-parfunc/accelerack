#lang htdp/isl+

;; Front-most facing API

(require "../lib/accelerack.rkt")

; Empty arrays
(generate '(0) (const 4))
(generate '(3 4 0 1) (const 4))

; Singleton non-arrays
(generate '() (const 4))
(generate '() (const (list 5 6)))

; Array of length one
(generate '(1) (const (list 5 6)))

; Note: The two above examples differ except only in dimensionality,
;       and so they are indexed differently, (Z) vs (Z 0).
; This is consistent with Accelerack's internal representation and Accelerate itself.

; Singleton payloads:  two ways of returning singleton payload elements
(generate '(5) sqr)  ;as numbers
(generate '(5) list) ;as single-element lists

; Multi-dimensional array:  function arity must match number of dimensions
(generate '(5 5) +)
(generate '(2 4 4) *)

; Tuple payloads:  lists for tuples
(generate '(2 3) (λ (i j) (list i j)))
(generate '(10) (λ (i) (list i (sqr i))))

; Teaching languages do not allow var-arg lambdas
; Full Racket will allow this example
(require racket)
(generate '(2 1 1 1 2) (λ ls (length ls)))

;--------------------------------------------------------------------------------------
; Below is old
;--------------------------------------------------------------------------------------

;; dot-product : [ListOf Number] [ListOf Number] -> Number
;; (dot-product p1 p2) produces p1·p2, the algebraic dot product
;(define (dot-product p1 p2)
;  (foldr

