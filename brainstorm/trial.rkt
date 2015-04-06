#lang htdp/isl+

(require "../lib/accelerack.rkt")

(generate '(5) sqr)

(generate '(5 5) +)

;; dot-product : [ListOf Number] [ListOf Number] -> Number
;; (dot-product p1 p2) produces p1·p2, the algebraic dot product
;(define (dot-product p1 p2)
;  (foldr

