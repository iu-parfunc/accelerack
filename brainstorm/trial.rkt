#lang htdp/isl+

(require "../lib/accelerack.rkt")
  
;; an Array is a (make-array Shape Element* Payload)
;(define-struct array [shape element-type payload])

;; generate : Shape [Index -> Element] -> Array
;; (generate sh fn) produces an array of shape sh
;; with elements defined by fn applied to their respective index
;(define (generate sh fn)
;  ((gen-indices sh)

;; dot-product : [ListOf Number] [ListOf Number] -> Number
;; (dot-product p1 p2) produces p1·p2, the algebraic dot product
;(define (dot-product p1 p2)
;  (foldr
