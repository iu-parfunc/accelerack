#lang htdp/isl+

;; a Base* is one of:
;; - 'Int
;; - 'Float

;; a Shape is a [ListOf Nat]
;; an Index is a Shape

;; a NonEmptyShape is a Shape containing no zeroes
;; empty-shape? : Shape -> Boolean
(define (empty-shape? sh)
  (andmap positive? sh))

;; gen-indices : NonEmptyShape -> [ListOf Index]
;; (gen-indices sh) produces a complete list of all valid indices in sh
;; Confirmed linear run time
(define (gen-indices sh)
  (if
   (empty? sh) (list empty)
   (local [(define sub-indices (gen-indices (rest sh)))]
     (foldr append empty
            (build-list (first sh)
                        (λ (n) (map (λ (index) (cons n index))
                                    sub-indices)))))))
  
;; an Element* is a [ListOf Base*]
;; an Element is a [ListOf Number]

;; a Payload stores all the elements of the array
;; currently represented as a [... [ListOf [ListOf Element]]...]

;; an Array is a (make-array Shape Element* Payload)
(define-struct array [shape element-type payload])

;; generate : Shape [Index -> Element] -> Array
;; (generate sh fn) produces an array of shape sh
;; with elements defined by fn applied to their respective index
;(define (generate sh fn)
;  ((gen-indices sh)

;; dot-product : [ListOf Number] [ListOf Number] -> Number
;; (dot-product p1 p2) produces p1·p2, the algebraic dot product
;(define (dot-product p1 p2)
;  (foldr
