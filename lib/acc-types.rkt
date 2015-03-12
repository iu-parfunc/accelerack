#lang racket

;;  Accelerack Types

(provide 
         acc-type? acc-base-type? acc-payload-type?
         acc-shape-type? acc-array-type?
         
         Z DIM0 DIM1 DIM2 DIM3
         shape-size
         
         )

;; Accelerate type include base types, payload, shapes, and arrays
;; TODO: Add functions
;; TODO: Add Arrays, that is, multiple arrays
(define (acc-type? t0)
  (or (acc-base-type? t0)
      (acc-payload-type? t0)
      (acc-shape-type? t0)
      (acc-array-type? t0)))

;; Base types include selected numeric primitives
(define (acc-base-type? t0)
  (match t0
    [(or 'Int 'Int8 'Int16 'Int32 'Int64) #t]
    [(or 'Word 'Word8 'Word16 'Word32 'Word64) #t]
    ;; TODO: support C Types?  Let's not...
    [(or 'Float 'Double) #t]
    [else false]))

;; Allows two representations of singleton payloads
; '(Array (Z Int) Float)
; '(Array (Z Int) #(Float))
;; FIXME: Pick one and tighten down type predicate
;;   - Could use the constructor guards to normalize one to the other

;; Payloads (tuples) is a vector of primitives
(define (acc-payload-type? t0)
  (match t0
    [(vector t1* ...) (andmap acc-base-type? t1*)]
    [else (acc-base-type? t0)]
    ))

;; Shapes denote dimensionality and 
(define (acc-shape-type? sht)
  (match sht
    [`(Z ,ints ...) 
     (andmap (lambda (x) (eq? 'Int x)) ints)]
    [else #f]))

;; Array
(define (acc-array-type? t0)
  (match t0
    [`(Array ,sh ,pl) (and (acc-shape-type? sh) (acc-payload-type? pl))]
    [else false]))

;; a Shape is (Z [Int] ...)
; DIM0 is (Z)
; DIM1 is (Z Int)
; DIM2 is (Z Int Int)
(define (Z . ls)
  (if (andmap integer? ls)
      (cons 'Z ls)
      (error 'Z "cannot construct shape value with non-numeric argument: ~a" ls)))

;; TODO: Keep these?  Are they a convenience given the need to unquote?
;; Convenience shapes
(define DIM0 '(Z))
(define DIM1 '(Z Int))
(define DIM2 '(Z Int Int))
(define DIM3 '(Z Int Int Int))

;; shape-size : Shape -> Int
;; Produces the size of an array corresponding to the given shape
(define (shape-size shape)
  (foldl * 1 (rest shape)))
