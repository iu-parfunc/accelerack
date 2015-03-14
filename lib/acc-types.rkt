#lang racket

;;  Accelerack Types

(require ffi/vector)

(provide 
 acc-type? acc-base-type? acc-payload-type?
 acc-shape-type? acc-array-type?
 
 acc-payload? acc-base? acc-shape?
 
 payload-type->vector-pred
 payload-type->vector-length
 payload-type->vector-ref
 payload-type->list->vector
 
 arr-shape arr-payload arr-dim
 
 Z shape-size shape-dim acc-index? flatten-index
 DIM0 DIM1 DIM2 DIM3
 
 (all-from-out ffi/vector) ;only necessary for very manual array construction
 )

;; Accelerate type include base types, payload, shapes, and arrays
;; TODO: Add functions
;; TODO: Add Arrays (plural), that is, multiple arrays
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

(define acc-base? real?)

;; TODO: Combine and return (values pred length) ?
;; payload-type->vector-pred : Base -> VectorPredicate
(define (payload-type->vector-pred plty)
  (match plty
    ['Word64 u64vector?]
    ['Float f64vector?]))
;; payload-type->vector-pred : Base -> VectorLengthFn
(define (payload-type->vector-length plty)
  (match plty
    ['Word64 u64vector-length]
    ['Float f64vector-length]))
;; payload-type->vector-ref : Base -> VectorRefFn
(define (payload-type->vector-ref plty)
  (match plty
    ['Word64 u64vector-ref]
    ['Float f64vector-ref]))
(define (payload-type->list->vector plty)
  (match plty
    ['Word64 list->u64vector]
    ['Float list->f64vector]))

;; Payloads (tuples) is a vector of primitives
(define (acc-payload-type? t0)
  (match t0
    [(vector t1* ...) (andmap acc-base-type? t1*)]
    [else false]
    ))

;; 
(define (acc-payload? x)
  (or (vector? x)
      (acc-base? x)))

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

(define (arr-shape arrty)
  (match arrty
    [`(Array ,sh ,pl) sh]))
(define (arr-payload arrty)
  (match arrty
    [`(Array ,sh ,pl) pl]))
(define (arr-dim arrty)
  (match arrty
    [`(Array ,sh ,pl) (shape-dim sh)]))

;; a Shape is (Z [Int] ...)
; DIM0 is (Z)
; DIM1 is (Z Int)
; DIM2 is (Z Int Int)
(define (Z . ls)
  (if (andmap integer? ls)
      (cons 'Z ls)
      (error 'Z "cannot construct shape value with non-numeric argument: ~a" ls)))

(define (acc-shape? x)
  (and (symbol=? 'Z (first x))
       (andmap integer? (rest x))))

;; TODO: Keep these?  Are they a convenience given the need to unquote?
;; Convenience shapes
(define DIM0 '(Z))
(define DIM1 '(Z Int))
(define DIM2 '(Z Int Int))
(define DIM3 '(Z Int Int Int))

;; shape-size : Shape -> Nat
;; Produces the size of an array corresponding to the given shape
(define (shape-size shape)
  (foldl * 1 (rest shape)))

;; shape-dim : (U Shape ShapeType) -> Nat
;; Produces the dimensionality of shape
(define (shape-dim shape)
  (sub1 (length shape)))

;; acc-index? : Shape Shape -> Boolean
;; Determines if index is valid for shape
(define (acc-index? index shape)
  (and
   ; Check that dimensionality matches
   (= (shape-dim index) (shape-dim shape))
   ; Check that each index is within bounds
   (andmap (λ (i range) (if (symbol? range)
                            (symbol=? i range)
                            (<= 0 i range)))
           index shape)))

;; flatten-index : Shape Shape -> Nat
;; Convert multi-dimensional index in Shape form to a linear index into a flat vector
(define (flatten-index index shape)
  (begin (unless (acc-index? index shape) (error "Invalid index for given shape"))
         (let helper ([index (rest index)] [shape (rest shape)])
           (if (empty? index)
               0
               (+ (foldr * (first index) (rest shape))
                  (helper (rest index) (rest shape)))))
         ))
      