#lang racket

;;  Accelerack Types

(require ffi/vector)

(provide 
 acc-type? acc-base-type? acc-payload-type?
 acc-shape-type? acc-array-type?
 
 acc-payload? acc-base? acc-shape? acc-index-valid?
 acc-payload-val?
 
 (contract-out
  [acc-payload-val-instance? (-> acc-payload-val? acc-payload-type? boolean?)]
  )
 
 payload-type->vector-pred
 payload-type->vector-length
 payload-type->vector-ref
 payload-type->list->vector
 payload-type->vector-set!
 
 arr-shty arr-plty arr-plty-list arr-dim
 
 Z shape-size shape-dim acc-index-valid? flatten-index
 (contract-out
  (acc-index-0 (-> acc-shape? acc-shape?))
  )
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

(define (acc-vector? x)
  (or (u64vector? x)
      (f64vector? x)))

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
(define (payload-type->vector-set! plty)
  (match plty
    ['Word64 u64vector-set!]
    ['Float f64vector-set!]))


;; A Payload-Type is one of:
;; - acc-base-type?
;; - (vector acc-base-type? ...)
(define (acc-payload-type? t0)
  (match t0
    [(vector t1* ...) (andmap acc-base-type? t1*)]
    [else (acc-base-type? t0)]
    ))
;; acc-vect-length : acc-vector? -> Nat
(define (acc-vector-length pl)
  (cond
    [(u64vector? pl) (u64vector-length pl)]
    [(f64vector? pl) (f64vector-length pl)]
    ))
;; A Payload is a non-empty list of acc-vectors of unzipped tuples
(define (acc-payload? x)
  (let ([len (acc-vector-length (first x))])
    (and (cons? x)
         (andmap (λ (v) (and (acc-vector? v)
                             (= (acc-vector-length v) len)))
                 x))))
       
;; A PayloadVal is on of:
;; - acc-base?
;; - (vector acc-base? ...)
(define (acc-payload-val? x)
  (or (acc-base? x)
      (and (vector? x) (andmap acc-base? (vector->list x)))))

;; Determines if x as a payload-val is of type plty
;; Does not check if numerical value is valid c-type
(define (acc-payload-val-instance? x plty)
  (or (and (acc-base-type? plty)
           (acc-base? x))
      (= (vector-length plty)
         (vector-length x))))

;; ShapeTypes denote dimensionality and 
(define (acc-shape-type? sht)
  (match sht
    [`(Z ,ints ...) 
     (andmap (lambda (x) (eq? 'Int x)) ints)]
    [else #f]))

; DIM0 is (Z)
; DIM1 is (Z Int)
; DIM2 is (Z Int Int)
;; TODO: Keep these?  Are they a convenience given the need to unquote?
;; Convenience shapes
(define DIM0 '(Z))
(define DIM1 '(Z Int))
(define DIM2 '(Z Int Int))
(define DIM3 '(Z Int Int Int))

;; ArrayType
(define (acc-array-type? t0)
  (match t0
    [`(Array ,shty ,plty) (and (acc-shape-type? shty) (acc-payload-type? plty))]
    [else false]))

;; FnType
(define (acc-fn-type? t0)
  (match t0
    [`(Fn ,sh ,plty) (and (acc-shape? sh) (acc-payload-type? plty))]
    [else false]))

(define (arr-shty arrty)
  (match arrty
    [`(Array ,sh ,pl) sh]))
(define (arr-plty arrty)
  (match arrty
    [`(Array ,sh ,pl) pl]))
(define (arr-plty-list arrty)
  (match arrty
    [`(Array ,sh ,pl) #:when (vector? pl) (vector->list pl)]
    [`(Array ,sh ,pl) #:when (acc-base-type? pl) (list pl)]))
(define (arr-dim arrty)
  (match arrty
    [`(Array ,sh ,pl) (shape-dim sh)]))

(define (natural? n)
  (and (integer? n) (positive? n)))

;; a ShapeType is (Z [Int] ...)
(define (Z . ls)
  (if (andmap integer? ls)
      (cons 'Z ls)
      (error 'Z "cannot construct shape value with non-numeric argument: ~a" ls)))

(define (acc-shape? x)
  (and (symbol=? 'Z (first x))
       (andmap integer? (rest x))))

;; shape-size : Shape -> Nat
;; Produces the size of an array corresponding to the given shape
(define (shape-size shape)
  (foldl * 1 (rest shape)))

;; shape-dim : (U Shape ShapeType) -> Nat
;; Produces the dimensionality of shape
(define (shape-dim shape)
  (sub1 (length shape)))

;; acc-index-valid? : Shape Shape -> Boolean
;; Determines if index is valid for shape
(define (acc-index-valid? index shape)
  (and
   ; Check that dimensionality matches
   (= (shape-dim index) (shape-dim shape))
   ; Check that each index is within bounds
   (andmap (λ (i range) (if (symbol? range)
                            (symbol=? i range)
                            (<= 0 i (sub1 range))))
           index shape)))

;; flatten-index : Shape Shape -> Nat
;; Convert multi-dimensional index in Shape form to a linear index into a flat vector
(define (flatten-index index shape)
  (begin (unless (acc-index-valid? index shape) (error "Invalid index for given shape"))
         (let helper ([index (rest index)] [shape (rest shape)])
           (if (empty? index)
               0
               (+ (foldr * (first index) (rest shape))
                  (helper (rest index) (rest shape)))))
         ))

;; acc-index-0 : Shape -> Shape
;; Produces the 0-index for sh
(define (acc-index-0 sh)
  (cons (first sh) (map (λ (n) 0) (rest sh))))