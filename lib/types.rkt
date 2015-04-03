#lang racket

;;  Accelerack Types

(require "types-hi.rkt")

(define predicate/c (unconstrained-domain-> boolean?))

(provide
 
 base? avector?
 
 (contract-out
  [acc-vector? (-> base*? (unconstrained-domain-> boolean?))]
  [acc-vector-length (-> avector? natural-number/c)]
  [acc-vector-ref (-> avector? natural-number/c base?)]
  [acc-list->vector (-> base*? (-> list? avector?))]
  [acc-vector-set! (-> avector? natural-number/c base? void?)]

  [payload? predicate/c]
  [payload-val? (unconstrained-domain-> boolean?)]
  [payload-val-valid? (-> payload-val? payload*? boolean?)]
  
  [shape? predicate/c]
  [shape-size (-> shape? natural-number/c)]
  [shape-dim (-> shape? natural-number/c)]
  [index-valid? (-> shape? shape? boolean?)]
  [flatten-index (-> shape? shape? natural-number/c)]
  [index-0 (-> shape? shape?)]
  )
 
 arr-shty arr-plty arr-plty-list arr-dim
 
 (all-from-out "types-hi.rkt")
 )

; a Base is any complex number
; TODO: complete?
(define base? number?)

;; an AccVector is one of ffi/vector, just two for now
(define (avector? t0)
  (or (u64vector? t0)
      (f64vector? t0)))

;;;; Need a way to polymorphically access vector helper functions for payloads
;; TODO: Consider using a macro
;; acc-vector? : BaseType* -> VectorPredicate
(define (acc-vector? bty)
  (match bty
    ['Word64 u64vector?]
    ['Float f64vector?]
    [else (error 'acc-vector?
                 (format "Not all BaseTypes are yet supported, given:~a" bty))]))
;; acc-vector-length : AccVector -> Nat
(define (acc-vector-length v)
  (cond
    [(u64vector? v) (u64vector-length v)]
    [(f64vector? v) (f64vector-length v)]
    ))
;; acc-vector-ref : AccVector Nat -> Base
(define (acc-vector-ref v k)
  (cond
    [(u64vector? v) (u64vector-ref v k)]
    [(f64vector? v) (f64vector-ref v k)]
    ))
;; acc-list->vector : BaseType* -> VectorFromListFn
(define (acc-list->vector bty)
  (match bty
    ['Word64 list->u64vector]
    ['Float list->f64vector]
    [else (error 'acc-list->vector
                 (format "Not all BaseTypes are yet supported, given:~a" bty))]))
;; acc-vector-set! : AccVector Nat Base  -> Void
(define (acc-vector-set! v k val)
  (cond
    [(u64vector? v) (u64vector-set! v k val)]
    [(f64vector? v) (f64vector-set! v k val)]
    ))

;; a Payload is either a Singleton or unzipped Tuples
;; unzipped Tuples are represented as a Sequence of same-length AccVectors
;; AccVectors can be different types
;; Allows the empty sequence and a sequence of empty AccVectors
;; TODO: too general?
(define (payload? x)
  (or (base? x)
      (and (sequence? x)
           (or (zero? (sequence-length x))
               (let ([len (acc-vector-length (sequence-ref x 0))])
                 (sequence-andmap (λ (v) (and (avector? v)
                                              (= (acc-vector-length v) len)))
                                  x))))))
       
;; a PayloadVal is an element of a Payload, either a Singleton or a Tuple, one of
;; - acc-base?
;; - (vector acc-base? ...)
(define (payload-val? x)
  (or (base? x)
      (and (vector? x) (sequence-andmap base? x))))

;; payload-val-valid? : PayloadVal PayloadType* -> Boolean
;; Determines if x is a valid PayloadVal for PayloadType* 
(define (payload-val-valid? x plty)
  (if (base*? plty)
      (instance-of-base*? x plty)
      (and (= (vector-length plty)
              (vector-length x))
           #;(for*/fold ([result true])
                      ([val x]
                       [ty plty])
             (values (and (instance-of-base*? x plty) result)))
           (andmap identity
                   (map instance-of-base*? (vector->list x) (vector->list plty))))))

;; a Shape is a concrete instance of ShapeType*
(define (shape? x)
  (and (symbol=? 'Z (first x))
       (andmap integer? (rest x))))

;; shape-size : (U Shape ShapeType*) -> Nat
;; Produces the number of elements in arrays of given shape
(define (shape-size shape)
  (foldl * 1 (rest shape)))

;; shape-dim : (U Shape ShapeType*) -> Nat
;; Produces the dimensionality of shape
(define (shape-dim shape)
  (sub1 (length shape)))

;; index-valid? : Shape Shape -> Boolean
;; Determines if index is valid for shape
(define (index-valid? index shape)
  (and
   ; Check that dimensionality matches
   (= (shape-dim index) (shape-dim shape))
   ; Check that each index is within bounds
   (andmap (λ (i range) (if (symbol? range)
                            (symbol=? i range)
                            (<= 0 i (sub1 range))))
           index shape)))

;; flatten-index : Shape Shape -> Nat
;; Convert multi-dimensional index in Shape form to a linear index into a flat sequence
(define (flatten-index index shape)
  (begin (unless (index-valid? index shape) (error "Invalid index for given shape"))
         (let helper ([index (rest index)] [shape (rest shape)])
           (if (empty? index)
               0
               (+ (foldr * (first index) (rest shape))
                  (helper (rest index) (rest shape)))))
         ))

;; index-0 : Shape -> Shape
;; Produces the index for first element of an array of shape sh
(define (index-0 sh)
  (cons (first sh) (map (λ (n) 0) (rest sh))))

(define (arr-shty arrty)
  (match arrty
    [`(Array ,sh ,pl) sh]))
(define (arr-plty arrty)
  (match arrty
    [`(Array ,sh ,pl) pl]))
(define (arr-plty-list arrty)
  (match arrty
    [`(Array ,sh ,pl) #:when (vector? pl) (vector->list pl)]
    [`(Array ,sh ,pl) #:when (base*? pl) (list pl)]))
(define (arr-dim arrty)
  (match arrty
    [`(Array ,sh ,pl) (shape-dim sh)]))
