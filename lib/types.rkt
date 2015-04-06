#lang racket

;;  Accelerack Types

(require "types-hi.rkt")

(define predicate/c (unconstrained-domain-> boolean?))

(provide
 
 base? avector?
 
 (contract-out
  [acc-vector? (-> base*? (unconstrained-domain-> boolean?))]
  [acc-vector (-> base*? (->* () #:rest (listof number?) avector?))]
  [acc-vector-length (-> avector? natural-number/c)]
  [acc-vector-ref (-> avector? natural-number/c base?)]
  [acc-list->vector (-> base*? (-> list? avector?))]
  [acc-vector-set! (-> avector? natural-number/c base? void?)]

  [payload? predicate/c]
  [element? (unconstrained-domain-> boolean?)]
  [element-valid? (-> payload*? element? boolean?)]
  
  [shape? predicate/c]
  [shape-size (-> shape? natural-number/c)]
  [shape-dim (-> shape? natural-number/c)]
  [shape-empty? (-> shape? boolean?)]
  [shape-nonempty? (-> shape? boolean?)]
  [index? predicate/c]
  [index-valid? (-> shape? index? boolean?)]
  [flatten-index (-> shape? index? natural-number/c)]
  
  )
 
 (struct-out r-arr)
 (struct-out r-fn)
 
 (all-from-out "types-hi.rkt")
 )

; a Base is any complex number
; TODO: complete?
(define base? number?)

;; an AccVector is one of ffi/vector, just two for now
(define (avector? t0)
  (vector? t0)
  #;(or (u64vector? t0)
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
;; acc-vector : BaseType* -> AccVectorConstructor
(define (acc-vector bty)
  (match bty
    ['Word64 u64vector]
    ['Float f64vector]
    [else (error 'acc-vector
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
       
;; a Element is an element of a Payload, either a Singleton or a Tuple, one of
;; - acc-base?
;; - (vector acc-base? ...)
(define (element? x)
  (or (base? x)
      (and (vector? x) (sequence-andmap base? x))))

;; element-valid? : PayloadType* Element -> Boolean
;; Determines if x is a valid Element for PayloadType* 
(define (element-valid? plty x)
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

;; a Shape is a concrete instance of ShapeType*,
;; denoting both number of dimensions and sizes of each
(define (shape? x)
  (and (list? x) (not (empty? x))
       (equal? 'Z (first x))
       (andmap integer? (rest x))))

;; shape-size : Shape -> Nat
;; Produces the total number of elements in arrays of given shape
(define (shape-size shape)
  (foldl * 1 (rest shape)))

;; shape-dim : Shape -> Nat
;; Produces the dimensionality of shape
(define (shape-dim shape)
  (sub1 (length shape)))

;; an EmptyShape has at least one zero-length dimension
;; shape-empty? : Shape -> Boolean
(define (shape-empty? sh)
  (and (positive? (shape-dim sh))
       (ormap zero? (rest sh))))

;; a NonEmptyShape is a Shape with no zero-length dimensions
(define shape-nonempty? (negate shape-empty?))

;; an Index is a Shape
(define index? shape?)

;; index-valid? : Shape Index -> Boolean
;; Determines if index is valid for shape
(define (index-valid? shape index)
  (and
   ; Check that dimensionality matches
   (= (shape-dim index) (shape-dim shape))
   ; Check that each index is within bounds
   (andmap (λ (i range) (if (equal? range 'Z)
                            (equal? i 'Z)
                            (<= 0 i (sub1 range))))
           index shape)))

;; flatten-index : Shape Index -> Nat
;; Convert multi-dimensional index in Shape form to a linear index into a flat sequence
(define (flatten-index shape index)
  (begin (unless (index-valid? shape index) (error "Invalid index for given shape"))
         (let helper ([index (rest index)] [shape (rest shape)])
           (if (empty? index)
               0
               (+ (foldr * (first index) (rest shape))
                  (helper (rest index) (rest shape)))))
         ))

;; index-0 : ShapeType* -> Index
;; Produces the index for first element of an array with a non-empty shape of type shty
(define (index-0 shty)
  (apply Z (build-list shty (const 0))))

;; a Rack-Array is a (r-arr Shape ArrayType Payload)
(struct r-arr (shape arrty payload)
  #:transparent
  #:guard (λ (shape arrty payload _)
            (cond
              [(not (= (shape-dim shape)
                       (array*-sh* arrty)))
               (error 'r-arr "Array dimensionality mismatch")]
              [(let [(len (shape-size shape))]
                 (andmap (λ (plty vec)
                           (cond
                             [(not ((acc-vector? plty) vec))
                              (error 'r-arr "Array Payload type mismatch")]
                             [(not (= (acc-vector-length vec) len))
                              (error 'r-arr "Array Shape size mismatch")]
                             [else true]))
                         (pl*->list (array*-pl* arrty)) payload))
               (values shape arrty payload)])))

;; a Rack-Fn is a (r-fn ShapeType* PayloadType* [Base ... -> Element])
(struct r-fn (shty plty fn)
  #:transparent
  #:guard (λ (shty plty fn _)
            (cond
              [(not (element-valid? plty (apply fn (rest (index-0 shty)))))
               (error 'r-fn "Calling fn with zero index did not produce valid payload element")]
              [else (values shty plty fn)]))
  #:property prop:procedure
  ; Rack-Fun Index -> Element
  (λ (this i)
    (apply (r-fn-fn this) (rest i)))
  )