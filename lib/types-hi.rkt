#lang racket

;; Accelerack Higher Level Types (?? universes, classes, kinds, categories ??)

(provide
 acc-type*?
 base*? payload*? shape*? array*? fn*?
 instance-of-base*?
 Z ;TODO: should have a contract accept (or/c (listof Int?) (listof integer?))
 DIM0 DIM1 DIM2 DIM3
 (struct-out array*)
 pl*->list
 u64vector u64vector? u64vector-length u64vector-ref u64vector-set! list->u64vector u64vector->list
 f64vector f64vector? f64vector-length f64vector-ref f64vector-set! list->f64vector f64vector->list
 )

;for transparency, ffi/vector not needing yet
(define u64vector vector)
(define u64vector? vector?)
(define u64vector-length vector-length)
(define u64vector-ref vector-ref)
(define u64vector-set! vector-set!)
(define list->u64vector list->vector)
(define u64vector->list vector->list)
(define f64vector vector)
(define f64vector? vector?)
(define f64vector-length vector-length)
(define f64vector-ref vector-ref)
(define f64vector-set! vector-set!)
(define list->f64vector list->vector)
(define f64vector->list vector->list)

;; Accelerack type categories include...
(define (acc-type*? t0)
  (or (base*? t0)
      (payload*? t0)
      (shape*? t0)
      (array*? t0)
      (fn*? t0)
      ))

;; a BaseType* is one of these selected numeric primitives
(define (base*? t0)
  (match t0
    [(or 'Int 'Int8 'Int16 'Int32 'Int64) #t]
    [(or 'Word 'Word8 'Word16 'Word32 'Word64) #t]
    ;; TODO: support C Types?  Let's not...
    [(or 'Float 'Double) #t]
    [else false]))

;; instance-of-base? : Any BaseType* -> Boolean
;; Determines if x is an instance of bty
;; TODO: implement!
;; TODO: and then test!
(define (instance-of-base*? x bty)
  (number? x))

;; a PayloadType* is one of:
;; - BaseType*
;; - (vector BaseType* ...)
(define (payload*? t0)
  (match t0
    [(vector t1* ...) (andmap base*? t1*)]
    [else (base*? t0)]
    ))

;; pl*->list : PayloadType* -> [ListOf Base*]
;; (pl*->list plty) produces a (normalized) list rep of plty
;; Normalization refers to treatment of singleton payload types
(define (pl*->list plty)
  (cond
    [(vector? plty) (vector->list plty)]
    [(base*? plty) (list plty)]))

;; ShapeType* denotes number of dimensions
(define shape*? natural-number/c; t0)
  #;(match t0
    [`(Z ,ints ...) 
     (andmap (lambda (x) (eq? 'Int x)) ints)]
    [else #f]))

;; a ShapeType* is Nat ;;old;;(Z ['Int] ...)
;; a Shape is (Z [Int] ...)
(define (Z . ls)
  (cons 'Z ls))

; DIM0 is (Z)
; DIM1 is (Z Int)
; DIM2 is (Z Int Int)
;; TODO: Keep these?  Are they a convenience given the need to unquote?
;; Convenience ShapeType*s
;(define DIM0 (Z))
;(define DIM1 (Z 'Int))
;(define DIM2 (Z 'Int 'Int))
;(define DIM3 (Z 'Int 'Int 'Int))
(define DIM0 0)
(define DIM1 1)
(define DIM2 2)
(define DIM3 3)

;; ArrayType*
;; TODO: add guard
(struct array* (sh* pl*) #:transparent)

;; FnType*
(define (fn*? t0)
  (match t0
    [`(,at0 ,at1 ... -> ,atR) (andmap acc-type*? (cons atR (cons at0 at1)))]
    [else false]))