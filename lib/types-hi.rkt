#lang racket

;; Accelerack Higher Level Types (?? universes, classes, kinds, categories ??)

(require ffi/vector)

(provide
 acc-type*? base*? payload*? shape*? array*? fn*?
 instance-of-base*?
 Z ;TODO: should have a contract accept (or/c (listof Int?) (listof integer?))
 DIM0 DIM1 DIM2 DIM3
 (all-from-out ffi/vector)
 )

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

;; ShapeType* denotes number and size of dimensions
(define (shape*? t0)
  (match t0
    [`(Z ,ints ...) 
     (andmap (lambda (x) (eq? 'Int x)) ints)]
    [else #f]))

;; a ShapeType* is (Z ['Int] ...)
;; a Shape is (Z [Int] ...)
(define (Z . ls)
  (cons 'Z ls))

; DIM0 is (Z)
; DIM1 is (Z Int)
; DIM2 is (Z Int Int)
;; TODO: Keep these?  Are they a convenience given the need to unquote?
;; Convenience ShapeType*s
(define DIM0 (Z))
(define DIM1 (Z 'Int))
(define DIM2 (Z 'Int 'Int))
(define DIM3 (Z 'Int 'Int 'Int))

;; ArrayType*
(define (array*? t0)
  (match t0
    [`(Array ,shty ,plty) (and (shape*? shty) (payload*? plty))]
    [else false]))

;; FnType*
(define (fn*? t0)
  (match t0
    [`(,at0 ,at1 ... -> ,atR) (andmap acc-type*? (cons atR (cons at0 at1)))]
    [else false]))