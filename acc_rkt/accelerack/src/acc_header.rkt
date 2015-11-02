#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         racket/runtime-path)

(provide libacclib)

(define-runtime-path libacc "../acc_c/libacc")

(define libacclib (ffi-lib libacc))
(define-ffi-definer define-libintegrator libacclib)
(define-libintegrator C_INT _int)
(define-libintegrator C_BOOL _int)
(define-libintegrator C_DOUBLE _int)
(define-libintegrator C_PTR _int)
(define-libintegrator ACC_PAYLOAD_PTR _int)
(define-libintegrator SCALAR_PAYLOAD _int)
(define-libintegrator TUPLE_PAYLOAD _int)
(define-libintegrator RKT_PAYLOAD_PTR _int)

(provide
   _acc-array
   _acc-array-pointer
   acc-array-type
   acc-array-shape
   acc-array-data
   make-acc-array
   _segment
   _segment-pointer
   segment-length
   segment-type
   segment-data
   make-segment
   rkt-segment
   rkt-segment-length
   rkt-segment-type
   rkt-segment-data
   rkt-segment?
   make-rkt-segment
   rkt-acc-array
   rkt-acc-array-type
   rkt-acc-array-shape
   rkt-acc-array-data
   make-rkt-acc-array
   scalar
   scalar-length)

;; C structure to store tuple/scalar information (one payload)
(define-cstruct _segment
   ([length _int]
    [type _int]
    [data _gcpointer]))

;; C structure to store accelerate arrays information (one logical array)
(define-cstruct _acc-array
  ([type _int]
   [shape _segment-pointer]
   [data _segment-pointer]))

;; Racket structure to store tuple/scalar information
(define-struct rkt-segment
   (length
    type
    data))

;; Racket structure to store accelerate arrays information
(define-struct rkt-acc-array
  (type
   shape
   data))


;; Enum for storing the type of scalar
(define scalar
  (_enum `(c-int = ,C_INT
           c-double = ,C_DOUBLE
           c-bool = ,C_BOOL
           acc-payload-ptr = ,ACC_PAYLOAD_PTR
           scalar-payload = ,SCALAR_PAYLOAD
           tuple-payload = ,TUPLE_PAYLOAD
           c-ptr = ,C_PTR
           rkt-payload-ptr = ,RKT_PAYLOAD_PTR)))

;; Number of scalar enum values defined
(define scalar-length 7)
