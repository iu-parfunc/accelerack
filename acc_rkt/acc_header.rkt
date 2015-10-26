#lang racket

(require ffi/unsafe
         ffi/unsafe/define)

(define libacclib (ffi-lib "../acc_c/libacc"))
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
   _c-array
   _c-array-pointer
   c-array-type
   c-array-shape
   c-array-data
   make-c-array
   _c-vector
   _c-vector-pointer
   c-vector-length
   c-vector-type
   c-vector-data
   make-c-vector
   rkt-vector
   rkt-vector-length
   rkt-vector-type
   rkt-vector-data
   rkt-vector?
   make-rkt-vector
   rkt-array
   rkt-array-type
   rkt-array-shape
   rkt-array-data
   make-rkt-array
   scalar
   scalar-length)

;; C structure to store tuple/scalar information (one payload)
(define-cstruct _c-vector
   ([length _int]
    [type _int]
    [data _gcpointer]))

;; C structure to store accelerate arrays information (one logical array)
(define-cstruct _c-array
  ([type _int]
   [shape _c-vector-pointer]
   [data _c-vector-pointer]))

;; Racket structure to store tuple/scalar information
(define-struct rkt-vector
   (length
    type
    data))

;; Racket structure to store accelerate arrays information
(define-struct rkt-array
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
