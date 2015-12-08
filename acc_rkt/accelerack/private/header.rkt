#lang racket

;; Define core types for Accelerack arrays and their constituient parts.

(require ffi/unsafe
         ffi/unsafe/define
         racket/runtime-path
         c-defs)

;; Import the acc type definitions from the c header file to maintain consistent naming with haskell
(define-runtime-path acc_types "../../../acc_c/acc_types.h")
(define ACC_TYPES (c-defs (path->string acc_types)))
(define-values (C_INT
                C_DOUBLE
                C_BOOL
                ACC_PAYLOAD_PTR
                SCALAR_PAYLOAD
                TUPLE_PAYLOAD
                ACC_ARRAY_PTR
                C_PTR
                RKT_PAYLOAD_PTR)
  (ACC_TYPES "%d" "C_INT"
                  "C_DOUBLE"
                  "C_BOOL"
                  "ACC_PAYLOAD_PTR"
                  "SCALAR_PAYLOAD"
                  "TUPLE_PAYLOAD"
                  "ACC_ARRAY_PTR"
                  "C_PTR"
                  "RKT_PAYLOAD_PTR"))

(provide

   ;; The public type for Accelerate/Accelerack arrays:
   acc-array?
   acc-array-type
   acc-array-shape
   acc-array-data
   make-acc-array

   _acc-array
   _acc-array-pointer

   segment?
   _segment
   _segment-pointer
   segment-length
   segment-type
   segment-data
   make-segment

   ;; Racket-side versions of the C struct data (not currently used)
   rkt-segment
   rkt-segment-length
   rkt-segment-type
   rkt-segment-data
   rkt-segment?
   make-rkt-segment

   rkt-acc-array?
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
           acc-payload-ptr = ,ACC_PAYLOAD_PTR ;; segment-ptr
           scalar-payload = ,SCALAR_PAYLOAD
           tuple-payload = ,TUPLE_PAYLOAD
           acc-array-ptr = ,ACC_ARRAY_PTR
           c-ptr = ,C_PTR
           rkt-payload-ptr = ,RKT_PAYLOAD_PTR)))

;; Number of scalar enum values defined
(define scalar-length 7)
