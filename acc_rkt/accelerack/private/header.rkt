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
   acc-manifest-array?
   acc-manifest-array-type
   acc-manifest-array-shape
   acc-manifest-array-data
   make-acc-manifest-array

   _acc-manifest-array
   _acc-manifest-array-pointer

   segment?
   _segment
   _segment-pointer
   segment-length
   segment-type
   segment-data
   make-segment

   scalar
   scalar-length)

;; C structure to store tuple/scalar information (one payload)
(define-cstruct _segment
   ([length _int]
    [type _int]
    [data _gcpointer]))

;; This is the Racket-side notion of a manifest array.
;; C structure to store accelerate arrays information (one logical array)
(define-cstruct _acc-manifest-array
  ([type _int]
   [shape _segment-pointer]
   [data _segment-pointer]))

;; Enum for storing the type of scalar
(define scalar
  (_enum `(c-int = ,C_INT
           c-double = ,C_DOUBLE
           c-bool = ,C_BOOL
           acc-payload-ptr = ,ACC_PAYLOAD_PTR ;; segment-ptr
           scalar-payload = ,SCALAR_PAYLOAD
           tuple-payload = ,TUPLE_PAYLOAD
           acc-manifest-array-ptr = ,ACC_ARRAY_PTR
           c-ptr = ,C_PTR
           rkt-payload-ptr = ,RKT_PAYLOAD_PTR)))

;; Number of scalar enum values defined
(define scalar-length 7)
