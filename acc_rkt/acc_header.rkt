#lang racket

(require ffi/unsafe)

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
   scalar
   scalar-length)

;; C structure to store tuple/scalar information
(define-cstruct _c-vector
   ([length _int]
    [type _int]
    [data _gcpointer]))

;; C structure to store accelerate arrays information
(define-cstruct _c-array
  ([type _int]
   [shape _c-vector-pointer]
   [data _c-vector-pointer]))

;; Enum for storing the type of scalar
(define scalar
  (_enum '(_tuple-pointer _gcpointer _double _int _bool _scalar _tuple)))

;; Number of scalar enum values defined
(define scalar-length 7)
