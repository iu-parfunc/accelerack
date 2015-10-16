#lang racket

(require ffi/unsafe)

(define-cstruct _tuple
   ((length _int)
    (data   _gcpointer)))

(define _scalar
  (_enum '(double int bool)))

(define _type
  (make-union-type _tuple-pointer _scalar))

