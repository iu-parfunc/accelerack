#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)

(define the-lib (ffi-lib "mylib1"))

(define-ffi-definer define-mylib the-lib)

(define-mylib isquare (_fun _int -> _int))