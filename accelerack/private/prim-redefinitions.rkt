#lang racket

(require (except-in racket/base map sqrt round ceiling floor)
         (prefix-in r: racket/base))
(provide sqrt round ceiling floor)

(define (sqrt n)
  (exact->inexact (r:sqrt n)))

(define (round n)
  (inexact->exact (r:round n)))

(define (ceiling n)
  (inexact->exact (r:ceiling n)))

(define (floor n)
  (inexact->exact (r:floor n)))
