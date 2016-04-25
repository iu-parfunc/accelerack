#lang racket

(require (except-in racket/base map sqrt)
         (prefix-in r: racket/base))
(provide sqrt)

(define (sqrt n)
  (exact->inexact (r:sqrt n)))
