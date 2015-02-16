#lang racket

(provide acc)

(define-syntax acc
  (syntax-rules ()
    [(acc (define (fn x) body)) (define (fn x) body)]
    ;[(acc (generate x 
    [(acc exp) (begin (display "uncaught") exp)]
    [(acc exp exp2 x ...) (begin (acc exp) (acc exp2 x ...))]))
