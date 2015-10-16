#lang racket
(require "verify-accelerack.rkt")

;(define-syntax (acc stx)
 ; (syntax-case stx (define view load run get)
  ;;  [(acc (define x exp)) (verify-accelerack (syntax exp))]))


(define-syntax use
  (syntax-rules ()
    [(use exp) `(use ,exp)]))

(define-syntax acc
  (syntax-rules ()
    [(acc (use exp)) (if (verify-accelerack `(use ,exp))  'false)]))
