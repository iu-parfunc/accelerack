#lang racket

(require accelerack)
(require rackunit)
(require "test1.rkt") ;; Import x definition.

; FIXME: FAILING TEST
; (define z (map (lambda(x) (+ x 1)) x))

;; (check-equal? 2 (car (acc-array->sexp z)))
