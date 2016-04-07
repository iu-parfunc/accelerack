#lang racket

(require accelerack)
(require rackunit)
(require "test1.rkt") ;; Import x definition.

(printf "Test2 side effects beginning.  Here is x/y ~a ~a\n" x y)

(define-acc z1 (acc-array (1 2 3 4)))
; FIXME: FAILING TEST
; (define-acc z (map (lambda(x) (+ x 1)) x))

;; (check-equal? 2 (car (acc-array->list z)))


(printf "Test2 side effects complete.\n")
