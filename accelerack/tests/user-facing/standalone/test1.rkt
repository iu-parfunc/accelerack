#lang racket

(provide x)

(require accelerack)
(require rackunit)

(define-acc x (acc-array (1 2 3)))
(define-acc y (map (lambda(x) (+ x 1)) x))

(check-equal? 2 (car (acc-array->sexp y)))
