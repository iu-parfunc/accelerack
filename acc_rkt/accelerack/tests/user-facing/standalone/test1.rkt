#lang racket

(provide x)

(require accelerack)
(require rackunit)

(define x (acc-array (1 2 3)))
(define y (map (lambda (x) (+ x 1)) x))

(check-equal? 2 (car (acc-array->list y)))
