#lang racket

(provide x)

(require accelerack)
(require rackunit)

(define x (acc-array (1 2 3)))
(define-acc y (map (lambda(x) (+ x 1)) (use x (Array 1 Int))))

(check-equal? 2 (car (acc-array->sexp y)))
