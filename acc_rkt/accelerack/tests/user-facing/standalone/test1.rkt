#lang racket

(provide x y)

(require accelerack)
(require rackunit)

(define-acc x (acc-array (1 2 3)))
(define-acc y (map (lambda(x) (+ x 1)) x))

(check-equal? 2 (car (acc-array->list y)))

(printf "Test1 side effects complete.\n")
