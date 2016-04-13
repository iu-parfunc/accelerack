#lang racket
(require accelerack rackunit)

(test-case "acc: map + fold"
  (define x (acc-array (1 2 3)))
  (define y (map (lambda(x) (+ x 1)) x))
  (define z (fold + 0 y))
  (check-equal? (acc-array->sexp z) 9))

(test-case "plain-racket: map + fold"
  (define x (acc-array (1 2 3)))
  (define y (map (lambda(x) (+ x 1)) x))
  (define z (fold + 0 y))
  (check-equal? (acc-array->sexp z) 9 ))

;; Test cases to check forcing of deferred array in fold
(test-case "test-case 10"
  (define x (acc-array (1 2 3)))
  (define y (fold + 0 x))
  (check-equal? (acc-array->sexp y) 6))
