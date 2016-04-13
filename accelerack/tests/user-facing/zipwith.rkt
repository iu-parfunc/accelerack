#lang racket
(require accelerack rackunit)

#;
  (test-case "plain-racket: zipwith"
             (define x (acc-array (2 2 2 2)))
             (define y (map (lambda(x) (+ x 1)) x))
             (define z (zipwith + x y))
             (check-equal? (acc-array->sexp z) '(6 6 6 6)))
