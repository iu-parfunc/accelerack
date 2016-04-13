#lang racket
(require accelerack rackunit)

(test-case "identity function definition"
  (define-acc (f x) x)
  (check-true (procedure? f))
  (check-equal? 3 (f 3)))
