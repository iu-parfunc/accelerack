#lang racket
(require accelerack rackunit)

(test-case "plain-racket: zipwith"
  (define x (acc-array (2 2 2 2)))
  (define y (map (lambda(x) (+ x 1)) x))
  (define z (zipwith * x y))
  (check-equal? (acc-array->sexp z) '(6 6 6 6)))

(test-case "plain-racket: zipwith"
  (define x (acc-array ((1 2 3)
                        (4 5 6))))
  (define y (acc-array ((100 200)
                        (300 400)
                        (500 600))))
  (define z (zipwith + x y))
  (check-equal? (acc-array->sexp z)
                '((101 202)
                  (304 405))))
