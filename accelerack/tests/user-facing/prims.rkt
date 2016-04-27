#lang racket

(require (except-in racket/base sqrt map round ceiling floor))
(require accelerack rackunit)

;; Huh... it's too badthat this is needed:
(require (for-syntax (only-in accelerack sqrt round ceiling floor)))


(test-case "sqrt"
  (define-acc a (sqrt 9.0))
  (check-equal? a 3.0))

(test-case "sqrt int"
  (define-acc a (sqrt 9))
  (check-equal? a 3.0))


(check-equal? (round 9.6)   10)
(check-true   (exact? (round 9.6)))
(check-equal? (ceiling 9.6) 10)
(check-equal? (floor   9.6) 9)

(check-equal? (acc (round 9.6))   10)
(check-true   (exact? (acc (round 9.6))))
(check-equal? (acc (ceiling 9.6)) 10)
(check-equal? (acc (floor   9.6)) 9)

(check-equal? (acc (abs -5)) 5)
(check-equal? (acc (abs -5.5)) 5.5)

(test-case "min"
  (define-acc a (min 2 3))
  (check-equal? a 2))

(test-case "and"
  (define-acc a (and #t #f))
  (check-equal? a #f))
