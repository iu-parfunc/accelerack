#lang racket

(require accelerack)  ;; Thus this module is really all we need.
(require rackunit
         ; (only-in accelerack/private/utils accelerack-debug-mode?)
         ; syntax/macro-testing
         )

(test-case "array ascription"
  (define-acc x (acc-array (#f #f #f)))
  (void))

(test-case "use a scalar in an array expression 0"
  (define-acc x 4)
  ;; [2016.04.23] FIXME: This causes an error, after first
  ;; incorrectly resolving the type of 'x' to (Array 1 Bool)
  (define-acc y (generate (lambda () x)))
  (void))

