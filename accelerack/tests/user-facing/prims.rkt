#lang racket

(require accelerack rackunit)


(test-case "sqrt"
  (define-acc a (sqrt 9.0))
  (check-equal? a 3.0))

;; FIXME: Need to ensure inexact output:
#;
(test-case "sqrt int"
  (define-acc a (sqrt 9))
  (check-equal? a 3.0))

#;
(check-equal (acc (abs -5))
             5)


(test-case "min"
  (define-acc a (min 2 3))
  (check-equal? a 2))

(test-case "and"
  (define-acc a (and #t #f))
  (check-equal? a #f))


  
                
