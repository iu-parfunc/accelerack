#lang racket

(require (except-in racket/base sqrt map))
(require accelerack rackunit)

(require (for-syntax (only-in accelerack sqrt)))


(test-case "sqrt"
  (define-acc a (sqrt 9.0))
  (check-equal? a 3.0))

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


  
                
