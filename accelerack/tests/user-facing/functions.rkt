#lang racket
(require accelerack rackunit)

(test-case "identity function definition"
  (define-acc (f x) x)
  (check-true (procedure? f))
  (check-equal? 3 (f 3)))


(test-case "plus 5 function definition"
  (define-acc (f x) (+ x 5))
  (check-true (procedure? f))
  (check-equal? 8 (f 3)))

#; ;; FIXME! Should fail.  
(test-case "plus 5 function definition"
  (define-acc (f x) (+ x #t))
  (check-true (procedure? f))
  (check-equal? 8 (f 3)))


