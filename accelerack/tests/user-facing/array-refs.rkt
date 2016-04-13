#lang racket

;; Test basic operations on acc-arrays.

(require accelerack
         rackunit)

(test-case "0d ref"
  (define x (acc-array 100))
  (check-equal? (acc-array-ref x) 100))

(test-case "1d ref"
  (define x (acc-array (10 20 30)))
  (check-equal? (acc-array-ref x 2) 30))

(test-case "2d ref"
  (define x (acc-array ((10 20 30)
                        (40 50 60))))
  (check-equal? (acc-array-ref x 0 0) 10)
  (check-equal? (acc-array-ref x 1 2) 60))


(test-case "plain-racket: array ref"
  (define x (acc-array (11 22 33)))
  (check-equal? 11 (acc-array-flatref x 0))
  (check-equal? 22 (acc-array-flatref x 1))
  (check-equal? 33 (acc-array-flatref x 2))
  (check-exn exn:fail?
             (lambda () (acc-array-flatref x -1))
             "array ref negative")
  (check-exn exn:fail?
             (lambda () (acc-array-flatref x 3))
             "array ref past end")
  )

(test-case "plain-racket: 0D array of tuple ref"
           (define x (acc-array #(11 22 33)))
           (check-equal? 0 (acc-array-dimension x))
           (check-equal? '#(11 22 33) (acc-array-flatref x 0))
           ; (check-equal? '#(11 22 33) (acc-array-ref x)) ;; FINISHME
           )

(test-case "plain-racket: 1D array of tuple ref"
  (define x (acc-array ( #(11 22 33)) ))
  ; (check-equal? 1 (acc-array-dimension x))
  (check-equal? '#(11 22 33) (acc-array-flatref x 0))
  (check-exn exn:fail?
             (lambda () (acc-array-flatref x 1))
             "array ref past end"))
