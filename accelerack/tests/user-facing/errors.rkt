#lang racket
(require accelerack rackunit
         syntax/macro-testing
         )

;; Test for the expected (hopefully good) error messages

(test-case "test-case 1"
  (check-exn
   #rx"undefined variable used in Accelerack expression"
   (lambda ()
     (convert-compile-time-error
      (let ()
        ;; (define-acc x (acc-array (1 2 3)))
        (define q 1)
        (define-acc y (map (lambda(y) (+ z (use q))) (acc-array (1 2 3))))
        (check-equal? 2 (car (acc-array->sexp y))))
      ))))

(test-case "test-case 2"
  (check-exn
   #rx"Unbound variable used in Accelerack 'use'\n"
   (lambda ()
     (convert-compile-time-error
      (let ()
        ;; (define-acc x (acc-array (1 2 3)))
        (define-acc y (map (lambda(y) (+ y (use q))) (acc-array (1 2 3))))
        (check-equal? 2 (car (acc-array->sexp y))))
                 ))))
