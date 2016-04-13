#lang racket
(require
  accelerack/acc-array/private/manifest-array
  rackunit  )

(test-case "acc-manifest-array-flatref"
  (define y (make-empty-manifest-array #(2 3) 'Int))
  (check-equal? (manifest-array-flatref y 1) 0)  
  )

(test-case "list->manifest-array 1D - flatset"
  (define x (list->manifest-array 'Double #(3) '(4.4 5.5 6.6)))
  (manifest-array-flatset! x 1 99.9)
  (manifest-array-flatset! x 2 9.9)
  (check-equal? (manifest-array->sexp x) '(4.4 99.9 9.9) )
  (check-exn exn:fail?
             (lambda () (manifest-array-flatref x -1))
             "manifest flatref past negative")
  (check-exn exn:fail?
             (lambda () (manifest-array-flatref x 3))
             "manifest flatref past end")
  )

