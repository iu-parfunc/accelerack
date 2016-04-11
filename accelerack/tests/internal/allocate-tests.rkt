#lang racket

;; tests corresponding to allocate.rkt

(require
  ffi/unsafe
  accelerack/acc-array/private/manifest-array/allocate
  rackunit
  )

(test-case "list->manifest-array 1"
  (define x (list->manifest-array _int '(2 3) '(1 2 3 4 5 6)))
  ;; Huh?  This looks bogus!
  (check-equal? 0 (get-type x))  
  (check-equal? '(2 3) (get-shape x))
  (check-equal? '((1 2 3) (4 5 6)) (read-data* x)))

(test-case "make-empty-manifest-array 1"
  (define y (make-empty-manifest-array '(2 3) _int))
  (check-equal? 0 (get-type y))
  (check-equal? '(2 3) (get-shape y))
  (check-equal? '((0 0 0) (0 0 0)) (read-data* y)))
