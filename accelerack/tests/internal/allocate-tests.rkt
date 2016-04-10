#lang racket

(require
  ffi/unsafe
  accelerack/private/allocate
  rackunit
  )

(test-case "acc-alloc 1"
  (define x (acc-alloc _int '(2 3) '(1 2 3 4 5 6)))
  ;; Huh?  This looks bogus!
  (check-equal? 0 (get-type x))  
  (check-equal? '(2 3) (get-shape x))
  (check-equal? '((1 2 3) (4 5 6)) (read-data* x)))

(test-case "alloc-unit 1"
  (define y (alloc-unit '(2 3) _int))
  (check-equal? 0 (get-type y))
  (check-equal? '(2 3) (get-shape y))
  (check-equal? '((0 0 0) (0 0 0)) (read-data* y)))
