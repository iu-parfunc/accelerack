#lang racket

;; tests corresponding to allocate.rkt

(require
  ffi/unsafe
  accelerack/acc-array/private/manifest-array
  (only-in accelerack/acc-array/private/manifest-array/structs scalar)
  rackunit
  (only-in '#%foreign ctype-scheme->c ctype-c->scheme)
  )

(test-case "list->manifest-array 0D"
  (define x (list->manifest-array 'Int '() '(99)))
  (check-equal? (manifest-array-type x)  '(Array 0 Int))
  (check-equal? (manifest-array-shape x) #())
  (check-equal? (manifest-array-size  x) '1)
  (check-equal? (manifest-array-dimension x) 0)
  (check-equal? (manifest-array->sexp x) '99)
  )

(test-case "list->manifest-array 1D"
  (define x (list->manifest-array 'Double '(3) '(4.4 5.5 6.6)))
  (check-equal? (manifest-array-type x)  '(Array 1 Double))
  (check-equal? (manifest-array-shape x) #(3))
  (check-equal? (manifest-array-size x)  '3)
  (check-equal? (manifest-array-dimension x) 1)
  (check-equal? (manifest-array->sexp x) '(4.4 5.5 6.6) )
  )

(test-case "list->manifest-array 2D"
  (define x (list->manifest-array 'Int '(2 3) '(1 2 3 4 5 6)))
  (check-equal? (manifest-array-type x)  '(Array 2 Int))
  (check-equal? (manifest-array-shape x) #(2 3))
  (check-equal? (manifest-array-size x)  '6)
  (check-equal? (manifest-array-dimension x) 2)
  (check-equal? (manifest-array->sexp x) '((1 2 3) (4 5 6)) )
  )

(test-case "make-empty-manifest-array 2D"
  (define y (make-empty-manifest-array '(2 3) 'Int))
  (check-equal? (manifest-array-type y)  '(Array 2 Int))
  (check-equal? (manifest-array-shape y) #(2 3))
  (check-equal? (manifest-array-size  y) 6)
  (check-equal? (manifest-array-dimension y) 2)
  (check-equal? (manifest-array->sexp y) '((0 0 0) (0 0 0))))

(test-case "make-empty-manifest-array 0D"
  (define y (make-empty-manifest-array '() 'Int))
  (check-equal? (manifest-array-type y)  '(Array 0 Int))
  (check-equal? (manifest-array-shape y) #() )
  (check-equal? (manifest-array-size  y) 1 )
  (check-equal? (manifest-array-dimension y) 0)
  (check-equal? (manifest-array->sexp  y) '0))

(test-case "acc-manifest-array-flatref"
  (define y (make-empty-manifest-array '(2 3) 'Int))
  (check-equal? (manifest-array-flatref y 1) 0)
  )

(test-case "list->manifest-array 1D - flatset"
  (define x (list->manifest-array 'Double '(3) '(4.4 5.5 6.6)))
  (manifest-array-flatset! x 1 99.9)
  (manifest-array-flatset! x 2 9.9)
  (check-equal? (manifest-array->sexp x) '(4.4 99.9 9.9) )
  )
