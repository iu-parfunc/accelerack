#lang racket

;; tests corresponding to allocate.rkt

(require
  ffi/unsafe
  accelerack/acc-array/private/manifest-array/allocate
  (only-in accelerack/acc-array/private/manifest-array/structs scalar)
  rackunit
  (only-in '#%foreign ctype-scheme->c ctype-c->scheme)
  )

(test-case "list->manifest-array 0D"
  (define x (list->manifest-array 'Int '() '(99)))
  (check-equal? (get-type x) 0)
  (check-equal? (manifest-array-shape x) #())
  (check-equal? (manifest-array-size  x) '1)
  (check-equal? (manifest-array-dimension x) 0)
  (check-equal? (read-data* x) '99)
  )

(test-case "list->manifest-array 2D"
  (define x (list->manifest-array 'Int '(2 3) '(1 2 3 4 5 6)))
  ; (check-equal? ((ctype-scheme->c scalar) _int) (get-type x))
  (check-equal? (get-type x) 0)
  (check-equal? (manifest-array-shape x) #(2 3))
  (check-equal? (manifest-array-size x)  '6)
  (check-equal? (manifest-array-dimension x) 2)
  (check-equal? (read-data* x) '((1 2 3) (4 5 6)) )
  )

(test-case "make-empty-manifest-array 2D"
  (define y (make-empty-manifest-array '(2 3) _int))
  (check-equal? (get-type y) 0 )
  (check-equal? (manifest-array-shape y) #(2 3))
  (check-equal? (manifest-array-size  y) 6)
  (check-equal? (manifest-array-dimension y) 2)
  (check-equal? (read-data* y) '((0 0 0) (0 0 0))))

(test-case "make-empty-manifest-array 0D"
  (define y (make-empty-manifest-array '() _int))
  (check-equal?  (get-type y) 0 )
  (check-equal? (manifest-array-shape y) #() )
  (check-equal? (manifest-array-size  y) 1 )
  (check-equal? (manifest-array-dimension y) 0)
  (check-equal? (read-data* y) '0))

(test-case "acc-manifest-array-flatref"
  (define y (make-empty-manifest-array '(2 3) _int))
  (check-equal? (manifest-array-flatref y 1) 0)
  )

