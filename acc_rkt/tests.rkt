#lang racket

(require ffi/unsafe 
         ffi/unsafe/define 
         ffi/unsafe/cvector
         rackunit)
(require "verify-accelerack.rkt")
(require "allocate.rkt")
(require "ArrayUtils.rkt")
(require "acc_header.rkt")
(require "accelerack.rkt")

(define libacclib (ffi-lib "libacc"))
(define-ffi-definer define-libintegrator libacclib)
(define-libintegrator modify_vector (_fun _c-vector-pointer _int -> _void))
(define-libintegrator modify_array (_fun _c-array-pointer _string -> _void))
(define-libintegrator rkt_handler (_fun _c-array-pointer _string -> _void))

(define l1 '((2.0 3.2 11.2) (50.1 2.2 41.9)))
(define l2 '((2 (2 #t #f)) (1 (3 #f #f)) (4 (16 #t #f))))
(define l3 '(((2 (2 (3 #t))) (1 (3 (5 #t))) (4 (16 (7 #t)))) ((4 (5 (3 #f))) (1 (6 (15 #f))) (14 (26 (7 #t))))))

;; Array of nested tuples #(int #(int #(int bool)))
(define l4 '((((2 (2 (3 #t))) (1 (3 (5 #t))) (4 (16 (7 #t)))) ((4 (5 (3 #f))) (1 (6 (15 #f))) (14 (26 (7 #t)))))
            (((12 (12 (13 #t))) (11 (13 (15 #t))) (14 (26 (17 #t)))) ((14 (25 (13 #f))) (11 (16 (25 #f))) (24 (26 (17 #t)))))))
(define l5 '((2 (2 (3 5))) (1 (3 (5 6))) (4 (16 (7 13)))))
(define l6 '(((2 (2 (3 #t))) (1 (3 (5 #t))) (4 (16 (7 #t)))) ((4 (5 (3 #f))) (1 (6 (15 #f))) (14 (26 (7 #t))))))

(printf "test 1\n")
(acc (define x (vector _double '(2 3) l1)))
(define x-c (car x))
(define x-rkt (cadr x))
(readData* x-c)
(readData*-rkt x-rkt)

(printf "test 2\n")
(acc (define y (vector (_tuple _int (_tuple _int _bool _bool)) '(3) l2)))
(define y-c (car y))
(define y-rkt (cadr y))
(readData* y-c)
(readData*-rkt y-rkt)

(printf "test 3\n")
(define ax (acc_alloc '(_tuple _int (_tuple _int (_tuple _int _bool))) '(2 3) l3 "use"))
(define ax-c (car ax))
(define ax-rkt (cadr ax))
(readData* ax-c)
;(readData*-rkt ax-rkt)
(acc (map 'add1 ax-c))
(readData* ax-c)

(test-case "test 4"
  (display "test 4\n")
  (define bx (acc_alloc '(_tuple _int (_tuple _int (_tuple _int _bool))) '(2 2 3) l4 "use"))
  (define bx-c (car bx))
  (define bx-rkt (cadr bx))
  (display (readData* bx-c))(newline)  
  (display bx-rkt)(newline)
  (readData*-rkt bx-rkt)
  (acc (map 'add1 bx-c))
  (readData* bx-c)  
  (check-true (rkt-array? bx-rkt))  
  )

(test-case "test 5"
  (display "test 5\n")
  (acc (define z (vector (_tuple _int (_tuple _int (_tuple _int _int))) '(3) l5)))
  (define z-c (car z))
  (define z-rkt (cadr z))
  (readData* z-c)
  (acc (map 'sub1 z-c))
  (readData* z-c)
  ;(readData*-rkt z-rkt)
  )

(acc (define a (vector (_tuple _int (_tuple _int (_tuple _int _bool))) '(2 3) l6)))


