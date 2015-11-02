#lang racket

;; General tests??

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/cvector
         rackunit
         racket/runtime-path)

(require accelerack)

(define-runtime-path libacc_hs "../../build/libacc-hs.so")
;; Disabling for now, gets a link error on stg_forkOnzh
(define libacchslib (ffi-lib libacc_hs))

; (define-ffi-definer define-libintegrator libacclib)
; (define-libintegrator modify_vector (_fun _c-vector-pointer _int -> _void))
; (define-libintegrator modify_array (_fun _c-array-pointer _string -> _void))
; (define-libintegrator rkt_handler (_fun _c-array-pointer _string -> _void))

(test-case "test 1"
  (display "test 1\n")
  (define t1 (array (2 3) _double ((2.0 3.2 11.2) (50.1 2.2 41.9))))
  (define t1-c (car t1))
  (define t1-rkt (cadr t1))
  (display (readData* t1-c)) (newline)
  (display (readData*-rkt t1-rkt)) (newline) (newline))

(test-case "test 2"
  (display "test 2\n")
  (define t2 (array (3) (_tuple _int (_tuple _int _bool _bool)) (#(2 #(2 #t #f)) #(1 #(3 #f #f)) #(4 #(16 #t #f)))))
  (define t2-c (car t2))
  (define t2-rkt (cadr t2))
  (display (readData* t2-c)) (newline)
  (display (readData*-rkt t2-rkt)) (newline) (newline))

(test-case "test 3"
  (display "test 3\n")
  (define t3 (array (2 3) (_tuple _int (_tuple _int (_tuple _int _bool)))
                    ((#(2 #(2 #(3 #t))) #(1 #(3 #(5 #t))) #(4 #(16 #(7 #t))))
                     (#(4 #(5 #(3 #f))) #(1 #(6 #(15 #f))) #(14 #(26 #(7 #t)))))))
  (define t3-c (car t3))
  (define t3-rkt (cadr t3))
  (display (readData* t3-c)) (newline)
  (acc (map 'add1 t3-c))
  (display (readData* t3-c)) (newline) (newline))

(test-case "test 4"
  (display "test 4\n")
  (define t4 (array (2 2 3) (_tuple _int (_tuple _int (_tuple _int _bool)))
                    (((#(2 #(2 #(3 #t))) #(1 #(3 #(5 #t))) #(4 #(16 #(7 #t)))) (#(4 #(5 #(3 #f))) #(1 #(6 #(15 #f))) #(14 #(26 #(7 #t)))))
                     ((#(12 #(12 #(13 #t))) #(11 #(13 #(15 #t))) #(14 #(26 #(17 #t)))) (#(14 #(25 #(13 #f))) #(11 #(16 #(25 #f))) #(24 #(26 #(17 #t))))))))
  (define t4-c (car t4))
  (define t4-rkt (cadr t4))
  (display (readData* t4-c)) (newline)
  (display (readData*-rkt t4-rkt)) (newline)
  (acc (map 'add1 t4-c))
  (display (readData* t4-c)) (newline) (newline))

(test-case "test 5"
  (display "test 5\n")
  (define t5 (array (3) (_tuple _int (_tuple _int (_tuple _int _int))) (#(2 #(2 #(3 5))) #(1 #(3 #(5 6))) #(4 #(16 #(7 13))))))
  (define t5-c (car t5))
  (define t5-rkt (cadr t5))
  (display (readData* t5-c)) (newline)
  (acc (map 'sub1 t5-c))
  (display (readData* t5-c)) (newline)
  (display (readData*-rkt t5-rkt)) (newline) (newline))

(test-case "test 6"
  (display "test 6\n")
  (define t6 (array (2 2 3) (_tuple _int _bool (_tuple _double))
                    (((#(0 #f #(0.0)) #(0 #f #(0.0)) #(0 #f #(0.0)))
                      (#(0 #f #(0.0)) #(0 #f #(0.0)) #(0 #f #(0.0))))
                     ((#(0 #f #(0.0)) #(0 #f #(0.0)) #(0 #f #(0.0)))
                      (#(0 #f #(0.0)) #(0 #f #(0.0)) #(0 #f #(0.0)))))))
  (define t6-c (car t6))
  (define t6-rkt (cadr t6))
  (display (readData* t6-c)) (newline)
  (acc (map 'add1 t6-c))
  (display (readData* t6-c)) (newline)
  (display (readData*-rkt t6-rkt)) (newline) (newline))

(test-case "test 7"
  (display "test 7\n")
  (define t7 (array (8) _int (1 2 3 4 5 6 7 8)))
  (define t7-c (car t7))
  (define t7-rkt (cadr t7))
  (display (readData* t7-c)) (newline)
  (display (readData*-rkt t7-rkt)) (newline) (newline))

(test-case "test 8"
  (display "test 8\n")
  (define t8 (array () _int 24))
  (define t8-c (car t8))
  (define t8-rkt (cadr t8))
  (display (readData* t8-c)) (newline)
  (display (readData*-rkt t8-rkt)) (newline) (newline))

(test-case "test 9"
  (display "test 9\n")
  (define t9 (array (2 4) _bool ((#t #t #t #t) (#f #f #f #f))))
  (define t9-c (car t9))
  (define t9-rkt (cadr t9))
  (display (readData* t9-c)) (newline)
  (display (readData*-rkt t9-rkt)) (newline) (newline))
