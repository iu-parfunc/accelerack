#lang racket

;; General tests??

(require ffi/unsafe
         ;ffi/unsafe/define
         ;ffi/unsafe/cvector
         ;racket/runtime-path
         rackunit
         rackunit/text-ui)

(require accelerack)
(require "test-utils.rkt")
;(define-runtime-path libacc_hs "../../build/libacc-hs.so")
;; Disabling for now, gets a link error on stg_forkOnzh
;(define libacchslib (ffi-lib libacc_hs))

; (define-ffi-definer define-libintegrator libacclib)
; (define-libintegrator modify_vector (_fun _c-vector-pointer _int -> _void))
; (define-libintegrator modify_array (_fun _c-array-pointer _string -> _void))
; (define-libintegrator rkt_handler (_fun _c-array-pointer _string -> _void))

(define acc-invalid-test_cases (test-suite
  "invalid test cases"
  
  (test-case "test-case 1"
             "test-case 1"
             (check-exn exn:fail?
                        (lambda ()
                          (array () _int #f)))
             (display "Test 1 Success !!!") (newline))

  (test-case "test-case 2"
             "test-case 2"
             (check-exn exn:fail?
                        (lambda ()
                          (array () _double #f)))
             (display "Test 2 Success !!!") (newline))
  
  (test-case "test-case 3"
             "test-case 3"
             (check-exn exn:fail?
                        (lambda ()
                          (array (2 3) _int 5)))
             (display "Test 3 Success !!!") (newline))

  (test-case "test-case 4"
             "test-case 4"
             (check-exn exn:fail?
                        (lambda ()
                          (array (9) _int (1 2 3 4 5 6 7 8))))
             (display "Test 4 Success !!!") (newline))
  
  (test-case "test-case 5"
             "test-case 5"
             (check-exn exn:fail?
                        (lambda ()
                          (array (4) (_tuple _int (_tuple _int _bool _bool)) (#(2 #(2 #t #f)) #(1 #(3 #f #f)) #(4 #(16 #t #f))))))
             (display "Test 5 Success !!!") (newline))))

(define acc-valid-test_cases (test-suite
  "valid test cases"
  
  (test-case "test-case 1"
             "test-case 1"
             (letrec ([ls (array () _int 24)]
                      [cptr (car ls)]
                      [rktptr (cadr ls)])
                     (check-equal? '(24) (readData* cptr)))
             (display "Test 1 Success !!!") (newline))

  (test-case "test-case 2"
             "test-case 2"
             (letrec ([ls (array (8) _int (1 2 3 4 5 6 7 8))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)])
                     (check-equal? '(1 2 3 4 5 6 7 8) (readData* cptr)))
             (display "Test 2 Success !!!") (newline))

  (test-case "test-case 3"
             "test-case 3"
             (letrec ([ls (array (2 4) _bool ((#t #t #t #t) (#f #f #f #f)))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)])
                     (check-equal? '((#t #t #t #t) (#f #f #f #f)) (readData* cptr)))
             (display "Test 3 Success !!!") (newline))

  (test-case "test-case 4"
             "test-case 4"
             (letrec ([ls (array (2 3) _double ((2.0 3.2 11.2) (50.1 2.2 41.9)))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)])
                     (check-equal? (readData* cptr) (readData*-rkt rktptr)))
             (display "Test 4 Success !!!") (newline))

  (test-case "test-case 5"
             "test-case 5"
             (letrec ([ls (array (3) (_tuple _int (_tuple _int _bool _bool)) (#(2 #(2 #t #f)) #(1 #(3 #f #f)) #(4 #(16 #t #f))))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)])
                     (check-equal? (readData* cptr) (readData*-rkt rktptr)))
             (display "Test 5 Success !!!") (newline))

  (test-case "test-case 6"
             "test-case 6"
             (letrec ([ls (array (2 3)
                                 (_tuple _int (_tuple _int (_tuple _int _int)))
                                 ((#(2 #(2 #(3 5))) #(1 #(3 #(5 6))) #(4 #(16 #(7 7))))
                                  (#(4 #(5 #(3 7))) #(1 #(6 #(15 6))) #(14 #(26 #(7 5))))))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [data (readData* cptr)])
                     (begin
                       (acc (map 'add1 cptr))
                       (check-equal? (readData* cptr) (add1* data))))
             (display "Test 6 Success !!!") (newline))

  (test-case "test-case 7"
             "test-case 7"
             (letrec ([ls (array (2 2 3)
                                 (_tuple _int _double (_tuple _double))
                                 (((#(0 1.1 #(0.0)) #(0 2.2 #(0.0)) #(0 3.3 #(0.0)))
                                   (#(0 3.3 #(0.0)) #(0 5.5 #(0.0)) #(0 7.7 #(0.0))))
                                  ((#(0 2.2 #(0.0)) #(0 3.3 #(0.0)) #(0 4.4 #(0.0)))
                                   (#(0 4.4 #(0.0)) #(0 6.6 #(0.0)) #(0 1.1 #(0.0))))))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [data (readData* cptr)])
                     (begin
                       (acc (map 'add1 cptr))
                       (check-equal? (readData* cptr) (add1* data))))
             (display "Test 7 Success !!!") (newline))

  (test-case "test-case 8"
             "test-case 8"
             (letrec ([ls (array (2 2 3)
                                 (_tuple _int (_tuple _int (_tuple _int _double)))
                                 (((#(2 #(2 #(3 1.1))) #(1 #(3 #(5 2.2))) #(4 #(16 #(7 4.3))))
                                   (#(4 #(5 #(3 2.1))) #(1 #(6 #(15 5.4))) #(14 #(26 #(7 23.3)))))
                                  ((#(12 #(12 #(13 4.3))) #(11 #(13 #(15 12.1))) #(14 #(26 #(17 21.6))))
                                   (#(14 #(25 #(13 7.5))) #(11 #(16 #(25 8.3))) #(24 #(26 #(17 2.7)))))))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [data (readData* cptr)])
                     (begin
                       (acc (map 'add1 cptr))
                       (check-equal? (readData* cptr) (add1* data))))
             (display "Test 8 Success !!!") (newline))

  (test-case "test-case 9"
             "test-case 9"
             (letrec ([ls (array (3) (_tuple _int (_tuple _int (_tuple _int _int))) (#(2 #(2 #(3 5))) #(1 #(3 #(5 6))) #(4 #(16 #(7 13)))))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [data (readData* cptr)])
                     (begin
                       (acc (map 'sub1 cptr))
                       (check-equal? (readData* cptr) (sub1* data))))
             (display "Test 9 Success !!!") (newline))))

(display "\n<----------- Invalid test-cases Run ----------->\n")
(if (run-tests acc-invalid-test_cases) (display "\n!!! Test Run Successfull !!!\n") (display "\n!!! Test Run Failed !!!\n"))
(display "\n<----------- Valid test-cases Run ----------->\n")
(if (run-tests acc-valid-test_cases) (display "\n!!! Test Run Successfull !!!\n\n") (display "\n!!! Test Run Failed !!!\n\n"))


