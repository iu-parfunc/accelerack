#lang racket

;; General tests??

(require (except-in ffi/unsafe ->)
         ;ffi/unsafe/define
         ;ffi/unsafe/cvector
         ;racket/runtime-path
         rackunit
         rackunit/text-ui)

(require accelerack)
(require (only-in accelerack/private/prototype cmap)
         (only-in accelerack/private/syntax array)
         (only-in accelerack/private/allocate read-data* read-data)
         (only-in accelerack/private/global_utils vector->list*)
         (only-in accelerack/private/header acc-manifest-array-data)
         accelerack/private/racket_ops
         (only-in accelerack/private/global_utils accelerack-debug-mode?)
         )

; (require "test-utils.rkt")

(define (maybe-display msg)
  (when (accelerack-debug-mode?)
    (fprintf (current-error-port) "~a\n" msg)))

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
             (maybe-display "Test 1 Success !!!"))

  (test-case "test-case 2"
             "test-case 2"
             (check-exn exn:fail?
                        (lambda ()
                          (array () _double #f)))
             (maybe-display "Test 2 Success !!!"))

  (test-case "test-case 3"
             "test-case 3"
             (check-exn exn:fail?
                        (lambda ()
                          (array (2 3) _int 5)))
             (maybe-display "Test 3 Success !!!"))

  (test-case "test-case 4"
             "test-case 4"
             (check-exn exn:fail?
                        (lambda ()
                          (array (9) _int (1 2 3 4 5 6 7 8))))
             (maybe-display "Test 4 Success !!!"))

  (test-case "test-case 5"
             "test-case 5"
             (check-exn exn:fail?
                        (lambda ()
                          (array (4) #(_int #(_int _bool _bool)) (#(2 #(2 #t #f)) #(1 #(3 #f #f)) #(4 #(16 #t #f))))))
             (maybe-display "Test 5 Success !!!"))))

(define acc-valid-test_cases (test-suite
  "valid test cases"

  (test-case "test-case 1"
             "test-case 1"
             (letrec ([cptr (array () _int 24)])
                     (check-equal? '24 (read-data* cptr)))
             (maybe-display "Test 1 Success !!!"))

  (test-case "test-case 2"
             "test-case 2"
             (letrec ([cptr (array (8) _int (1 2 3 4 5 6 7 8))])
                     (check-equal? '(1 2 3 4 5 6 7 8) (read-data* cptr)))
             (maybe-display "Test 2 Success !!!"))

  (test-case "test-case 3"
             "test-case 3"
             (letrec ([cptr (array (2 4) _bool ((#t #t #t #t) (#f #f #f #f)))])
                     (check-equal? '((#t #t #t #t) (#f #f #f #f)) (read-data* cptr)))
             (maybe-display "Test 3 Success !!!"))

  (test-case "test-case 4"
             "test-case 4"
             (letrec ([cptr (array (2 3) _double ((2.0 3.2 11.2) (50.1 2.2 41.9)))])
                     (check-equal? (read-data* cptr) '((2.0 3.2 11.2) (50.1 2.2 41.9))))
             (maybe-display "Test 4 Success !!!"))

  (test-case "test-case 5"
             "test-case 5"
             (letrec ([cptr (array (3) #(_int #(_int _bool _bool)) (#(2 #(2 #t #f)) #(1 #(3 #f #f)) #(4 #(16 #t #f))))])
                     (check-equal? (read-data* cptr) '(#(2 #(2 #t #f)) #(1 #(3 #f #f)) #(4 #(16 #t #f)))))
             (maybe-display "Test 5 Success !!!"))

  (test-case "test-case 6"
             "test-case 6"
             (letrec ([cptr (array (2 3)
                                 #(_int #(_int #(_int _int)))
                                 ((#(2 #(2 #(3 5))) #(1 #(3 #(5 6))) #(4 #(16 #(7 7))))
                                  (#(4 #(5 #(3 7))) #(1 #(6 #(15 6))) #(14 #(26 #(7 5))))))]
                      [data (vector->list* (read-data* cptr))])
                     (begin
                       (cmap 'add1 cptr)
                       (check-equal? (vector->list* (read-data* cptr)) (add1* data))))
             (maybe-display "Test 6 Success !!!"))

  (test-case "test-case 7"
             "test-case 7"
             (letrec ([cptr (array (2 2 3)
                                 #(_int _double #(_double))
                                 (((#(0 1.1 #(0.0)) #(0 2.2 #(0.0)) #(0 3.3 #(0.0)))
                                   (#(0 3.3 #(0.0)) #(0 5.5 #(0.0)) #(0 7.7 #(0.0))))
                                  ((#(0 2.2 #(0.0)) #(0 3.3 #(0.0)) #(0 4.4 #(0.0)))
                                   (#(0 4.4 #(0.0)) #(0 6.6 #(0.0)) #(0 1.1 #(0.0))))))]
                      [data (vector->list* (read-data* cptr))])
                     (begin
                       (cmap 'add1 cptr)
                       (check-equal? (vector->list* (read-data* cptr)) (add1* data))))
             (maybe-display "Test 7 Success !!!"))

  (test-case "test-case 8"
             "test-case 8"
             (letrec ([cptr (array (2 2 3)
                                 #(_int #(_int #(_int _double)))
                                 (((#(2 #(2 #(3 1.1))) #(1 #(3 #(5 2.2))) #(4 #(16 #(7 4.3))))
                                   (#(4 #(5 #(3 2.1))) #(1 #(6 #(15 5.4))) #(14 #(26 #(7 23.3)))))
                                  ((#(12 #(12 #(13 4.3))) #(11 #(13 #(15 12.1))) #(14 #(26 #(17 21.6))))
                                   (#(14 #(25 #(13 7.5))) #(11 #(16 #(25 8.3))) #(24 #(26 #(17 2.7)))))))]
                      [data (vector->list* (read-data* cptr))])
                     (begin
                       (cmap 'add1 cptr)
                       (check-equal? (vector->list* (read-data* cptr)) (add1* data))))
             (maybe-display "Test 8 Success !!!"))

  (test-case "test-case 9"
             "test-case 9"
             (letrec ([cptr (array (3) #(_int #(_int #(_int _int))) (#(2 #(2 #(3 5))) #(1 #(3 #(5 6))) #(4 #(16 #(7 13)))))]
                      [data (vector->list* (read-data* cptr))])
                     (begin
                       (cmap 'sub1 cptr)
                       (check-equal? (vector->list* (read-data* cptr)) (sub1* data))))
             (maybe-display "Test 9 Success !!!"))

  (test-case "test-case 10"
             "test-case 10"
             (letrec ([cptr (array (3) #(_int #(_int _bool _bool) _double) (#(2 #(2 #t #f) 1.1) #(1 #(3 #f #f) 3.3) #(4 #(16 #t #f) 2.4)))])
                     (check-equal? (read-data* cptr) '(#(2 #(2 #t #f) 1.1) #(1 #(3 #f #f) 3.3) #(4 #(16 #t #f) 2.4))))
             (maybe-display "Test 10 Success !!!"))

  (test-case "test-case 11"
             "test-case 11"
             (letrec ([cptr (array (2 3 4 5) #(_int #(_bool) _double)
                                           ((((#(2 #(#t) 1.1) #(1 #(#t) 3.3) #(4 #(#t) 2.4) #(5 #(#t) 3.4) #(6 #(#t) 2.4))
                                              (#(3 #(#t) 1.1) #(6 #(#t) 3.3) #(9 #(#t) 2.4) #(15 #(#f) 13.4) #(16 #(#t) 21.4))
                                              (#(4 #(#t) 1.1) #(7 #(#t) 3.3) #(10 #(#t) 2.4) #(25 #(#f) 23.4) #(26 #(#f) 22.4))
                                              (#(5 #(#t) 1.1) #(8 #(#t) 3.3) #(11 #(#t) 2.4) #(35 #(#t) 33.4) #(16 #(#t) 23.4)))

                                             ((#(12 #(#t) 1.1) #(1 #(#t) 3.3) #(4 #(#t) 2.4) #(5 #(#t) 3.4) #(6 #(#t) 2.4))
                                              (#(13 #(#t) 1.1) #(6 #(#t) 3.3) #(9 #(#t) 2.4) #(15 #(#f) 13.4) #(16 #(#t) 21.4))
                                              (#(14 #(#t) 1.1) #(7 #(#t) 3.3) #(10 #(#t) 2.4) #(25 #(#f) 23.4) #(26 #(#f) 22.4))
                                              (#(15 #(#t) 1.1) #(8 #(#t) 3.3) #(11 #(#t) 2.4) #(35 #(#t) 33.4) #(16 #(#t) 23.4)))

                                             ((#(22 #(#t) 1.1) #(1 #(#t) 2.3) #(4 #(#t) 21.4) #(5 #(#t) 3.4) #(6 #(#t) 2.4))
                                              (#(23 #(#t) 1.1) #(16 #(#t) 3.3) #(9 #(#t) 22.4) #(15 #(#f) 13.4) #(16 #(#t) 21.4))
                                              (#(24 #(#t) 1.1) #(3 #(#t) 2.3) #(10 #(#t) 12.4) #(25 #(#f) 23.4) #(3 #(#f) 22.4))
                                              (#(25 #(#t) 1.1) #(4 #(#t) 3.3) #(11 #(#t) 22.4) #(35 #(#f) 33.4) #(6 #(#t) 23.4))))

                                            (((#(12 #(#t) 1.1) #(1 #(#t) 1.3) #(4 #(#t) 2.4) #(0 #(#t) 3.4) #(6 #(#t) 2.4))
                                              (#(13 #(#t) 1.1) #(6 #(#t) 3.3) #(9 #(#t) 2.4) #(1 #(#f) 13.4) #(16 #(#t) 21.4))
                                              (#(14 #(#t) 1.1) #(7 #(#t) 13.3) #(10 #(#t) 2.4) #(2 #(#f) 3.4) #(26 #(#f) 22.4))
                                              (#(15 #(#t) 1.1) #(8 #(#t) 3.3) #(11 #(#t) 2.4) #(3 #(#t) 5.4) #(16 #(#f) 23.4)))

                                             ((#(2 #(#t) 1.1) #(1 #(#t) 2.3) #(4 #(#t) 2.4) #(5 #(#t) 3.4) #(6 #(#t) 2.4))
                                              (#(3 #(#t) 1.1) #(6 #(#t) 3.3) #(9 #(#f) 2.4) #(1 #(#f) 13.4) #(16 #(#t) 21.4))
                                              (#(4 #(#t) 1.1) #(7 #(#t) 2.3) #(10 #(#f) 2.4) #(2 #(#f) 23.4) #(26 #(#f) 22.4))
                                              (#(5 #(#t) 1.1) #(8 #(#t) 3.3) #(11 #(#t) 2.4) #(3 #(#t) 33.4) #(16 #(#t) 23.4)))

                                             ((#(72 #(#t) 1.1) #(1 #(#t) 2.3) #(4 #(#t) 21.4) #(1 #(#t) 3.4) #(6 #(#t) 2.4))
                                              (#(63 #(#t) 1.1) #(6 #(#t) 3.3) #(8 #(#t) 2.4) #(2 #(#f) 3.4) #(16 #(#t) 21.4))
                                              (#(54 #(#t) 1.1) #(7 #(#t) 2.3) #(1 #(#t) 12.4) #(3 #(#f) 1.4) #(3 #(#f) 22.4))
                                              (#(35 #(#t) 1.1) #(2 #(#t) 3.3) #(7 #(#t) 7.4) #(4 #(#f) 3.4) #(6 #(#t) 23.4))))))])
                     (test-not-exn "test-case 8" (lambda () (read-data* cptr)))
             (maybe-display "Test 11 Success !!!")))

  (test-case "test-case 12"
             "test-case 12"
             (letrec ([cptr (array (1) _int (99))]
                      [temp (acc-map add1 cptr)])
                     (check-equal? (read-data* temp) (map add1 (read-data* cptr))))
             (maybe-display "Test 12 Success !!!"))

  (test-case "test-case 13"
             "test-case 13"
             (letrec ([cptr (array (6) _int (98 99 100 101 102 103))]
                      [temp (acc-map sub1 cptr)])
                     (check-equal? (read-data* temp) (map sub1 (read-data* cptr))))
             (maybe-display "Test 13 Success !!!"))

  (test-case "test-case 14"
             "test-case 14"
             (letrec ([cptr (array (2 3) _int ((198 199 200) (301 331 109)))]
                      [temp (acc-map (lambda (x) (+ x 10)) cptr)])
                     (check-equal? (read-data (acc-manifest-array-data temp)) (map (lambda (x) (+ x 10)) (read-data (acc-manifest-array-data cptr)))))
             (maybe-display "Test 14 Success !!!"))

  (test-case "test-case 15"
             "test-case 15"
             (letrec ([cptr (array (2 3 4) _int (((1 2 3 4) (5 6 7 8) (9 10 11 12)) ((13 14 15 16) (17 18 19 20) (21 22 23 24))))]
                      [temp (acc-map (lambda (x) (+ x 100)) cptr)])
                     (check-equal? (read-data (acc-manifest-array-data temp)) (map (lambda (x) (+ x 100)) (read-data (acc-manifest-array-data cptr)))))
             (maybe-display "Test 15 Success !!!"))

  (test-case "test-case 16"
             "test-case 16"
             (letrec ([cptr (array (3) #(_int #(_int _int _double)) (#(2 #(2 2 2.2)) #(1 #(3 4 4.4)) #(4 #(6 5 5.5))))]
                      [temp (acc-map add1 cptr)])
                     (begin
                       (cmap 'add1 cptr)
                       (check-equal? (read-data* temp) (read-data* cptr))))
             (maybe-display "Test 16 Success !!!"))

  (test-case "test-case 17"
             "test-case 17"
             (letrec ([cptr (array (2 2 3) #(_int _double #(_double))
                                 (((#(1 1.1 #(0.1)) #(2 2.2 #(1.0)) #(3 3.3 #(1.1)))
                                   (#(2 3.3 #(0.2)) #(3 5.5 #(2.0)) #(6 7.7 #(2.2))))
                                  ((#(3 2.2 #(0.3)) #(4 3.3 #(3.0)) #(7 4.4 #(3.3)))
                                   (#(4 4.4 #(0.4)) #(5 6.6 #(4.0)) #(8 1.1 #(4.4))))))]
                      [temp (acc-map sub1 cptr)])
                     (begin
                       (cmap 'sub1 cptr)
                       (check-equal? (read-data* temp) (read-data* cptr))))
             (maybe-display "Test 17 Success !!!"))))

(define (add1* ls)
  (cond
    ((null? ls) '())
    ((pair? (car ls)) (cons (add1* (car ls)) (add1* (cdr ls))))
    (else (cons (add1 (car ls)) (add1* (cdr ls))))))

(define (sub1* ls)
  (cond
    ((null? ls) '())
    ((pair? (car ls)) (cons (sub1* (car ls)) (sub1* (cdr ls))))
    (else (cons (sub1 (car ls)) (sub1* (cdr ls))))))

(maybe-display "\n<----------- Invalid test-cases Run ----------->\n")
(if (run-tests acc-invalid-test_cases)
    (display "\n!!! Test Run Successfull !!!\n")
    (display "\n!!! Test Run Failed !!!\n"))

(display "\n<----------- Valid test-cases Run ----------->\n")
(if (run-tests acc-valid-test_cases)
    (display "\n!!! Test Run Successfull !!!\n\n")
    (display "\n!!! Test Run Failed !!!\n\n"))