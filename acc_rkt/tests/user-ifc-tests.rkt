#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         rackunit
         rackunit/text-ui
         racket/runtime-path
         (only-in '#%foreign ctype-scheme->c ctype-c->scheme))

(require accelerack)

(define-runtime-path librts "../../acc_c/librts.so")
(define lib-rts (ffi-lib librts))
(define-ffi-definer define-rts lib-rts)


(define-rts ark_init (_fun -> _int))
(define-rts ark_exit (_fun -> _int))

(ark_init)

(define user-ifc-invalid-test_cases (test-suite
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

(define user-ifc-valid-test_cases (test-suite
  "valid test cases"
  
  (test-case "test-case 1"
             "test-case 1"
             (acc (define x (array () _int 15)))
             (check-equal? '(15) (readData* x))
             (display "Test 1 Success !!!") (newline))

  (test-case "test-case 2"
             "test-case 2"
             (acc (define x (array (2) _double (15.15 25.25))))
             (check-equal? '(15.15 25.25) (readData* x))
             (display "Test 2 Success !!!") (newline))

  (test-case "test-case 3"
             "test-case 3"
             (acc (define x (array (2 3) _bool ((#f #f #f) (#t #t #t)))))
             (check-equal? '((#f #f #f) (#t #t #t)) (readData* x))
             (display "Test 3 Success !!!") (newline))
  
  (test-case "test-case 4"
             "test-case 4"
             (acc (define x (array (3) (_tuple _int (_tuple _int _double _bool)) (#(2 #(2 1.1 #f)) #(1 #(3 2.2 #f)) #(4 #(16 3.3 #f))))))
             (check-equal? '((2 (2 1.1 #f)) (1 (3 2.2 #f)) (4 (16 3.3 #f))) (readData* x))
             (display "Test 4 Success !!!") (newline))

  (test-case "test-case 5"
             "test-case 5"
             (acc (define x (array (3 2) (_tuple _int _double) ((#(1 1.0) #(2 2.0)) (#(3 3.0) #(4 4.0)) (#(5 5.0) #(6 6.0))))))
             (check-equal? '(((1 1.0) (2 2.0)) ((3 3.0) (4 4.0)) ((5 5.0) (6 6.0))) (readData* x))
             (display "Test 5 Success !!!") (newline))
  
  (test-case "test-case 6"
             "test-case 6"
             (acc (define x (array (10) _int (0 1 2 3 4 5 6 7 8 9))))
             (check-equal? (readData* (acc-map add1 x)) (readData* (acc (map add1 x))))
             (display "Test 6 Success !!!") (newline))

  (test-case "test-case 7"
             "test-case 7"
             (acc (define x (array (2 3) _double ((1.1 2.2 3.3) (4.4 5.5 6.6)))))
             (check-equal? (readData* (acc-map add1 x)) (readData* (acc (map add1 x))))
             (display "Test 7 Success !!!") (newline))
  
  (test-case "test-case 8"
             "test-case 8"
             (acc (define x (array (4 3 3) _int (((10 20 30) (30 40 50) (50 60 70))
                                                 ((70 80 90) (90 100 110) (110 120 130))
                                                 ((130 140 150) (150 160 170) (170 180 190))
                                                 ((190 200 210) (210 220 230) (230 240 250))))))
             (check-equal? (readData* (acc-map sub1 x)) (readData* (acc (map sub1 x))))
             (display "Test 8 Success !!!") (newline))
  
  (test-case "test-case 9"
             "test-case 9"
             (acc (define x (array (2 3 3) _int (((2 22 222) (3 33 333) (4 44 444)) ((7 77 777) (8 88 888) (9 99 999))))))
             (check-equal? (readData* (acc-map (lambda (x) (+ x 5)) x)) (readData* (acc (map (++ 5) x))))
             (display "Test 9 Success !!!") (newline))

  (test-case "test-case 10"
             "test-case 10"
             (acc (define x (array (8) _double (1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8))))
             (check-equal? (readData* (acc-map (lambda (x) (* x 10)) x)) (readData* (acc (map (** 10) x))))
             (display "Test 10 Success !!!") (newline))

  (test-case "test-case 11"
             "test-case 11"
             (acc (define x (array (2 5) _double ((10.10 20.20 30.30 40.40 50.50) (100.100 200.200 300.300 400.400 500.500)))))
             (check-equal? (readData* (acc-map (lambda (x) (- x 5)) x)) (readData* (acc (map (-- 5) x))))
             (display "Test 11 Success !!!") (newline))

  (test-case "test-case 12"
             "test-case 12"
             (acc (define x (array (2 3) _double ((11.1 22.2 33.3) (1.11 2.22 3.33)))))
             (check-equal? (readData* (acc-map (lambda (x) (* x 2)) x)) (readData* (acc (map (** 2) x))))
             (display "Test 12 Success !!!") (newline))

  (test-case "test-case 13"
             "test-case 13"
             (acc (define x (array (3) (_tuple _int (_tuple _int _int)) (#(1 #(2 3)) #(4 #(5 6)) #(7 #(8 9))))))
             (check-equal? (readData* (acc-map add1 x)) (readData* (acc (map add1 x))))
             (display "Test 13 Success !!!") (newline))

  (test-case "test-case 14"
             "test-case 14"
             (acc (define x (array (2 3)
                                   (_tuple _double (_tuple _double (_tuple _int _int)))
                                   ((#(2.2 #(2.2 #(3 5))) #(1.1 #(3.1 #(5 6))) #(4.4 #(16.4 #(7 7))))
                                    (#(4.4 #(5.4 #(3 7))) #(1.1 #(61.1 #(15 6))) #(14.14 #(26.14 #(7 5))))))))
             (check-equal? (readData* (acc-map sub1 x)) (readData* (acc (map sub1 x))))
             (display "Test 14 Success !!!") (newline))

  (test-case "test-case 15"
             "test-case 15"
             (acc (define x (array (2 3)
                                   (_tuple _double _double)
                                   ((#(2.2 22.22) #(1.1 11.11) #(3.3 33.33)) (#(4.4 44.44) #(5.5 55.55) #(6.6 66.66))))))
             (check-equal? (readData* (acc-map (lambda (x) (* x 10)) x)) (readData* (acc (map (** 10) x))))
             (display "Test 15 Success !!!") (newline))

  (test-case "test-case 16"
             "test-case 16"
             (acc (define x (array () _int 25)))
             (acc (define y (array () _int 75)))
             (check-equal? (readData* (acc-zipwith add x y)) (readData* (acc (zipwith (++) x y))))
             (display "Test 16 Success !!!") (newline))

  (test-case "test-case 17"
             "test-case 17"
             (acc (define x (array (5) _double (12.34 56.78 34.12 45.32 65.45))))
             (acc (define y (array (5) _double (71.50 45.12 45.68 76.34 57.90))))
             (check-equal? (readData* (acc-zipwith sub x y)) (readData* (acc (zipwith (--) x y))))
             (display "Test 17 Success !!!") (newline))

  (test-case "test-case 18"
             "test-case 18"
             (acc (define x (array (2 3) _int ((10 20 30) (40 50 60)))))
             (acc (define y (array (3 4) _int ((10 20 30 40) (50 60 70 80) (90 100 110 120)))))
             (check-equal? (readData* (acc-zipwith mult x y)) (readData* (acc (zipwith (**) x y))))
             (display "Test 18 Success !!!") (newline))

  ;; TODO : Currently acc-zipwith not working for higher dimensions
  #|(test-case "test-case 19"
             "test-case 19"
             (acc (define x (array (5 4 3 2) _int ((((10 20) (20 30) (30 40)) ((11 22) (22 33) (33 44)) ((13 23) (23 33) (33 43)) ((15 25) (25 35) (35 45)))
                                                   (((10 20) (20 30) (30 40)) ((11 22) (22 33) (33 44)) ((13 23) (23 33) (33 43)) ((15 25) (25 35) (35 45)))
                                                   (((10 20) (20 30) (30 40)) ((11 22) (22 33) (33 44)) ((13 23) (23 33) (33 43)) ((15 25) (25 35) (35 45)))
                                                   (((10 20) (20 30) (30 40)) ((11 22) (22 33) (33 44)) ((13 23) (23 33) (33 43)) ((15 25) (25 35) (35 45)))
                                                   (((10 20) (20 30) (30 40)) ((11 22) (22 33) (33 44)) ((13 23) (23 33) (33 43)) ((15 25) (25 35) (35 45)))))))
             ;; (((1 2 3) (3 4 3)) ((5 6 3) (7 8 3))))))
             (acc (define y (array (2 3 4 5) _int ((((10 20 30 40 50) (50 60 70 80 90) (90 100 110 120 130) (140 150 160 170 180))
                                               ((10 20 30 40 50) (50 60 70 80 90) (90 100 110 120 130) (140 150 160 170 180))
                                               ((10 20 30 40 50) (50 60 70 80 90) (90 100 110 120 130) (140 150 160 170 180)))

                                                   (((1 2 3 4 5) (5 6 7 8 9) (9 10 11 12 13) (14 15 16 17 18))
                                                    ((1 2 3 4 5) (5 6 7 8 9) (9 10 11 12 13) (14 15 16 17 18))
                                                    ((1 2 3 4 5) (5 6 7 8 9) (9 10 11 12 13) (14 15 16 17 18)))))))
                                   
                                   ;;(((5 3) (9 4) (3 5)) ((4 5) (5 8) (5 6))))))
             ;;(display (readData* (acc-zipwith add x y)))
             (check-equal? (readData* (acc-zipwith add x y)) (readData* (acc (zipwith (++) x y))))
             (display "Test 19 Success !!!") (newline))|#

  (test-case "test-case 20"
             "test-case 20"
             (acc (define x (array (2 3) _double ((1.0 2.0 3.0) (4.0 5.0 6.0)))))
             (acc (define y (array (3 2) _double ((1.0 2.0) (5.0 6.0) (7.0 8.0)))))
             (check-equal? (readData* (acc-zipwith mult x y)) (readData* (acc (zipwith (**) x y))))
             (display "Test 20 Success !!!") (newline))

  (test-case "test-case 21"
             "test-case 21"
             (acc (define x (array (2 3) _int ((1 2 3) (10 20 30)))))
             (check-equal? (readData* (acc-fold * 5 x)) (readData* (acc (fold (**) 5 x))))
             (display "Test 21 Success !!!") (newline))

  (test-case "test-case 22"
             "test-case 22"
             (acc (define x (array (2 3 2) _int (((1 2) (3 4) (5 6)) ((10 20) (30 40) (50 60))))))
             (check-equal? (readData* (acc-fold * 10 x)) (readData* (acc (fold (**) 10 x))))
             (display "Test 22 Success !!!") (newline))

  (test-case "test-case 23"
             "test-case 23"
             (acc (define x (array (2 3 4 5) _int ((((10 20 30 40 50) (50 60 70 80 90) (90 100 110 120 130) (140 150 160 170 180))
                                                    ((10 20 30 40 50) (50 60 70 80 90) (90 100 110 120 130) (140 150 160 170 180))
                                                    ((10 20 30 40 50) (50 60 70 80 90) (90 100 110 120 130) (140 150 160 170 180)))

                                                   (((1 2 3 4 5) (5 6 7 8 9) (9 10 11 12 13) (14 15 16 17 18))
                                                    ((1 2 3 4 5) (5 6 7 8 9) (9 10 11 12 13) (14 15 16 17 18))
                                                    ((1 2 3 4 5) (5 6 7 8 9) (9 10 11 12 13) (14 15 16 17 18)))))))
             
             (check-equal? (readData* (acc-fold + 0 x)) (readData* (acc (fold (++) 0 x))))
             (display "Test 23 Success !!!") (newline))
  
  (test-case "test-case 24"
             "test-case 24"
             (acc (define x (array (3 3 3) _double (((1.1 2.2 3.3) (1.1 2.2 3.3) (1.1 2.2 3.3))
                                                    ((4.4 5.5 6.6) (1.1 2.2 3.3) (7.7 8.8 9.9))
                                                    ((1.1 2.2 3.3) (10.10 11.11 12.12) (1.1 2.2 3.3))))))
             
             (check-equal? (readData* (acc-fold + 100 x)) (readData* (acc (fold (++) 100 x))))
             (display "Test 24 Success !!!") (newline))

  (test-case "test-case 25"
             "test-case 25"
             (acc (define x (array (2 3 4 3 2) _double (((((1.1 2.2) (3.3 4.4) (5.5 6.6))
                                                          ((7.7 8.8) (9.9 10.10) (11.11 12.12))
                                                          ((4.4 5.5) (6.6 7.7) (2.2 3.3))
                                                          ((7.7 8.8) (9.9 10.10) (11.11 12.12)))

                                                         (((1.1 2.2) (3.3 4.4) (5.5 6.6))
                                                          ((7.7 8.8) (9.9 10.10) (11.11 12.12))
                                                          ((4.4 5.5) (6.6 7.7) (2.2 3.3))
                                                          ((7.7 8.8) (9.9 10.10) (11.11 12.12)))

                                                         (((1.1 2.2) (3.3 4.4) (5.5 6.6))
                                                          ((7.7 8.8) (9.9 10.10) (11.11 12.12))
                                                          ((4.4 5.5) (6.6 7.7) (2.2 3.3))
                                                          ((7.7 8.8) (9.9 10.10) (11.11 12.12))))

                                                        ((((1.1 2.2) (3.3 4.4) (5.5 6.6))
                                                          ((7.7 8.8) (9.9 10.10) (11.11 12.12))
                                                          ((4.4 5.5) (6.6 7.7) (2.2 3.3))
                                                          ((7.7 8.8) (9.9 10.10) (11.11 12.12)))

                                                         (((1.1 2.2) (3.3 4.4) (5.5 6.6))
                                                         ((7.7 8.8) (9.9 10.10) (11.11 12.12))
                                                         ((4.4 5.5) (6.6 7.7) (2.2 3.3))
                                                         ((7.7 8.8) (9.9 10.10) (11.11 12.12)))

                                                         (((1.1 2.2) (3.3 4.4) (5.5 6.6))
                                                          ((7.7 8.8) (9.9 10.10) (11.11 12.12))
                                                          ((4.4 5.5) (6.6 7.7) (2.2 3.3))
                                                          ((7.7 8.8) (9.9 10.10) (11.11 12.12))))))))
             
             (check-equal? (readData* (acc-fold + 100 x)) (readData* (acc (fold (++) 100 x))))
             (display "Test 25 Success !!!") (newline))))

(display "\n<----------- Invalid test-cases Run ----------->\n")
(if (run-tests user-ifc-invalid-test_cases) (display "\n!!! Test Run Successfull !!!\n") (display "\n!!! Test Run Failed !!!\n"))
(display "\n<----------- Valid test-cases Run ----------->\n")
(if (run-tests user-ifc-valid-test_cases) (display "\n!!! Test Run Successfull !!!\n\n") (display "\n!!! Test Run Failed !!!\n\n"))
