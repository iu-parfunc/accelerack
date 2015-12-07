#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         rackunit
         rackunit/text-ui
         racket/runtime-path
         (only-in '#%foreign ctype-scheme->c ctype-c->scheme))

(require accelerack)

(printf "<-------------- Setting up the Haskell environment --------------->")
(define-runtime-path librachs "../../acc_hs/librachs.so")
(define lib-hs (ffi-lib librachs))
(define-ffi-definer define-hs lib-hs)

(define-runtime-path libsxp "../../acc_hs/libsxp.so")
(define lib-sxp (ffi-lib libsxp))
(define-ffi-definer define-sxp lib-sxp)

(define-runtime-path librts "../../acc_c/librts.so")
(define lib-rts (ffi-lib librts))
(define-ffi-definer define-rts lib-rts)

(define-sxp runAcc (_fun _string _acc-array-pointer _acc-array-pointer -> _void))
(define run-acc runAcc)

(define-rts ark_init (_fun -> _int))
(define-rts ark_exit (_fun -> _int))

(ark_init)
(printf "!!! init function success !!!\n")

(define-hs entrypoint (_fun _int -> _int))

(printf "calling entrypoint: ")
(entrypoint 0)
(printf "!!! entrypoint function called successfully!!!\n")
(newline)

(printf "########## S-EXP TEST CASES CALLING HASKELL ###########\n")


(define a (acc-array 10)) ;; DIM0

(define b (acc-array 20.20)) ;; DIM0

(define c (acc-array (10 10))) ;; DIM1

(define d (acc-array (10.10 20.20 30.30))) ;; DIM1

(define e (acc-array (1 2 3 4 5 6 7 8 9 10))) ;; DIM1

(define f (acc-array (1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 10.10))) ;; DIM1

(define g (acc-array ((1 2) (3 4) (5 6) (7 8) (9 10)))) ;; DIM2

(define h (acc-array ((1.3 2.3) (3.3 4.3) (5.3 6.3) (7.3 8.3) (9.3 10.3)))) ;; DIM2

(define i (acc-array (((1 2) (3 4)) ((5 6) (7 8)) ((9 10) (11 12))))) ;; DIM3

(define j (acc-array (((1.3 2.3) (3.3 4.3)) ((5.3 6.3) (7.3 8.3)) ((9.3 10.3) (11.3 12.3))))) ;; DIM3

(define k (acc-array (((22 222) (33 333)) ((55 555) (77 777)) ((99 999) (111 1111))))) ;; DIM3

(define l (acc-array (((12.3 22.3) (32.3 43.3)) ((54.3 65.3) (76.3 87.3)) ((98.3 109.3) (119.3 139.3))))) ;; DIM3

(define m (acc-array ((22 22 222 2222) (55 55 555 5555) (99 99 999 9999) (11 11 111 1111)))) ;; DIM2

(define n (acc-array ((2.22 22.2 .222 2.222) (.55 5.5 55.5 55.55) (.99 9.9 99.9 999.9) (.11 1.1 11.1 11.11)))) ;; DIM2

(define o (acc-array (((1 2 3 4 5) (6 7 8 9 10) (11 12 13 14 15) (16 17 18 19 20) (21 22 23 24 25))
                      ((11 22 33 44 55) (66 77 88 99 100) (110 120 130 140 150) (160 170 180 190 200) (210 220 230 240 250))))) ;; DIM3

(define p (acc-array (((1.1 2.2 3.3 4.4 5.5) (6.6 7.7 8.8 9.9 10.10) (11.11 12.12 13.13 14.14 15.15)
                      (16.16 17.17 18.18 19.19 20.20) (21.21 22.22 23.23 24.24 25.25))
                      ((10.1 20.2 30.3 40.4 50.5) (60.6 70.7 80.8 90.9 100.10) (110.11 120.12 130.13 140.14 150.15)
                      (160.16 170.17 180.18 190.19 200.20) (210.21 220.22 230.23 240.24 250.25))))) ;; DIM3

(define q (acc-array ((((51 52 53 54 55) (56 57 58 59 60) (61 62 63 64 65) (66 67 68 69 70) (71 72 73 74 75))
                      ((61 62 63 64 65) (66 67 68 69 66) (61 62 63 64 65) (66 67 68 69 70) (71 72 73 74 75)))
                     (((91 52 93 54 95) (56 57 58 59 60) (71 72 73 74 75) (76 77 78 79 77) (71 72 73 74 75))
                      ((71 52 73 54 75) (56 57 58 59 60) (61 62 63 64 65) (66 67 68 69 70) (81 82 83 84 85)))))) ;; DIM4

(define r (acc-array ((((5.1 5.2 5.3 5.4 5.5) (5.6 5.7 5.8 5.9 6.0) (6.1 6.2 6.3 6.4 6.5) (6.6 6.7 6.8 6.9 7.0) (7.1 7.2 7.3 7.4 7.5))
                     ((6.1 6.2 6.3 6.4 6.5) (5.6 5.7 5.8 5.9 6.0) (6.1 6.2 6.3 6.4 6.5) (6.6 6.7 6.8 6.9 7.0) (8.1 8.2 8.3 8.4 8.5)))
                     (((5.1 5.2 5.3 5.4 5.5) (7.6 7.7 7.8 7.9 7.0) (9.1 9.2 9.3 9.4 9.5) (4.6 4.7 4.8 4.9 4.0) (7.1 7.2 7.3 7.4 7.5))
                     ((7.1 7.2 7.3 7.4 7.5) (5.6 5.7 5.8 5.9 6.0) (6.1 6.2 6.3 6.4 6.5) (6.6 6.7 6.8 6.9 7.0) (7.1 7.2 7.3 7.4 7.5)))))) ;; DIM4

(define s (acc-array (((51 52) (59 60)) ((63 64) (66 67)) ((68 69) (70 71)) ((72 73) (74 75))))) ;; DIM3

(define t (acc-array (((5.1 5.2) (5.9 6.0)) ((6.3 6.4) (6.6 6.7)) ((6.8 6.9) (7.0 7.1)) ((7.2 7.3) (7.4 7.5))))) ;; DIM3

(define u (acc-array (((((0 1 2) (3 4 5)) ((6 7 8) (9 10 11)) ((0 1 2) (3 4 5))) (((6 7 8) (9 10 11)) ((0 1 2) (3 4 5)) ((6 7 8) (9 10 11))))))) ;; DIM4

(define v (acc-array (((((0.0 1.1 2.1) (3.1 4.1 5.1)) ((6.2 7.2 8.2) (9.3 10.3 11.3)) ((0.4 1.4 2.4) (3.5 4.5 5.5)))
                       (((6.0 7.0 8.0) (9.1 10.1 11.1)) ((0.3 1.3 2.4) (3.5 4.5 5.6)) ((6.7 7.7 8.3) (9.2 10.5 11.2))))))) ;; DIM4

(define w (acc-array (((((0 1 2) (3 4 5)) ((6 7 8) (9 10 11)) ((0 1 2) (3 4 5)))
                        (((6 7 8) (9 10 11)) ((0 1 2) (3 4 5)) ((6 7 8) (9 10 11))))
                      ((((10 21 32) (43 54 65)) ((76 87 98) (19 20 31)) ((40 51 62) (73 84 95)))
                        (((16 27 28) (49 10 11)) ((02 11 22) (33 42 53)) ((46 57 86) (39 10 51))))))) ;; DIM5

(define x (acc-array (((((0.0 1.1 2.1) (3.1 4.1 5.1)) ((6.2 7.2 8.2) (9.3 10.3 11.3)) ((0.4 1.4 2.4) (3.5 4.5 5.5)))
                       (((6.0 7.0 8.0) (9.1 10.1 11.1)) ((0.3 1.3 2.4) (3.5 4.5 5.6)) ((6.7 7.7 8.3) (9.2 10.5 11.2))))
                      ((((0.10 1.21 2.31) (3.41 4.15 5.15)) ((6.24 7.32 8.22) (9.63 10.43 11.53)) ((0.54 1.64 2.24) (3.35 4.54 5.55)))
                       (((6.20 7.30 8.40) (9.61 10.41 11.51)) ((0.23 1.43 2.74) (3.65 4.25 5.36)) ((6.37 7.74 8.53) (9.24 10.35 11.32))))))) ;; DIM5

(define y (acc-array ((((((20 21 32) (33 54 65)) ((26 87 98) (19 50 71)) ((40 51 62) (73 84 95)))
                        (((36 27 28) (49 10 11)) ((22 11 22) (33 52 73)) ((46 57 86) (39 10 51))))
                      ((((40 21 32) (53 54 65)) ((26 87 48) (19 50 71)) ((40 51 62) (73 84 95)))
                        (((56 27 48) (69 10 11)) ((22 11 42) (33 52 73)) ((46 57 86) (39 10 51)))))
                      (((((60 21 52) (73 54 65)) ((26 87 48) (19 50 71)) ((40 51 62) (73 84 95)))
                        (((76 27 68) (89 10 11)) ((22 11 42) (33 52 73)) ((46 57 86) (39 10 51))))
                      ((((80 21 72) (93 54 65)) ((26 87 98) (19 50 71)) ((40 51 62) (73 84 95)))
                        (((96 27 28) (49 10 11)) ((22 11 22) (33 52 73)) ((46 57 86) (39 10 51)))))))) ;; DIM6

(define z (acc-array ((((((1.0 1.1 1.2) (1.3 1.4 1.5)) ((1.6 1.7 1.8) (1.9 1.10 1.11)) ((1.0 1.1 1.2) (1.3 1.4 1.5)))
                        (((2.6 2.7 2.8) (2.9 2.10 2.11)) ((2.0 2.1 2.2) (2.3 2.4 2.5)) ((2.6 2.7 2.8) (2.9 2.10 2.11))))
                      ((((3.20 3.21 3.32) (3.33 3.54 3.65)) ((3.26 3.87 3.98) (3.19 3.50 3.71)) ((3.40 3.51 3.62) (3.73 3.84 3.95)))
                        (((3.6 .27 2.8) (.49 1.0 .11)) ((2.2 .11 2.2) (.33 5.2 .73)) ((4.6 5.7 8.6) (.39 .10 .51)))))
                      (((((6.0 .21 5.2) (.73 5.4 .65)) ((2.6 .87 4.8) (.19 5.0 .71)) ((4.0 5.1 6.2) (.73 .84 .95)))
                        (((7.6 .27 6.8) (.89 1.0 .11)) ((2.2 .11 4.2) (.33 5.2 .73)) ((4.6 5.7 8.6) (.39 .10 .51))))
                      ((((8.0 .21 7.2) (.93 5.4 .65)) ((2.6 .87 9.8) (.19 5.0 .71)) ((4.0 5.1 6.2) (.73 .84 .95)))
                        (((9.6 .27 2.8) (.49 1.0 .11)) ((2.2 .11 2.2) (.33 5.2 .73)) ((4.6 5.7 8.6) (.39 .10 5.1)))))))) ;; DIM6


(define sexp-test-cases (test-suite
  "GPU Test Cases"

  (test-case "case 1"
             (display "test-case 1 running") (newline)
             (test-not-exn "test-case 1" (lambda () (map add1 a))))

  (test-case "case 2"
             (display "test-case 2 running") (newline)
             (test-not-exn "test-case 2" (lambda () (map add1 b))))

  (test-case "case 3"
             (display "test-case 3 running") (newline)
             (test-not-exn "test-case 3" (lambda () (map sub1 c))))

  (test-case "case 4"
             (display "test-case 4 running") (newline)
             (test-not-exn "test-case 4" (lambda () (map sub1 d))))

  (test-case "case 5"
             (display "test-case 5 running") (newline)
             (test-not-exn "test-case 5" (lambda () (map sub1 (map add1 e)))))

  (test-case "case 6"
             (display "test-case 6 running") (newline)
             (test-not-exn "test-case 6" (lambda () (map add1 (map add1 f)))))

  (test-case "case 7"
             (display "test-case 7 running") (newline)
             (test-not-exn "test-case 7" (lambda () (map add1 (map add1 (map add1 g))))))

  (test-case "case 8"
             (display "test-case 8 running") (newline)
             (test-not-exn "test-case 8" (lambda () (zipwith (++) i k))))

  (test-case "case 9"
             (display "test-case 9 running") (newline)
             (test-not-exn "test-case 9" (lambda () (zipwith (++) (map add1 i) k))))

  (test-case "case 10"
             (display "test-case 10 running") (newline)
             (test-not-exn "test-case 10" (lambda () (map add1 (zipwith (++) j l)))))

  (test-case "case 11"
             (display "test-case 11 running") (newline)
             (test-not-exn "test-case 11" (lambda () (map sub1 (zipwith (++) j (map add1 l))))))

  (test-case "case 12"
             (display "test-case 12 running") (newline)
             (test-not-exn "test-case 12" (lambda () (fold (++) 0 (zipwith (++) j (map add1 l))))))

  (test-case "case 13"
             (display "test-case 13 running") (newline)
             (test-not-exn "test-case 13" (lambda () (fold (++) 10 (map (++ 5) (zipwith (++) j (map add1 l)))))))

  (test-case "case 14"
             (display "test-case 14 running") (newline)
             (test-not-exn "test-case 14" (lambda () (fold (--) 1 (map (++ 5) (zipwith (++) j (map add1 l)))))))

  (test-case "case 15"
             (display "test-case 15 running") (newline)
             (test-not-exn "test-case 15" (lambda () (map add1 (zipwith (--) j (map add1 l))))))

  (test-case "case 16"
             (display "test-case 16 running") (newline)
             (test-not-exn "test-case 16" (lambda () (fold (**) 1 (map (++ 5) (zipwith (++) j (map add1 j)))))))

  (test-case "case 17"
             (display "test-case 17 running") (newline)
             (test-not-exn "test-case 17" (lambda () (map (** 10) (fold (--) 2 (zipwith (++) (map sub1 i) (map add1 k)))))))

  (test-case "case 18"
             (display "test-case 18 running") (newline)
             (test-not-exn "test-case 18" (lambda () (fold (--) 100 (map (++ 100) (zipwith (++) j j))))))
  
  (test-case "case 19"
             (display "test-case 19 running") (newline)
             (test-not-exn "test-case 19" (lambda () (map (** 20)
                                                          (map (++ 30)
                                                               (fold (--) 1
                                                                     (fold (**) 2
                                                                           (map (++ 5) (zipwith (++) j (map add1 l))))))))))

  (test-case "case 20"
             (display "test-case 20 running") (newline)
             (test-not-exn "test-case 20" (lambda () (fold (--) 5 (map (++ 5) m)))))

  (test-case "case 21"
             (display "test-case 21 running") (newline)
             (test-not-exn "test-case 21" (lambda () (map add1 (map (++ 25) (fold (++) 100 (fold (--) 50 p)))))))

  (test-case "case 22"
             (display "test-case 22 running") (newline)
             (test-not-exn "test-case 22" (lambda () (fold (--) 5 (zipwith (++) k (map (++ 50) k))))))

  (test-case "case 23"
             (display "test-case 23 running") (newline)
             (test-not-exn "test-case 23" (lambda () (map (-- 10) (zipwith (--) (map (++ 5) x) (map (++ 10) x))))))

  (test-case "case 24"
             (display "test-case 24 running") (newline)
             (test-not-exn "test-case 24" (lambda () (map (-- 10) (map (++ 5) (zipwith (++) w (fold (++) 20 y)))))))

  (test-case "case 25"
             (display "test-case 25 running") (newline)
             (test-not-exn "test-case 25" (lambda () (map (-- 10) (map (++ 5) (zipwith (++)
                                                                                       (map (++ 50) x)
                                                                                       (map (++ 10) (fold (--) 10 z))))))))))

(display "\n<----------- S-Exp test-cases Run Start ----------->\n")
(if (run-tests sexp-test-cases) (display "\n!!! Test Run Successfull !!!\n") (display "\n!!! Test Run Failed !!!\n"))
(display "\n<----------- S-Exp test-cases Run End   ----------->\n")


;;(acc:make-acc-array 6 shp arr)
;; (map add1 x)
;;(zipwith (++) x y)
;; (fold (++) 100 y)
;; (map add1 d)
;;(map add1 (zipwith (++) x y))
;;(map sub1 (zipwith (++) x y))
;;(zipwith (--) y (map sub1 x))

(printf "########## Exiting racket safely ############\n")
