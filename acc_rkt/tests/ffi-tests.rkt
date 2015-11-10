#lang racket/base

;; Test C FFI and load code from ../acc_c

(require ffi/unsafe
         ffi/unsafe/define
         rackunit
         rackunit/text-ui)

(require accelerack)

(printf "<-------------- Setting up the Haskell environment --------------->")
(define lib-hs (ffi-lib "../acc_hs/librachs"))
(define-ffi-definer define-hs lib-hs)

;;(define lib-ifc (ffi-lib "../../acc_hs/libhsifc"))
;;(define-ffi-definer define-ifc lib-ifc)

(define lib-rts (ffi-lib "../acc_c/librts"))
(define-ffi-definer define-rts lib-rts)


(define-rts ark_init (_fun -> _int))
(define-rts ark_exit (_fun -> _int))

(ark_init)
(printf "!!! init function success !!!\n")

(define-hs entrypoint (_fun _int -> _int))

(printf "calling entrypoint: ")
(entrypoint 0)
(printf "!!! entrypoint function called successfully!!!\n")
(newline)

(printf "########## Ready to call haskell functions ###########\n")

(define-hs accelerateMap (_fun _acc-array-pointer _int -> _void))

(define accelerate-test-cases (test-suite
  "GPU Test Cases"

  (test-case "test-case 1"
             "test-case 1"
             (letrec ([ls (array () _int 24)]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map add1 cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr 1)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* cptr))))
             (display "Test 1 Success !!!\n") (newline))

  (test-case "test-case 2"
             "test-case 2"
             (letrec ([ls (array (8) _int (1 2 3 4 5 6 7 8))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map (lambda (x) (+ x 5)) cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr 5)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* cptr))))
             (display "Test 2 Success !!!\n") (newline))

  (test-case "test-case 3"
             "test-case 3"
             (letrec ([ls (array (2 3) _int ((2 3 11) (50 2 41)))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map add1 cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr 1)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* cptr))))
             (display "Test 3 Success !!!\n") (newline))

  (test-case "test-case 4"
             "test-case 4"
             (letrec ([ls (array (2 3 2) _int (((10 20) (30 40) (50 60)) ((50 60) (70 80) (90 100))))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map add1 cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr 1)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* cptr))))
             (display "Test 4 Success !!!\n") (newline))

  (test-case "test-case 5"
             "test-case 5"
             (letrec ([ls (array (4 3 3) _int (((10 20 30) (30 40 50) (50 60 70))
                                               ((50 60 70) (70 80 90) (90 100 110))
                                               ((100 110 120) (120 130 140) (140 150 200))
                                               ((210 220 230) (240 250 260) (260 270 300))))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map (lambda (x) (+ x 100)) cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr 100)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* cptr))))
             (display "Test 5 Success !!!\n") (newline))

  (test-case "test-case 6"
             "test-case 6"
             (letrec ([ls (array () _int 100)]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map sub1 cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr -1)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* cptr))))
             (display "Test 6 Success !!!\n") (newline))

  (test-case "test-case 7"
             "test-case 7"
             (letrec ([ls (array (8) _int (10 20 30 40 50 60 70 80))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map (lambda (x) (- x 10)) cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr -10)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* cptr))))
             (display "Test 7 Success !!!\n") (newline))

  (test-case "test-case 8"
             "test-case 8"
             (letrec ([ls (array (2 3) _int ((100 1000 10000) (200 3000 40000)))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map sub1 cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr -1)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* cptr))))
             (display "Test 8 Success !!!\n") (newline))

  (test-case "test-case 9"
             "test-case 9"
             (letrec ([ls (array (2 3 2) _int (((9 99) (999 9999) (8 88)) ((888 8888) (7 77) (777 7777))))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map (lambda (x) (- x 5)) cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr -5)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* cptr))))
             (display "Test 9 Success !!!\n") (newline))

  (test-case "test-case 10"
             "test-case 10"
             (letrec ([ls (array (3 3 3) _int (((1 2 3) (4 5 6) (7 8 9))
                                               ((10 11 12) (13 14 15) (16 17 18))
                                               ((19 20 21) (22 23 24) (25 26 27))))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map (lambda (x) (- x 1)) cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr -1)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* cptr))))
             (display "Test 10 Success !!!\n") (newline))))

(display "\n<----------- Accelerate test-cases Run ----------->\n")
(if (run-tests accelerate-test-cases) (display "\n!!! Test Run Successfull !!!\n") (display "\n!!! Test Run Failed !!!\n"))

;;(define-hs gpu (_fun _string -> _string))

;;(printf "\nSample GPU computation function:\n")
;;(gpu "show (run $ (use (fromList (Z :.3 :.5) [1..] :: Array DIM2 Int)))")

(printf "########## Exiting racket safely ############\n")
