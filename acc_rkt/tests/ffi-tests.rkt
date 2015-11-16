#lang racket/base

;; Test C FFI and load code from ../acc_c

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

;;(define lib-ifc (ffi-lib "../../acc_hs/libhsifc"))
;;(define-ffi-definer define-ifc lib-ifc)

(define-runtime-path librts "../../acc_c/librts.so")
(define lib-rts (ffi-lib librts))
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

(define-hs accelerateMap (_fun _acc-array-pointer _acc-array-pointer _int -> _void))
(define-hs accelerateZipWith (_fun _acc-array-pointer _acc-array-pointer _acc-array-pointer _int -> _void))

(define (get-type arr)
  (if (acc-array? arr) (segment-type (acc-array-data arr)) (segment-type arr)))

(define (get-shape arr) 
  (readData (acc-array-shape arr)))

(define (get-result-array input-arr)
  (letrec ([type* (if (equal? ((ctype-scheme->c scalar) 'acc-payload-ptr) (get-type input-arr))
                      (get-tuple-type (unzip (readData* input-arr)) (get-shape input-arr))
                      (mapType (get-type input-arr)))]
           [temp (car (alloc-unit (get-shape input-arr) type*))])
          temp))
  

(define accelerate-test-cases (test-suite
  "GPU Test Cases"

  (test-case "test-case 1"
             "test-case 1"
             (letrec ([ls (array () _int 24)]
                      [cptr (car ls)]
                      [value (segment-type (acc-array-data cptr))]
                      [rktptr (cadr ls)]
                      [temp (acc-map add1 cptr)]
                      [result-arr (get-result-array cptr)])
                     (begin
                       (printf "---------------------------------------- ~s\n" value)
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr result-arr 1)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 1 Success !!!\n") (newline))

  (test-case "test-case 2"
             "test-case 2"
             (letrec ([ls (array (8) _int (1 2 3 4 5 6 7 8))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map (lambda (x) (+ x 5)) cptr)]
                      [result-arr (get-result-array cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr result-arr 5)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 2 Success !!!\n") (newline))

  (test-case "test-case 3"
             "test-case 3"
             (letrec ([ls (array (2 3) _int ((2 3 11) (50 2 41)))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map add1 cptr)]
                      [result-arr (get-result-array cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr result-arr 1)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 3 Success !!!\n") (newline))

  (test-case "test-case 4"
             "test-case 4"
             (letrec ([ls (array (2 3 2) _int (((10 20) (30 40) (50 60)) ((50 60) (70 80) (90 100))))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map add1 cptr)]
                      [result-arr (get-result-array cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr result-arr 1)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 4 Success !!!\n") (newline))

  (test-case "test-case 5"
             "test-case 5"
             (letrec ([ls (array (4 3 3) _int (((10 20 30) (30 40 50) (50 60 70))
                                               ((50 60 70) (70 80 90) (90 100 110))
                                               ((100 110 120) (120 130 140) (140 150 200))
                                               ((210 220 230) (240 250 260) (260 270 300))))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map (lambda (x) (+ x 100)) cptr)]
                      [result-arr (get-result-array cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr result-arr 100)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 5 Success !!!\n") (newline))

  (test-case "test-case 6"
             "test-case 6"
             (letrec ([ls (array () _int 100)]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map sub1 cptr)]
                      [result-arr (get-result-array cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr result-arr -1)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 6 Success !!!\n") (newline))

  (test-case "test-case 7"
             "test-case 7"
             (letrec ([ls (array (8) _int (10 20 30 40 50 60 70 80))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map (lambda (x) (- x 10)) cptr)]
                      [result-arr (get-result-array cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr result-arr -10)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 7 Success !!!\n") (newline))

  (test-case "test-case 8"
             "test-case 8"
             (letrec ([ls (array (2 3) _int ((100 1000 10000) (200 3000 40000)))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map sub1 cptr)]
                      [result-arr (get-result-array cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr result-arr -1)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 8 Success !!!\n") (newline))

  (test-case "test-case 9"
             "test-case 9"
             (letrec ([ls (array (2 3 2) _int (((9 99) (999 9999) (8 88)) ((888 8888) (7 77) (777 7777))))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map (lambda (x) (- x 5)) cptr)]
                      [result-arr (get-result-array cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr result-arr -5)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 9 Success !!!\n") (newline))

  (test-case "test-case 10"
             "test-case 10"
             (letrec ([ls (array (3 3 3) _int (((1 2 3) (4 5 6) (7 8 9))
                                               ((10 11 12) (13 14 15) (16 17 18))
                                               ((19 20 21) (22 23 24) (25 26 27))))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map (lambda (x) (- x 1)) cptr)]
                      [result-arr (get-result-array cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr result-arr -1)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 10 Success !!!\n") (newline))
  
  (test-case "test-case 11"
             "test-case 11"
             (letrec ([ls (array (8) _double (1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map add1 cptr)]
                      [result-arr (get-result-array cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr result-arr 1)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 11 Success !!!\n") (newline))

  (test-case "test-case 12"
             "test-case 12"
             (letrec ([ls (array (2 3) _double ((2.1 3.4 11.0) (50.0 2.5 41.6)))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map (lambda(x) (+ x 5)) cptr)]
                      [result-arr (get-result-array cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr result-arr 5)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 12 Success !!!\n") (newline))

  (test-case "test-case 13"
             "test-case 13"
             (letrec ([ls (array (2 3 2) _double (((10.1 20.2) (30.1 40.2) (50.1 60.2)) ((50.2 60.1) (70.2 80.1) (90.2 100.1))))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map add1 cptr)]
                      [result-arr (get-result-array cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr result-arr 1)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 13 Success !!!\n") (newline))
  (test-case "test-case 14"
             "test-case 14"
             (letrec ([ls (array (8) _double (10.00 20.20 30.30 40.40 50.50 60.60 70.70 80.80))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map (lambda (x) (- x 9)) cptr)]
                      [result-arr (get-result-array cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr result-arr -9)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 14 Success !!!\n") (newline))

  (test-case "test-case 15"
             "test-case 15"
             (letrec ([ls (array (2 3) _double ((100.01 1000.01 10000.01) (200.002 3000.003 40000.004)))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map sub1 cptr)]
                      [result-arr (get-result-array cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr result-arr -1)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 15 Success !!!\n") (newline))
  
  (test-case "test-case 16"
             "test-case 16"
             (letrec ([ls (array (3) (_tuple _int (_tuple _int _int)) (#(2 #(2 3)) #(1 #(3 6)) #(4 #(16 17))))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map add1 cptr)]
                      [result-arr (get-result-array cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr result-arr 1)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 16 Success !!!\n") (newline))
  
    (test-case "test-case 17"
             "test-case 17"
             (letrec ([ls (array (2 3)
                                 (_tuple _int (_tuple _int (_tuple _int _int)))
                                 ((#(2 #(2 #(3 5))) #(1 #(3 #(5 6))) #(4 #(16 #(7 7))))
                                  (#(4 #(5 #(3 7))) #(1 #(6 #(15 6))) #(14 #(26 #(7 5))))))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map add1 cptr)]
                      [result-arr (get-result-array cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr result-arr 1)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 17 Success !!!\n") (newline))
    
    (test-case "test-case 18"
             "test-case 18"
             (letrec ([ls (array (2 2 3)
                                 (_tuple _int _double (_tuple _double))
                                 (((#(0 1.1 #(0.0)) #(0 2.2 #(0.0)) #(0 3.3 #(0.0)))
                                   (#(0 3.3 #(0.0)) #(0 5.5 #(0.0)) #(0 7.7 #(0.0))))
                                  ((#(0 2.2 #(0.0)) #(0 3.3 #(0.0)) #(0 4.4 #(0.0)))
                                   (#(0 4.4 #(0.0)) #(0 6.6 #(0.0)) #(0 1.1 #(0.0))))))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map (lambda(x) (+ x 5)) cptr)]
                      [result-arr (get-result-array cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr result-arr 5)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 18 Success !!!\n") (newline))

    (test-case "test-case 19"
             "test-case 19"
             (letrec ([ls (array (3) (_tuple _int (_tuple _int (_tuple _int _int))) (#(2 #(2 #(3 5))) #(1 #(3 #(5 6))) #(4 #(16 #(7 13)))))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map sub1 cptr)]
                      [result-arr (get-result-array cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr result-arr -1)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 19 Success !!!\n") (newline))
    
    (test-case "test-case 20"
             "test-case 20"
             (letrec ([ls (array (2 2 3)
                                 (_tuple _int (_tuple _int (_tuple _int _double)))
                                 (((#(2 #(2 #(3 4.3))) #(1 #(3 #(5 6.4))) #(4 #(16 #(7 4.3))))
                                   (#(4 #(5 #(3 2.5))) #(1 #(6 #(15 5.4))) #(14 #(26 #(7 23.3)))))
                                  ((#(12 #(12 #(13 4.3))) #(11 #(13 #(15 12.1))) #(14 #(26 #(17 21.6))))
                                   (#(14 #(25 #(13 7.5))) #(11 #(16 #(25 8.5))) #(24 #(26 #(17 2.6)))))))]
                      [cptr (car ls)]
                      [rktptr (cadr ls)]
                      [temp (acc-map sub1 cptr)]
                      [result-arr (get-result-array cptr)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr))
                       (accelerateMap cptr result-arr -1)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 20 Success !!!\n") (newline))
    
    (test-case "test-case 21"
             "test-case 21"
             (letrec ([ls1 (array (8) _int (1 2 3 4 5 6 7 8))]
                      [ls2 (array (8) _int (10 20 30 40 50 60 70 80))]
                      [cptr1 (car ls1)]
                      [cptr2 (car ls2)]
                      [rktptr1 (cadr ls1)]
                      [rktptr2 (cadr ls2)]
                      [temp (acc-zipwith add cptr1 cptr2)]
                      [result-arr (get-result-array temp)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr1))
                       (printf "~s\n" (readData* cptr2))
                       (accelerateZipWith cptr1 cptr2 result-arr 0)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 21 Success !!!\n") (newline))
    
    (test-case "test-case 22"
             "test-case 22"
             (letrec ([ls1 (array () _double 50.0)]
                      [ls2 (array () _double 25.0)]
                      [cptr1 (car ls1)]
                      [cptr2 (car ls2)]
                      [rktptr1 (cadr ls1)]
                      [rktptr2 (cadr ls2)]
                      [temp (acc-zipwith div cptr1 cptr2)]
                      [result-arr (get-result-array temp)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr1))
                       (printf "~s\n" (readData* cptr2))
                       (accelerateZipWith cptr1 cptr2 result-arr 3)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 22 Success !!!\n") (newline))
    
     (test-case "test-case 23"
             "test-case 23"
             (letrec ([ls1 (array (4) _double (1.1 2.2 3.3 4.4))]
                      [ls2 (array (8) _double (10.50 20.50 30.50 40.50 50.50 60.50 70.50 80.50))]
                      [cptr1 (car ls1)]
                      [cptr2 (car ls2)]
                      [rktptr1 (cadr ls1)]
                      [rktptr2 (cadr ls2)]
                      [temp (acc-zipwith add cptr1 cptr2)]
                      [result-arr (get-result-array temp)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr1))
                       (printf "~s\n" (readData* cptr2))
                       (accelerateZipWith cptr1 cptr2 result-arr 0)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 23 Success !!!\n") (newline))
     
     (test-case "test-case 24"
             "test-case 24"
             (letrec ([ls1 (array (4) _int (10 20 30 40))]
                      [ls2 (array (6) _int (5 10 20 25 30 35))]
                      [cptr1 (car ls1)]
                      [cptr2 (car ls2)]
                      [rktptr1 (cadr ls1)]
                      [rktptr2 (cadr ls2)]
                      [temp (acc-zipwith sub cptr1 cptr2)]
                      [result-arr (get-result-array temp)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr1))
                       (printf "~s\n" (readData* cptr2))
                       (accelerateZipWith cptr1 cptr2 result-arr 1)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 24 Success !!!\n") (newline))
     
     (test-case "test-case 25"
             "test-case 25"
             (letrec ([ls1 (array (3) _int (1 2 3))]
                      [ls2 (array (4) _int (10 20 30 40))]
                      [cptr1 (car ls1)]
                      [cptr2 (car ls2)]
                      [rktptr1 (cadr ls1)]
                      [rktptr2 (cadr ls2)]
                      [temp (acc-zipwith mult cptr1 cptr2)]
                      [result-arr (get-result-array temp)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr1))
                       (printf "~s\n" (readData* cptr2))
                       (accelerateZipWith cptr1 cptr2 result-arr 2)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 25 Success !!!\n") (newline))
     
     (test-case "test-case 26"
             "test-case 26"
             (letrec ([ls1 (array (5) _double (10.0 20.0 30.0 40.0 50.0))]
                      [ls2 (array (5) _double (2.0 2.0 2.0 2.0 2.0))]
                      [cptr1 (car ls1)]
                      [cptr2 (car ls2)]
                      [rktptr1 (cadr ls1)]
                      [rktptr2 (cadr ls2)]
                      [temp (acc-zipwith div cptr1 cptr2)]
                      [result-arr (get-result-array temp)])
                     (begin
                       (printf "----------------------------------------\n")
                       (printf "Before accelerateMap Call --\n")
                       (printf "~s\n" (readData* cptr1))
                       (printf "~s\n" (readData* cptr2))
                       (accelerateZipWith cptr1 cptr2 result-arr 3)
                       (printf "After accelerateMap Call --\n")
                       (printf "~s\n" (readData* result-arr))
                       (printf "----------------------------------------\n")
                       (check-equal? (readData* temp) (readData* result-arr))))
             (display "Test 26 Success !!!\n") (newline))))

(display "\n<----------- Accelerate test-cases Run ----------->\n")
(if (run-tests accelerate-test-cases) (display "\n!!! Test Run Successfull !!!\n") (display "\n!!! Test Run Failed !!!\n"))

;;(define-hs gpu (_fun _string -> _string))

;;(printf "\nSample GPU computation function:\n")
;;(gpu "show (run $ (use (fromList (Z :.3 :.5) [1..] :: Array DIM2 Int)))")

(printf "########## Exiting racket safely ############\n")
