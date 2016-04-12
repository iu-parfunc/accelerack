#lang racket

;; These should contain end-to-end tests of the functionality the end
;; user can access.
(require accelerack)  ;; Thus this module is really all we need.

(require rackunit
         rackunit/text-ui
         (only-in accelerack/private/utils accelerack-debug-mode?)
         syntax/macro-testing
         )

(define (maybe-display msg)
  (when (accelerack-debug-mode?)
    (fprintf (current-error-port) "~a\n" msg)))

(define user-ifc-invalid-test_cases (test-suite
  "invalid test cases"
  (test-case "test-case 1"
             "test-case 1"
             (check-exn
              #rx"undefined variable used in Accelerack expression"
              (lambda ()
                (convert-compile-time-error
                 (let ()
                   ;; (define-acc x (acc-array (1 2 3)))
                   (define q 1)
                   (define-acc y (map (lambda(y) (+ z (use q))) (acc-array (1 2 3))))
                   (check-equal? 2 (car (acc-array->sexp y))))
                 ))))
  (test-case "test-case 2"
             "test-case 2"
             (check-exn
              #rx"Unbound variable used in Accelerack 'use'\n"
              (lambda ()
                (convert-compile-time-error
                 (let ()
                   ;; (define-acc x (acc-array (1 2 3)))
                   (define-acc y (map (lambda(y) (+ y (use q))) (acc-array (1 2 3))))
                   (check-equal? 2 (car (acc-array->sexp y))))
                 ))))

  ;; TODO: Using invalid KEYWORDS in accelerack expressions by
  ;; accident should give a good error:
#;
  (test-case "add1 function definition"
    "add1 function definition"
    (define-acc (f x) (begin (printf "hello") (add1 x)))
    (check-true (procedure? f))
    (check-equal? 4 (f 3)))

 #|
-- TODO: Test different unbound variable errors.

  (test-case "failing-test-case 1"
             "failing-test-case 1"
             (define-acc x (map add1 unbound))
             (check-equal? 2 3)
             )

  (define unbound 3)
  (define-acc x (map add1 unbound))

  |#

  ;; TODO: (use v t) where the value doesn't match the type at runtime.
  ))

(define user-ifc-valid-test_cases (test-suite
  "valid test cases"

  (test-case "plain-racket: literal1"
             (define x (acc-array 15))
             (check-equal? '15 (acc-array->sexp x))
             (maybe-display "Test 1 Success !!!"))

  (test-case "plain-racket: literal2"
             (define x (acc-array (15.15 25.25)))
             (check-equal? '(15.15 25.25) (acc-array->sexp x))
             (maybe-display "Test 2 Success !!!"))

  (test-case "plain-racket: literal3"
             (define x (acc-array ((#f #f #f)
                                   (#t #t #t))))
             (check-equal? '((#f #f #f) (#t #t #t)) (acc-array->sexp x))
             (maybe-display "Test 3 Success !!!"))

  (test-case "plain-racket: literal4"
             (define x (acc-array (#(2 #(2 1.1 #f))
                                   #(1 #(3 2.2 #f))
                                   #(4 #(16 3.3 #f)))))
             (check-equal? '(#(2 #(2 1.1 #f))
                             #(1 #(3 2.2 #f))
                             #(4 #(16 3.3 #f))) (acc-array->sexp x))
             (maybe-display "Test 4 Success !!!"))

  (test-case "plain-racket: literal5 (0D)"
             (define x (acc-array 1))
             (check-equal? (acc-array->sexp x) '1)
             (maybe-display "Test 5 Success !!!"))
  
  (test-case "plain-racket: literal6"
             (define x (acc-array (1 2)))
             (check-equal? (acc-array->sexp x) '(1 2))
             (maybe-display "Test 6 Success !!!"))

  (test-case "plain-racket: print 0D array of tuple ref"
             (define x (acc-array #(11 22 33)))
             (with-output-to-string (lambda () (display x))))
  
  (test-case "plain-racket: acc-array=?"
             (define x (acc-array (#(2 #(2 1.1 #f))
                                   #(1 #(3 2.2 #f))
                                   #(4 #(16 3.3 #f)))))
             (define y (acc-array (#(2 #(2 1.1 #t))
                                   #(1 #(3 2.2 #f))
                                   #(4 #(16 3.3 #f)))))
             (define z (acc-array (#(2 #(2 1.1 #f)))))
             (check-true (acc-array=? x x))
             (check-true (acc-array=? y y))
             (check-true (acc-array=? z z))
             (check-false (acc-array=? x y))             
             (check-false (acc-array=? x z))
             (check-false (acc-array=? y z))
             (check-false (acc-array=? y x))             
             (check-false (acc-array=? z x))
             (check-false (acc-array=? z y)))
  
  
  (test-case "acc: literal5"
             (define-acc x (acc-array (#(2 #(2 1.1 #f))
                                       #(1 #(3 2.2 #f))
                                       #(4 #(16 3.3 #f)))))
             (check-pred acc-array? x))
  
  (test-case "acc: literal7"
             (define-acc x (acc-array (1 2 3)))
             (check-equal? 1 (car (acc-array->sexp x))))

  (test-case "plain-racket: array ref"
             (define x (acc-array (11 22 33)))    
             (check-equal? 11 (acc-array-flatref x 0))
             (check-equal? 22 (acc-array-flatref x 1))
             (check-equal? 33 (acc-array-flatref x 2))
             (check-exn exn:fail?
                        (lambda () (acc-array-flatref x -1))
                        "array ref negative")
             (check-exn exn:fail?
                        (lambda () (acc-array-flatref x 3))
                        "array ref past end")
             )

  (test-case "plain-racket: 1D array of tuple ref"
             (define x (acc-array ( #(11 22 33)) ))
             ; (check-equal? 1 (acc-array-dimension x))
             (check-equal? '#(11 22 33) (acc-array-flatref x 0))
             (check-exn exn:fail?
                        (lambda () (acc-array-flatref x 1))
                        "array ref past end"))

#;
  (test-case "plain-racket: 0D array of tuple ref"
             (define x (acc-array #(11 22 33)))
             ; (check-equal? 0 (acc-array-dimension x))
             ; (check-equal? '#(11 22 33) (acc-array-flatref x 0))
             ; (check-equal? '#(11 22 33) (acc-array-ref x))
             ;(printf "TEMP: ~a\n" x)
             (void) ;; FINISHME
             )
  
  (test-case "acc: map1"
             (define-acc x (map (lambda(x) (+ x 1))
                                (acc-array (1 2 3))))
             (check-equal? 2 (car (acc-array->sexp x))))

  (test-case "acc: map2, separate def"
             (define-acc x (acc-array (1 2 3)))
             (define-acc y (map (lambda(x) (+ x 1)) x))
             (check-equal? 2 (car (acc-array->sexp y)))
             (maybe-display "Test 8 Success !!!"))

  (test-case "acc: map + fold"
             (define-acc x (acc-array (1 2 3)))
             (define-acc y (map (lambda(x) (+ x 1)) x))
             (define-acc z (fold + 0 y))
             (check-equal? 9 (car (acc-array->sexp z))))
  
  (test-case "plain-racket: map + fold"
             (define x (acc-array (1 2 3)))
             (define y (map (lambda(x) (+ x 1)) x))
             (define z (fold + 0 y))
             (check-equal? 9 (car (acc-array->sexp z))))

  ;; Test case for valid (use v t)
  (test-case "test-case 9"
             (convert-compile-time-error
                 (let ()
                   ;; (define-acc x (acc-array (1 2 3)))
                   (define q 1)
                   (define-acc y (map (lambda(y) (+ y  (use q))) (acc-array (1 2 3))))
                   (check-equal? 2 (car (acc-array->sexp y))))))

  ;; Test cases to check forcing of deferred array in fold
  (test-case "test-case 10"
             (define-acc x (acc-array (1 2 3)))
             (define-acc y (fold + 0 x))
             (check-equal? (car (acc-array->sexp y)) 6))

  ;; Same as test 8 but on floats and with add1 function:
  (test-case "11: map test"
             (define-acc x (acc-array (15.15 25.25)))
             (define-acc y (map add1 x))
             ; (printf "Y array: ~a, acc-array? ~a\n" y (acc-array? y))
             (check-equal? '(16.15 26.25) (acc-array->sexp y)))

  (test-case "12: map in racket"
             (define x (acc-array (15.15 25.25)))
             (define y (map add1 x))
             ; (printf "X array: ~a, Y array: ~a\n" x y)
             (check-equal? '(16.15 26.25) (acc-array->sexp y)))

  (test-case "13: map of use"
             (define x (acc-array (15.15 25.25)))
             (define-acc y (map add1 (use x)))
             ; (printf "X array: ~a, Y array: ~a\n" x y)
             (check-equal? '(16.15 26.25) (acc-array->sexp y)))

  ;; WAIT TILL A LATER VERSION:

  ; TODO: Scalar support
  #;
  (test-case "scalar"
             "scalar"
    (define-acc x 3)
    (printf "scalar x: ~a\n" x)
    (check-true (acc-scalar? x))
    (check-false (acc-array? x)))
 
  #;
  (test-case "scalar ascription"
             "scalar ascription"
    (define-acc x (: 3 Int))
    (printf "scalar x: ~a\n" x)
    (check-true (acc-scalar? x))
    (check-false (acc-array? x))
    ; (check-equal? )
    )

#;
  (test-case "array ascription"
             "array ascription"
             (define-acc x
               (: (acc-array ((#f #f #f)
                              (#t #t #t)))
                  (Array 2 Bool)))
             (check-equal? '((#f #f #f) (#t #t #t))
                           (acc-array->sexp x)))
#;
  (test-case "use a scalar in an array expression"
             "use a scalar in an array expression"
    (define-acc x 4)
    (define-acc y (map (lambda (_) x)
                       (acc-array (1 2 3))))
    (check-equal? '(4 4 4)
                  (acc-array->sexp y))
    )

  (test-case "identity function definition"
             "identity function definition"
    (define-acc (f x) x)
    (check-true (procedure? f))
    (check-equal? 3 (f 3)))

#; ;; [2016.03.31] This is getting a totally bogus error
  (test-case "add1 function definition"
             "add1 function definition"
    (define-acc (f x) (add1 x))
    (check-true (procedure? f))
    (check-equal? 4 (f 3)))

  ))


; (run-acc (acc-array (1 2 3)))

(display "\n<----------- Invalid test-cases Run ----------->\n")
(if (run-tests user-ifc-invalid-test_cases)
    (display "\n!!! Test Run Successfull !!!\n")
    (begin (display "\n!!! Test Run Failed !!!\n")
           (exit 1)))

(display "\n<----------- Valid test-cases Run ----------->\n")
(if (run-tests user-ifc-valid-test_cases)
    (display "\n!!! Test Run Successfull !!!\n\n")
    (begin (display "\n!!! Test Run Failed !!!\n\n")
           (exit 1)))
