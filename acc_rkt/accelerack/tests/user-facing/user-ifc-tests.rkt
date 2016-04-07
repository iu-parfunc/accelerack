#lang racket

;; These should contain end-to-end tests of the functionality the end
;; user can access.
(require accelerack)  ;; Thus this module is really all we need.

(require rackunit
         rackunit/text-ui
         (only-in accelerack/private/global_utils accelerack-debug-mode?)
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
                   (check-equal? 2 (car (acc-array->list y))))
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
                   (check-equal? 2 (car (acc-array->list y))))
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

  (test-case "test-case 1"
             "test-case 1"
             (define x (acc-array 15))
             (check-equal? '(15) (acc-array->list x))
             (maybe-display "Test 1 Success !!!"))

  (test-case "test-case 2"
             "test-case 2"
             (define x (acc-array (15.15 25.25)))
             (check-equal? '(15.15 25.25) (acc-array->list x))
             (maybe-display "Test 2 Success !!!"))

  (test-case "test-case 3"
             "test-case 3"
             (define x (acc-array ((#f #f #f)
                                   (#t #t #t))))
             (check-equal? '((#f #f #f) (#t #t #t)) (acc-array->list x))
             (maybe-display "Test 3 Success !!!"))

  (test-case "test-case 4"
             "test-case 4"
             (define x (acc-array (#(2 #(2 1.1 #f))
                                   #(1 #(3 2.2 #f))
                                   #(4 #(16 3.3 #f)))))
             (check-equal? '(#(2 #(2 1.1 #f)) #(1 #(3 2.2 #f)) #(4 #(16 3.3 #f))) (acc-array->list x))
             (maybe-display "Test 4 Success !!!"))

  (test-case "test-case 5"
             "test-case 5"
             (define-acc x (acc-array (#(2 #(2 1.1 #f))
                                       #(1 #(3 2.2 #f))
                                       #(4 #(16 3.3 #f)))))
             (check-pred acc-array? x)
             (maybe-display "Test 5 Success !!!"))
  (test-case "test-case 7"
             "test-case 7"
             (define-acc x (acc-array (1 2 3)))
             (check-equal? 1 (car (acc-array->list x)))
             (maybe-display "Test 7 Success !!!"))
  (test-case "test-case 6"
             "test-case 6"
             (define-acc x (map (lambda(x) (+ x 1))
                                (acc-array (1 2 3))))
             (check-equal? 2 (car (acc-array->list x)))
             (maybe-display "Test 6 Success !!!"))

  (test-case "test-case 8"
             "test-case 8"
             (define-acc x (acc-array (1 2 3)))
             (define-acc y (map (lambda(x) (+ x 1)) x))
             (check-equal? 2 (car (acc-array->list y)))
             (maybe-display "Test 8 Success !!!"))

  ;; Test case for valid (use v t)
  (test-case "test-case 9"
             "test-case 9"
             (convert-compile-time-error
                 (let ()
                   ;; (define-acc x (acc-array (1 2 3)))
                   (define q 1)
                   (define-acc y (map (lambda(y) (+ y  (use q))) (acc-array (1 2 3))))
                   (check-equal? 2 (car (acc-array->list y))))))

  ;; Test cases to check forcing of deferred array in fold
  (test-case "test-case 10"
             "test-case 10"
             (define-acc x (acc-array (1 2 3)))
             (define-acc y (fold + 0 x))
             (check-equal? (car (acc-array->list y)) 6)
             (maybe-display "Test 10 Success !!!"))

  ;; Same as test 8 but on floats and with add1 function:
  (test-case "11: map test"
             "11: map test"
             (define-acc x (acc-array (15.15 25.25)))
             (define-acc y (map add1 x))
             ; (printf "Y array: ~a, acc-array? ~a\n" y (acc-array? y))
             (check-equal? '(16.15 26.25) (acc-array->list y))
             (maybe-display "Test 11 Success !!!"))

  (test-case "12: map in racket"
             "12: map in racket"
             (define x (acc-array (15.15 25.25)))
             (define y (map add1 x))
             ; (printf "X array: ~a, Y array: ~a\n" x y)
             (check-equal? '(16.15 26.25) (acc-array->list y))
             (maybe-display "Test 11 Success !!!"))

  (test-case "13: map of use"
             "13: map of use"
             (define x (acc-array (15.15 25.25)))
             (define-acc y (map add1 (use x)))
             ; (printf "X array: ~a, Y array: ~a\n" x y)
             (check-equal? '(16.15 26.25) (acc-array->list y))
             (maybe-display "Test 11 Success !!!"))

;; TODO: Get scalars working AT ALL.  Even eagerly.
 #;
  (test-case "scalar"
             "scalar"
    (define-acc x 3)
    (printf "scalar x: ~a\n" x)
    (check-true (acc-scalar? x))
    (check-false (acc-array? x)))

  ;; WAIT TILL VERSION 0.2:

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
                           (acc-array->list x)))
#;
  (test-case "use a scalar in an array expression"
             "use a scalar in an array expression"
    (define-acc x 4)
    (define-acc y (map (lambda (_) x)
                       (acc-array (1 2 3))))
    (check-equal? '(4 4 4)
                  (acc-array->list y))
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
