#lang racket

;; These should contain end-to-end tests of the functionality the end
;; user can access.
(require accelerack)  ;; Thus this module is really all we need.

(require rackunit
         rackunit/text-ui
         (only-in accelerack/private/global_utils accelerack-debug-mode?)
         )

(define (maybe-display msg)
  (when (accelerack-debug-mode?)
    (fprintf (current-error-port) "~a\n" msg)))

(define user-ifc-invalid-test_cases (test-suite
  "invalid test cases"
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
             (define x (acc-array ((#f #f #f) (#t #t #t))))
             (check-equal? '((#f #f #f) (#t #t #t)) (acc-array->list x))
             (maybe-display "Test 3 Success !!!"))

  (test-case "test-case 4"
             "test-case 4"
             (define x (acc-array (#(2 #(2 1.1 #f)) #(1 #(3 2.2 #f)) #(4 #(16 3.3 #f)))))
             (check-equal? '(#(2 #(2 1.1 #f)) #(1 #(3 2.2 #f)) #(4 #(16 3.3 #f))) (acc-array->list x))
             (maybe-display "Test 4 Success !!!"))
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
