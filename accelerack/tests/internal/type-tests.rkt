#lang racket

(require rackunit rackunit/text-ui
         ;; See NOTE below:
         (only-in accelerack acc-array acc-array-ref fold map zipwith generate :
                  acc-array?
                  Int Bool Double use)
         accelerack/private/passes/typecheck
         syntax/macro-testing
         (only-in accelerack/acc-array/private/delayed acc-delayed-array?)
         )

;; Helper wrappers
(define (run-get-type e)
  (let-values (((type ex) (typecheck-expr (box '()) e)))
    type))

(define user-ifc-invalid-test_cases
  (test-suite
   "invalid test cases"
   ;; (test-case "test-case 1"
   ;;   "test-case 1"
   ;;   (check-exn
   ;;    #rx"typeerror"
   ;;    (lambda ()
   ;;      (convert-compile-time-error
   ;;       (let ()
   ;;         ;; (define-acc x (acc-array (1 2 3)))
   ;;         (define q 1)
   ;;         (define-acc y (map (lambda(y) (+ z (use q))) (acc-array (1 2 3))))
   ;;         (check-equal? 2 (car (acc-array->list y))))
   ;;       ))))
  ))
;; ================================================================================
(define user-ifc-valid-test_cases (test-suite
  "valid test cases"
  (test-case "type of scalar items"
    (check-equal? (run-get-type '9) 'Int)
    (check-equal? (run-get-type '#t) 'Bool)
    (check-equal? (run-get-type '#f) 'Bool)
    (check-equal? (run-get-type '9.333) 'Double))

  (test-case "type of array items"
    (check-equal? (run-get-type '(acc-array (1.1 2.1 3.1))) '(Array 1 Double))
    (check-equal? (run-get-type '(acc-array (1.1 2.1 3.1))) '(Array 1 Double))
    (check-equal? (run-get-type '(acc-array ((1 2) (2 3) (2 2)))) '(Array 2 Int))
    (check-equal? (run-get-type '(acc-array ((1.1 2.1) (2.0 3.22) (2.1 2.33)))) '(Array 2 Double))
    (check-equal? (run-get-type '(acc-array #(2 #(2 1.1 #f)))) '(Array 0 #(Int #(Int Double Bool))))
    )
  (test-case "lambda type tests"
    (check-match (run-get-type '(lambda (x) x))
                 `(-> ,x ,y) (equal? x y))
    (check-match (run-get-type '(lambda (x) y))
                 `(-> ,x ,y) (not (equal? x y)))
    (check-match (run-get-type '(lambda (x) (+ x x)))
                 `(-> ,x ,y) (equal? x y))
    (check-match (run-get-type '(lambda (x) (* x x)))
                 `(-> ,x ,y) (equal? x y))
    (check-match (run-get-type '(lambda (x y) y))
                 `(-> ,x ,y ,z) (equal? y z))
    (check-match (run-get-type '(lambda (x y z) y))
                 `(-> ,x ,y ,z ,k) (equal? y k))
    (check-match (run-get-type '(lambda ((x : Int) y z) y))
                 `(-> ,x ,y ,z ,k) (equal? x 'Int))
    )

  (test-case "application type tests"
    (check-match (run-get-type '((lambda (x y z) y) 1.1 2 1.1))
                 'Int)
    (check-match (run-get-type '(+ 1 2))
                 'Int)
    (check-match (run-get-type '((lambda (x) (+ x x)) 11))
                 'Int)
    ;; (check-match (run-get-type '(+ 1.1 2.2))
    ;;              'Double)
    )
  (test-case "map tests"
    (check-match (run-get-type '(map (lambda (x) (+ x 1)) (acc-array (1 2 3))))
                 '(Array 1 Int))
    )
  (test-case "fold tests"
    (check-match (run-get-type '(fold + 0 (acc-array (1 2 3))))
                 '(Array 0 Int))
    )
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
