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

;; ================================================================================
(define user-ifc-invalid-test_cases (test-suite
  "invalid test cases"


  ;; TODO: Using invalid KEYWORDS in accelerack expressions by
  ;; accident should give a good error:
#;
  (test-case "add1 function definition"
    "add1 function definition"
    (define (f x) (begin (printf "hello") (add1 x)))
    (check-true (procedure? f))
    (check-equal? 4 (f 3)))

 #|
-- TODO: Test different unbound variable errors.

  (test-case "failing-test-case 1"
             "failing-test-case 1"
             (define x (map add1 unbound))
             (check-equal? 2 3)
             )

  (define unbound 3)
  (define x (map add1 unbound))

  |#

  ;; TODO: (use v t) where the value doesn't match the type at runtime.
  ))


;; ================================================================================
(define user-ifc-valid-test_cases (test-suite
  "valid test cases"

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

  (test-case "plain-racket: 1d generate"
    (check-equal? (acc-array->sexp (generate (lambda (x) x) 5))
                  '(0 1 2 3 4)))
    ; (define arr (generate (lambda (x y) (vector x y)) 10 10))

  (test-case "plain-racket: stencil3x3 returning zeros"
             (define x (acc-array ((1 2 3)(1 2 3)(1 2 3))))
             (define y (stencil3x3 (lambda (x1 x2 x3 x4 x5 x6 x7 x8 x9) 0) '(Constant 0) x))
             (check-equal? (acc-array->sexp y) '((0 0 0)(0 0 0)(0 0 0))))

  (test-case "plain-racket: stencil3x3 returning tuple"
    (define x (acc-array ((1 2 3)(1 2 3)(1 2 3))))
    (stencil3x3 (lambda (x1 x2 x3
                         x4 x5 x6
                         x7 x8 x9)
                  (vector x1 x2 x3
                          x4 x5 x6
                          x7 x8 x9))
                `(Constant 0)
		x))
  
  ;; Test case for valid (use v t)
  (test-case "test-case 9"
             (convert-compile-time-error
                 (let ()
                   ;; (define x (acc-array (1 2 3)))
                   (define q 1)
                   (define y (map (lambda(y) (+ y  q)) (acc-array (1 2 3))))
                   (check-equal? 2 (car (acc-array->sexp y))))))


  ;; WAIT TILL A LATER VERSION:

  ; TODO: Scalar support
  #;
  (test-case "scalar"
             "scalar"
    (define x 3)
    (printf "scalar x: ~a\n" x)
    (check-true (acc-scalar? x))
    (check-false (acc-array? x)))

  #;
  (test-case "scalar ascription"
             "scalar ascription"
    (define x (: 3 Int))
    (printf "scalar x: ~a\n" x)
    (check-true (acc-scalar? x))
    (check-false (acc-array? x))
    ; (check-equal? )
    )

#;
  (test-case "array ascription"
             "array ascription"
             (define x
               (: (acc-array ((#f #f #f)
                              (#t #t #t)))
                  (Array 2 Bool)))
             (check-equal? '((#f #f #f) (#t #t #t))
                           (acc-array->sexp x)))
#;
  (test-case "use a scalar in an array expression"
             "use a scalar in an array expression"
    (define x 4)
    (define y (map (lambda (_) x)
                       (acc-array (1 2 3))))
    (check-equal? '(4 4 4)
                  (acc-array->sexp y))
    )


#; ;; [2016.03.31] This is getting a totally bogus error
  (test-case "add1 function definition"
             "add1 function definition"
    (define (f x) (add1 x))
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
