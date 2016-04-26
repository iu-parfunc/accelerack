#lang racket

;; These should contain end-to-end tests of the functionality the end
;; user can access.
(require accelerack)  ;; Thus this module is really all we need.

(require rackunit
         rackunit/text-ui
         (only-in accelerack/private/utils accelerack-debug-mode?)
         syntax/macro-testing
         )

;; ================================================================================
;;  "invalid test cases"


  ;; TODO: Using invalid KEYWORDS in accelerack expressions by
  ;; accident should give a good error:
#;
  (test-case "add1 function definition"
    (define-acc (f x) (begin (printf "hello") (add1 x)))
    (check-true (procedure? f))
    (check-equal? 4 (f 3)))

 #|
-- TODO: Test different unbound variable errors.

  (test-case "failing-test-case 1"
             (define-acc x (map add1 unbound))
             (check-equal? 2 3)
             )

  (define unbound 3)
  (define-acc x (map add1 unbound))

  |#

;; TODO: (use v t) where the value doesn't match the type at runtime.


;; Valid test cases
;; ================================================================================

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

(test-equal? "simple acc macro 1" (acc 3) 3)
(test-equal? "simple acc macro 2" (acc (: 3 Int)) 3)

(test-equal? "until"
             (acc-array->sexp
              (generate (lambda ()
                          (until (i 1 (= i 10))
                                 (add1 i)))))
             10)

(test-case "acc: until"
   (define-acc arr
     (generate (lambda ()
                 (until (i 1 (= i 10))
                        (add1 i)))))
   (check-equal? arr (acc-array 10)))

(test-case "auntil"
           (define arr (acc-array (1 1 1)))
           (define arr2 (auntil (a arr (acc-array=? (fold + 0 a) (acc-array 9)))
                                (map (lambda (x) (add1 x)) a)))
           (check-equal? (acc-array->sexp arr2) '(3 3 3)))

(test-equal? "replicate"
             (acc-array->sexp
              (replicate () (5)
                         (generate (lambda () 9))))
             '(9 9 9 9 9))

(test-equal? "replicate'"
             (let ([x 5])
               (acc-array->sexp
                (replicate () (x)
                           (generate (lambda () 9)))))
             '(9 9 9 9 9))

(test-equal? "replicate2"
             (acc-array->sexp
              (replicate (r) (r 5)
                         (acc-array (1 2))))
             '((1 1 1 1 1)
               (2 2 2 2 2)))

(test-equal? "replicate2'"
             (let ([x 5])
               (acc-array->sexp
                (replicate (r) (r x)
                         (acc-array (1 2)))))
             '((1 1 1 1 1)
               (2 2 2 2 2)))  

(test-equal? "replicate3"
             (acc-array->sexp
              (replicate (c) (5 c)
                         (acc-array (1 2))))
             '((1 2)
               (1 2)
               (1 2)
               (1 2)
               (1 2)))

(test-equal? "replicate4"
             (acc-array->sexp
              (replicate (i) (3 i 3)
                         (acc-array (1 2))))
             '(((1 1 1)
                (2 2 2))
               ((1 1 1)
                (2 2 2))
               ((1 1 1)
                (2 2 2))))

(test-equal? "replicate zipwith"
             (acc-array->sexp
              (zipwith vector
                       (replicate (c) (3 c) (acc-array (1 2 3)))
                       (replicate (r) (r 3) (acc-array (1 2 3)))))
             '((#(1 1) #(2 1) #(3 1))
               (#(1 2) #(2 2) #(3 2))
               (#(1 3) #(2 3) #(3 3))))

;; Disabled until use is supported:
;; Test case for valid (use v t)
(test-case "test-case 9"
           (convert-compile-time-error
               (let ()
                 (define x (acc-array (1 2 3)))
                 (define q 1)
                 ;; Scalar use:
                 #;(define-acc y (map (lambda(y) (+ y  (use q)))
                                    (acc-array (1 2 3))))
                 ;; Array use
                 (define-acc y (map add1 (use x (Array 1 Int))))
                 (check-equal? 2 (car (acc-array->sexp y))))))

; (test-case "use of expr" (acc (use (+ 1 2) Int)))

(test-case "use of scalar"
  (define x (+ 1 2))
  (check-equal? (acc (+ 3 (use x Int)))
                6))

(test-case "scalar ascription"
  (define-acc x (: 3 Int))
  (check-true (acc-scalar? x))
  (check-false (acc-array? x))
  )

(test-case "scalar definition"
  (define-acc x 3)
  ;; No deferred scalars for now:
  (check-equal? x 3))

(test-case "scalar definition/ascription"
  (convert-compile-time-error
   (let ()  
     (: x Int)
     (define-acc x 3)
     ;; No deferred scalars for now:
     (check-equal? x 3))))


(test-case "array ascription"
           (define-acc x
             (: (acc-array ((#f #f #f)
                            (#t #t #t)))
                (Array 2 Bool)))
           (check-equal? '((#f #f #f) (#t #t #t))
                         (acc-array->sexp x)))

(test-case "use a scalar in an array expression"
  (define-acc foo 4)
  (define-acc bar (map (lambda (_) foo)
                     (acc-array (1 2 3))))
  (check-equal? '(4 4 4)
                (acc-array->sexp bar))
  )

(test-case "lexical scoping"
  (let () (define-acc foo 4) (void))
  (let () (define-acc foo #t) (void))  
  )

(test-case "replicate with define-acc (1)"
  (define-acc arr (acc-array (1 2)))
  (define-acc arr2 (let ([x 5]) (replicate (r) (r 5) arr)))
  (check-equal? (acc-array->sexp arr2)
		'((1 1 1 1 1)
		  (2 2 2 2 2))))

(test-case "replicate with define-acc (2)"
  (define-acc arr (acc-array (1 2)))
  (define-acc a 5)
  (define-acc arr2 (replicate (r) (r a) arr))
  (check-equal? (acc-array->sexp arr2)
		'((1 1 1 1 1)
		  (2 2 2 2 2))))


(test-case "replicate with define-acc (3)"
  (define-acc arr (acc-array (1 2)))
  (define-acc a 3)
  (define-acc arr2 (replicate (r) (a r a) arr))
  (check-equal? (acc-array->sexp arr2)
		'(((1 1 1)
		   (2 2 2))
		  ((1 1 1)
		   (2 2 2))
		  ((1 1 1)
		   (2 2 2)))))
