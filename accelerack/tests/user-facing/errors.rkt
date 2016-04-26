#lang racket

;; Test for the expected (hopefully good) error messages

(require accelerack rackunit
         syntax/macro-testing
         )

(test-case "test-case 1"
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
  (check-exn
   #rx"Unbound variable used in Accelerack 'use'\n"
   (lambda ()
     (convert-compile-time-error
      (let ()
        ;; (define-acc x (acc-array (1 2 3)))
        (define-acc y (map (lambda(y) (+ y (use q))) (acc-array (1 2 3))))
        (check-equal? 2 (car (acc-array->sexp y))))
                 ))))

(test-case "fold of unknown dimension"
  (check-exn
   #rx"array of known dimension"
   (lambda ()
     (convert-compile-time-error
      (let ()
        (define-acc f (lambda (arr) (fold + 0 arr)))
        (void))
      ))))


(test-case "plus 5 function definition"
  (check-exn
   #rx"numeric type"
   (lambda ()
     (convert-compile-time-error
      (let ()
        (define-acc (f x) (+ x #t))
        (void))))))



; (acc-echo-types)
(test-case "app sqr / bad arg"
  (check-exn
   #rx"numeric type"
   (lambda ()
     (convert-compile-time-error
      (let () (define-acc (sqr x) (* x x))
           (define-acc y (sqr #t))
           (void))))))

(test-case "sqr function with bad signature"
  (check-exn
   #rx"rigid" 
   (lambda ()
     (convert-compile-time-error
      (let ()
        (: sqr (-> num_a Int))
        (define-acc (sqr x) (* x x))
        (void))))))

(test-case "expects a type"
  (convert-compile-time-error
   (let ()  
     (: x Int)
     (define-acc x 3)
     (void))))

(test-case "scalar definition / err"
  (check-exn
   #rx"Branches of an"
   (lambda ()
     (convert-compile-time-error
      (let ()
        (define-acc x 3)
        (define-acc y (if #t x #t))
        (void))))))


; (define-acc a2 (replicate (r c) (r (+ r 3) c)
;                          (acc-array ((1 2) (3 4)))))

(test-case "vector-ref index"
  (check-exn
   #rx"bad index argument"
   (lambda ()
     (convert-compile-time-error
      (acc (lambda (i) (vector-ref (vector 1 2 3 4) i)))))))


(check-exn
   #rx"vector type of known length"
   (lambda ()
     (convert-compile-time-error
      (acc (lambda (v) (vector-ref v 3))))))




