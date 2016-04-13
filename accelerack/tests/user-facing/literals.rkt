#lang racket
(require accelerack
         rackunit)

(test-case "plain-racket: literal1"
           (define x (acc-array 15))
           (check-equal? '15 (acc-array->sexp x)))

(test-case "plain-racket: literal2"
           (define x (acc-array (15.15 25.25)))
           (check-equal? '(15.15 25.25) (acc-array->sexp x)))

(test-case "plain-racket: literal3"
           (define x (acc-array ((#f #f #f)
                                 (#t #t #t))))
           (check-equal? '((#f #f #f) (#t #t #t)) (acc-array->sexp x)))

(test-case "plain-racket: literal4"
           (define x (acc-array (#(2 #(2 1.1 #f))
                                 #(1 #(3 2.2 #f))
                                 #(4 #(16 3.3 #f)))))
           (check-equal? '(#(2 #(2 1.1 #f))
                           #(1 #(3 2.2 #f))
                           #(4 #(16 3.3 #f))) (acc-array->sexp x)))

(test-case "plain-racket: literal5 (0D)"
           (define x (acc-array 1))
           (check-equal? (acc-array->sexp x) '1))

(test-case "plain-racket: literal6"
           (define x (acc-array (1 2)))
           (check-equal? (acc-array->sexp x) '(1 2)))

(test-case "plain-racket: print 0D array of tuple ref"
           (define x (acc-array #(11 22 33)))
           (with-output-to-string (lambda () (display x))))


(test-case "acc: literal5"
           (define x (acc-array (#(2 #(2 1.1 #f))
                                     #(1 #(3 2.2 #f))
                                     #(4 #(16 3.3 #f)))))
           (check-pred acc-array? x))

(test-case "acc: literal7"
           (define x (acc-array (1 2 3)))
           (check-equal? 1 (car (acc-array->sexp x))))
