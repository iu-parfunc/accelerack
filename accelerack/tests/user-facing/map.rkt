#lang racket
(require accelerack
         rackunit)

(test-case "plain-racket:: map, change type"
    (define x (map (lambda(x) (>= x 20))
                   (acc-array (10 20 30))))
    (check-equal? (acc-array->sexp x) '(#f #t #t)))

(test-case "acc: map1"
  (define-acc x (map (lambda (arg) (+ arg 1))
                     (acc-array (1 2 3))))
  (check-equal? 2 (car (acc-array->sexp x))))

(test-case "acc: map2, separate def"
  (define-acc x (acc-array (1 2 3)))
  (define-acc y (map (lambda(x) (+ x 1)) x))
  (check-equal? 2 (car (acc-array->sexp y))))

;; Same as test 8 but on floats and with add1 function:
(test-case "11: map test"
  (define-acc x (acc-array (15.15 25.25)))
  (define-acc y (map add1 x))
  (check-equal? '(16.15 26.25) (acc-array->sexp y)))

(test-case "12: map in racket"
  (define x (acc-array (15.15 25.25)))
  (define y (map add1 x))
  ; (printf "X array: ~a, Y array: ~a\n" x y)
  (check-equal? '(16.15 26.25) (acc-array->sexp y)))

;; Disabled until we support use:
;; (test-case "13: map of use"
;;   (define x (acc-array (15.15 25.25)))
;;   (define-acc y (map add1 (use x (Array 1 Double))))
;;   ; (printf "X array: ~a, Y array: ~a\n" x y)
;;   (check-equal? '(16.15 26.25) (acc-array->sexp y)))
