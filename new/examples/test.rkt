#lang racket

(require ffi/unsafe)
(require "accelerack.rkt")

(acc (define x (use (vector _double '(2 3) '((2.21 3.3 4.2) (5.2 8.9 3.1))))))
x
(acc (define y (use (vector _double '(2 3) '((2.0 3.2 11.2) (50.1 2.1 41.9))))))
y
(acc (define a (use (vector _int '(2 3) '((1 2 5) (3 2 7))))))
a
(acc (define b (use (vector _int '(2 3) '((6 3 1) (23 12 9))))))
b
