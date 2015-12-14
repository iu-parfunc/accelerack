#lang racket

(provide add1*
         sub1*)

(define (add1* ls)
  (cond
    ((null? ls) '())
    ((pair? (car ls)) (cons (add1* (car ls)) (add1* (cdr ls))))
    (else (cons (add1 (car ls)) (add1* (cdr ls))))))

(define (sub1* ls)
  (cond
    ((null? ls) '())
    ((pair? (car ls)) (cons (sub1* (car ls)) (sub1* (cdr ls))))
    (else (cons (sub1 (car ls)) (sub1* (cdr ls))))))
