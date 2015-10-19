#lang racket

(provide
  unzip
  zip)
  

;; create empty list preserving the data-list structure
(define (create-ls fin-ls data-ls)
  (cond
    ((null? data-ls) fin-ls)
    ((pair? (car data-ls)) (cons (create-ls '() (car data-ls)) (create-ls fin-ls (cdr data-ls))))
    (else (create-ls (cons '() fin-ls) (cdr data-ls)))))

;;insert to the end of the list
(define (append-end x ls)
  (append ls (list x)))

;; distribute data-ls elements into new list structure
(define (populate fin-ls data-ls)
  (cond
    ((null? data-ls) '())
    ((pair? (car data-ls)) (cons (populate (car fin-ls) (car data-ls)) (populate (cdr fin-ls) (cdr data-ls))))
    (else (cons (append-end (car data-ls) (car fin-ls)) (populate (cdr fin-ls) (cdr data-ls))))))

;; flatten the tuple list
(define (unzip ls)
 (letrec ((fin-ls (reverse (create-ls '() (reverse (car ls))))))
         (foldl (lambda (x y) (populate y x)) fin-ls ls)))

;; zip the first element of each member in the data-ls
(define (zip-first fin-ls data-ls)
  (cond
    ((null? data-ls) '())
    ((pair? (caar data-ls)) (cons (zip-first '() (car data-ls)) (zip-first fin-ls (cdr data-ls))))
    (else (cons (caar data-ls) (zip-first fin-ls (cdr data-ls))))))

;; delete the first element of each member in the data-ls
(define (delete-first data-ls)
  (cond
    ((null? data-ls) '())
    ((pair? (caar data-ls)) (cons (delete-first (car data-ls)) (delete-first (cdr data-ls))))
    (else (cons (cdar data-ls) (delete-first (cdr data-ls))))))

;; zip the list elements into original form from the flattened version
(define (zip ls)
  (cond
    ((null? (car ls)) '())
    (else (cons (zip-first '() ls) (zip (delete-first ls))))))

;; test data
(define l1 '((2 (2 #t #f)) (1 (3 #f #f)) (4 (16 #t #f))))
(define l1* '((2 1 4) ((2 3 16) (#t #f #t) (#f #f #f))))

(define l2 '((2 #t) (1 #f) (3 #t)))
(define l2* '((2 1 3) (#t #f #t)))