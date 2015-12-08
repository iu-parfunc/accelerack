#lang racket

(provide 
 (contract-out 
  [add (-> number? number? number?)]
  [sub (-> number? number? number?)]
  [mult (-> number? number? number?)]
  [div (-> number? number? number?)]
  [vector->list* (-> (or/c pair? vector?) pair?)])
  ++
  --
  **
  )

(define-syntax (** stx)
  (syntax-case stx ()
    [(** x) #''(* x)]
    [(**) #''(**)]))

(define-syntax (++ stx)
  (syntax-case stx ()
    [(++ x) #''(+ x)]
    [(++) #''(++)]))

(define-syntax (-- stx)
  (syntax-case stx ()
    [(-- x) #''(- x)]
    [(--) #''(--)]))

;; add two numbers
(define (add x y)
  (+ x y))

;; subtract two numbers
(define (sub x y)
  (- x y))

;; multiply two numbers
(define (mult x y)
  (* x y))

;; divide two numbers
(define (div x y)
  (/ x y))

;; Convert given vector to list recursively
;; Arguments -> vector or list with nested vectors
;; Return value -> list with all nested vectors converted to list

(define (vector->list* vec/ls)
  (cond
    ((vector? vec/ls) (vector->list* (vector->list vec/ls)))
    ((null? vec/ls) '())
    ((vector? (car vec/ls)) (cons (vector->list* (car vec/ls)) (vector->list* (cdr vec/ls))))
    ((pair? (car vec/ls)) (cons (vector->list* (car vec/ls)) (vector->list* (cdr vec/ls))))
    (else (cons (car vec/ls) (vector->list* (cdr vec/ls))))))

