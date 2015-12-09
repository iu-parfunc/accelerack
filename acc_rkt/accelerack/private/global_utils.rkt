#lang racket

;; Part of prototype.rkt...

(provide
 (contract-out
  [add (-> number? number? number?)]
  [sub (-> number? number? number?)]
  [mult (-> number? number? number?)]
  [div (-> number? number? number?)]
  [vector->list* (-> (or/c pair? vector?) pair?)]
  [accelerack-debug-mode? (-> boolean?)])
  pass-output-chatter
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


(define (accelerack-debug-mode?)
  (match (getenv "DEBUG_ACCELERACK")
    [#f      #f]
    ["0"     #f]
    ["False" #f]
    ["false" #f]
    ["#f"    #f]
    ["True"  #t]
    ["true"  #t]
    ["#t"    #t]
    ["1"     #t]
    ;; TODO: in the future may have a verbosity level instead of just a boolean.
    [other
     (fprintf (current-error-port) "WARNING: don't know how to interpret as boolean: DEBUG_ACCELERACK=~a\n" other)
     #f]))

(define (pass-output-chatter name res)
  (when (accelerack-debug-mode?)
    (fprintf (current-error-port)
             "\nPass output, ~a:\n~a\n~a\n" name
             "================================================================================"
             res)))
