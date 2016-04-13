#lang racket

(provide
 (contract-out
  [accelerack-debug-mode? (-> boolean?)])
 pass-output-chatter
 vector->list*)

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

;; DEPRECATED:
;; It is a bad sign if you need this.
(define (vector->list* vec/ls)
  (cond
    ((vector? vec/ls) (vector->list* (vector->list vec/ls)))
    ((null? vec/ls) '())
    ((vector? (car vec/ls)) (cons (vector->list* (car vec/ls)) (vector->list* (cdr vec/ls))))
    ((pair? (car vec/ls)) (cons (vector->list* (car vec/ls)) (vector->list* (cdr vec/ls))))
    (else (cons (car vec/ls) (vector->list* (cdr vec/ls))))))

