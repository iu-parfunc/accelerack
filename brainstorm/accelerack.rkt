#lang racket

(provide acc)

(begin-for-syntax
  (define ht (make-hash))
  )

(define-syntax (acc stx)
  (syntax-case stx (view load run)
    [(_ (f (fn x) body)) (begin (hash-set! ht (syntax->datum (syntax fn)) (syntax->datum (syntax body)))
                                     #'(f (fn x) body))]  ;<--separate handling for fn defn...todo
    [(_ (f x exp)) (begin  (display ht) #'(define x exp))]
    
    ; Designed to be called in Definitions Window to create a run-time binding to the AccRack hashtable
    [(acc view) (datum->syntax #'acc ht)]
    
    ; Designed to be called in Interactions Windows to reset the REPL compilation environment's hashtable
    [(acc load ht2) (begin (set! ht (syntax->datum (syntax ht2))) #'(display ht2))]
    
    ; Catch unrecognized commands
    [(acc exp) (begin (printf "uncaught:~a~n" (syntax->datum stx)) #'(void))]
    
    ; Handles multiple commands
    [(acc exp exp2 x ...) #'(begin (acc exp) (acc exp2 x ...))]
    ))

(acc 
 (define (sqr x) (* x x))
 )

(define ht (acc view)) ; creates run-time binding of hashtable

;(acc load ht) ; populates REPL's compilation environment with the hashtable
;; This could be more automated when this is packaged into one or more modules:
;; - But maybe not due to clear separation of compile environments and compile/run-time environs