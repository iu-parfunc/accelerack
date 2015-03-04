#lang racket

(provide acc)

(begin-for-syntax
  (define ht (make-hash))
  )

(define-syntax (acc stx)
  (syntax-case stx (define load run)
    
    ;Defines allow redefinition!
    
    ; Variable definition
    [(acc (define x exp)) (identifier? #'x)
                          (begin (hash-set! ht (syntax->datum (syntax x)) (syntax->datum (syntax exp)))
                                 #'(define x exp))]
    
    ; Function definition
    [(acc (define (fn x ...) body)) (begin
                                  (hash-set! ht (syntax->datum (syntax fn)) (syntax->datum (syntax (λ (x ...) body))))
                                  #'(define (fn x ...) body))]
   
    ; Generic use of higher order function, not well-understood in here yet
    ;[(acc (f (fn x) body)) (begin (hash-set! ht (syntax->datum (syntax fn)) (syntax->datum (syntax body)))
    ;                                 #'(f (fn x) body))]  ;<--separate handling for fn defn...todo
            
    ; Designed to be called in Definitions Window to create a run-time binding to the AccRack hashtable
    [(acc) (datum->syntax #'acc ht)]
    
    ; Designed to be called in Interactions Windows to reset the REPL compilation environment's hashtable
    [(acc load ht2) (begin (set! ht (syntax->datum (syntax ht2))) #'(display ht2))]
    
    ; Placeholder for run command.  Just display the hashtable contents.
    [(acc run) #'(begin (display "Wish I could run: ") (display (acc view)))]
    
    ; Catch unrecognized commands
    [(acc exp) (begin (printf "uncaught:~a~n" (syntax->datum stx)) #'(void))]
    
    ; Handles multiple commands
    [(acc exp exp2 x ...) #'(begin (acc exp) (acc exp2 x ...))]
    ))

(acc
 (define y 25)
 (define (sqr x) (* x x))
 )

(define ht (acc)) ; creates run-time binding of hashtable

ht

;(acc load ht) ; populates REPL's compilation environment with the hashtable
;; This could be more automated when this is packaged into one or more modules:
;; - But maybe not due to clear separation of compile environments and compile/run-time environs