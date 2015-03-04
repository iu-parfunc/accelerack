#lang racket

(provide acc
         generate)

(define generate build-list)

(begin-for-syntax
  (define ht (make-hash))
  (define UNDEFINED 'accrack-undefined)
  (define (undefined? id)
    (equal? (hash-ref ht id UNDEFINED) UNDEFINED))
  )

(define-syntax (acc stx)
  (syntax-case stx (define view load run get)
    
    ;Redefinitions are ignored.  Should throw an error.
    ; - but DrRacket's error-handling of redefinitions works well.  Comment out the guard and see.
    
    ; Variable definition
    [(acc (define x exp)) (and (identifier? #'x)
                               (undefined? (syntax->datum #'x)))
                          (begin (hash-set! ht (syntax->datum (syntax x)) (syntax->datum (syntax exp)))
                                 #'(define x exp))]
    
    ; Function definition
    [(acc (define (fn x ...) body)) ;(undefined? (syntax->datum #'fn))
                                    (begin
                                      (hash-set! ht (syntax->datum (syntax fn)) (syntax->datum (syntax (λ (x ...) body))))
                                      #'(define (fn x ...) body))]
    
    ; Generic use of higher order function, not well-understood in here yet
    ;[(acc (f (fn x) body)) (begin (hash-set! ht (syntax->datum (syntax fn)) (syntax->datum (syntax body)))
    ;                                 #'(f (fn x) body))]  ;<--separate handling for fn defn...todo
    
    ; Designed to be called in Definitions Window to create a run-time binding to the AccRack hashtable
    [(acc) (datum->syntax #'acc ht)]
    [(acc view) (begin (printf "~a~n" ht) #'(void))]
    [(acc get) #'ht]
    
    ; Designed to be called in Interactions Windows to reset the REPL compilation environment's hashtable
    [(acc load ht3) (begin (set! ht (syntax->datum (syntax ht3))) #'(display ht3))]
    
    ; Placeholder for run command.  Just display the hashtable contents.
    [(acc run) #'(begin (printf "Wish I could run: ~a~n" (acc)))]
    
    ; Catch unrecognized commands
    [(acc exp) (begin (printf "uncaught:~a~n" (syntax->datum stx)) #'(void))]
    
    ; Handles multiple commands
    [(acc exp exp2 x ...) #'(begin (acc exp) (acc exp2 x ...))]
    ))

#;(acc
 (define y 25)
 (define (sqr x) (* x x))
 
 ;attempt redefine
 (define y 9)
 (define (sqr x z) (+ x z))

 ;larger expressions
 (define z (sqr 3)) ;  (z . (sqr 3)) How is this more insufficient than the others?
 (define u (sqr y))
 (define v (sqr z))
 
 ; Should z, u, v be expanded?
 ; - No, because the Acc AST presumably stores references.
 ; - But visibly, it should be.  It's all connected.
 ; - But, no, the constructors have been called in squence and have returned only pointers.
 ; - So, do we save the origination info?  Or do we not call out to the API until run?
 ; Should AccRack check for undefined variables?
 ; - Again, like redefinitions, DrRacket does a good job of identifying the error
 ; - Comment out the defns of y and see. 
 
 )

(acc
 (define a (generate 10
                     (lambda (x) (* 2 x))))
 (define b (map sqr a))
 )

(define ht2 (acc)) ; creates run-time binding of hashtable...?

ht2

;; This does NOT work.  The reference works, but was it ever extensible?
;
;(acc load ht) ; populates REPL's compilation environment with the hashtable
;; This could be more automated when this is packaged into one or more modules:
;; - But maybe not due to clear separation of compile environments and compile/run-time environs