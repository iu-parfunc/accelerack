#lang racket

;; This provides the Accelerack core library.

(require "acc-types.rkt")

(require ffi/vector)

(provide acc run-acc 
         
         generate
         
         Z shape-size
         DIM0 DIM1 DIM2 DIM3
         
         (contract-out
          [r-arr (-> acc-shape? acc-array-type? acc-payload? r-arr?)]
          [r-arr-shape (-> r-arr? acc-shape?)]
          [r-arr-payload (-> r-arr? acc-payload?)]
         
          [rget (-> r-arr? acc-shape? acc-payload-val?)]
          
          
          )
         
         ;; FIXME: this should only be accessible from an internal module
         ;; so that the user does not mess with it:
         acc-syn-table
         
         (all-from-out ffi/vector) ;only necessary for very manual array construction
         )

;; a Rack-Array is a (r-arr Shape ArrayType Payload)
(struct r-arr (shape arrty payload)
  #:transparent
  #:guard (λ (shape arrty payload _)
            (cond
              [(not (= (shape-dim shape)
                       (arr-dim arrty)))
               (error "Array dimensionality mismatch")]
              [(let [(len (shape-size shape))]
                 (andmap (λ (plty vec)
                           (cond
                             [(not ((payload-type->vector-pred plty) vec))
                              (error "Array Payload type mismatch")]
                             [(not (= len ((payload-type->vector-length plty) vec)))
                              (error "Array Shape size mismatch")]
                             [else true]))
                         (arr-plty-list arrty)
                         payload))
               (values shape arrty payload)]))
  )

; rget : Rack-Array Shape -> PayloadVal
(define (rget rarr index)
  (match-let ([(r-arr sh `(Array ,sh1 ,plty) vs) rarr])
    (unless (acc-index? index sh) (error "Invalid index for given shape"))
    (let ([i (flatten-index index sh)])
      (cond
        [(acc-base-type? plty) ((payload-type->vector-ref plty) (first vs) i)]
        [else (list->vector (map (λ (fld v)
                                   ((payload-type->vector-ref fld) v i))
                                 (vector->list plty) vs))]
        ))))

;; Helper function for 1D references into payloads:
(define (payloads-ref elt payloads index)
  ;; assert (acc-type? elt)
  ;; assert (number? index)
  (match elt
    [`#(,flds ...)
     ; assert (= (length payloads) (length flds))
     (list->vector
      (map (lambda (fld pay)
             (payloads-ref fld (list pay) index))
           flds payloads))]
    
    ; [(? scalar-type? elt) ]
    [`(Array ,sh ,_) (error 'payloads-ref "should take element type, not array type: ~s" elt)]
    
    [Word64 (match-let ([(list p) payloads])
              (u64vector-ref p index))]
    
    ))

;; FIXME: need to introduce an abstract datatype for Accelerate arrays:
(define generate build-list)

;; generate : Shape [Shape -> Payload] -> Rack-Array
;(define generate2 (λ (sh fn)

;(define make-rarr (λ (sh vals 

;; The core library defines a global hash table that keeps track of
;; all Accelerate bindings defined anywhere.
;; 
;; Here we use an immutable hash table to support persistent snapshots
;; in case they are needed in the future.
(define acc-syn-table (box (make-immutable-hash)))

;; The main Accelerate syntax form for introducing bindings in the Accelerate 
;; namespace as well as the main Racket namespace.
;;
;; Note that this evaluates its subexpressions on the Racket side immediately, 
;; whereas Accelerate execution is deferred until "run-acc".
(define-syntax (acc stx)
  (syntax-case stx (define view load run get)
    
    ;Redefinitions are ignored.  Should throw an error.
    ; - but DrRacket's error-handling of redefinitions works well.  Comment out the guard and see.
    
    ; Variable definition
    [(acc (define x exp))  (identifier? #'x)
                           #'(begin 
                               ;; Note, concurrency safe (non-atomic update):                              
                               (set-box! acc-syn-table                    
                                         (hash-set (unbox acc-syn-table)
                                                   ;; Use the FULL identifier as the key:
                                                   (syntax x)
                                                   ;; For now strip source locations (BAD):
                                                   (syntax->datum (syntax exp))
                                                   ))
                               (define x exp))]
    
    ; Function definition
    [(acc (define (fn x ...) body)) ;(undefined? (syntax->datum #'fn))
     #'(begin 
         ;; Note, concurrency safe (non-atomic update):                              
         (set-box! acc-syn-table                    
                   (hash-set (unbox acc-syn-table)
                             ;; Use the FULL identifier as the key:
                             (syntax fn)
                             ;; For now strip source locations (BAD):
                             (syntax->datum (syntax (lambda (x ...) body)))
                             ))
         (define (fn x ...) body))]    
    #|        
    ; Catch unrecognized commands
    [(acc exp) (begin (printf "uncaught:~a~n" (syntax->datum stx)) #'(void))]
    |#
    ; Handles multiple commands
    [(acc exp exp2 x ...) #'(begin (acc exp) (acc exp2 x ...))]
    ))

;; The syntax form for running programs 
;;
;; FINISHME!
(define-syntax (run-acc stx)
  (syntax-case stx ()
    [(run-acc x ...) 
     #'(begin (printf "Running Acc program:\n")
              (pretty-print '(begin x ...)))]
    ))

;; TEMP: testing:
(acc (define y 25)
     (define (sqr x) (* x x))
     )

#|

#; (acc
 (define y 25)
 (define (sqr x) (* x x))
 
 ;attempt redefine
 ;(define y 9)
 ;(define (sqr x z) (+ x z))

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

|#



