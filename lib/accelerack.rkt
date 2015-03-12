#lang racket

;; This provides the Accelerack core library.

(require "acc-types.rkt")
(require ffi/vector)

(provide acc run-acc 
         
         generate
         
         Z shape-size
         DIM0 DIM1 DIM2 DIM3
         
         r-arr r-arr-shape r-arr-vectors rget
         
         ;; FIXME: this should only be accessible from an internal module
         ;; so that the user does not mess with it:
         acc-syn-table
         
         (all-from-out ffi/vector) ;only necessary for very manual array construction
         )

;; a Rack-Array is a (r-arr Shape ArrayType TupleOfVectors)
(struct r-arr (shape arrty vectors)
  #:transparent
  #:guard (λ (shape arrty vectors _)
            (cond
              [(not (= (shape-dim shape)
                          (arr-dim arrty)))
               (error "Array dimensionality mismatch")]
              [(let [(len (shape-size shape))]
                 (andmap (λ (pl-ty vec)
                           (cond
                             [(not ((payload-type->vector-pred pl-ty) vec))
                              (error "Array Payload type mismatch")]
                             [(not (= len ((payload-type->vector-length pl-ty) vec)))
                              (error "Array Shape size mismatch")]
                             [else true]))
                         (vector->list (arr-payload arrty))
                         vectors))
               (values shape arrty vectors)]))
  )

;(struct r-arr (shapes list-of-vectors))

;; FIXME: I bet rget doesn't work for both ways of specifying singleton payloads

; rget : Rack-Array Shape -> Payload
(define (rget rarr index)
  (match-let ([(r-arr sh `(Array ,sh1 #(,flds ...)) vs) rarr])
    (if (acc-index? index sh)
        (let ([i (flatten-index index sh)])
          (list->vector
           (map (λ (fld vs)
                  ((payload-type->vector-ref fld) vs i))
                flds vs))
          )
        (error "Invalid index for given shape")) ; FIXME: Make this an assertion at pred call
       ))
    
    #|
    (match rarr
      ;[(r-arr (Z) vs) (first vs)]
      [(r-arr `(Z ,d1) `(Array (Z Int) ,elt) vs)
       (match index 
         [`(Z ,ix) (payloads-ref elt vs ix)])]
      [(r-arr `(Z ,d1 ,d2) `(Array (Z Int Int) ,elt) vs)
       (match index 
         [`(Z ,ix0 ,ix1) 
          (payloads-ref elt vs (+ ix1 (* ix0 d2)))])]
      )))
|#

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

;;;;; Some confusion here between when to use (Z Int Int) vs (Z 3 5)
; No way to use (Z Int Int) in Racket yet.

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

(define ht2 (acc)) ; creates run-time binding of hashtable...?

ht2

;; This does NOT work.  The reference works, but was it ever extensible?
;
;(acc load ht) ; populates REPL's compilation environment with the hashtable
;; This could be more automated when this is packaged into one or more modules:
;; - But maybe not due to clear separation of compile environments and compile/run-time environs

|#



