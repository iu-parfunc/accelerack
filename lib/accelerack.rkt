#lang racket

;; This provides the accelerack core library.

(require ffi/vector)

(provide acc run-acc 
         
         generate 
         
         acc-type? acc-base-type? acc-payload-type? acc-shape-type? acc-array-type?
         
         Z shape-size DIM0 DIM1 DIM2 DIM3
         
         r-arr r-arr-shape r-arr-vectors rget
         
         ;; FIXME: this should only be accessible from an internal module
         ;; so that the user does not mess with it:
         acc-syn-table
         )

;; Accelerate type include base types, payload, shapes, and arrays
;; TODO: Add functions
;; TODO: Add Arrays, that is, multiple arrays
(define (acc-type? t0)
  (or (acc-base-type? t0)
      (acc-payload-type? t0)
      (acc-shape-type? t0)
      (acc-array-type? t0)))

;; Base types include selected numeric primitives
(define (acc-base-type? t0)
  (match t0
    [(or 'Int 'Int8 'Int16 'Int32 'Int64) #t]
    [(or 'Word 'Word8 'Word16 'Word32 'Word64) #t]
    ;; TODO: support C Types?  Let's not...
    [(or 'Float 'Double) #t]
    [else false]))

;; Allows two representations of singleton payloads
; '(Array (Z Int) Float)
; '(Array (Z Int) #(Float))
;; FIXME: Pick one and tighten down type predicate
;;   - Could use the constructor guards to normalize one to the other

;; Payloads (tuples) is a vector of primitives
(define (acc-payload-type? t0)
  (match t0
    [(vector t1* ...) (andmap acc-base-type? t1*)]
    [else (acc-base-type? t0)]
    ))

;; Shapes denote dimensionality and 
(define (acc-shape-type? sht)
  (match sht
    [`(Z ,ints ...) 
     (andmap (lambda (x) (eq? 'Int x)) ints)]
    [else #f]))

;; a Shape is (Z [Int] ...)
; DIM0 is (Z)
; DIM1 is (Z Int)
; DIM2 is (Z Int Int)
(define (Z . ls)
  (if (andmap integer? ls)
      (cons 'Z ls)
      (error 'Z "cannot construct shape value with non-numeric argument: ~a" ls)))

;; TODO: Keep these?  Are they a convenience given the need to unquote?
;; Convenience shapes
(define DIM0 '(Z))
(define DIM1 '(Z Int))
(define DIM2 '(Z Int Int))
(define DIM3 '(Z Int Int Int))

;; shape-size : Shape -> Int
;; Produces the size of an array having the given shape
(define (shape-size shape)
  (foldl * 1 (rest shape)))

;; Array
(define (acc-array-type? t0)
  (match t0
    [`(Array ,sh ,pl) (and (acc-shape-type? sh) (acc-payload-type? pl))]
    [else false]))

;; a Rack-Array is a (r-arr Shape TupleOfVectors)
(struct r-arr (shape arrty vectors)
  #:transparent
  #:guard (λ (shape arrty vectors _)
            ;; TODO: check vector lengths against shape
            (values shape arrty vectors))
  )
;(struct r-arr (shapes list-of-vectors))

;; FIXME: I bet rget doesn't work for both ways of specifying singleton payloads

; rget : Rack-Array Shape-Index -> Payload
(define rget
  (λ (rarr index)
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



