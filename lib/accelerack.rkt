#lang racket

;; This provides the Accelerack core library.

(require "types.rkt")

;; Types exposed to Racket side
;; Base:  simplified categorization of numerical types
;; Element:  Number and Tuple/[ListOf Number]
;; Array:  Nested list  [ListOf [ListOf Element]] **using Rack-Arrays for now

;; Or should we provide limited access to structures?
;; They could contain helper functions not exported but used on Accelerack side
;; to convert/expand to needed functionality:  type inference, normalization, vectorization

(provide
 ;acc run-acc 
 
 (contract-out
  
  [rget (-> r-arr? index? element?)]
  [rput (-> r-arr? index? element? void?)]
  
  [generate/a (-> shape? r-gen-fn? r-arr?)]
  [map/a (-> r-map-fn? r-arr? r-arr?)]
  
  ; FIXME: The check for natural-number input in function argument is not generalized for 
  ;       any number of inputs, like the rest of the contract.
  ;       AND, it doesn't really work:  string-append gets through the contract.
  ; TODO: Look into custom contract combinators/creators, but this shouldn't be so hard
  ;       Will the case-lambda contract work for other than case-lambda functions?
  [generate (->i ([index (listof natural-number/c)]
                  [fn (index)
                      (and/c (λ (f) (procedure-arity-includes? f (length index)))
                             (unconstrained-domain-> (or/c real? (listof real?)))
                             #;(or/c (-> any) (-> natural-number/c any)
                                     (-> natural-number/c natural-number/c any)
                                     (-> natural-number/c natural-number/c
                                         natural-number/c any)
                                     (-> natural-number/c natural-number/c
                                         natural-number/c natural-number/c any)
                                     (-> natural-number/c natural-number/c
                                         natural-number/c natural-number/c
                                         natural-number/c any)
                                     (-> natural-number/c natural-number/c
                                         natural-number/c natural-number/c
                                         natural-number/c natural-number/c any))
                             )])
                 [result list? #;r-arr? #;(listof (vectorof real?))])]
  [amap (-> procedure? list? list?)]
  [complicate (-> list? r-arr?)]
  [simplify (-> r-arr? list?)]
  )
 
 ;; FIXME: this should only be accessible from an internal module
 ;; so that the user does not mess with it:
 ;acc-syn-table
 
 ;(all-from-out "types.rkt")
 )

; rget : Rack-Array Shape -> Element
(define (rget rarr index)
  (match-let ([(r-arr sh (array* shty plty) vs) rarr])
    ;    (displayln sh)
    ;    (displayln index)
    (unless (index-valid? sh index) (error 'rget "Invalid index for array"))
    (let ([i (flatten-index sh index)])
      (cond
        [(base*? plty) (acc-vector-ref (first vs) i)]
        [else (list->vector (map (λ (fld v)
                                   (acc-vector-ref v i))
                                 (pl*->list plty) vs))]
        ))))

;; rput : Rack-Array Shape Element -> (void)
(define (rput rarr index elem)
  (match-let ([(r-arr sh (array* shty plty) vs) rarr])
    (unless (index-valid? sh index)
      (error 'rput "Invalid index for array's shape"))
    (unless (element-valid? plty elem)
      (error 'rput "Value invalid for array's payload"))
    (let ([i (flatten-index sh index)])
      (cond
        [(base*? plty) (acc-vector-set! (first vs) i elem)]
        [else (for ([v (in-list vs)]
                    [fld (in-vector plty)]
                    [val (in-vector elem)])
                (acc-vector-set! v i val))]
        ))))

;; zero-array : Shape PayloadType* -> Rack-Array
;; (zero-array sh plty) produces an array of shape sh filled with zero elements of plty
(define (zero-array sh plty)
  (r-arr sh (array* (shape-dim sh) plty)
         (let ([zeros (build-list (shape-size sh) (const 0))])
           (map (λ (bty) (apply (acc-vector bty) zeros))
                (pl*->list plty)))))

;; gen-indices : Shape -> [ListOf Index]
;; (gen-indices sh) produces a complete list of all valid indices in sh
;; Confirmed linear run time
(define (gen-indices sh)
  (cond
    [(shape-empty? sh) empty]
    [(equal? sh '(Z)) (list (Z))]
    [else
     (letrec
         ([; gen-indices-h : [ListOf Nat] -> [ListOf Index]
           ; this helper processes non-empty shapes with the 'Z stripped off
           gen-indices-h
           (λ (sh)
             (if (empty? sh) (list empty)
                 (foldr append empty
                        (build-list (first sh)
                                    (λ (n) (map (λ (index) (cons n index))
                                                (gen-indices-h (rest sh))))))))])
       (map (λ (ls) (apply Z ls))
            (gen-indices-h (rest sh))))]))


;; generate/a : Shape Gen-Fn -> Rack-Array
;; Produces a Rack-Array with Shape sh
(define (generate/a sh fn)
  (let ([arr0 (zero-array sh (r-fn-plty fn))])
    (for ([index (gen-indices sh)])
      (rput arr0 index (fn index)))
    arr0))

;; map/a : Map-Fn Rack-Array -> Rack-Array
;; TODO contract can check dimensionality matchup
;;      - and fn arg applicability
;;      - in generate/a also...
(define (map/a fn arr)
  (generate/a (r-arr-shape arr)
              (r-gen-fn (r-fn-shty fn) (r-fn-plty fn)
                        (λ (index) (fn (rget arr index))))))

;; a SShape is a [ListOf Nat]
;; a SIndex is a SShape
;; a SElement is (U Real [ListOf Real])
;; an SArray is a [ListOf [ListOf SElement]]

; plty-default : Nat -> PayloadType*
(define plty-default
  (case-lambda
    [() 'Float]
    [(d) (apply vector (build-list d (const 'Float)))]))

;; generate : SShape [Nat ... -> SElement] -> SArray
(define (generate ssh sfn)
  (let* ([sh (apply Z ssh)]
         [shty (shape-dim sh)]
         [el0 (apply sfn (rest (index-0 shty)))]
         [plty (if (number? el0) (plty-default) (plty-default (length el0)))]
         [arr (generate/a sh (r-gen-fn shty plty (make-gen-fn sfn)))])
    #;(pretty-print arr)
    (simplify arr)))

;; map : [SElement -> SElement] SArray -> SArray
(define (amap f arr)
  (let ([rarr (complicate arr)])
    (simplify (map/a (r-map-fn (shape-dim (r-arr-shape rarr))
                               (array*-pl* (r-arr-arrty rarr))
                               (make-map-fn-ls f))
                     rarr))))

;; complicate : SArray -> Rack-Array
;; vectorizes & flatten dimensions and unzips tuples
;; NOTE: always uses 1-D shape
(define (complicate lls)
  (letrec ([sh-finder (λ (lls) (cond
                                 [(empty? lls) (cons 0 empty)]
                                 [(empty? (first lls)) (cons 0 empty)]
                                 [(number? (first (first lls))) (cons (length lls) empty)]
                                 [else (cons (length lls) (sh-finder (first lls)))]))]
           [helper (λ (lls) (cond
                              [(empty? lls) empty]
                              [(number? (first lls)) (list lls)]
                              [else (apply append (map helper lls))]))])
    (let ([payload (if (empty? lls) (list (vector))
                        (apply ((curry map) vector) (helper lls)))]
          [shape (apply Z (sh-finder lls))])
      (r-arr shape (array* (shape-dim shape) (plty-default (length payload)))
             payload))))  

;; simplify: Rack-Array -> SArray
;; zips tuples and devectorizes & splits dimensions
(define (simplify arr)
  (let ([stage1 (apply map list (map vector->list (r-arr-payload arr)))]
        [shape (r-arr-shape arr)])
    (letrec ([split* (λ (ls sh)
                       (let ([n (shape-size sh)])
                         (cond [(empty? ls) empty]
                               [else (let-values ([(front others) (split-at ls n)])
                                       (cons (split-all front sh)
                                             (split* others sh)))])))]
             [split-all (λ (lls sh)
                          (if (< (shape-dim sh) 2)
                              lls
                              (split* lls (apply Z (rest (rest sh))))))])
      (split-all stage1 shape))))



;--------------------------------------------------------------------------------------
; Below is old
;--------------------------------------------------------------------------------------
#|

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



|#