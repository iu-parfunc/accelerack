#lang racket

;; ---------------------------------------------------------------
;; The syntax-capture and verification step.
;; ---------------------------------------------------------------

(provide define-acc
         ; acc ;; Not exposing this yet.
         run-gpu
         snapshot-current-acc-syn-table
         )

(require
         ;; We use the identifiers from "wrappers" as our names for map/fold/etc
         accelerack/private/wrappers
         accelerack/private/types         
         ; accelerack/private/racket_ops
         (only-in accelerack/private/syntax acc-array  :)
         (only-in accelerack/acc-array/private make-acc-array)
         accelerack/acc-array/private/delayed
         (for-syntax racket/base racket/trace racket/match     
                     syntax/parse syntax/id-table racket/dict
                     (only-in accelerack/private/syntax acc-array acc-type acc-lambda-param)
                     accelerack/private/syntax-table
                     accelerack/private/passes/typecheck
                     accelerack/private/types
		     accelerack/private/front-end
                     (only-in accelerack/private/executor launch-accelerack-ast)
                     (only-in accelerack/private/utils accelerack-debug-mode?)
                     rackunit
                     ))

(begin-for-syntax
  
  (define (is-a-lambda? stx)
    (syntax-parse stx
      #:literals (lambda)
      [(lambda rest ...) #t]
      [else #f]))

  (define (create-binding! name maybeType bod)
    (let-values ([(stripped inferredTy progWithTys normalized-sexp) (front-end-compiler bod)])
      (define finalTy (apply-to-syn-table maybeType inferredTy name progWithTys))
      (when (echo-types-param)
        (printf " define-acc ~a : ~a\n" (syntax->datum name) finalTy))      
      (match finalTy
        ;; TODO: Need support for delayed scalars:
        ; #`(define #,name (make-acc-scalar (acc-delayed-scalar (lambda () #,stripped))))        
        [(? acc-element-type?)
        ;; No deferred scalars currently, so all scalar computations evaluate eagerly:
         #`(define #,name  #,stripped)]
        [`(-> ,_ ...)
         ;; Policy 1:
         #`(define #,name  #,stripped)
         ;; Policy 2:
         #;
         (if (is-a-lambda? bod)
             #`(define #,name  #,stripped)
             (raise-syntax-error 'define-acc
             "does not currently handle function definitions that do not start with a lambda"
             name))
         ]
        [`(Array ,_ ,_)                 
         #`(define #,name (make-acc-array (acc-delayed-array (lambda () #,stripped))))]
        
        [else (raise-syntax-error 'define-acc
               (format "unknown type for define-acc bound variable: ~a" finalTy)
               name)])))
  )

;; Surprisingly, this form of persistence changes the VALUES of the
;; hash into normal lists, but not the keys.
(define-syntax (snapshot-current-acc-syn-table stx)
  (syntax-parse stx [(_) (snap-as-syntax)]))

;; --------------------------------------------------------------------------------

;; Not doing this yet until we sort out the story of where/how the
;; captured syntax would be stored.
;;
;; (define-syntax (acc stx)
;;   (syntax-parse stx
;;     [(_ e) (front-end-compiler #'e)]))

;; Type-checking happens at expansion time.
(define-syntax (run-gpu stx)
  (syntax-parse stx
    [(_ e) #`(launch-accelerack-ast #,(front-end-compiler #'e))]))

;; TODO: Need to defer execution ...
(define-syntax (define-acc stx)
  ;; Infers the type of the given expression and adds that type and
  ;; the expression to the syntax table.
  (syntax-parse stx
    [(_ x:identifier e)                     (create-binding! #'x #f #'e)]
    [(_ x:identifier : t:acc-type e)        (create-binding! #'x #'t #'e)]
    [(_ (f:identifier x:acc-lambda-param ...) e)
     (with-handlers
       ()
       #;
       ([exn:fail? (lambda (exn)
                     (raise-syntax-error 'define-acc
                                         (format "caught error during compilation:\n\n~a\n" exn)
                                         stx))])
       (create-binding! #'f #f #'(lambda (x ...) e)))]))


; --------------------------------------------------------------------------------
;; (define-acc x (acc-array (1 2 3)))
;; (define-acc x 1)
