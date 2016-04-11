#lang racket

;; Type definitions (structs) used throughout the code base.
;; This includes most things EXCEPT the main acc-array datatype,
;; which is exported from accelerack/acc-array

(require
 (only-in accelerack/acc-array/private/manifest-array/allocate
          read-data* acc-manifest-array-flatref)
 (only-in accelerack/acc-array/private/manifest-array/structs
          acc-manifest-array?)
 )

(provide                  
         ;; Elements
         acc-scalar? acc-element?

         ;; Syntax
         acc-syn-entry acc-syn-entry-type acc-syn-entry-expr

         ;; Types
         acc-type? acc-scalar-type? acc-element-type?

         ;; delayed scalars are not fully implemented yet [2016.04.11]:
         acc-delayed-scalar? acc-delayed-scalar acc-delayed-scalar-thunk

         ;; Boundary values for stencils
         stencil-boundary?
         
         )

;; Is a given Racket datum compatible with ANY accelerack scalar types?
;; A scalar here is defined as a single numeric or boolean value.
(define (acc-scalar? x)
  (or ; (fixnum? x) ;; FIXME: this rules out some numbers at the high ange.
      (and (integer? x)
           (<= (- (expt 2 63)) x (sub1 (expt 2 64))))
      (boolean? x)
      (flonum? x)))

;; This is anything that goes inside an array.
(define (acc-element? x)
  (or (acc-scalar? x)
      (and (vector? x)
           (andmap acc-element? (vector->list x)))))


(define (stencil-boundary? x)
  (match x
    ['Clamp   #t]
    ['Mirror  #t]
    ['Wrap    #t]
    [`(Constant ,v) (acc-scalar? v)]
    [else #f]))

;; An entry in the syntax table.  It provides everything Accelerack
;; needs to know about a symbol bound with define-acc.
(define-struct acc-syn-entry (type expr)
  #:guard (lambda (t e _)
            (unless (acc-type? t)
              (raise-argument-error 'acc-syn-entry "acc-type?" t))
            (unless (syntax? e)
              (raise-argument-error 'acc-syn-entry "syntax?" e))
            (values t e))
  #:transparent)

;; The SExp representation for an Accelerack scalar type.
(define (acc-scalar-type? t)
  (match t
    ['Int #t]
    ['Bool #t]
    ['Double #t]
    [_ #f]))

;; The SExp representation for an Accelerack element type.
(define (acc-element-type? t)
  (match t
    [`#( ,t* ...) (andmap acc-element-type? t*)]
    [else (acc-scalar-type? t)]))

;; Tests if a value is a valid SExpression encoding an Accelerack type.
(define (acc-type? t)
  (match t
    [`(Array ,n ,elt) (and (fixnum? n) (acc-element-type? elt))]
    [`#( ,t* ...)     (andmap acc-type? t*)]
    [`(-> ,t* ...)    (andmap acc-type? t*)]
    [t (acc-element-type? t)]))


;; The same idea, but for scalar data.
;;
;; PROBLEM: are we going to overload +,*,etc to work over delayed scalars?
(struct acc-delayed-scalar (thunk)
  #:guard (lambda (th _)
            (unless (procedure? th)
              (raise-argument-error 'acc-delayed-scalar "procedure?" th))
            th))

;; TODO:

;; The type of self-contained, serializable, Accelerack computations.
;; These can be sent across the FFI, or, in principle, even across the network.
(struct acc-portable-package (sexp array-table)

  )
