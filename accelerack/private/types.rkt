#lang racket

(require racket/trace rackunit)

;; Type definitions (structs) used throughout the code base.
;; This includes most things EXCEPT the main acc-array datatype,
;; which is exported from accelerack/acc-array
(provide
         ;; Elements
         acc-scalar? acc-int? acc-element?
         acc-sexp-data? acc-sexp-data-shallow?
         acc-element->type

         ;; Syntax
         acc-syn-entry acc-syn-entry-type acc-syn-entry-expr         
         
         ;; Types
         acc-type? acc-scalar-type? acc-element-type?
         numeric-type-var?
         make-type-schema type-schema type-schema? type-schema-vars type-schema-monoty
         
         ;; delayed scalars are not fully implemented yet [2016.04.11]:
         acc-delayed-scalar? acc-delayed-scalar acc-delayed-scalar-thunk

         ;; Boundary values for stencils
         stencil-boundary?
         acc-shape?

         )

;; Is a given Racket datum compatible with ANY accelerack scalar types?
;; A scalar here is defined as a single numeric or boolean value.
(define (acc-scalar? x)
  (or ; (fixnum? x) ;; FIXME: this rules out some numbers at the high ange.
   (vector? x)
   (acc-int? x)
   (boolean? x)
   (flonum? x)))

;; Confirm that an integer is in the expected range for the acc "Int" type.
(define (acc-int? x)
  (and (integer? x)
       (<= (- (expt 2 63)) x (sub1 (expt 2 64)))))

(define (acc-element->type x)
  (cond
    [(acc-int? x) 'Int]
    [(boolean? x) 'Bool]
    [(flonum? x)  'Double]
    [(vector? x) (vector-map acc-element->type x)]
    [else (error 'acc-element->type "this is not an array element: ~a" x)]))

;; This is anything that goes inside an array.
(define (acc-element? x)
  (or (acc-scalar? x)
      (and (vector? x)
           (andmap acc-element? (vector->list x)))))

;; O(1) Something which is plausibly SExp data representing Accelerack arrays or elements.
(define acc-sexp-data-shallow?
  (or/c pair? number? boolean? null? vector?))

;; O(N) Something which is SExp data representing Accelerack arrays or elements.
(define (acc-sexp-data? x)
  (or (acc-sexp-data-shallow? x)
      ((listof acc-sexp-data?) x)))


;; Valid shapes are just lists of numbers
(define acc-shape? (vectorof exact-nonnegative-integer?))

(define (stencil-boundary? x)
  (match x
    ['Clamp   #t]
    ['Mirror  #t]
    ['Wrap    #t]
    [`(Constant ,v) (acc-element? v)]
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
;; Note: this is a monotype.  No typevars allowed.
(define (acc-element-type? t)
  (match t
    [`#( ,t* ...) (andmap acc-element-type? t*)]
    [else (acc-scalar-type? t)]))

;; Tests if a value is a valid SExpression encoding an Accelerack type.
(define (acc-type? t)
  (match t
    [`(Array ,n ,elt) (and (or (symbol? n) (fixnum? n))
                           (or (symbol? elt) (acc-element-type? elt)))]
    [`#( ,t* ...)     (andmap acc-type? t*)]
    [`(-> ,t* ...)    (andmap acc-type? t*)]
    [(? acc-scalar-type? t) #t]
    ;; For some special forms like stencil bounds:
    ['SExp #t]
    ;; Type variables.  Currently these must start lower case.
    [(? symbol?)
     ;; Can't have zero-char symbols so this should be safe:
     #:when (char-lower-case? (string-ref (symbol->string t) 0))
     #t]
    [t (acc-element-type? t)]))

(check-true (acc-type? '(Array n a)))
(check-true (acc-type? '(Array 2 Int)))

(define (numeric-type-var? t)
  (match t
    ; [`(Num ,a) (symbol? a)] ;; First design.
    [a #:when (symbol? a)
       (string-prefix? (symbol->string a) "num_")]
    [else #f]))

;; TypeSchema:
(define-struct type-schema
  (vars   ;; setof TermVariable   
   monoty ;; acc-type? or instantiated-type? -- decide which
   )
  #:guard (lambda (v m _)
            (unless (set-eq? v)
              (raise-argument-error 'make-type-schema "set-eq?" v))

            ;; Loosening this for now to include instantiated types too:
            ;; (unless (acc-type? m)
            ;;   (raise-argument-error 'make-type-schema "acc-type?" m))
            
            (values v m))
  #:transparent)

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
