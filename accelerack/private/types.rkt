#lang racket

;; Struct definitions used throughout the code base.
;; Most importantly, this is where the main acc-array datatype
;; and core operations are defined.

(require
 (only-in accelerack/private/allocate read-data* acc-manifest-array-flatref)
 (only-in accelerack/private/header acc-manifest-array?)
 ; racket/trace
 )

(provide ;; Complete Arrays:
         acc-array? make-acc-array
         acc-array-val
         acc-array-ref acc-array-flatref
         acc-array=?
         acc-array->sexp
         (contract-out [force-acc-array! (-> acc-array? acc-manifest-array?)])
         
         ;; Lower-level pieces:
         acc-delayed-array?  acc-delayed-array  acc-delayed-array-thunk
         
         ;; Elements
         acc-scalar? acc-element?

         ;; Syntax
         acc-syn-entry acc-syn-entry-type acc-syn-entry-expr

         ;; Types
         acc-type? acc-element-type?

         ;; delayed scalars are not fully implemented yet [2016.04.11]:
         acc-delayed-scalar? acc-delayed-scalar acc-delayed-scalar-thunk
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

;; Check if 2 acc-arrays are equal
(define (acc-array=? x nexp)
  (cond
    ;; ((or (boolean? x) (number? x)) (eqv? x nexp))
    ((acc-manifest-array? x) (equal? (read-data* x) (read-data* nexp)))
    ;; TODO - This doesn't work
    ;; ((acc-delayed-array? ))
    ((acc-array? x) (equal? (acc-array->sexp x) (acc-array->sexp nexp)))
    ;; Throw error if you can't find reason
    (else #f)))


;; Resolves an acc-array containing a delayed payload.  
;; Overwrites the acc-array's contents.
;; Returns the resulting acc-manifest-array?
(define (force-acc-array! x)
  (cond
    [(and (acc-array? x) (acc-manifest-array? (acc-array-val x)))
     (acc-array-val x)]
    [(and (acc-array? x) (acc-delayed-array? (acc-array-val x)))
     (let* ((v (acc-array-val x))
            (val (acc-delayed-array-thunk v))
            ;; val is of type () -> acc-array?
            ;; FIXME: change the convention to have the thunk return acc-manifest-array?
            (fval (acc-array-val (val))))
       (set-acc-array-val! x fval)
       fval)]
    [else (error 'force-acc-array! "Expected an acc-array, got ~a" x)]))

;; RRN: This should go away.  There's only one notion of a Racket-side acc-array:
;; I think this is resolved.
(define (acc-array->sexp x)
  (if (acc-array? x)
      (if (acc-manifest-array? (acc-array-val x))
          (read-data* (acc-array-val x))
          ;; (acc-array-val ((acc-delayed-array-thunk (acc-array-val x))))
          (read-data* (force-acc-array! x)))
      (error 'acc-array->sexp "works only on acc-array"))) ;;(read-data* x)))

;; Retrieve an element of an N-dimensional using an N-dimensional reference.
(define (acc-array-ref arr . inds)  
  (error 'acc-array-ref "FINISHME: acc-array-ref unimplemented")
  ; (let ((shape ...))
  ;  (acc-manifest-array-flatref (acc-array-val arr) ind))
  )

;; Retrieve an element of an N-dimensional array using a 1-dimensional
;; index into its "row-major" repesentation.
(define (acc-array-flatref arr ind)
  (acc-manifest-array-flatref (acc-array-val arr) ind))

;; The data-type for Racket-side arrays, which may be either
;; manifest or delayed.
(define-struct acc-array
  (val) ;; Eventually, hide acc-array-val & make-acc-array from user!
  #:guard (lambda (v _)
            (unless (or (acc-delayed-array? v) (acc-manifest-array? v))
              (raise-argument-error 'acc-array "acc-array?" v))
            v)
  #:methods gen:custom-write
  [(define (write-proc v prt mode)
     ((if mode write print)
      (let ((arr (acc-array-val v)))
        (if (acc-delayed-array? arr)
            (list 'acc-array "<DELAYED ARRAY>")
            (list 'acc-array (read-data* arr))))
      prt))]
  #:transparent ;; Temporary!  For debugging.
  #:mutable
  #:omit-define-syntaxes)

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

;; The SExp representation for an Accelerack type.
(define (acc-element-type? t)
  (match t
    ['Int #t]
    ['Bool #t]
    ['Double #t]
    [`#( ,t* ...) (andmap acc-element-type? t*)]
    [_ #f]))

;; Tests if a value is a valid SExpression encoding an Accelerack type.
(define (acc-type? t)
  (match t
    [`(Array ,n ,elt) (and (fixnum? n) (acc-element-type? elt))]
    [`#( ,t* ...)     (andmap acc-type? t*)]
    [`(-> ,t* ...)    (andmap acc-type? t*)]
    [t (acc-element-type? t)]))

;; The datatype for delayed arrays that are not yet computed by either
;; Racket/Accelerack or Haskell/Accelerate.
;;
;; TODO: If we eventually want to make (define x (acc y)) equivalent to (define-acc x y)
;; then we will need to store the captured syntax in here also, because we will not
;; have top-level bindings entered in the table.
(struct acc-delayed-array (thunk)
  #:guard (lambda (th _)
            (unless (procedure? th)
              (raise-argument-error 'acc-delayed-array "procedure?" th))
            th))

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


;; (display "Is Acc-array :  ")
;; (define  x (make-acc-array (acc-delayed-array (lambda(x) 1))))
;; (display (acc-array? x))
;; (display "\n")
