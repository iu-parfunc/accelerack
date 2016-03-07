#lang racket

;; Struct definitions used throughout the code base.

(require
 (only-in accelerack/private/allocate read-data*)
 (only-in accelerack/private/header acc-manifest-array?)
 ; racket/trace
 )

(provide  acc-array?
          make-acc-array
          acc-array-val
          eq-acc-array?
          acc-array->list
          acc-scalar?
          acc-syn-entry acc-syn-entry-type acc-syn-entry-expr
          acc-type? acc-scalar-type?
          acc-delayed-array?  acc-delayed-array  acc-delayed-array-thunk
          ;; we should get rid of delayed scalars!
          acc-delayed-scalar? acc-delayed-scalar acc-delayed-scalar-thunk
          force-delayed-array!
          )

;; Is the datum compatible with ANY accelerack scalar types?
(define (acc-scalar? x)
  (or ; (fixnum? x) ;; FIXME: this rules out some numbers at the high ange.
      (and (integer? x)
           (<= (- (expt 2 63)) x (sub1 (expt 2 64))))
      (boolean? x)
      (flonum? x)))

;; Check if 2 acc-arrays are equal
(define (eq-acc-array? x nexp)
  (cond
    ;; ((or (boolean? x) (number? x)) (eqv? x nexp))
    ((acc-manifest-array? x) (equal? (read-data* x) (read-data* nexp)))
    ;; TODO - This doesn't work
    ;; ((acc-delayed-array? ))
    ((acc-array? x) (equal? (acc-array->list x) (acc-array->list nexp)))
    ;; Throw error if you can't find reason
    (else #f)))


;; Resolves a acc-array with delayed array , resolves it and overwrites it and return value
;; acc-array? -> acc-manifest-array?
(define (force-delayed-array! x)
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
    [else (error 'force-delayed-array! "Expected an acc-array, got ~a" x)]))

;; RRN: This should go away.  There's only one notion of a Racket-side acc-array:
;; I think this is resolved.
(define (acc-array->list x)
  (if (acc-array? x)
      (if (acc-manifest-array? (acc-array-val x))
          (read-data* (acc-array-val x))
          ;; (acc-array-val ((acc-delayed-array-thunk (acc-array-val x))))
          (read-data* (force-delayed-array! x)))
      (error 'acc-array->list "works only on acc-array"))) ;;(read-data* x)))

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
(define (acc-scalar-type? t)
  (match t
    ['Int #t]
    ['Bool #t]
    ['Double #t]
    [`#( ,t* ...) (andmap acc-scalar-type? t*)]
    [_ #f]))

(define (acc-type? t)
  (match t
    [`(Array ,n ,elt) (and (fixnum? n) (acc-scalar-type? elt))]
    [`#( ,t* ...)     (andmap acc-type? t*)]
    [`(-> ,t* ...)    (andmap acc-type? t*)]
    [t (acc-scalar-type? t)]))

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
