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
          acc-array->list

          acc-syn-entry acc-syn-entry-type acc-syn-entry-expr
          acc-type? acc-scalar-type?

          acc-delayed-array?  acc-delayed-array  acc-delayed-array-thunk
          acc-delayed-scalar? acc-delayed-scalar acc-delayed-scalar-thunk
          )

;; RRN: This should go away.  There's only one notion of a Racket-side acc-array:
;; I think this is resolved.
(define (acc-array->list x)
  (if (acc-array? x)
      (read-data* (acc-array-val x))
      (error 'acc-array->list "works only on acc-array"))) ;;(read-data* x)))

;; The data-type for Racket-side arrays, which may be either
;; manifest or delayed.
(define-struct acc-array (val)
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
  #:transparent
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
; (struct acc-portable-package )
