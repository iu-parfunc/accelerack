#lang racket

;; Struct definitions used throughout the code base.

(require
 (only-in accelerack/private/allocate read-data*)
 ; accelerack/private/header
 (only-in accelerack/private/header acc-manifest-array?)
 ; racket/trace
 )

(provide  acc-array?
          make-acc-array
          acc-array-val
          acc-array->list

          acc-syn-entry acc-syn-entry-type acc-syn-entry-expr
          acc-type?
          )

;; RRN: This should go away.  There's only one notion of a Racket-side acc-array:
(define (acc-array->list x)
  (if (acc-array? x)
      (read-data* (acc-array-val x))
      (read-data* x)))

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
(define-struct acc-syn-entry (type expr))

;; The SExp representation for an Accelerack type.
(define (acc-type? t)
  (match t
    ['Int #t]
    ['Bool #t]
    ['Double #t]
    [`(Array ,n ,elt) (and (fixnum? n) (acc-type? elt))]
    [`#( ,t* ...) (andmap acc-type? t*)]
    [_ #f]))


;; The datatype for delayed arrays that are not yet computed by either
;; Racket/Accelerack or Haskell/Accelerate.
;;
(struct acc-delayed-array ())


;; TODO:

;; The type of self-contained, serializable, Accelerack computations.
;; These can be sent across the FFI, or, in principle, even across the network.
; (struct acc-portable-package )
