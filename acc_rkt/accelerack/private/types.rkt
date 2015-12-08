#lang racket

;; Struct definitions used throughout the code base.

(require
 (only-in accelerack/private/acc_allocate read-data*)
 ; accelerack/private/acc_header
 (prefix-in cdata: (only-in accelerack/private/acc_header acc-array?))
 )

(provide  acc-array?
          make-acc-array
          acc-array-val
          acc-array->list
          ; acc-array-data
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
            (unless (or (acc-delayed-array? v) (cdata:acc-array? v))
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

;; The datatype for delayed arrays that are not yet computed by either
;; Racket/Accelerack or Haskell/Accelerate.
(struct acc-delayed-array ())
