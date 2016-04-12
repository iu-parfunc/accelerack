#lang racket

;; This exposes MUTABLE, N-dimensional arrays, stored efficiently in memory, 
;; and in a manner that allows C or other languages to access them without
;; copying.

(require
 (only-in accelerack/acc-array/private/manifest-array read-data*)
 (only-in accelerack/acc-array/private/manifest-array/structs
          acc-manifest-array?)
 accelerack/acc-array/private/delayed)

(provide
 ;; Part of the public interface:
 acc-array? 

 ;; The private interface to the insides of acc-arrays:
 make-acc-array acc-array-val set-acc-array-val!
 )

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
