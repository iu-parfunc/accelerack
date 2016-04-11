#lang racket

;; The public interface to the acc-array ADT.
;; This exposes MUTABLE, N-dimensional arrays, stored efficiently in memory, 
;; and in a manner that allows C or other languages to access them without
;; copying.

(require
 (only-in accelerack/acc-array/private/manifest-array/allocate
          read-data* acc-manifest-array-flatref)
 (only-in accelerack/acc-array/private/manifest-array/structs
          acc-manifest-array?)
 accelerack/acc-array/private/delayed
 accelerack/acc-array/private/arrayutils
 )

(provide
 ;; Complete Arrays: full, private interface
 ;; ----------------------------------------
 ;; First, the public interface:
 acc-array? 
 acc-array-ref acc-array-flatref
 acc-array=?
 acc-array->sexp
 (contract-out [force-acc-array! (-> acc-array? acc-manifest-array?)])

 ;; Second, mutable interface, which is usually not exposed to end users:
 ;; acc-array-set! acc-array-flatset!
 
 ;; Third, private interface to the insides of acc-arrays:
 make-acc-array acc-array-val
 )


;; Check if two acc-arrays have the same contents.
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


