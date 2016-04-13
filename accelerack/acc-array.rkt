#lang racket

;; The public interface to the acc-array ADT.
;; This exposes MUTABLE, N-dimensional arrays, stored efficiently in memory, 
;; and in a manner that allows C or other languages to access them without
;; copying.

(require
 accelerack/acc-array/private/manifest-array
 accelerack/acc-array/private
 accelerack/acc-array/private/delayed
 accelerack/acc-array/private/arrayutils
 accelerack/private/utils
 )

(provide
 ;; Complete Arrays: full, private interface
 ;; ----------------------------------------
 ;; First, the public interface:
 acc-array? 
 acc-array-ref
 acc-array-flatref
 acc-array=?
 acc-array->sexp 
 (contract-out
  [force-acc-array! (-> acc-array? acc-manifest-array?)]
  [acc-array-dimension (-> acc-array? exact-nonnegative-integer?)]
  [acc-array-shape     (-> acc-array? (vectorof exact-nonnegative-integer?))]
  [acc-array-size      (-> acc-array? exact-nonnegative-integer?)])

 ;; Second, mutable interface, which is usually not exposed to end users:
 ;; acc-array-set! acc-array-flatset!
 )


;; Check if two acc-arrays have the same contents.
(define (acc-array=? x nexp)
  (cond
    ;; ((or (boolean? x) (number? x)) (eqv? x nexp))
    ((acc-manifest-array? x) (equal? (manifest-array->sexp x) (manifest-array->sexp nexp)))
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
          (manifest-array->sexp (acc-array-val x))
          ;; (acc-array-val ((acc-delayed-array-thunk (acc-array-val x))))
          (manifest-array->sexp (force-acc-array! x)))
      (error 'acc-array->sexp "works only on acc-array"))) ;;(manifest-array->sexp x)))

;; Retrieve an element of an N-dimensional using an N-dimensional reference.
(define (acc-array-ref arr . inds)
  (let ((offset (ND->1D-index (acc-array-shape arr) inds)))
    (acc-array-flatref arr offset)))

;; Retrieve an element of an N-dimensional array using a 1-dimensional
;; index into its "row-major" repesentation.
(define (acc-array-flatref arr ind)
  (manifest-array-flatref (force-acc-array! arr) ind))

;; Computing the rank of an array should NOT require forcing it.
;; This is statically available information that is part of the type.
(define (acc-array-dimension arr)
  (cond
    [(acc-manifest-array? (acc-array-val arr))
     (manifest-array-dimension (acc-array-val arr))]
    [else
     (error 'acc-array-dimension "FINISHME: need ot implement for delayed arrays")]))

;; Computing the exact shape forces the array:
(define (acc-array-shape arr)
  (manifest-array-shape (force-acc-array! arr)))

;; Computing the exact size forces the array:
(define (acc-array-size arr)
  (manifest-array-size (force-acc-array! arr)))

