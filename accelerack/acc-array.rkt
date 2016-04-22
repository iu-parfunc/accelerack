#lang racket

;; The public interface to the acc-array ADT.
;; This exposes MUTABLE, N-dimensional arrays, stored efficiently in memory, 
;; and in a manner that allows C or other languages to access them without
;; copying.

(require
 accelerack/acc-array/private/manifest-array
 accelerack/acc-array/private
 accelerack/acc-array/private/delayed
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
 force-acc-array!
 (contract-out
  [acc-array-dimension (-> acc-array? exact-nonnegative-integer?)]
  [acc-array-shape     (-> acc-array? (vectorof exact-nonnegative-integer?))]
  [acc-array-size      (-> acc-array? exact-nonnegative-integer?)])

 ;; Second, mutable interface, which is usually not exposed to end users:
 ;; acc-array-set! acc-array-flatset!
 )


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

