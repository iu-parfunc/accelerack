#lang racket

;; This is the public interface for manifest arrays in Accelerack.
;; End users should not need to use this.

;; Implementors should use this rather than violating manifest-array's
;; abstraction.  (Internally, it is implemented based on a tree of C
;; pointers.)

(require (except-in ffi/unsafe ->)
         ffi/unsafe/cvector
         accelerack/acc-array/private/arrayutils
         accelerack/private/utils

         (only-in accelerack/private/types
                  acc-element? acc-shape? acc-type? acc-scalar-type? acc-element-type?
                  acc-sexp-data? acc-sexp-data-shallow?)
         ; (only-in accelerack/private/paven_old/global_utils vector->list*)
         racket/contract
         (only-in '#%foreign ctype-scheme->c ctype-c->scheme))
(require racket/trace)

(provide
 acc-manifest-array?
 manifest-array-ref
 (contract-out
  ;; Manifest arrays:
  [list->manifest-array (-> acc-type? acc-shape? acc-sexp-data?
                            acc-manifest-array?)]
  [make-empty-manifest-array (-> acc-shape? acc-type? acc-manifest-array?)]
  [manifest-array-shape (-> acc-manifest-array? (vectorof exact-nonnegative-integer?))]
  [manifest-array-size  (-> acc-manifest-array? exact-nonnegative-integer?)]
  [manifest-array-dimension (-> acc-manifest-array? exact-nonnegative-integer?)]

  [manifest-array-flatref (-> acc-manifest-array? exact-nonnegative-integer?
                              acc-element?)]
  [manifest-array-flatset! (-> acc-manifest-array? exact-nonnegative-integer?
                               acc-element? void?)]
  [manifest-array-set! (-> acc-manifest-array? (listof exact-nonnegative-integer?)
                           acc-element? void?)]
  
  [manifest-array->sexp (-> acc-manifest-array? acc-sexp-data-shallow?)]
  [manifest-array-type  (-> acc-manifest-array? acc-type?)]

 ))


(define acc-manifest-array? manifest-array?)

(struct manifest-array (type shape data))

(define (manifest-array->sexp a)
  (reshape-list
   (manifest-array-shape a)
   (vector->list (manifest-array-data a))
   (manifest-array-size a)))

(define (reshape-list shp ls len)
  (cond [(null? shp)
         (if (= (length ls) 1)
             (car ls)
             (error 'reshape-list "Shape was zero-dim but list: ~a" ls))]
        [(null? (cdr shp)) ls]
        [else
         (define chunksize (quotient len (car shp)))
         (map (lambda (g) (reshape-list (cdr shp) g chunksize))
              (groups chunksize ls))]))

;; Maybe this is in the standard lib somewhere.
(define (groups n ls)
  (cond [(null? ls) ls]
        [else (let-values ([(a b) (split-at ls n)])
                (cons a (groups n b)))]))

;; Retrieve an element of an N-dimensional array using a 1-dimensional
;; index into its "row-major" repesentation.
(define (manifest-array-flatref arr ind)
  (vector-ref (manifest-array-data arr) ind))

;; Multi-dimensional access.
(define (manifest-array-ref arr . inds)
   (let ((offset (ND->1D-index (manifest-array-shape arr) inds)))
     (manifest-array-flatref arr offset)))

(define (manifest-array-set! arr inds val)
   (let ((offset (ND->1D-index (manifest-array-shape arr) inds)))
     (manifest-array-flatset! arr offset val)))

;; Set an element of an N-dimensional array using a 1-dimensional
;; index into its "row-major" repesentation.
(define (manifest-array-flatset! arr ind val)
  (vector-set! (manifest-array-data arr) ind val))

(define (make-empty-manifest-array shape type)
  (define ty2 ;; FIXME
    (match type
      [`(Array ,_ ,elt) elt]
      [else type]))  
  (define (mkzer ty)
    (match ty
      ['Bool  #f]
      ['Int    0]
      ['Double 0.0]
      [v #:when (vector? v)
         (vector-map mkzer v)]
      [else 'make-empty-manifest-array! "unexpected type: ~a" ty]))
  (define len (apply * (vector->list shape)))
  (make-manifest-array
   `(Array ,(vector-length shape) ,ty2)
   shape (make-vector len (mkzer ty))))

(define (list->manifest-array type shape data)
  (define ty2 ;; FIXME
    (match type
      [`(Array ,_ ,elt) elt]
      [else type]))
  (make-manifest-array
   `(Array ,(vector-length shape) ,ty2)
   shape (list->vector (flatten data))))

(define (manifest-array-size arr)
  (apply * (vector->list (manifest-array-shape arr))))

(define (manifest-array-dimension a)
  (vector-length (acc-manifest-array-shape a)))
