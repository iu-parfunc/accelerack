#lang racket

;; This file provides the Racket-side implementation of core Accelerate operators.
;; They operate exclusively over MANIFEST data.



(require accelerack/acc-array
         (only-in accelerack/private/types acc-element?
                  acc-element->type stencil-boundary?))

;; TODO: REMOVE ANY DEPENDENCE ON NON-PUBLIC ARRAY INTERFACES:
(require (except-in ffi/unsafe ->)
         accelerack/acc-array/private/manifest-array/structs
         accelerack/acc-array/private/manifest-array
         accelerack/acc-array/private/arrayutils
         (only-in accelerack/private/utils vector->list*)
         (only-in '#%foreign ctype-scheme->c ctype-c->scheme)
         racket/contract
         )

(provide
 (contract-out
  ;; POLICY: We should have no contracts or only O(1) contracts from this file.
  ;; We don't want to layer bigger and bigger contracts around per-element functions.
  ;; We may do more aggressive enforcement at the "wrappers.rkt" layer.
  ;; --------------------------------------------------------------------------------
  [acc-map  (-> procedure? acc-manifest-array? acc-manifest-array?)]
  [acc-fold (-> procedure? ; (-> acc-element? acc-element? acc-element?)
                acc-element? acc-manifest-array?
                acc-manifest-array?)]
  [acc-zipwith
   (-> procedure? acc-manifest-array? acc-manifest-array? acc-manifest-array?)]
  [acc-stencil3x3 (-> procedure? stencil-boundary? acc-manifest-array?
                      acc-manifest-array?)]

  ;; TODO: acc-stencil3x3

  [array-get (-> acc-manifest-array? exact-integer? any/c)] ;; Remove me.
 ))

;; Utilities: these look broken/wrong -RRN [2016.04.11]
;; --------------------------------------------------------------------------------

;; Set the value at given position in an acc array
;; Arguments -> reference to the acc array, offset, the value to set
;; Return value -> void

(define (array-set!! arr-ref offset value)
  (let ([type (mapType ((ctype-scheme->c scalar) (get-ctype value)))]
        [data (if (acc-manifest-array? arr-ref)
                  (segment-data (acc-manifest-array-data arr-ref))
                  (segment-data arr-ref))])
    (ptr-set! data type offset value)))

;; Get the value at given position in an acc array
;; Arguments -> reference to the acc array, offset
;; Return value -> value at given position

;; FIXME : this doesn't work for tuples!!!
(define (array-get arr-ref offset)
  (let ([type (mapType (type arr-ref))]
        [data (if (acc-manifest-array? arr-ref)
                  (segment-data (acc-manifest-array-data arr-ref))
                  (segment-data arr-ref))])
    (ptr-ref data type offset)))

;; Execute the given function over the given acc tuple data
;; Arguments -> reference to result acc array ,reference to the input acc array, input function
;; Return value -> void

(define (tuple-array-set!! arr-ref input-arr fn)
  (let ([acc-tuple ((ctype-scheme->c scalar) 'acc-payload-ptr)]
        [acc-int ((ctype-scheme->c scalar) 'c-int)]
        [acc-double ((ctype-scheme->c scalar) 'c-double)])
       (cond
         [(equal? (type input-arr) acc-tuple)
          (let ([len (acc-length input-arr)])
            (for ([i (in-range 0 len)])
              (tuple-array-set!!
               (ptr-ref (segment-data arr-ref) _segment-pointer i)
               (ptr-ref (segment-data input-arr) _segment-pointer i)
               fn)))]
         [(or (equal? (type input-arr) acc-int)
              (equal? (type input-arr) acc-double))
          (let ([len (acc-length input-arr)])
            (for ([i (in-range 0 len)])
              (array-set!! arr-ref i (fn (array-get input-arr i)))))])))

;; REMOVE THIS:
;; returns the length of the given acc array
(define (acc-length arr)
  (if (acc-manifest-array? arr)
      (segment-length (acc-manifest-array-data arr))
      (segment-length arr)))

;; TEMP: REMOVE THIS:
(define (shape x) (vector->list (manifest-array-shape x)))


;; Map
;; --------------------------------------------------------------------------------

;; Map a function over every element, irrespective of dimension.
(define (acc-map fn arr)
  ;; The acc-manifest-array is not mutable for end users, but for this library implementation
  ;; we leverage a mutable representation internally.
  (let* ([len  (manifest-array-size arr)]
         [ty   (manifest-array-type arr)]
         [shp  (manifest-array-shape arr)])
    (if (= len 0)
        arr
        (let* ([elm0 (fn (manifest-array-flatref arr 0))]
               [tyout (acc-element->type elm0)]
               [new (make-empty-manifest-array shp tyout)])
          (manifest-array-flatset! new 0 elm0)
          (for ((i (range 1 len)))
            (manifest-array-flatset! new i
                                     (fn (manifest-array-flatref arr i))))
          new))))

;; Fold:
;; --------------------------------------------------------------------------------

(define (acc-fold fn init arr)
  (let* ([len      (manifest-array-size arr)]
         [ty       (manifest-array-type arr)]
         [shp      (manifest-array-shape arr)]
         [new-shp  (reverse (cdr (reverse (vector->list shp))))]
         [new      (make-empty-manifest-array (list->vector new-shp) ty)]
         [stride   (last (vector->list shp))])
    (for ((i (range (/ len stride))))
      (manifest-array-flatset! new i init)
      (for ((j (range stride)))
        (manifest-array-flatset! new i (fn (manifest-array-flatref arr (+ (* i stride) j))
                                           (manifest-array-flatref new i)))))
    new))

;; ZipWith:
;; --------------------------------------------------------------------------------

#;
(define (zipwith fn a1 a2)
  ;; The acc-manifest-array is not mutable for end users, but for this library implementation
  ;; we leverage a mutable representation internally.
  (let* ([len  (manifest-array-size a1)]
         [ty   (manifest-array-type a1)]
         [shp1  (manifest-array-shape a1)]
         [shp2  (manifest-array-shape a2)]
         [shp   (intersect-shape a1 a2)])
    (if (= len 0)
        arr
        (let* ([elm0 (fn (manifest-array-flatref a1 0)
                         (manifest-array-flatref a2 0))]
               [tyout (acc-element->type elm0)]
               [new (make-empty-manifest-array shp tyout)])
          (manifest-array-flatset! new 0 elm0)
          (for ((i (range 1 len)))
            (manifest-array-flatset! new i
                                     (fn (manifest-array-flatref arr i))))
          new))))


(define (acc-zipwith fn arr1 arr2)
  (letrec ([type* (if (equal? ((ctype-scheme->c scalar) 'acc-payload-ptr) (type arr1)) (get-tuple-type (unzip (vector->list* (manifest-array->sexp arr1))) (shape arr1)) (mapType (type arr1)))]
           [shape* (if (equal? (shape arr1) (shape arr2)) (shape arr1) (error 'acc-zipwith "shape of array 1 and array 2 not equal"))]
           [temp* (make-empty-manifest-array-lame shape* type*)]
           [len (manifest-array-size temp*)])
          (begin
            (for ([i (in-range 0 len)])
              (array-set!! temp* i (fn (array-get arr1 i) (array-get arr2 i))))
            temp*)))

;; Accelerate zipwith (development) function
;; Arguments -> binary function, reference to array 1,reference to array 2
;; Return value -> result array

(define (acc-zipwith-dev fn arr1 arr2)
  (letrec ([type* (if (equal? ((ctype-scheme->c scalar) 'acc-payload-ptr) (type arr1))
                      (get-tuple-type (unzip (vector->list* (manifest-array->sexp arr1))) (shape arr1))
                      (mapType (type arr1)))]
           [shape* (find-shape (shape arr1) (shape arr2) '())]
           [temp* (make-empty-manifest-array-lame shape* type*)]
           [len (manifest-array-size temp*)]
           [new-arr1 (list->manifest-array type* (list->vector shape*)
                                           (reshape shape* (manifest-array->sexp arr1)))]
           [new-arr2 (list->manifest-array type* (list->vector shape*)
                                           (reshape shape* (manifest-array->sexp arr2)))])
          (begin
            (for ([i (in-range 0 len)])
              (array-set!! temp* i (fn (array-get new-arr1 i) (array-get new-arr2 i))))
            temp*)))

(define (reshape shp ls)
  (cond
    ((null? shp) ls)
    ((equal? (length ls) (car shp)) (map (lambda (x) (reshape (cdr shp) x)) ls))
    (else (map (lambda (x) (reshape (cdr shp) x)) (take ls (car shp))))))


;; Stencils:
;; --------------------------------------------------------------------------------

;; TODO

(define (acc-stencil3x3 fn b arr)
  (let* ([len  (manifest-array-size arr)]
         [ty   (manifest-array-type arr)]
         [shp  (manifest-array-shape arr)])
         ;[new (make-empty-manifest-array (vector->list shp) ty)])
    (let* ([elm0 (apply fn (map (lambda (i) (manifest-array-flatref arr i)) (range 9)))]
           [tyout (acc-element->type elm0)]
           [new (make-empty-manifest-array shp tyout)])
      (for ((i (range (vector-ref shp 0))))
        (for ((j (range (vector-ref shp 1))))
          (manifest-array-flatset! 
           new (+ (* (vector-ref shp 0) i) j)
           (apply fn (stencil-range2d b i j 3 3 arr)))))
      new)))

(define (stencil-range2d b x y xd yd arr)
  (let ([x-base (- x (floor (/ x 2)))]
        [y-base (- y (floor (/ y 2)))])                                          
    (for*/list ([i (in-range xd)] [j (in-range yd)])
      (let ([ind (+ (* (vector-ref (manifest-array-shape arr) 0) (+ x-base i))
                                         (+ y-base j))])
      (if (or (< ind 0) (>= ind (manifest-array-size arr)))
          (match b 
            [`(Constant ,v) v]
            ;; handle other boundary conditions here
            [else (error 'stencil-range2d "Invalid boundary condition")])
          (manifest-array-flatref arr ind))))))
