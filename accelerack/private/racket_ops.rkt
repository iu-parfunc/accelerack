#lang racket

;; This file provides the Racket-side implementation of core Accelerate operators.
;; They operate exclusively over MANIFEST data.



(require accelerack/acc-array
         (only-in accelerack/acc-array/private make-acc-array)
         (only-in accelerack/private/types acc-element?
                  acc-element->type stencil-boundary? acc-shape?)
         racket/trace)

;; TODO: REMOVE ANY DEPENDENCE ON NON-PUBLIC ARRAY INTERFACES:
(require (except-in ffi/unsafe ->)
         accelerack/acc-array/private/manifest-array
         accelerack/private/utils
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
  [acc-zipwith  (-> procedure? acc-manifest-array? acc-manifest-array?
                    acc-manifest-array?)]
  [acc-stencil3x3 (-> procedure? stencil-boundary? acc-manifest-array?
                      acc-manifest-array?)]
  [acc-stencil5x3 (-> procedure? stencil-boundary? acc-manifest-array?
		      acc-manifest-array?)]
  [acc-stencil3x5 (-> procedure? stencil-boundary? acc-manifest-array?
                      acc-manifest-array?)]
  [acc-stencil5x5 (-> procedure? stencil-boundary? acc-manifest-array?
                      acc-manifest-array?)]

  [acc-generate (->* (procedure?) () #:rest (listof exact-nonnegative-integer?)  acc-manifest-array?)]
  )
 )

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


(define (acc-zipwith fn a1 a2)
  ;; The acc-manifest-array is not mutable for end users, but for this library implementation
  ;; we leverage a mutable representation internally.
  (let* (; [ty    (manifest-array-type a1)]  ;; This errors [2016.04.13]
         [shp1  (manifest-array-shape a1)]
         [shp2  (manifest-array-shape a2)]
         [shp   (intersect-shape shp1 shp2)]
         [len   (apply * (vector->list shp))])
    (if (= len 0)
        ;; FIXME: the type is bogus here.  No good support for polymorphic constants atm:
        (make-acc-array (list->manifest-array #() #() '()))

        ;; The "upper left" point is always in the intersection:
        (let* ([elm0 (fn (manifest-array-flatref a1 0)
                         (manifest-array-flatref a2 0))]
               [tyout (acc-element->type elm0)]
               [new (make-empty-manifest-array shp tyout)])
          (let loop ([inds '()]
                     [shpls (vector->list shp)])
            (if (null? shpls)
                (let ((ix (reverse inds)))
                  (manifest-array-set! new ix
                                       (fn (apply manifest-array-ref a1 ix)
                                           (apply manifest-array-ref a2 ix))))
                (for ((i (range (car shpls))))
                  (loop (cons i inds) (cdr shpls)))
                ))
          new))))

(define (acc-generate fn . shp)
  (let* ([len   (apply * shp)])
    (if (= len 0)
        ;; FIXME: the type is bogus here.  No good support for polymorphic constants atm:
        (make-acc-array (list->manifest-array #() #() '()))

        ;; The "upper left" point:
        (let* ([elm0 (apply fn (map (lambda (_) 0) shp))]
               [tyout (acc-element->type elm0)]
               [new (make-empty-manifest-array (list->vector shp) tyout)])
          (let loop ([inds '()]
                     [shpls shp])
            (if (null? shpls)
                (let ((ix (reverse inds)))
                  (manifest-array-set! new ix (apply fn ix)))
                (for ((i (range (car shpls))))
                  (loop (cons i inds) (cdr shpls)))))
          new))))

;; Stencils:
;; --------------------------------------------------------------------------------

(define (acc-stencil3x3 fn b arr)
  (acc-stencil2d fn b arr 3 3))

(define (acc-stencil3x5 fn b arr)
  (acc-stencil2d fn b arr 3 5))

(define (acc-stencil5x3 fn b arr)
  (acc-stencil2d fn b arr 5 3))

(define (acc-stencil5x5 fn b arr)
  (acc-stencil2d fn b arr 5 5))

(define (acc-stencil3 fn b arr)
  (acc-stencil1d fn b arr 3))

(define (acc-stencil5 fn b arr)
  (acc-stencil1d fn b arr 5))

(define (acc-stencil1d fn b arr)
  (error 'acc-stencil1d "Not implemented yet"))

(define (acc-stencil2d fn b arr xd yd)
  (let* ([len  (manifest-array-size arr)]
         [ty   (manifest-array-type arr)]
         [shp  (manifest-array-shape arr)]
	 [nty  (acc-element->type (apply fn (stencil-range2d b 0 0 3 3 arr)))]
         [new  (make-empty-manifest-array shp nty)])
  (for ((i (range (vector-ref shp 0))))
    (for ((j (range (vector-ref shp 1))))
      (manifest-array-flatset!
       new (+ (* (vector-ref shp 0) i) j)
       (apply fn (stencil-range2d b i j xd yd arr)))))
      new))

(define (stencil-range2d b x y xd yd arr)
  (let ([x-base (- x (floor (/ xd 2)))]
        [y-base (- y (floor (/ yd 2)))])
    (flatten
     (for/list ([i (in-range xd)])
       (for/list ([j (in-range yd)])
	 (let ([ind (+ (* (vector-ref (manifest-array-shape arr) 0) (+ x-base i))
		       (+ y-base j))])
	   (if (or (< ind 0) (>= ind (manifest-array-size arr)))
	       (match b
		 [`(Constant ,v) v]
		 ;; handle other boundary conditions here
		 [else (error 'stencil-range2d "Invalid boundary condition")])
	       (manifest-array-flatref arr ind))))))))
