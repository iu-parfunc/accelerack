#lang racket

(require
 accelerack/private/types
 (only-in accelerack/acc-array
          acc-array->sexp acc-array-size acc-array-shape acc-array-flatref)
 accelerack/acc-array/private
 accelerack/acc-array/private/manifest-array
 2htdp/image
 (only-in mrlib/image-core render-image bitmap->image)
 rackunit
 (except-in racket/draw make-pen make-color)
 racket/trace
 )

(provide
 (contract-out
  [image->acc-array (-> image? acc-array?)]
  [acc-array->image (-> acc-array? image?)]
  [color->acc-element (-> color? acc-element?)]
  [acc-element->color (-> acc-element? color?)]
  ))

;; Uses RGBA ordering for the fields.
(define (color->acc-element c)
  (vector (color-red c)
          (color-green c)
          (color-blue c)
          (color-alpha c)))

(define (acc-element->color v)
  (make-color (vector-ref v 0)
              (vector-ref v 1)
              (vector-ref v 2)
              (vector-ref v 3)))

;; Based on image->color-list from 2htdp/private/image-more.rkt
(define (image->acc-array image)
  (define w (image-width image))
  (define h (image-height image))
  (cond
    [(or (= w 0) (= h 0)) '()]
    [else
     (define bm (make-bitmap w h))
     (define bdc (make-object bitmap-dc% bm))
     (define c (make-object color%))
     ;; We unfortunately make one copy just to be able to read it:
     (define bytes (make-bytes (* w h 4)))
     (send bdc erase)
     (render-image image bdc 0 0)
     (send bdc get-argb-pixels 0 0 w h bytes)
     (let ((arr (make-empty-manifest-array (vector w h) '#(Int Int Int Int))))
       ;; TODO: use 2D indexing here.. We may actually want to transpose it..
       (for ([i (in-range 0 (* w h))])
         ;; Bytes uses ARGB order:
         (manifest-array-flatset! arr i
          (vector (bytes-ref bytes (+ (* i 4) 1))
                  (bytes-ref bytes (+ (* i 4) 2))
                  (bytes-ref bytes (+ (* i 4) 3))
                  (bytes-ref bytes    (* i 4)  ))))
       (make-acc-array arr))]))

(define (acc-array->image arr)
  (define len    (acc-array-size arr))
  (define shp    (acc-array-shape arr)) 
  (define width  (vector-ref shp 0)) ;; It's debatable which way we should do it.
  (define height (vector-ref shp 1))
  (cond
    [(or (zero? width) (zero? height))
     (rectangle width height "solid" "black")]
    [else
     (define bmp (make-bitmap width height))
     (define bytes (make-bytes (* width height 4) 0))
     (define o (make-object color%))
     (for ([i (range len)])
       (define v (acc-array-flatref arr i))
       (define j (* i 4))
       (bytes-set! bytes  j      (vector-ref v 3))
       (bytes-set! bytes (+ j 1) (vector-ref v 0))
       (bytes-set! bytes (+ j 2) (vector-ref v 1))
       (bytes-set! bytes (+ j 3) (vector-ref v 2)))
     (send bmp set-argb-pixels 0 0 width height bytes)
     (bitmap->image bmp)]))


