#lang racket

(require
 accelerack/private/types
 (only-in accelerack/acc-array acc-array->sexp)
 accelerack/acc-array/private
 accelerack/acc-array/private/manifest-array
 (only-in 2htdp/image image-width image-height color bitmap/url circle image?)
 (only-in mrlib/image-core render-image)
 rackunit
 (except-in racket/draw make-pen )
 )

(provide
 (contract-out
  [image->acc-array (-> image? acc-array?)]))

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
     (let ((arr (make-empty-manifest-array (list w h) '#(Int Int Int Int))))
       (for ([i (in-range 0 (* w h))])
         (manifest-array-flatset! arr i
          (vector (bytes-ref bytes (+ (* i 4) 1))
                  (bytes-ref bytes (+ (* i 4) 2))
                  (bytes-ref bytes (+ (* i 4) 3))
                  (bytes-ref bytes    (* i 4)))))
       (make-acc-array arr))]))

#;
(test-case "Convert image->array"
  ;(define x (bitmap/url "http://racket-lang.org/logo-and-text.png"))
  ; (define x (bitmap/url "http://cf.ydcdn.net/1.0.1.50/images/wiktionary/eng-wik-lambda-2.jpg"))
  (define x (circle 5 "solid" "red"))
  ; (define y (image->color-list x))
  (define z (image->acc-array x))
  (acc-array->sexp z)
  )

; (test)
