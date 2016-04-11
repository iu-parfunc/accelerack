#lang racket

(require
 accelerack/private/types
 (only-in accelerack/private/allocate
          )
 (only-in 2htdp/image image-width image-height color bitmap/url circle)
 (only-in mrlib/image-core render-image)

 ; (only-in racket/draw make-bitmap bitmap-dc%)
 (except-in racket/draw make-pen )
         ; "img-err.rkt"
;         racket/match
;         racket/contract
;         racket/class
;         (except-in racket/draw
;                    make-pen make-color)
;         ;(only-in racket/gui/base frame% canvas% slider% horizontal-panel% button%)
;         htdp/error
;         racket/math
;         (for-syntax racket/base
;                     racket/list)
;         lang/posn
;         net/url
         )

#;
(define (image->list image)
  (define w (image-width image))
  (define h (image-height image))
  (cond
    [(or (= w 0) (= h 0)) '()]
    [else
     (define bm (make-bitmap w h))
     (define bdc (make-object bitmap-dc% bm))
     (define c (make-object color%))
     (define bytes (make-bytes (* w h 4)))
     (send bdc erase)
     (render-image image bdc 0 0)
     (send bdc get-argb-pixels 0 0 w h bytes)
     (for/list ([i (in-range 0 (* w h 4) 4)])
       (color (bytes-ref bytes (+ i 1))
              (bytes-ref bytes (+ i 2))
              (bytes-ref bytes (+ i 3))
              (bytes-ref bytes i))
       )]))

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
     (let ((arr 'finishme))
       (for ([i (in-range 0 (* w h))])
         (set-acc-array-flat! arr i
          (vector (bytes-ref bytes (+ (* i 4) 1))
                  (bytes-ref bytes (+ (* i 4) 2))
                  (bytes-ref bytes (+ (* i 4) 3))
                  (bytes-ref bytes    (* i 4)))))
     )]))

(define (set-acc-array-flat! arr i val)
  (printf " set-acc-array-flat .. array[~a] = ~a \n" i val)
  )

(define (test)
  ;(define x (bitmap/url "http://racket-lang.org/logo-and-text.png"))
  ; (define x (bitmap/url "http://cf.ydcdn.net/1.0.1.50/images/wiktionary/eng-wik-lambda-2.jpg"))
  (define x (circle 5 "solid" "red"))
  ; (define y (image->color-list x))
  (define z (image->acc-array x))
  (list 
   ; (length y)
  ))

; (test)

