;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mandelbrot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "../lib/accelerack.rkt")
(require "../lib/types.rkt")

(require 2htdp/image)

; Size in pixels of each cell
(define D 2)

;; Number of cells
(define W 30)
(define H 30)

;; Max Iterations
(define I 12)

;; Boundaries of computation
(define X_MAX 1.0)
(define X_MIN -2.0)
(define Y_MAX 1.0)
(define Y_MIN -1.0)

;; interp : Number Number Number Number Number -> Number
;; Interpolates b in b-range from a in a-range
(define (interp a amin amax bmin bmax)
  (+ bmin (* (/ (- a amin) (- amax amin))
             (- bmax bmin))))

;; gen-coords : Nat Nat -> [Nat Nat -> SElement]
;; Produces a function for generating 3-tuples to initialize an array
;; for Mandelbrot scaled for an image w X h pixels
(define (gen-coords w h)
  (λ (i j)
    (let ([x0 (interp i 0 (sub1 w) X_MIN X_MAX)]
          [y0 (interp j 0 (sub1 h) Y_MIN Y_MAX)])
      (list x0 y0
            x0 y0
            0))))

(define (init-coords w h)
  (generate (list w h) (gen-coords w h)))

(define init-array (init-coords W H))

;; update : SElement -> SElement
;; one step of the Mandelbrot computation
(define (update x0 y0 x y i)
  (if (>= (+ (* x x) (* y y)) 4.0)
      (list x0 y0 x y i)
      (let ([x2 (+ (- (* x x) (* y y)) x0)]
            [y2 (+ (* 2.0 x y) y0)])
        (list x0 y0 x2 y2 (add1 i)))))

;; Iterating map
;; applies update n times to array
(define (amap/n update array n)
  (cond
    [(zero? n) array]
    [else (amap/n update (amap update array) (sub1 n))]))

;; Computation
(define result (amap/n update init-array I))

;; Throw out coordinates, keeping only iteration counts
(define (get-iters array)
  (amap (λ (x0 y0 x y i) i) array 1))

;; Remove one layer
(define (array->bmp array)
  (let ([array-row->bmp-row (λ (row) (map first row))])
    (map array-row->bmp-row array)))

(define bmp (array->bmp (get-iters result)))

;; a Palette is a [Nat -> Color]
(define colors '(black purple red orange yellow green turquoise blue))
(define N (length colors))
(define palette (λ (n)
                  (list-ref colors (floor (interp n I 0 0 (sub1 N))))))
                                 
;; Rendering routine
(define (draw bmp palette)
  (let ([draw-row (λ (row) (foldr (λ (n bg) (beside (square D 'solid (palette n)) bg))
                                  empty-image
                                  row))])
    (foldr (λ (row bg) (above (draw-row row) bg)) empty-image bmp)))

;; Final image
(draw bmp palette)
