#lang racket
(require math)

;make-vector and build-vector, and array versions

(array #[#[ 0 203 4] #[6 0 "sti"]])
(array #[(list 5 6 6) (list 2 3 4) (list 4)])

(define arr
    (build-array #(4 5) (λ (js)
                          (match-define (vector j0 j1) js)
                          (+ j0 j1))))
arr

(define a 3)
(define b 5)
(define arr1 (build-array #(3 5) (λ (sh)
                                   (match-define (vector d1 d2) sh)
                                   (+ 1 (* d1 b) d2))))
arr1

(define arr2 (build-array #(3) (λ (sh)
                                 (match-define (vector d1) sh)
                                 d1)))

; shape parameter to build-array must be integer literals

(define dot (λ (arr1 arr2)
              (array-axis-sum (array* arr1 arr2) 0)))

(struct tuple (x y z))
(define at1 (array #[(tuple 1 2 3) (tuple 3 3 3) (tuple 2 2 2)]))

(define dott (λ (at)
               (array-axis-sum (array-map (λ (t)
                                            (match t
                                              [(tuple x y z) (* x y z)]))
                                          at)
                               0)))
