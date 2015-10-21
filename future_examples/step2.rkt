#lang racket

(require accelerack)
(require rackunit)

;; Define-acc puts the computation into both namespaces:
(define-acc vec (array (1 2 3 4)))

;; sqr is a regular function and an Accelerack function:
(define-acc (sqr x) (* x x))

;; The following returns #t:
(procedure? sqr)

(define-acc vec2 (map add1 vec))

;; This should still be true, whether or not the computation
;; has happened eagerly or lazily:
(array? vec2)

;; This runs the computation on the GPU, and returns a Racket-side array:
(array? (acc vec2))

;; Whether or not it was forced before, this forces the computation on the racket side.
;; With lazy semantics we would see any exceptions here:
(array->list vec2)
;; One reason to use lazy semantics is so we don't compute things on the Racket side
;; that should really only be done on the GPU side.
