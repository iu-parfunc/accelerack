#lang racket

(require accelerack)
(require rackunit)

;; Define-acc puts the computation into both namespaces:
(define-acc vec (array (1 2 3 4)))

;; sqr is a regular function and an Accelerack function:
(define-acc (sqr x) (* x x))

;; The following returns #t:
(procedure? sqr)

(define-acc vec2 (map sqr vec))

;; This should still be true, whether or not the computation
;; has happened eagerly or lazily:
(check-true (array? vec2))

;; This runs the computation on the GPU, and returns a Racket-side array:
(check-true (array? (acc vec2)))

;; Whether or not it was forced before, this forces the computation on the racket side.
;; With lazy semantics we would see any exceptions here:
(array->list vec2)
;; One reason to use lazy semantics is so we don't compute things on the Racket side
;; that should really only be done on the GPU side.

;; Accelerate *does* have array level conditionals, so the following could be valid: 
(define-acc vec3 (if (= (! vec2 1) 3) vec vec2))

;; The fact that (array? vec3), as opposed to (procedure? vec3) is due
;; to type inference in Accelerack.

;; But what about this?:
; (define-acc fn (if (= (! vec2 1) 3) sqr sqr))
;; (1) That could be a type error in the front-end.  The typing rule for conditionals
;;     should rule out arrow types in its branches.
;; (2) It could also *work*, if we have commuting conversions in our normalization pass.

