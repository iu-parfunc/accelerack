#lang racket

(require accelerack)
(require rackunit)

;; Now let' stress the gradual-acceleration capability.
;; The weakest case of removing "acc" annotations is to
;; remove them incrementally from the TAIL of the computaition.
;; Here we instead remove them from the MIDDLE.

(define-acc vec (array (1 2 3 4)))

;; sqr is a regular function and an Accelerack function:
(define-acc (sqr x) (* x x))

(define-acc vec2 (map sqr vec))

;; Here we can run arbitrary Racket computations:
(define vec3 (map non-acc-function vec2))
;; Vec3 is bound in the Racket namespace, but not in the
;; Acc syntax table.  Whenever vec3 is forced, it will throw
;; errors if non-acc-function returns any values not accepted by
;; Accelerate (or, if it returns heterogeneously typed values).
  
  
;; Allowing the following would mean allowing "implicit" imports of Racket
;; data without any kind of unquote or "use" form:
(define-acc vec4 (map add1 vec3))
;; Maybe this is a warning?
;; The operational effect of this would be to force vec3 as a Racket-side computation.

(array->list vec4)

 