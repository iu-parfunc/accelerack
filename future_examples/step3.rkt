#lang racket

(require accelerack)
(require rackunit)

;; Next, let's stress the gradual-acceleration capability.
;; The weakest case of removing "acc" annotations is to
;; remove them incrementally from the TAIL of the computaition.
;; Here we instead remove them from the MIDDLE.

(define-acc vec (array (1 2 3 4)))

;; sqr is a regular function and an Accelerack function:
;; Everything defined with define-acc-fun satisfies procedure?
(define-acc-fun (sqr x) (* x x))

;; The contract for now is that everything from define-acc satisfies array?:
(define-acc vec2 (map sqr vec))
;; Everything from run-acc is also satisfies "array?"
  (define vec2b (run-acc (map sqr vec))) ;; Also ok.
  (define vec2c (map sqr vec)) ;; Also ok.

;; Should the following:
;;  (1) be an error?
;;  (2) go to the GPU?
;;  (3) compute Racket-side? because only (run-acc e) runs on GPU
;;    3A - eagerly?
;;    3B - lazily?
(array->list vec2)
;; Ryan's argument for (3) is that we only run on the GPU when we
;; say (run-acc), Sam wants to avoid lazy-semantics for the Racket
;; side computations if at all possible.  (Sam's initial intuition
;; was that this would run on the GPU because it was defined with 
;; define-acc.)

;; Here we can run arbitrary Racket computations:
(define vec3 (map non-acc-function vec2))
;; Vec3 is bound in the Racket namespace, but not in the
;; Acc syntax table.  Whenever vec3 is forced, it will throw
;; errors if non-acc-function returns any values not accepted by
;; Accelerate (or, if it returns heterogeneously typed values).

;; Allowing the following would mean allowing "implicit" imports of Racket
;; data without any kind of unquote or "use" form:
;;  (define-acc vec4 (map add1 vec3))
;; That may be good for a strong gradual guarantee, but for students
;; explicit use is better:
(define-acc vec4 (map add1 (use vec3)))
;; Flexible handling of un-accelerack-bound identifiers is important.
;; For beginning students, it should be an error.
;; In any case, the operational effect of the above
;; would be to force vec3 as a Racket-side computation (if deferred)
;; or to marshal the data across (if manifest).

;; An attempt to run on the GPU will only be able to run the add1:
(array->list (run-acc vec4))

 
