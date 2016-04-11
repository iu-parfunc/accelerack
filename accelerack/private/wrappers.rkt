#lang racket

;; Wrappers around Accelerack array routines to make them fall back to
;; regular Racket list operations as appropriate.

(require accelerack/acc-array/private/manifest-array/structs
         accelerack/private/racket_ops
         ;(for-syntax racket/base syntax/parse)
         (prefix-in r: racket/base)
         accelerack/private/types
         )

(provide map
         fold
         zipwith stencil3x3 generate

         ;; Reexported:
         acc-array-ref
         acc-array-flatref
         )

;; The contract for this map is complicated.
;; Either, it works like this:
;;    [map (-> procedure? acc-array? acc-array?)]
;; Or it falls through to the normal map behavior.
(define (map f x)
  (cond
    [(acc-array? x)
     ;; If we're running on the Racket side, we force upstream computations
     ;; in the same way:
     (make-acc-array (acc-map f (force-acc-array! x)))]
    [else (r:map f x)]))

(define (fold f def x)
  (cond
    [(acc-array? x)
     (make-acc-array (acc-fold f def (force-acc-array! x)))]
    ; [else (r:fold f def x)]
    [else (error 'fold "FINISHME: define list version of fold")]
    ))

(define (zipwith f x y)
  (cond
    [(and (acc-array? x) (acc-array? y))
     (make-acc-array (acc-zipwith f (force-acc-array! x) (force-acc-array! y)))]
    [else (error 'fold "FINISHME: zipwith: handle non-manifest case")]
    ))

(define (stencil3x3 f x y)
  (error 'stencil3x3 "FINISHME: stencil3x3 unimplemented"))

(define (generate f . dims)
  (error 'generate "FINISHME: generate unimplemented"))

