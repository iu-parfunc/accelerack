#lang racket

;; Wrappers around Accelerack array routines to make them fall back to
;; regular Racket list operations as appropriate.

(require accelerack/private/header
         accelerack/private/racket_ops
         ;(for-syntax racket/base syntax/parse)
         (prefix-in r: racket/base)
         accelerack/private/types
         )

(provide map
         (contract-out [fold (-> procedure? acc-scalar? acc-array? acc-array?)])
         zipwith stencil3x3 generate
         acc-array-ref)

;; The contract for this map is complicated.
;; Either, it works like this:
;;    [map (-> procedure? acc-array? acc-array?)]
;; Or it falls through to the normal map behavior.
(define (map f x)
  (cond
    [(and (acc-array? x) (acc-manifest-array? (acc-array-val x)))
     (acc-map f (acc-array-val x))]
    [(acc-array? x) (error 'map "deferred array not handled yet!!")]
    [else (r:map f x)]))

(define (fold f def x)
  (cond
    [(and (acc-array? x) (acc-manifest-array? (acc-array-val x)))
     (acc-fold f def (acc-array-val x))]
    [(acc-array? x) (error 'fold "deferred array not handled yet!!")]
    ; [else (r:fold f def x)]
    [else (error 'fold "FINISHME: define list version of fold")]
    ))

(define (zipwith f x y)
  (cond
    [(and (acc-array? x) (acc-manifest-array? (acc-array-val x))
          (acc-array? y) (acc-manifest-array? (acc-array-val y)))
     (acc-zipwith f (acc-array-val x) (acc-array-val y))]
    [else (error 'fold "FINISHME: zipwith: handle non-manifest case")]
    ))

(define (stencil3x3 f x y)
  (error 'stencil3x3 "FINISHME: stencil3x3 unimplemented"))

(define (generate f . dims)
  (error 'generate "FINISHME: generate unimplemented"))

(define (acc-array-ref arr . inds)
  (error 'acc-array-ref "FINISHME: acc-array-ref unimplemented"))
