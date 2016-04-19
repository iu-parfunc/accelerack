#lang racket

;; Wrappers around Accelerack array routines to make them fall back to
;; regular Racket list operations as appropriate.

(require 
         accelerack/private/racket_ops
         (for-syntax racket/base syntax/parse)
         (prefix-in r: racket/base)
         accelerack/private/types
         (only-in accelerack/acc-array/private make-acc-array)
         accelerack/acc-array
         )

(provide ;; Our two combinators with name collisions:
         map fold         
         
         ;; These have no name conflict, but still need to be wrapped:
         (contract-out
          [zipwith (-> (-> acc-element? acc-element? acc-element?)
                       acc-array? acc-array? acc-array?)]
          [stencil3x3 (-> (-> acc-element? acc-element? acc-element?
                              acc-element? acc-element? acc-element?
                              acc-element? acc-element? acc-element?
                              acc-element?)
                          stencil-boundary? acc-array?
                          acc-array?)])
         generate
         until
         replicate
         
         ;; Reexported:
         acc-array-ref acc-array-flatref
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
  (make-acc-array (acc-zipwith f (force-acc-array! x) (force-acc-array! y))))

(define (stencil3x3 f b x)
  (make-acc-array (acc-stencil3x3 f b (force-acc-array! x))))

(define (generate f . dims)
  (make-acc-array (apply acc-generate f dims)))

(define-syntax (until stx)
  (syntax-parse stx
    [(_ (i:id initv pred) bod)
     #'(acc-until initv
                  (lambda (i) pred)
                  (lambda (i) bod))]))

(define-syntax (replicate stx)
  (syntax-parse stx
    [(_ pat1 pat2 arr)
     #'(make-acc-array
        (acc-replicate (quote pat1) (quote pat2) (force-acc-array! arr)))]))

