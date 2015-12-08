#lang racket

;; Wrappers around Accelerack array routines to make them fall back to
;; regular Racket list operations as appropriate.

(require ffi/unsafe
         ffi/unsafe/define
	 racket/runtime-path

         accelerack/private/acc_parse
         accelerack/private/acc_allocate
         accelerack/private/acc_arrayutils
         accelerack/private/acc_global_utils

         (except-in accelerack/private/acc_header acc-array? make-acc-array)

         (prefix-in cdata: (only-in accelerack/private/acc_header acc-array? make-acc-array))
         (only-in '#%foreign ctype-scheme->c ctype-c->scheme)
         (for-syntax racket/base syntax/parse)
         (prefix-in r: racket/base)

         (prefix-in rkt: accelerack/private/acc_racket_ops)

         accelerack/private/types
         )

(provide map fold zipwith
         )

(define (map f x)
  (cond
    [(and (acc-array? x) (cdata:acc-array? (acc-array-val x)))
     (rkt:acc-map f (acc-array-val x))]
    [(acc-array? x) (error 'map "deferred array not handled yet!!")]
    [else (r:map f x)]))

(define (fold f def x)
  (cond
    [(and (acc-array? x) (cdata:acc-array? (acc-array-val x)))
     (rkt:acc-fold f def (acc-array-val x))]
    [(acc-array? x) (error 'fold "deferred array not handled yet!!")]
    ; [else (r:fold f def x)]
    [else (error 'fold "FINISHME: define list version of fold")]
    ))

(define (zipwith f x y)
  (cond
    [(and (acc-array? x) (cdata:acc-array? (acc-array-val x))
          (acc-array? y) (cdata:acc-array? (acc-array-val y)))
     (rkt:acc-zipwith f (acc-array-val x) (acc-array-val y))]
    [else (error 'fold "FINISHME: zipwith: handle non-manifest case")]
    ))
