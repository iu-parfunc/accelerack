#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)

(printf "Loading .so file...\n")

; (define the-lib (ffi-lib "./wrapper2"))
(define the-lib (ffi-lib "./libexample.so"))

; Pull hs_init from here:
; (define the-lib (ffi-lib "./libHSrts.so"))

(printf "Shared object loaded without error!\n")

(define-ffi-definer define-mylib the-lib)
(define-mylib entrypoint (_fun _int -> _int))

(printf "Aquired entrypoint function, now calling:\n")
(entrypoint 33)

(printf "Exiting racket safely\n")

