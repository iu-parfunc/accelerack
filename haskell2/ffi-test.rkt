#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)

(printf "Loading .so file...\n")

(define the-lib (ffi-lib "./libexample.so"))
(printf "Shared object loaded without error!\n")

(define-ffi-definer define-mylib the-lib)


; Pull hs_init from here:
(define rts-lib (ffi-lib "./libHSrts-ghc7.8.3.so"))
(define-ffi-definer define-rts rts-lib)

(define-mylib entrypoint (_fun _int -> _int))
(printf "Aquired entrypoint function\n")

(define-rts init (_fun -> _int))
(printf "Aquired init function\n")

(printf "Now calling entrypoint:\n")
(entrypoint 33)

(printf "Exiting racket safely\n")

