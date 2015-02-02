#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)

(printf "Loading .so file...\n")

(define rts-lib (ffi-lib "./libinit"))
(define-ffi-definer define-rts rts-lib)

(printf "Shared object loaded without error!\n")

(define-rts entrypoint (_fun _int -> _int))
(printf "Aquired entrypoint function\n")

(define-rts init (_fun -> _int))
(printf "Aquired init function\n")

(init)

(printf "Now calling entrypoint:\n")
(entrypoint 33)

(printf "Exiting racket safely\n")

