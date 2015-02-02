#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)

(printf "Loading .so files...\n")

(define lib-example (ffi-lib "./libexample"))
(define-ffi-definer define-example lib-example)

(define lib-rts (ffi-lib "./librts"))
(define-ffi-definer define-rts lib-rts)

(printf "Shared objects loaded without error!\n")

(define-rts init (_fun -> _int))
(printf "Aquired init function\n")

(init)
(printf "Initialized RTS\n")

(define-example entrypoint (_fun _int -> _int))
(printf "Aquired entrypoint function\n")

(printf "Now calling entrypoint:\n")
(entrypoint 33)

(printf "Exiting racket safely\n")

