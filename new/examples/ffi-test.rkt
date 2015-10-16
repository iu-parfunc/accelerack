#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)
(require "verify-accelerack.rkt")

(define-syntax use
  (syntax-rules ()
    [(use exp) `(use ,exp)]))

(define-syntax acc
  (syntax-rules ()
    [(acc (use exp)) (if (verify-accelerack `(use ,exp)) #t 'false)]))

; One time, I had to restart DrRacket in order for it to reload new copies of .so
(printf "Loading .so files...\n")

(define lib-capi (ffi-lib "./libcapi"))
(define-ffi-definer define-capi lib-capi)

(define lib-rts (ffi-lib "../rts/librts"))
(define-ffi-definer define-rts lib-rts)

(printf "Shared objects loaded without error!\n")

(define-rts ark_init (_fun -> _int))
(define-rts ark_exit (_fun -> _int))
(printf "Aquired init function\n")

(ark_init)
(printf "Initialized RTS\n")

(define-capi entrypoint (_fun _int -> _int))
(printf "Aquired entrypoint function\n")

(printf "Now calling entrypoint:\n")
(entrypoint 33)

(define-capi gpu (_fun _string -> _string))
(printf "Aquired gpu function\n")

(printf "Now calling GPU_FUNCTION:\n")
;(gpu "Racket")
(gpu "(use (vector _double (1) 2.9))")

(printf "Exiting racket safely\n")

