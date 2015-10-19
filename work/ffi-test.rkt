#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)

(require "allocate.rkt")
; One time, I had to restart DrRacket in order for it to reload new copies of .so
(printf "Loading .so files...\n")

(define lib-capi (ffi-lib "./libcapi"))
(define-ffi-definer define-capi lib-capi)

(define lib-rts (ffi-lib "../rts/librts"))
(define-ffi-definer define-rts lib-rts)

(printf "Shared objects loaded without error!\n")

(define-cstruct _c-array
  ([length _int]
   [type _string]
   [dimension _gcpointer]
   [data _gcpointer]))

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
(gpu "show (I.run $ (A.use (A.fromList (A.Z A.:.3 A.:.5) [1..] :: A.Array A.DIM2 Int)))")

(acc_alloc _double '(2 3) '((8.0 6.0 4.0) (3.3 5.5 7.7)) "use")

(define-capi getTypeHs (_fun _c-array-pointer (_fun _c-array-pointer -> _string) -> _string))
(printf "Aquired getType function\n")

(getTypeHs cptr getType)

(define-capi getDimensionHs (_fun _c-array-pointer (_fun _c-array-pointer -> _gcpointer) -> _gcpointer))
(printf "Aquired getDimension function\n")

(ptr-ref* (getDimensionHs cptr getDimension) _int 0 (c-array-length cptr))

(define-capi getDataHs (_fun _c-array-pointer (_fun _c-array-pointer -> _gcpointer) -> _gcpointer))
(printf "Aquired getData function\n")

(ptr-ref* (getDataHs cptr getData) (string->type (c-array-type cptr)) 0 6)

(printf "Exiting racket safely\n")

