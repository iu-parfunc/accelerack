#lang racket/base

;; Test C FFI and load code from ../acc_c

(require ffi/unsafe
         ffi/unsafe/define
         rackunit
         rackunit/text-ui
         racket/runtime-path
         (only-in '#%foreign ctype-scheme->c ctype-c->scheme))

(require (except-in accelerack ->))

(require accelerack/acc-array/private/manifest-array/structs ;; C data reps.
         ; (only-in accelerack/private/syntax   array)
         (only-in accelerack/acc-array/private/manifest-array/allocate
                  read-data*)
         accelerack/private/racket_ops
         ; (only-in accelerack/private/paven_old/global_utils add sub mult)
         )


(printf "<-------------- Setting up the Haskell environment --------------->")
(define-runtime-path librachs "../../../build/librachs.so")
(define lib-hs (ffi-lib librachs))
(define-ffi-definer define-hs lib-hs)

;;(define lib-ifc (ffi-lib "../../../acc_hs/libhsifc"))
;;(define-ffi-definer define-ifc lib-ifc)

(define-runtime-path librts "../../../acc_c/librts.so")
(define lib-rts (ffi-lib librts))
(define-ffi-definer define-rts lib-rts)


(define-rts ark_init (_fun -> _int))
(define-rts ark_exit (_fun -> _int))

(ark_init)
(printf "!!! init function success !!!\n")

(define-hs entrypoint (_fun _int -> _int))

(printf "calling entrypoint: ")
(entrypoint 0)
(printf "!!! entrypoint function called successfully!!!\n")
(newline)

(printf "########## Ready to call haskell functions ###########\n")

(define-hs accelerateMap (_fun _acc-manifest-array-pointer _acc-manifest-array-pointer _int _int -> _void))
(define-hs accelerateZipWith (_fun _acc-manifest-array-pointer _acc-manifest-array-pointer _acc-manifest-array-pointer _int -> _void))
(define-hs accelerateFold (_fun _acc-manifest-array-pointer _acc-manifest-array-pointer _int _int -> _void))

(define accelerate-test-cases (test-suite
  "GPU Test Cases"
  ))

(display "\n<----------- Accelerate test-cases Run ----------->\n")
(if (run-tests accelerate-test-cases)
    (display "\n!!! Test Run Successfull !!!\n")
    (display "\n!!! Test Run Failed !!!\n"))

(printf "########## Exiting racket safely ############\n")
