#lang racket/base

(require accelerack/private/acc_allocate
         accelerack/private/acc_global_utils
         (prefix-in acc: (only-in accelerack/private/acc_header acc-array? make-acc-array))
         (except-in accelerack/private/acc_header acc-array? make-acc-array)
         accelerack/private/acc_syntax
         accelerack/private/acc_parse
         (prefix-in rkt: accelerack/private/acc_racket_ops)
         ffi/unsafe
         (prefix-in r: racket/base)
         (for-syntax racket/base syntax/parse)

         accelerack/private/wrappers
         accelerack/private/types
         )

;; RRN: most of this should NOT be exported from the publically visible module:
(provide (all-from-out accelerack/private/acc_allocate)
         (all-from-out accelerack/private/acc_global_utils)
         (all-from-out accelerack/private/acc_header)
         (all-from-out accelerack/private/acc_syntax)
         (all-from-out accelerack/private/acc_parse)
         (all-from-out accelerack/private/acc_racket_ops)

         ; (all-from-out accelerack/private/wrappers)
         (all-from-out accelerack/private/types)
         )

;; RRN: Provide an explicit export list as a final gate-keeper for what's in the language.
; (provide acc-array acc-array? ...)
