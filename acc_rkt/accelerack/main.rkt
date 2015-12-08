#lang racket/base

(require accelerack/private/allocate
         accelerack/private/global_utils
         (prefix-in acc: (only-in accelerack/private/header acc-array?))
         (prefix-in acc: (only-in accelerack/private/header make-acc-array))
         (except-in accelerack/private/header acc-array? make-acc-array)
         accelerack/private/syntax
         accelerack/private/parse
         (prefix-in rkt: accelerack/private/racket_ops)

         ffi/unsafe
         (prefix-in r: racket/base)
         (for-syntax racket/base syntax/parse)

         accelerack/private/wrappers
         accelerack/private/types
         )

;; RRN: most of this should NOT be exported from the publically visible module:
(provide (all-from-out accelerack/private/allocate)
         (all-from-out accelerack/private/global_utils)
         (all-from-out accelerack/private/header)
         (all-from-out accelerack/private/syntax)
         (all-from-out accelerack/private/parse)
         (all-from-out accelerack/private/racket_ops)

         ; (all-from-out accelerack/private/wrappers)
         (all-from-out accelerack/private/types)
         )

;; RRN: Provide an explicit export list as a final gate-keeper for what's in the language.
; (provide acc-array acc-array? ...)
