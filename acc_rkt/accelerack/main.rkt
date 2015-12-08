#lang racket/base

(require accelerack/private/allocate
         accelerack/private/global_utils
         accelerack/private/header
         accelerack/private/syntax
         accelerack/private/parse
         (prefix-in rkt: accelerack/private/racket_ops)

         ffi/unsafe
         (prefix-in r: racket/base)
         (for-syntax racket/base syntax/parse)

         accelerack/private/wrappers
         accelerack/private/types

         accelerack/private/executor
         accelerack/private/passes/syntax-capture
         )

;; RRN: Here we provide an explicit export list as a final gate-keeper
;; for what's in the language.  This should very closely match
;; accelerack_grammar.txt:
(provide acc-array acc-array? acc-array->list

         map fold zipwith stencil3x3
         array-ref

         acc run-acc define-acc
         ; stencil3x3
         ; array-ref
         ; Int Bool Double
         )
