#lang racket/base

(require accelerack/acc-array/private/manifest-array/allocate
         accelerack/acc-array/private/manifest-array/structs
         accelerack/private/syntax
         accelerack/private/parse
         accelerack/private/racket_ops

         (prefix-in r: racket/base)
         (for-syntax racket/base syntax/parse)

         accelerack/private/wrappers
         accelerack/private/types

         accelerack/private/executor
         accelerack/private/syntax-capture

         accelerack/acc-array
         )

;; RRN: Here we provide an explicit export list as a final gate-keeper
;; for what's in the language.  This should very closely match
;; accelerack_grammar.txt:
(provide acc-array acc-array? acc-array=?
         acc-array-ref acc-array-flatref
         acc-array->sexp
         acc-scalar? acc-element?

         map fold zipwith stencil3x3 generate

         define-acc run-gpu
         Int Bool Double

         ;; Not reexporting from base:
         ;; vector vector-ref + * - ...
	 ;; if let lambda
         -> : use
         )
