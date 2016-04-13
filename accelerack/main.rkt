#lang racket/base

(require accelerack/acc-array/private/manifest-array
         accelerack/acc-array/private/manifest-array/structs
         accelerack/private/syntax
         accelerack/private/parse
         accelerack/private/racket_ops

         accelerack/private/image-interop

         (prefix-in r: racket/base)
         (for-syntax racket/base syntax/parse)

         accelerack/private/wrappers
         accelerack/private/types

         accelerack/private/executor
         accelerack/private/syntax-capture

         accelerack/private/utils
         accelerack/acc-array
         )

;; RRN: Here we provide an explicit export list as a final gate-keeper
;; for what's in the language.  This should very closely match
;; accelerack_grammar.txt:
(provide
    ;; The array datatype and conversions
    acc-array acc-array? acc-array=?
    acc-array-ref acc-array-flatref
    acc-array-dimension
    acc-array-size
    acc-array-shape 
    acc-array->sexp

    ;; Data-parallel aggregate operations
    map fold zipwith stencil3x3 generate

    ;; Individual elements plus shapes
    acc-scalar? acc-element?
    intersect-shape
    
    ;; Image conversions
    image->acc-array acc-array->image

    ;; Types and Typed operations
    acc-type? acc-element-type?

    define-acc run-gpu
    Int Bool Double

    ;; Not reexporting from base:
    ;; vector vector-ref + * - ...
    ;; if let lambda
    -> : use
    )
