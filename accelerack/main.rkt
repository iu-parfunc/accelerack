#lang racket/base

(require
         accelerack/private/syntax
         accelerack/private/parse
         accelerack/private/racket_ops

         accelerack/private/image-interop

         (prefix-in r: racket/base)
         (only-in racket/base car for/list list syntax->datum)
         (only-in racket in-dict)
         (for-syntax racket/base syntax/parse)

         (for-syntax (only-in accelerack/private/syntax-table echo-types-param))
                     (only-in accelerack/private/syntax-table echo-types-param)

         accelerack/private/wrappers
         accelerack/private/types
         ;         accelerack/private/prim-table
         accelerack/private/prim-redefinitions

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
    sexp->acc-array

    ;; A list of primitives for reference:
    acc-prims
    acc-keywords
    acc-prim-types

    ;; Data-parallel aggregate operations
    map fold zipwith stencil3x3 generate
    until
    auntil
    replicate

    ;; Standard Racket operations with slightly modified behavior.
    sqrt round ceiling floor

    ;; Individual elements plus shapes
    acc-scalar? acc-element?
    stencil-boundary?
    intersect-shape

    ;; Image conversions
    image->acc-array acc-array->image
    color->acc-element acc-element->color

    ;; Types and Typed operations
    acc-type? acc-element-type?
    acc-echo-types
    ; echo-types-param ;; Exporting, but only for macro-expansion, not for end users.

    define-acc type-of acc ; run-gpu
    Int Bool Double Array

    ;; Other keywords
    ;; ----------------------------------------
    ;; (require accelerack) must be sufficient to avoid problems with unbound
    ;; identifiers in define-acc blocks.

    -> : use

    ;; However, not currently reexporting from base:
    ;; vector vector-ref + * - ...
    ;; if let lambda
    )

;; Simplify this for exposing to the end user:
(define acc-prim-types
  (for/list ([(k v) (in-dict acc-primop-types)])
    (list (syntax->datum k) v)))

;; This is really only so we can refer to it in
;; an error message.
(define acc-prims
  ;; For documentation purposes we include the special syntaxes:
  (append (list 'fold 'generate 'acc-array) ;; : use ...
          (r:map car acc-prim-types)))

(define acc-keywords
  (map syntax->datum acc-keyword-lits))

(define-syntax (acc-echo-types stx)
  (syntax-parse stx
    ;; We could stick the parameter right in the syntax so we don't
    ;; have to export its binding.  BUT, this can lead to the error:
    ;; "write: cannot marshal value that is embedded in compiled code"
    ; [(_) #`(begin-for-syntax (#,echo-types #t))]
    [(_) (syntax-protect #'(begin-for-syntax (echo-types-param #t)))]

    ))
