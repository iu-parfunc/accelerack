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
    
    ;; A list of primitives for reference:
    acc-prims
    acc-prim-types
    
    ;; Data-parallel aggregate operations
    map fold zipwith stencil3x3 generate
    until
    replicate 

    ;; Individual elements plus shapes
    acc-scalar? acc-element?
    stencil-boundary?
    intersect-shape
    
    ;; Image conversions
    image->acc-array acc-array->image
    color->acc-element acc-element->color

    ;; Types and Typed operations
    acc-type? acc-element-type?

    define-acc run-gpu
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
