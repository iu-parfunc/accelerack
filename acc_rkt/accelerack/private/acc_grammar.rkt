#lang racket/base

(require (for-syntax racket/base syntax/parse))

(provide)

#|

scalar-type st
  = Int
  | Bool
  | Double
  | #(st ...)   ; Tuple of Scalars

type t
  = st                ; Scalar Type
  | #(t ...)          ; Tuple Type
  | (Array n t)       ; Array Type
  | (-> t t)          ; Function Type

n = non-negative integer

expression e
  = (acc-array a)
  | add1
  | sub1
  | (map e e)
  | (zipwith e e e)
  | (fold e e e)
  | (stencil3x3 e b e)    ; Take a function: (lambda (x1 .. x9) e)
  | (let ((x e) ...) e)   ; Let binding
  | x                     ; Variable
  | (lambda (x ...) e)    ; Lambda abs
  | (e e ...)             ; Application
  | #(e ...)              ; Tuple expression

;; Q: how much of racket do we need to cover here?
;;    viz. how necessary is it to include expressions like
;;    lambdas, applications, let bindings, etc. ?
;;
;;    the compiler will need to examine identifiers to see if
;;    they are known accelerate operations, yes?

value v
  = <boolean>
  | <integer>
  | <double>

boundary conditions b
  = Clamp | Mirror | Wrap | (Constant v)

array-data a
  = v                 ; zero-dimensional scalar
  | (a ...), all a have same length  ; (S n)-dimensional data

shape sh = (n ...)

|#
