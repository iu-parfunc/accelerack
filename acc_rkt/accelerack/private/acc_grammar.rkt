#lang racket/base

(require (for-syntax racket/base syntax/parse))

(provide)

#|

scalar-type st
  = _int
  | _bool
  | _double
  | (_tuple st ...)   Tuple of Scalars

type t
  = st                Scalar Type
  | (_tuple t ...)    Tuple Type
  | (_array n t)      Array Type
  | (-> t t)          Function Type

n = non-negative integer

expression e
  = (array sh st a)
  | add1
  | sub1
  | (map e e)
  | (zipwith e e)
  | (fold e e e)
  | (let ((x e) ...) e) Let binding
  | x                   Variable
  | (lambda (x ...) e)  Lambda abs
  | (e e ...)           Application
  | #(e ...)            Tuple expression

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

array-data a
  = e
  | (a ...), all a have same length

shape sh = (n ...)

|#

