#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         syntax/parse)

(begin-for-syntax

  (define-literal-set scalar-type-lit
    (_int)))
  ;;(_int _bool _double _tuple))

#|
(define-literal-set type-lit
  (_tuple _array ->))

(define-literal-set primop-lit
  (add1 sub1))

(define primop? (literal-set->predicate primop-lit))
|#
