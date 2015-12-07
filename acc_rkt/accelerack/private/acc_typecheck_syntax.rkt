#lang racket

(require (for-syntax racket/base)
         syntax/parse
         syntax/to-string)

(require accelerack/private/acc_syntax)
(require (for-syntax syntax/parse))

#;
(begin-for-syntax

  (define-literal-set scalar-type-lit
    (_int))
  )
  ;;(_int _bool _double _tuple))

; (define-syntax (define-acc stx))
; (walk (map add1 (acc-array (1 2 3 4))))

#|
(define-literal-set type-lit
  (_tuple _array ->))

(define-literal-set primop-lit
  (add1 sub1))

(define primop? (literal-set->predicate primop-lit))
|#
