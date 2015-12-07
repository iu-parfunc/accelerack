#lang racket

(require (for-syntax racket/base)
         syntax/parse
         syntax/to-string)

(begin-for-syntax

  (define-literal-set scalar-type-lit
    (_int _bool _double _tuple))

  (define-literal-set type-lit
    (_tuple _array ->))

  (define-literal-set primop-lit
    (add1 sub1))

  (define primop? (literal-set->predicate primop-lit))

  (define-syntax-class acc-scalar-type
    #:literal-sets (scalar-type-lit)
    (pattern _int)
    (pattern _bool)
    (pattern _double)
    (pattern (_tuple t:acc-scalar-type ...)))

  (define-syntax-class acc-scalar
    #:attributes (type)
    (pattern _:boolean #:with type #'_bool)
    (pattern _:

  (define-syntax-class acc-type
    #:literal-sets (type-lit)
    (pattern (-> _:acc-type _:acc-type))
    (pattern (_tuple _:acc-type ...))
    (pattern (_array _:exact-nonnegative-integer _:acc-type))
    (pattern _:acc-scalar-type))

  (define-syntax-class acc-primop
    (pattern p #:when #'(primop? p)))
  
  (define (type-mismatch expected actual)
    (string-append
      "expected type "
      expected
      ", actual type "
      actual))
  )

(define-syntax (typecheck-pass e env) e)
  
(define-syntax (check-type e env typ)
   (syntax-parse e
     (b:boolean
       #:fail-unless #`(eq? ,typ _bool)
         (type-mismatch
           "_bool"
           (syntax->string typ))
       #'b)
     (n:number
       #:fail-unless #`(eq? ,typ _double)
         (type-mismatch
           "_double"
           (syntax->string typ))
         #'n)
     ))

(define-syntax (infer-type e env) e)

