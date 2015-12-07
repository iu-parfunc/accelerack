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


(begin-for-syntax
  (define (helper stx)
    (printf "Calling walk helper on a syntax object: ~a\n" stx)
    (syntax-parse stx
      #:literals (map acc-array)
      [(_ (map exp data)) (printf "Helper: map case\n")]
      [(_ (acc-array data)) (printf "Helper: acc-array case\n")]
      )
    (printf "Helper succeeded.\n")
    ))

(define-syntax (walk stx)
  (helper stx)
  (syntax-parse stx
    #:literals (map acc-array)
    [(_ (map exp data))
     (printf "Got map: \n")
     #'(map exp (walk data))
     ]

    [(_ (acc-array dat))
     (printf "Got acc-array: ")
     #'(acc-array dat)]

#;    (b:boolean
       #:fail-unless #`(eq? ,typ _bool)
         (type-mismatch
           "_bool"
           (syntax->string typ))
       #'b)
    ))

; (define-syntax (define-acc stx))
(walk (map add1 (acc-array (1 2 3 4))))

#|
(define-literal-set type-lit
  (_tuple _array ->))

(define-literal-set primop-lit
  (add1 sub1))

(define primop? (literal-set->predicate primop-lit))
|#
