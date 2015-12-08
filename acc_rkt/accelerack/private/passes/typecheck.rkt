#lang racket

;; ---------------------------------------------------------------
;; Defines a compiler pass which:
;;  (1) Adds type annotations (whereever missing) for all binders.
;;  (2) Throws an exception localized to the right syntax object
;;      for any ype errors.
;; ---------------------------------------------------------------

(provide (contract-out
          [typecheck-pass (-> syntax? syntax?)]
          ))

(require (for-syntax racket/base)
         syntax/parse
         syntax/to-string)
; (require accelerack/private/acc_syntax)
(require (for-syntax syntax/parse))

;; Temp Placeholders
(define (acc-array d) #f)
(define (map f x) #f)


;; The full type-checking pass.
(define (typecheck-pass e) e)

;; Everything below this point is just a playground for small tests:
;; -----------------------------------------------------------------

(begin-for-syntax
  (define (helper stx)
    (printf "Calling walk helper on a syntax object: ~a\n" stx)
    (syntax-parse stx
      #:literals (map acc-array)
      [(map exp data) (printf "Helper: map case\n")
                      (helper #'data)]
      [(acc-array data) (printf "Helper: acc-array case\n")]
      [b:boolean (printf "Helper: bool case\n" )]
      [_ (printf "Helper: unhandled case 2....\n")]
      )
    (printf "Helper succeeded.\n")
    ))

(define-syntax (walk stx)
  ; (helper stx)
  (syntax-parse stx
    #:literals (map acc-array)
    [(_ (map exp data))
     (printf "walk: Got map.. \n")
     #'(map exp (walk data))
     ]

    [(_ (acc-array dat))
     (printf "walk: Got acc-array.. \n")
     #'(acc-array dat)]

    [(_ b:boolean)
      ; #:fail-unless #`(eq? ,typ _bool)
     (printf "walk: Got bool\n")
     #'b]

    ))

(define-syntax (acc-temp stx)
  (syntax-parse stx
    [(_ bod)
     (helper #'bod)
     ; #`(quote #,(helper #'bod))
     #'(quote blah)
     ]))

;; Uncomment to run some simple tests:
#;
(begin
  ; (printf "\nSome simple tests of a recursive macro:\n\n")
  (walk (map add1 #f))
  ; (walk (map add1 (acc-array (1 2 3 4))))
  (walk (map add1 (acc-array #f)))

  ; (printf "Result of calling expand:\n  ~a\n"
  ;         (expand '(walk (map add1 (acc-array #f)))))

  ; (printf "\nSome simple tests of regular recursive function over syntax objects:\n\n")
  (acc-temp #f)
  (acc-temp (map add1 (acc-array (1 2 3 4))))
  )

;; Scratch work below here
;; ================================================================================

#;
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

;  (define-syntax-class acc-scalar
;    #:attributes (type)
;    (pattern _:boolean #:with type #'_bool)
;    (pattern _:

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

; (define-syntax (typecheck-pass e env) e)

#;
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

; (define-syntax (infer-type e env) e)
