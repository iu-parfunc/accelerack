#lang racket

;; Macros for working with Accelerack data.
;; Syntax helpers for defining core Accelerate macros and compiler passes.

(require (only-in ffi/unsafe ctype?)
	 racket/runtime-path
         accelerack/private/parse
         accelerack/private/allocate
         accelerack/private/arrayutils
         accelerack/private/global_utils
         (prefix-in rkt: accelerack/private/racket_ops)
         accelerack/private/header
         (only-in '#%foreign ctype-scheme->c ctype-c->scheme)
         (for-syntax racket/base syntax/parse)
         (prefix-in r: racket/base)

         syntax/parse
         (only-in accelerack/private/types make-acc-array)
         (for-template racket/base)
         (for-template (only-in racket/contract ->))

         ;; Regular require, careful of phasing of these identifiers:
         accelerack/private/accelerack-types
         (for-template accelerack/private/accelerack-types)
         (for-syntax accelerack/private/accelerack-types)

         (only-in rackunit check-not-false)
         ; (for-meta 2 (only-in racket/base #%top quote))
         )

(provide acc-array

         acc-primop
         acc-primop-lits
         ; acc-primop-identifier?

         acc-scalar-lits
         acc-scalar-type
         )
(provide (all-from-out accelerack/private/accelerack-types))

;; Infer the type of constant data.
(define-for-syntax infer-type
  (lambda (d)
    (printf "INFERING TYPE ~a\n" d)
    (syntax-parse d
      [_:boolean #'(quote Bool)]
      [_:number
       (printf "NUMBER CASE... ~a\n" (if (flonum? (syntax-e d))
                                         #'(quote Double)
                                         #'(quote Int)))
       (if (flonum? (syntax-e d))
           #'(quote Double)
           #'(quote Int))]
      [#(v ...)
       (list->vector (map infer-type (syntax->list #'(v ...))))]
      [(v more ...) (infer-type #'v)]
      )))

(define-for-syntax (infer-shape d)
  (syntax-parse d
    [(~or _:boolean _:number) #'()]
    [_
     #:when (vector? (syntax-e d))
     #'()]
    [(v more ...)
     #:with rest (infer-shape #'v)
     #:with l (add1 (length (syntax->list #'(more ...))))
     #'(l . rest)]))

(begin-for-syntax
  (define-syntax-class acc-data
    #:attributes (shape type)
    [pattern v
             #:with shape (infer-shape #'v)
             #:with type (infer-type #'v)])
  )

;; TODO: Replace with a primop type table:
(define acc-primop-lits
  (list #'add1 #'sub1 #'+ #'* #'/ #'-))

(define acc-scalar-lits
  (list #'Bool #'Int #'Double))

;; [2015.12.11] Huh.. these work when I load this modulue, but fail
;; when this moudle is imported by syntax-tests.rkt:
(check-not-false (andmap identifier-binding acc-primop-lits))
(check-not-false (andmap identifier-binding acc-scalar-lits))

(define (acc-primop-identifier? id)
  (and (identifier-binding id) ;; could throw an error for this.
       (member id acc-primop-lits free-identifier=?)))

(define (acc-scalar-identifier? id)
  (and (identifier-binding id) ;; could throw an error for this.
       (member id acc-scalar-lits free-identifier=?)))

(define-syntax-class acc-primop
  #:description "a primitive function supported by Accelerack (such as +, -, *, map, etc)"
  (pattern p:id #:when (acc-primop-identifier? #'p)))

(define-syntax-class acc-scalar-type
  #:description "a type for scalar (non-array) data"
  (pattern p:id #:when (acc-scalar-identifier? #'p))
  (pattern #(t:acc-scalar-type ...)))

;; A convenient syntax for literal arrays, which does not require the
;; user to provide type/shape information.
(define-syntax (acc-array stx)
  (syntax-parse stx
    [(_ data:acc-data)
     #'(make-acc-array (array data.shape data.type data))]))


;;------------------------------

(define-syntax (array stx) (error 'array "FINISHME"))
;; Need to replace this with direct use of the allocation API:
; (acc-alloc type* (syntax->datum (syntax (shape ...))) data)

(define test (acc-array (1 2 3 4)))
