#lang racket

;; Macros for working with Accelerack data.
;; Syntax helpers for defining core Accelerate macros and compiler passes.

(require ffi/unsafe
         ffi/unsafe/define
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
         (for-template racket/base))

(provide acc-array
         array

         accelerack-primitive-function
         acc-primop-lits
         acc-primop-identifier?

         Bool Int Double
         )

(define-for-syntax (infer-type d)
  (syntax-parse d
    [_:boolean #'_bool]
    [_:number (if (flonum? (syntax-e d)) #'_double #'_int)]
    [#(v ...) #`(#,@(list->vector (map infer-type (syntax->list #'(v ...)))))]
    [(v more ...) (infer-type #'v)]
    ))

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

(define acc-primop-lits
  (list #'add1 #'sub1 #'+ #'* #'/ #'-))

(define (acc-primop-identifier? id)
  (member id acc-primop-lits free-identifier=?))

(define-syntax-class accelerack-primitive-function
  #:description "a primitive function supported by Accelerack (such as +, -, *, map, etc)"
  ;; How to get this style to work?:
  (pattern p:id #:when (acc-primop-identifier? #'p)))


(define-syntax (Bool stx)
  (raise-syntax-error 'error "Bool type used outside of Accelerate block" stx))
(define-syntax (Int stx)
  (raise-syntax-error 'error "Int type used outside of Accelerate block" stx))
(define-syntax (Double stx)
  (raise-syntax-error 'error "Double type used outside of Accelerate block" stx))

;; A convenient syntax for literal arrays, which does not require the
;; user to provide type/shape information.
(define-syntax (acc-array stx)
  (syntax-parse stx
    [(_ data:acc-data)
     #'(make-acc-array (array data.shape data.type data))]))


;;------------------------------

;; TODO - Need to rework the macros

(define (process-data data)
  (match data
    ((list (list x ...) ...) (r:map process-data x))
    (`(,x ...) (r:map process-data x))
    (`,x (if (or (pair? x) (vector? x)) (vector->list* x) x))))

(define-syntax (array stx)
  (syntax-case stx ()

    [(array (shape ...) #(type ...) (data ...))
                           #'(letrec ([data* (process-data (syntax->datum (syntax (data ...))))]
                                      [type* (r:map map-type  (list type ...))]
                                      [ret (verify-accelerack (vector type* (syntax->datum (syntax (shape ...))) data*))])
                                     (if (car ret)
                                         (acc-alloc type* (syntax->datum (syntax (shape ...))) data*)
                                         (error 'verify-accelerack (cadr ret))))]
    [(array (shape ...) #(type ...) data)
                           #'(let ([type* (r:map map-type (list type ...))]
                                   [ret (verify-accelerack (vector type* (syntax->datum (syntax (shape ...))) (flatten data)))])
                                  (if (car ret)
                                      (acc-alloc type* (syntax->datum (syntax (shape ...))) data)
                                      (error 'verify-accelerack (cadr ret))))]
    [(array (shape ...) type (data ...))  #'(ctype? type)
                           #'(letrec ((data* (process-data (syntax->datum (syntax (data ...)))))
                                      (ret (verify-accelerack (vector type (syntax->datum (syntax (shape ...))) data*))))
                                     (if (car ret)
                                         (acc-alloc type (syntax->datum (syntax (shape ...))) data*)
                                         (error 'verify-accelerack (cadr ret))))]
    [(array (shape ...) type data)  #'(ctype? type)
                           #'(let ((ret (verify-accelerack (vector type (syntax->datum (syntax (shape ...))) (flatten data)))))
                                  (if (car ret)
                                      (acc-alloc type (syntax->datum (syntax (shape ...))) data)
                                      (error 'verify-accelerack (cadr ret))))]))

(define map-type
  (lambda (x)
    (cond
      ((ctype? x) (ctype->symbol x))
      ((symbol? x) (ctype->symbol x))
      ((pair? x) (cons (map-type (car x)) (map-type (cdr x))))
      ((vector? x) (cons (map-type (car (vector->list x))) (map-type (cdr (vector->list x)))))
      (else x))))

(define test (acc-array (1 2 3 4)))
