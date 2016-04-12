#lang racket

;; Macros for working with Accelerack data.
;; Syntax helpers for defining core Accelerate macros and compiler passes.

(require (only-in ffi/unsafe ctype? _int _double _bool) ;; FIXME: remove _*
         accelerack/private/parse
         accelerack/acc-array/private/manifest-array/allocate
         accelerack/acc-array/private/arrayutils
         (only-in accelerack/private/paven_old/global_utils
                  vector->list*) 
         (for-syntax racket/base syntax/parse)
         (prefix-in r: racket/base)

         syntax/parse
         (only-in accelerack/acc-array/private make-acc-array)
         (for-template racket/base)
         (for-template (only-in racket/contract ->))

         ;; Regular require, careful of phasing of these identifiers:
         accelerack/private/keywords
         (for-template accelerack/private/keywords)

         (only-in rackunit check-not-false)
         )

(provide acc-array
         array

         acc-primop
         acc-primop-lits
         ; acc-primop-identifier?

         acc-scalar-lits
         acc-element-type
         )
(provide (all-from-out accelerack/private/keywords))

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

(define-syntax-class acc-element-type
  #:description "a type for element data that can go inside an array"
  (pattern p:id #:when (acc-scalar-identifier? #'p))
  (pattern #(t:acc-element-type ...)))

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
    (`,x (if (vector? x) (vector->list* x) x))))

(define-syntax (array stx)
  (syntax-case stx ()

    [(array (shape ...) #(type ...) (data ...))
     #'(letrec ([data* (process-data (syntax->datum (syntax (data ...))))]
                [type* (r:map map-type  (list type ...))]
                [ret (verify-accelerack
                      (vector type* (syntax->datum (syntax (shape ...))) data*))])
         (if (car ret)
             (list->manifest-array type* (syntax->datum (syntax (shape ...))) data*)
             (error 'verify-accelerack (cadr ret))))]
    [(array (shape ...) #(type ...) data)
     #'(let* ([type* (r:map map-type (list type ...))]
              [ret (verify-accelerack
                    (vector type* (syntax->datum (syntax (shape ...))) (flatten data)))])
         (if (car ret)
             (list->manifest-array type* (syntax->datum (syntax (shape ...))) data)
             (error 'verify-accelerack (cadr ret))))]
    [(array (shape ...) type (data ...))  #'(ctype? type)
     #'(letrec ((data* (process-data (syntax->datum (syntax (data ...)))))
                (ret (verify-accelerack (vector type (syntax->datum (syntax (shape ...))) data*))))
         (if (car ret)
             (list->manifest-array type (syntax->datum (syntax (shape ...))) data*)
             (error 'verify-accelerack (cadr ret))))]
    [(array (shape ...) type data)  #'(ctype? type)
     #'(let ((ret (verify-accelerack (vector type (syntax->datum (syntax (shape ...))) (flatten data)))))
         (if (car ret)
             (list->manifest-array type (syntax->datum (syntax (shape ...))) data)
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
