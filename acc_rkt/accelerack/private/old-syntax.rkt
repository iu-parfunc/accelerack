#lang racket

;; TODO: figure out what to do with this.

(require (only-in ffi/unsafe ctype?)
         accelerack/private/parse
         accelerack/private/allocate
         accelerack/private/arrayutils
         accelerack/private/global_utils
         (only-in '#%foreign ctype-scheme->c ctype-c->scheme)
         (for-syntax racket/base syntax/parse)
         (prefix-in r: racket/base)

         (only-in accelerack/private/types make-acc-array)
         (for-template racket/base)
         (for-template (only-in racket/contract ->))

         ;; Regular require, careful of phasing of these identifiers:
         (for-template accelerack/private/accelerack-types)
         (for-syntax accelerack/private/accelerack-types)
         (only-in rackunit check-not-false)
         )

(provide array)

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
