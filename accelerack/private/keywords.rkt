#lang racket

;; Syntax definitions for keywords only allowed inside Accelerack
;; syntax blocks.  This includes teh names of types used in Accelerack
;; types.


(provide
 ;; Types:
 Bool Int Double Array
 Num SExp

 ;; Reexports:
 ->

 ;; Syntax for type ascription and other forms:
 use
 )

(require (only-in racket/contract ->))

(define-syntax (Bool stx)
  (raise-syntax-error 'error "Bool type used outside of Accelerate block" stx))
(define-syntax (Int stx)
  (raise-syntax-error 'error "Int type used outside of Accelerate block" stx))
(define-syntax (Double stx)
  (raise-syntax-error 'error "Double type used outside of Accelerate block" stx))
(define-syntax (Array stx)
  (raise-syntax-error 'error "Array type constructor used outside of Accelerate block" stx))

;(define-syntax (-> stx)
;  (raise-syntax-error 'error "function (->) type constructor used outside of Accelerate block" stx))

(define-syntax (use stx)
  (raise-syntax-error 'error "use keyword outside of Accelerate block" stx))

(define-syntax (Num stx)
  (raise-syntax-error 'error "Num type constructor used outside of Accelerate block" stx))

(define-syntax (SExp stx)
  (raise-syntax-error 'error "SExp type constructor used outside of Accelerate block" stx))
