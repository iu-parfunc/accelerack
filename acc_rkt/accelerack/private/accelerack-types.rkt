#lang racket

;; The grammar of types used by Accelerack itself.

(provide
 ;; Types:
 Bool Int Double Array
 Num SExp

 ;; Syntax for type ascription:
 :

 ;; Reexports:
 ->
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

(define-syntax (: stx)
  (raise-syntax-error 'error "colon (:) syntax for type annotation should be used in an Accelerate block" stx))

(define-syntax (Num stx)
  (raise-syntax-error 'error "Num type constructor used outside of Accelerate block" stx))

(define-syntax (SExp stx)
  (raise-syntax-error 'error "SExp type constructor used outside of Accelerate block" stx))
