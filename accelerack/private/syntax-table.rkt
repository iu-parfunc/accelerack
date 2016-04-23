#lang racket

(require syntax/id-table 
 )

(provide :
 snap-as-syntax acc-syn-table echo-types-param snap-as-list)

;; The table in which Accelerack syntax is accumulated so as to
;; communicate it between textually separate (acc ..) forms.
;;
;; It maps variables (symbols) onto acc-syn-entry records.
(define acc-syn-table (box (make-immutable-free-id-table)))

;; Another piece of syntax-time global state.
(define echo-types-param (make-parameter #f))

(define (snap-as-syntax)
  (with-syntax ((((k . v) ...)
		 (dict-map (unbox acc-syn-table) cons)))
    #'(list (list (quote-syntax k) (quote-syntax v)) ...)))


(define (snap-as-list) (dict-map (unbox acc-syn-table) cons))


(define-syntax (: stx)
  (raise-syntax-error 'error
    "colon (:) syntax for type annotation should be used in an Accelerate block" stx))
