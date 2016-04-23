#lang racket

(require syntax/id-table
         accelerack/private/types
 )

(provide :
         snap-as-syntax acc-syn-table echo-types-param snap-as-list
         extend-syn-table lookup-acc-expr)

;; The table in which Accelerack syntax is accumulated so as to
;; communicate it between textually separate (acc ..) forms.
;;
;; It maps variables (symbols) onto acc-syn-entry records.
(define acc-syn-table (box (make-immutable-free-id-table)))


(define (extend-syn-table name type expr)
  (define entry (acc-syn-entry type expr))
   (set-box! acc-syn-table
             (dict-set (unbox acc-syn-table) name entry)))
  ;; (let ((expr (if (syntax? expr) expr (datum->syntax #f expr))))
  ;;   (define entry (acc-syn-entry type expr))
  ;;   (set-box! acc-syn-table
  ;;             (dict-set (unbox acc-syn-table) name entry))))

(define (lookup-acc-type name)
  (acc-syn-entry-type (dict-ref (unbox acc-syn-table) name)))


(define (lookup-acc-expr name)
  (acc-syn-entry-expr (dict-ref (unbox acc-syn-table) name)))


(define (snap-as-syntax)
  (with-syntax ((((k . v) ...)
		 (dict-map (unbox acc-syn-table) cons)))
    #'(list (list (quote-syntax k) (quote-syntax v)) ...)))

(define (snap-as-list) (dict-map (unbox acc-syn-table) cons))

;; ------------------------------------------------------------

(define-syntax (: stx)
  (raise-syntax-error 'error
    "colon (:) syntax for type annotation should be used in an Accelerate block" stx))


;; Another piece of syntax-time global state.
(define echo-types-param (make-parameter #f))
