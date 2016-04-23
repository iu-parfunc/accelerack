#lang racket

(require  syntax/id-table
         accelerack/private/types
         (for-syntax syntax/parse)
 )

(provide snap-as-syntax acc-syn-table echo-types-param snap-as-list
         lookup-acc-expr lookup-acc-syn-entry
         (contract-out
          [extend-syn-table (-> syntax? (-> (or/c acc-type? #f) acc-type?)
                                (or/c syntax? #f) acc-syn-entry?)]))

;; The table in which Accelerack syntax is accumulated so as to
;; communicate it between textually separate (acc ..) forms.
;;
;; It maps variables (symbols) onto acc-syn-entry records.
(define acc-syn-table (box (make-immutable-free-id-table)))

;; This takes a function to merge in new type information, rather than a type directly.
;; Returns the new (merged) entry associated with the name.

(define (extend-syn-table name merge-type expr)
  (define oldentry (lookup-acc-syn-entry name))
  (define newentry (acc-syn-entry (merge-type #f) expr))
  (define entry (if oldentry
                    (acc-syn-entry (merge-type (acc-syn-entry-type oldentry))
                                   (or expr    (acc-syn-entry-expr oldentry)))
                    newentry))
  (define newdict
    (dict-set (unbox acc-syn-table) name entry))
  (set-box! acc-syn-table newdict)
  entry)


(define (lookup-acc-syn-entry name)
  (dict-ref (unbox acc-syn-table) name #f))

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

;; Another piece of syntax-time global state.
(define echo-types-param (make-parameter #f))
