#lang racket

(require
 (only-in accelerack/private/syntax acc-array)
 accelerack/private/wrappers
 accelerack/private/passes/verify-acc
 accelerack/private/passes/typecheck
 accelerack/private/types
 (only-in accelerack/private/executor launch-accelerack-ast)
 (only-in accelerack/private/utils accelerack-debug-mode?)
 syntax/parse syntax/id-table racket/dict syntax/to-string
 rackunit
 racket/trace
 )

(provide snap-as-syntax acc-syn-table front-end-compiler snap-as-list
         extend-syn-table apply-to-syn-table lookup-acc-expr)



;; The table in which Accelerack syntax is accumulated so as to
;; communicate it between textually separate (acc ..) forms.
;;
;; It maps variables (symbols) onto acc-syn-entry records.
(define acc-syn-table (box (make-immutable-free-id-table)))

(define (snap-as-syntax)
  (with-syntax ((((k . v) ...)
		 (dict-map (unbox acc-syn-table) cons)))
    #'(list (list (quote-syntax k) (quote-syntax v)) ...)))


(define (snap-as-list) (dict-map (unbox acc-syn-table) cons))

;; Just the front-end part of the compiler.
;; Returns three values.
(define (front-end-compiler e)
  (define syn-table (snap-as-list))
  (when (accelerack-debug-mode?)
    (fprintf (current-error-port)
             "\nInvoking compiler front-end, given syntax table: ~a\n"
             (map (lambda (x) (list (car x) (cdr x)))
                  syn-table)))
  (define stripped (verify-acc syn-table e))
  (define-values (main-type with-types)
    (with-handlers []
      (typecheck-expr syn-table e)))
  ;    (fprintf (current-error-port)
  ;             "TODO: May run normalize on ~a\n" (syntax->datum with-types))
  (values stripped main-type with-types))

(define (apply-to-syn-table maybeType inferredTy name progWithTys)
  (define finalTy (if maybeType
                      (if (unify-types #f inferredTy (syntax->datum maybeType))
                          (syntax->datum maybeType)
                          ;; TODO: can report a more detailed unification error:
                          (raise-syntax-error  name
                                               (format "inferred type of binding (~a) did not match declared type"
                                                       inferredTy)
                                               maybeType))
                      inferredTy))
  (extend-syn-table name finalTy progWithTys)
  finalTy)


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
