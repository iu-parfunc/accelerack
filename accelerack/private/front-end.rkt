#lang racket

(require
 (only-in accelerack/private/syntax acc-array)
 accelerack/private/wrappers
 accelerack/private/passes/verify-acc
 accelerack/private/passes/typecheck
 accelerack/private/types
 accelerack/private/passes/strip
 accelerack/private/passes/normalize
 accelerack/private/syntax-table
 (only-in accelerack/private/executor launch-accelerack-ast)
 (only-in accelerack/private/utils accelerack-debug-mode? pass-output-chatter)
 syntax/parse syntax/id-table racket/dict syntax/to-string
 rackunit
 racket/trace
 )

(provide
 front-end-compiler
 apply-to-syn-table
 (contract-out
  ))


;; Just the front-end part of the compiler.
;; Returns three values.
(define (front-end-compiler e)
  (define syn-table (snap-as-list))
  (when (accelerack-debug-mode?)
    (fprintf (current-error-port)
             "\nInvoking compiler front-end, given syntax table: ~a\n"
             (map (lambda (x) (list (car x) (cdr x)))
                  syn-table))
    (pass-output-chatter 'initial-program (syntax->datum e)))
  (define stripped (verify-acc syn-table e))
  (define-values (main-type with-types)
    (with-handlers []
      (typecheck-expr syn-table e)))
  ;    (fprintf (current-error-port)
  ;             "TODO: May run normalize on ~a\n" (syntax->datum with-types))
  ;; (values (datum->syntax stripped (normalize (strip-ast stripped) syn-table)) main-type with-types)
  (values stripped main-type with-types (normalize (strip-ast stripped) syn-table)))

;; Return the new type associated with the entry.
(define (apply-to-syn-table maybeType inferredTy name progWithTys)
  (define newTy (if maybeType
                    (if (unify-monos name inferredTy
                                     (syntax->datum maybeType))
                        (syntax->datum maybeType)
                        ;; TODO: can report a more detailed unification error:
                        (raise-syntax-error  name
                                             (format "inferred type of binding (~a) did not match declared type"
                                                     inferredTy)
                                             maybeType))
                    inferredTy))
  (acc-syn-entry-type
   (extend-syn-table name
                    (lambda (t)
                      (if t
                          (unify-monos name newTy t)
                          newTy))
                    progWithTys)))

(define (unify-monos ctxt t1 t2)
  (collapse (unify-types ctxt (instantiate t1)
                         ;; Leaving these user type signatures uninstantiated makes
                         ;; the explicitly mentioned variabes rigid and enforces parametricity.
                         (freshen-type-vars t2)
                         )))
