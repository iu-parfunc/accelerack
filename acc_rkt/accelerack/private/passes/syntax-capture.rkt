#lang racket

;; ---------------------------------------------------------------
;; The syntax-capture and verification step.
;; ---------------------------------------------------------------

(provide define-acc acc run-acc snapshot-current-acc-syn-table)

(require (for-syntax racket/base)
         syntax/parse
         syntax/to-string

         ;; We use the identifiers from "wrappers" as our names for map/fold/etc
         accelerack/private/wrappers
         ; accelerack/private/racket_ops
         (only-in accelerack/private/syntax acc-array)
         )
(require (for-syntax syntax/parse syntax/id-table racket/dict
                     (only-in accelerack/private/syntax acc-array)
                     accelerack/private/passes/verify-acc
                     accelerack/private/passes/typecheck
                     (only-in accelerack/private/executor launch-accelerack-ast)
                     (only-in accelerack/private/global_utils accelerack-debug-mode?)
                     rackunit
                     ))


(begin-for-syntax
  ;; The table in which Accelerack syntax is accumulated so as to
  ;; communicate it between textually separate (acc ..) forms.
  (define acc-syn-table (box (make-immutable-free-id-table)))

  (define (snap-as-syntax)
    (with-syntax ((((k . v) ...)
                      (dict-map (unbox acc-syn-table) cons)))
      #'(list (list (quote-syntax k) (quote-syntax v)) ...)))

  (define (snap-as-list) (dict-map (unbox acc-syn-table) cons))

  ;; Just the front-end part of the compiler.
  (define (front-end-compiler e)
    (define syn-table (snap-as-list))
    (when (accelerack-debug-mode?)
      (fprintf (current-error-port)
               "\nInvoking compiler front-end, given syntax table: ~a\n"
               (map (lambda (x) (list (syntax->datum (car x)) (syntax->datum (cdr x))))
                    syn-table)))
    ; (printf "Woo compiler frontend! ~a\n" e)
    (typecheck-expr (verify-acc syn-table e)))
  )

;; Surprisingly, this form of persistence changes the VALUES of the
;; hash into normal lists, but not the keys.
(define-syntax (snapshot-current-acc-syn-table stx)
  (syntax-parse stx [(_) (snap-as-syntax)]))

;; --------------------------------------------------------------------------------

;; TODO: need to defer execution:
(define-syntax (acc stx)
  (syntax-parse stx
    [(_ e) (front-end-compiler ; (snapshot-current-acc-syn-table)
            #'e)]))

;; Type-checking happens at expansion time.
(define-syntax (run-acc stx)
  (syntax-parse stx
    [(_ e) #`(launch-accelerack-ast #,(front-end-compiler #'e))]))

;; TODO: Need to defer execution ...
(define-syntax (define-acc stx)
  (syntax-parse stx
    [(_ (f:identifier x:identifier ...) e)
     (with-syntax ((bod (front-end-compiler #'(lambda (x ...) e))))
       ; (check-pred syntax? bod)
       ; (printf "Capturing lambda syntax: ~a\n" #'bod)
       (set-box! acc-syn-table
                 (dict-set (unbox acc-syn-table) #'f #'bod))
       #`(define f bod))]
    [(_ x:identifier e)
     (let ((e2 (front-end-compiler #'e)))
       (check-pred syntax? e2)
       (set-box! acc-syn-table
                 (dict-set (unbox acc-syn-table) #'x e2))
     #`(define x #,e2))]
  ))


; --------------------------------------------------------------------------------
