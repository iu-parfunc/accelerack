#lang racket

;; ---------------------------------------------------------------
;; The syntax-capture and verification step.
;; ---------------------------------------------------------------

(provide define-acc acc run-acc )

(require (for-syntax racket/base)
         syntax/parse
         syntax/to-string

         ;; We use the identifiers from "wrappers" as our names for map/fold/etc
         accelerack/private/wrappers
         ; accelerack/private/racket_ops
         (only-in accelerack/private/syntax acc-array)
         )
(require (for-syntax syntax/parse)
         (for-syntax (only-in accelerack/private/syntax acc-array))
         (for-syntax accelerack/private/passes/verify-acc)
         (for-syntax accelerack/private/passes/typecheck))

(begin-for-syntax
  ;; The table in which Accelerack syntax is accumulated so as to
  ;; communicate it between textually separate (acc ..) forms.
  (define acc-syn-table (box (make-immutable-hash)))

  ;; Just the front-end part of the compiler.
  (define (front-end-compiler e)
    ; (printf "Woo compiler frontend! ~a\n" e)
    (typecheck-expr (verify-acc e '())))
  )

;; TODO: need to defer execution:
(define-syntax (acc stx)
  (syntax-parse stx
    [(_ e) (front-end-compiler #'e)]))

(define-syntax (run-acc stx)
  (syntax-parse stx
    [(_ e) (front-end-compiler #'e)]))

;; TODO: Need to defer execution ...
(define-syntax (define-acc stx)
  (syntax-parse stx
    [(_ (f:identifier x:identifier ...) e)
     (let ((bod (front-end-compiler #'(lambda (x ...) e))))
       (set-box! acc-syn-table
                 (hash-set (unbox acc-syn-table) #'f bod))
       #`(define f #,bod))]
    [(_ x:identifier e)
     (let ((e2 (front-end-compiler #'e)))
       (set-box! acc-syn-table
                 (hash-set (unbox acc-syn-table) #'f e2))
     #`(define x #,e2))]
  ))

(define-syntax (persist-table stx)
  (syntax-parse stx
    [(_)
     #`(quote #,acc-syn-table)
     ; (datum->syntax acc-syn-table)
     ]))

; --------------------------------------------------------------------------------


; --------------------------------------------------------------------------------

#;
;; Verifies an accelerack expression's grammar:
(define (verify-acc p)
  (syntax-parse p
    )
  )

(define-acc (sqr x) (* x x))
(define-acc ac 3)

(define table (persist-table))
