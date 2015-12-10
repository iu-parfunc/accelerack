#lang racket

;; ---------------------------------------------------------------
;; The syntax-capture and verification step.
;; ---------------------------------------------------------------

(provide define-acc
         ; acc ;; Not exposing this yet.
         run-gpu
         snapshot-current-acc-syn-table)

(require syntax/parse syntax/to-string

         ;; We use the identifiers from "wrappers" as our names for map/fold/etc
         accelerack/private/wrappers
         accelerack/private/types
         ; accelerack/private/racket_ops
         (only-in accelerack/private/syntax acc-array)

         (for-syntax racket/base
                     syntax/parse syntax/id-table racket/dict
                     (only-in accelerack/private/syntax acc-array)
                     accelerack/private/passes/verify-acc
                     accelerack/private/passes/typecheck
                     accelerack/private/types
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
  ;; Returns three values.
  (define (front-end-compiler e)
    (define syn-table (snap-as-list))
    (when (accelerack-debug-mode?)
      (fprintf (current-error-port)
               "\nInvoking compiler front-end, given syntax table: ~a\n"
               (map (lambda (x) (list (syntax->datum (car x)) (syntax->datum (cdr x))))
                    syn-table)))
    (define stripped (verify-acc syn-table e))
    ; (printf "Woo compiler frontend! ~a\n" e)
    (define-values (main-type with-types) (typecheck-expr syn-table e))
    (values stripped main-type with-types))

  (define (extend-syn-table name type expr)
    (define entry (acc-syn-entry type expr))
    (set-box! acc-syn-table
              (dict-set (unbox acc-syn-table) name entry)))

  (define (lookup-acc-type name)
    (acc-syn-entry-type (dict-ref (unbox acc-syn-table) name)))

  (define (lookup-acc-expr name)
    (acc-syn-entry-expr (dict-ref (unbox acc-syn-table) name)))

  (define (is-a-lambda? stx)
    (syntax-parse stx
      #:literals (lambda)
      [(lambda rest ...) #t]
      [else #f]))
  )

;; Surprisingly, this form of persistence changes the VALUES of the
;; hash into normal lists, but not the keys.
(define-syntax (snapshot-current-acc-syn-table stx)
  (syntax-parse stx [(_) (snap-as-syntax)]))

;; --------------------------------------------------------------------------------

;; Not doing this yet until we sort out the story of where/how the
;; captured syntax would be stored.
;;
;; (define-syntax (acc stx)
;;   (syntax-parse stx
;;     [(_ e) (front-end-compiler #'e)]))

;; Type-checking happens at expansion time.
(define-syntax (run-gpu stx)
  (syntax-parse stx
    [(_ e) #`(launch-accelerack-ast #,(front-end-compiler #'e))]))

;; TODO: Need to defer execution ...
(define-syntax (define-acc stx)

  (define (go name bod)
    (let-values ([(stripped mainTy withTys) (front-end-compiler bod)])
      (extend-syn-table name mainTy withTys)

      ;; Expand into the stripped version with no types:

      ;; HACK: fix this to the second alternative after typechecking works:
      (if (is-a-lambda? bod)
          #`(define #,name #,stripped)
          #`(define #,name (acc-delayed-array (lambda () #,stripped))))
#;
      (cond
        [(is-array-type? mainTy)
         ;; Deferred execution:
         #`(define #,name (acc-delayed-array (lambda () #,stripped)))]
        [(is-function-type? mainTy)
         ;; If it's a function but not immediately a lambda, that's
         ;; sort of a weird case for us.
         (unless (is-a-lambda? bod)
           (raise-syntax-error
            'error "Accelerack implementation does not yet support functions that are not immediately lambdas" bod)
           #`(define #,name #,stripped))]
        [else
         ;(raise-syntax-error
         ; 'error "FINISHME: Accelerack needs to handle deferred scalar computations" bod)
         #`(define #,name (acc-delayed-scalar (lambda () #,stripped)))])

      ))

  ;; Infers the type of the given expression and adds that type and
  ;; the expression to the syntax table.
  (syntax-parse stx
    [(_ x:identifier e)                     (go #'x #'e)]
    [(_ (f:identifier x:identifier ...) e)  (go #'f #'(lambda (x ...) e))]))


; --------------------------------------------------------------------------------
