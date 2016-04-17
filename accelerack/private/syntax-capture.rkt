#lang racket

;; ---------------------------------------------------------------
;; The syntax-capture and verification step.
;; ---------------------------------------------------------------

(provide define-acc
         ; acc ;; Not exposing this yet.
         run-gpu
         snapshot-current-acc-syn-table)

(require
         ;; We use the identifiers from "wrappers" as our names for map/fold/etc
         accelerack/private/wrappers
         accelerack/private/types
         ; accelerack/private/racket_ops
         (only-in accelerack/private/syntax acc-array :)
         (only-in accelerack/acc-array/private make-acc-array)
         accelerack/acc-array/private/delayed
         (for-syntax racket/base
                     syntax/parse syntax/id-table racket/dict
                     (only-in accelerack/private/syntax acc-array)
                     accelerack/private/passes/verify-acc
                     accelerack/private/passes/typecheck
                     accelerack/private/types
		     accelerack/private/front-end
                     (only-in accelerack/private/executor launch-accelerack-ast)
                     (only-in accelerack/private/utils accelerack-debug-mode?)
                     rackunit
                     ))

(begin-for-syntax
  (define (is-a-lambda? stx)
    (syntax-parse stx
      #:literals (lambda)
      [(lambda rest ...) #t]
      [else #f]))

  (define (go name maybeType bod)
    (let-values ([(stripped inferredTy progWithTys) (front-end-compiler bod)])
      (apply-to-syn-table maybeType inferredTy name progWithTys)
      ;; Expand into the stripped version with no types:
      ;; HACK: fix this to the second alternative after typechecking works:
      (if (is-a-lambda? bod)
          #`(define #,name  #,stripped)
          #`(define #,name (make-acc-array (acc-delayed-array (lambda () #,stripped)))))
      #;
      (cond
        [(acc-scalar-type? inferredTy) #`(define #,name #,bod)]
        ;; FIXME once type check works
        [(is-a-lambda? bod) #`(define #,name  #,stripped)]
        [else #`(define #,name (make-acc-array (acc-delayed-array (lambda () #,stripped))))])
          ;; TODO: Need support for delayed scalars:
          ; #`(define #,name (make-acc-scalar (acc-delayed-scalar (lambda () #,stripped))))
      ;; Lets use this once type check starts working and remove the if above
      ;; (cond
      ;;   [(is-a-lambda? bod) #`(define #,name (make-acc-array #,stripped))]
      ;;   [(is-array-type? mainTy)
      ;;    ;; Deferred execution:
      ;;    #`(define #,name (make-acc-array (acc-delayed-array (lambda () #,stripped))))]
      ;;   [(is-function-type? mainTy)
      ;;    ;; If it's a function but not immediately a lambda, that's
      ;;    ;; sort of a weird case for us.
      ;;    (begin
      ;;      (raise-syntax-error
      ;;       'error "Accelerack implementation does not yet support functions that are not immediately lambdas" bod)
      ;;      #`(define #,name (make-acc-array #,stripped)))]
      ;;   [else
      ;;    ;(raise-syntax-error
      ;;    ; 'error "FINISHME: Accelerack needs to handle deferred scalar computations" bod)
      ;;    #`(define #,name (make-acc-array (acc-delayed-scalar (lambda () #,stripped))))])
      ))

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
  ;; Infers the type of the given expression and adds that type and
  ;; the expression to the syntax table.
  (syntax-parse stx
    ;; TODO: allow acc-lambda-param, not just identifier:
    [(_ x:identifier e)                     (go #'x #f #'e)]
    [(_ x:identifier : t:acc-type e)        (go #'x #'t #'e)]
    [(_ (f:identifier x:identifier ...) e) (with-handlers
                                             ([exn:fail? (lambda (exn)
                                                           (raise-syntax-error 'syntax-error "synatax error" #`#,stx))])
                                             (go #'f #f #'(lambda (x ...) e)))]))


; --------------------------------------------------------------------------------
;; (define-acc x (acc-array (1 2 3)))
;; (define-acc x 1)
