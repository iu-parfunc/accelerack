#lang racket
;; For scribble "at-exp  racket"

;; ---------------------------------------------------------------
;; The syntax-capture and verification step.
;; ---------------------------------------------------------------

(provide
 ; (proc-doc verify-acc (-> (syntax? list?) syntax?) "test")
 (contract-out
  [verify-acc (-> syntax? syntax?)]
  ))

(require ; (for-syntax (except-in racket/base map))
         syntax/parse
         syntax/to-string
         scribble/srcdoc
         )
(require ;; We use the identifiers from "wrappers" as our names for map/fold/etc
         (for-meta -1 accelerack/private/wrappers)
         (for-meta -1 (only-in racket/base lambda + * #%app))
         accelerack/private/syntax
         )

;  @proc-doc[ verify-acc any/c ]{ The identity compiler pass that simply checks the grammar. }
; (proc-doc verify-acc (-> any/c) () "The identity compiler pass that simply checks the grammar.")


;; The identity compiler pass that simply checks the grammar.
;;
;; This must precisely follow the spec in accelerate_grammar.txt
(define (verify-acc stx) (verify-acc-helper stx '()))

(define (verify-acc-helper stx env)
  (let loop ((stx stx))
   (syntax-parse stx
     #:literals (map zipwith fold lambda + *)
     ; (acc-array map zipwith fold lambda + *)
    ; [(acc-array dat) #'(acc-array dat)]
    [(map f e) #`(map #,(loop #'f) #,(loop #'e))]

    [(lambda (x:identifier ...) e)
     #`(lambda (x ...) #,(verify-acc-helper #'e (cons #'e env)))]

    ;; TODO: syntax class for primitive functions:
    [(* e1 e2) #`(* #,(loop #'e1) #,(loop #'e2))]

    [(p:acc-primop e ...) #`(p #,@(map loop (syntax->list #'(e ...))))]

    [x:identifier #'x] ;; TODO: check that it's bound...
    [n:integer #'n]
    )))
