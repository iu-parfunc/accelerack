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
         racket/trace
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
     #:literals (map zipwith fold lambda)
     ; (acc-array map zipwith fold lambda + *)
     ; [(acc-array dat) #'(acc-array dat)]
     [(map f e) #`(map #,(loop #'f) #,(loop #'e))]

     [(lambda (x:identifier ...) e)
      #`(lambda (x ...) #,(verify-acc-helper
                           #'e (append (syntax->list #'(x ...)) env)))]

     [(p:accelerack-primitive-function e ...)
      #`(p #,@(map loop (syntax->list #'(e ...))))]

     [x:identifier
      ; (printf "Handling identifier: ~a ~a\n" #'x (identifier-binding #'x))
      (cond
        [(ormap (lambda (id) (free-identifier=? id #'x)) env) #'x]
        [(not (identifier-binding #'x))
         (raise-syntax-error 'error
                             "undefined variable used in Accelerack expression."
                             #'x)]
        [else
         (raise-syntax-error 'error
                             (format "\n Regular Racket variable used Accelerack expression without (use ~a)"
                                     (syntax->datum #'x))
                             #'x)])]

    [n:integer #'n]
    )))
