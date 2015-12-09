#lang racket
;; For scribble "at-exp  racket"

;; ---------------------------------------------------------------
;; The syntax-capture and verification step.
;; ---------------------------------------------------------------

(provide
 ; (proc-doc verify-acc (-> (syntax? list?) syntax?) "test")
 (contract-out
  [verify-acc (-> list? syntax? syntax?)]
  ))

(require ; (for-syntax (except-in racket/base map))
         syntax/parse
         syntax/to-string
         scribble/srcdoc
         racket/trace
         (only-in accelerack/private/global_utils pass-output-chatter)
         (only-in accelerack/private/syntax accelerack-primitive-function)
         )
(require ;; We use the identifiers from "wrappers" as our names for map/fold/etc
 (for-meta -1
           accelerack/private/wrappers
           (only-in racket/base lambda let #%app if + * - / add1 sub1)
           (only-in accelerack/private/syntax acc-array))
         )

;  @proc-doc[ verify-acc any/c ]{ The identity compiler pass that simply checks the grammar. }
; (proc-doc verify-acc (-> any/c) () "The identity compiler pass that simply checks the grammar.")

;; The identity compiler pass that simply checks the grammar.
;;
;; This must precisely follow the spec in accelerate_grammar.txt
(define (verify-acc syn-table stx)
  (define initial-env (map car syn-table))
  (define res (verify-acc-helper stx initial-env))
  (pass-output-chatter 'verify-acc res)
  res)

(define (verify-acc-helper stx env)
  (let loop ((stx stx))
    (syntax-parse stx
      ;; TODO: use literal-sets:
     #:literals (acc-array map zipwith fold stencil3x3 lambda let if acc-array-ref)

     [n:number  #'n]
     [b:boolean #'b]

     ;; FIXME: use the acc-data syntax class:
     [(acc-array dat) #'(acc-array dat)]
     ;; Variable arity array dereference.  I.e. it's a special form:
     [(acc-array-ref e1 e2s ...)
      #`(acc-array-ref #,(loop #'e1) #,@(map loop (syntax->list #'(e2s ...))))]

     ;; Here these array ops are treated as special forms, not functions.
     [(map f e) #`(map #,(loop #'f) #,(loop #'e))]
     [(zipwith f e1 e2) #`(zipwith #,(loop #'f) #,(loop #'e1) #,(loop #'e2))]
     [(fold f e1 e2)    #`(fold    #,(loop #'f) #,(loop #'e1) #,(loop #'e2))]
     [(stencil3x3 f e1 e2) #`(stencil3x3 #,(loop #'f) #,(loop #'e1) #,(loop #'e2))]

     [(if e1 e2 e3) #`(if #,(loop #'e1) #,(loop #'e2) #,(loop #'e3))]

     [#(e* ...)
      ; #`#( #,@(map loop (syntax->list #'(e* ...))) )
      (datum->syntax #'(e* ...) (list->vector (map loop (syntax->list #'(e* ...)))))
      ]

     [(lambda (x:identifier ...) e)
      #`(lambda (x ...) #,(verify-acc-helper
                           #'e (append (syntax->list #'(x ...)) env)))]

     [(let ([x*:identifier e*] ...) ebod)
      (define xls (syntax->list #'(x* ...)))
      (define els (syntax->list #'(e* ...)))
      #`(let ([x* #,(map loop els)] ...)
          #,(verify-acc-helper
             #'ebod (append xls env)))]

     ;; We have to to be careful with how we influence the back-tracking search performed by
     ;; syntax-parse.
     [(p:accelerack-primitive-function e ...)
      #`(p #,@(map loop (syntax->list #'(e ...))))]

     [(rator e ...)
      #`(#,(loop #'rator) #,@(map loop (syntax->list #'(e ...))))]

     [p:accelerack-primitive-function #'p]

     [(~and x:id (~fail #:unless (ormap (lambda (id) (free-identifier=? id #'x)) env)
                         "identifier with an accelerack type"))
       #'x]

     [x:identifier
      #:fail-unless (identifier-binding #'x)
      ; "undefined variable used in Accelerack expression, should be bound"
      "expected bound variable in Accelerack expression"
      ; (printf "Handling identifier: ~a ~a\n" #'x (identifier-binding #'x))
      (raise-syntax-error 'error
                          (format "\n Regular Racket bound variable used in Accelerack expression.\n If it is an array variable, maybe you meant (use ~a) ?"
                                  (syntax->datum #'x))
                          #'x)]
     )))


(define (verify-type ty)
  (error 'verify-type "FINISHME"))
