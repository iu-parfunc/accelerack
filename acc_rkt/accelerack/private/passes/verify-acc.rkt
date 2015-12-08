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
(define (verify-acc stx) (verify-acc-helper stx '()))

(define (verify-acc-helper stx env)
  (let loop ((stx stx))
    (syntax-parse stx
      ;; TODO: use literal-sets:
     #:literals (acc-array map zipwith fold stencil3x3 lambda let if array-ref)

     [n:number  #'n]
     [b:boolean #'b]

     ;; FIXME: use the acc-data syntax class:
     [(acc-array dat) #'(acc-array dat)]
     ;; Variable arity array-ref.  I.e. it's a special form:
     [(array-ref e1 e2s ...)
      #`(array-ref #,(loop #'e1) #,@(map loop (syntax->list #'(e2s ...))))]

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

     [(p:accelerack-primitive-function e ...)
      #`(p #,@(map loop (syntax->list #'(e ...))))]

     ;; Problems with this causing bad cases to end up in the identifier case!!!
     #;
     [p:accelerack-primitive-function
      #'p]

     ;; Having problems [2015.12.08]:
     #; [(rator e ...)
      #`(#,(loop #'rator) #,@(map loop (syntax->list #'(e ...))))]

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

     )))


(define (verify-type ty)
  (error 'verify-type "FINISHME"))
