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
         (only-in accelerack/private/utils pass-output-chatter)
         (only-in accelerack/private/prim-table acc-all-bound-syms)
         accelerack/private/syntax
         accelerack/private/syntax-table
         (prefix-in r: racket/base)
         )
(require ;; We use the identifiers from "wrappers" as our names for map/fold/etc
 (for-template
  accelerack/private/wrappers
  ;; Keyword symbols come from a mix of three places currently:
  (only-in accelerack/private/syntax acc-array acc-lambda-param acc-type
           acc-element-literal acc-let-bind :)
  accelerack/private/syntax-table
  (only-in racket/base lambda let #%app if + * - / add1 sub1 vector vector-ref)
  (only-in accelerack/private/keywords Array Int Bool Double use ->))

 ;; Temp: at every stage to make sure:
 ;(for-syntax (only-in accelerack/private/syntax :))
 ;(only-in accelerack/private/syntax :)
 )

;  @proc-doc[ verify-acc any/c ]{ The identity compiler pass that simply checks the grammar. }
; (proc-doc verify-acc (-> any/c) () "The identity compiler pass that simply checks the grammar.")

(define (make-env ls)
  (unless (andmap identifier? ls)
    (error 'make-env "list contains non-identifiers: ~a\n" ls))
  ls)

(define (extend-env ls env)
  (unless (andmap identifier? ls)
    (error 'extend-env "list contains non-identifiers: ~a\n" ls))
  (append ls env))

;; The identity compiler pass that simply checks the grammar.
;;
;; This must precisely follow the spec in accelerate_grammar.txt
(define (verify-acc syn-table stx)
  (define initial-env (make-env (r:map car syn-table)))
  (define res (verify-acc-helper stx initial-env))
  (pass-output-chatter 'verify-acc (syntax->datum res))
  res)


;; Make sure a piece of syntax is a valid type.
;; If (verify-type s) then (acc-type? (syntax->datum s)),
;; but not the converse.
(define (verify-type ty)
  (syntax-parse ty
    [t:acc-type  (void)]
    [oth (raise-syntax-error 'Accelerack-type
                             (format "bad type expression.\nDetails: ~a\n~a" #'oth
                                     (examine-symbols #'oth))
                             #'oth)]))

;; Attempt better error messages:
(define (examine-symbols ty0)
  (define found-unbound #f)
  ;; These are the symbols that we EXPECT to be bound.
  (define syms (map syntax->datum acc-all-bound-syms))
  (define (loop ty)
    (match (syntax->list ty)
      [#f (match (syntax->datum ty)
            [(? symbol? s) #:when (memq s syms)
             (let ([a (and (identifier-binding ty) #t)]
                   [b (and (identifier-transformer-binding ty) #t)]
                   [c (and (identifier-template-binding ty) #t)])
               (set! found-unbound (or found-unbound (not a)))
               (format "  * Symbol ~a/~a/~a: ~a\n" a b c ty))]
            [else ""])]
      [(list x* ...)
       (apply string-append (map loop x*))]))
  (match (loop ty0)
    ["" ""]
    [s (string-append "\n   Each symbol below should be imported from accelerack.\n"
                      "   The a/b/c boolean flags below show whether the symbol is bound\n"
                      "   in the normal environment, transformer phase, and template phase respectively.\n\n"
                      s
                      (if found-unbound
                          ""
                          "\n   At least one symbol was UNBOUND, but should be imported from accelerack.\n"))]))


;; FIXME/TODO: compute this from the list of actual-syntax keywords:
(define acc-keywords-sexp-list
  '(lambda if let : use
    generate map zipwith fold generate stencil3x3 acc-array-ref))

;; AccelerackSyntax Env -> AccelerackSyntax
(define (verify-acc-helper stx env)
  (let loop ((stx stx))
    (syntax-parse stx
      ;; TODO: use literal-sets:
      #:literals (acc-array acc-array-ref : use
                  map zipwith fold stencil3x3 generate
                  lambda let if vector vector-ref)

      [n:number  #'n]
      [b:boolean #'b]

      ;; Strip away ascription to yield normal Racket code:
      [(: e t:acc-type) (verify-type #'t) (loop #'e)]

      ;; For now only allowing identifiers, not arbitrary expressions.
      [(use x:id)
       (if (identifier-binding #'x)
           #'x ;; The macro expands to this for the "regular Racket" execution.
           (raise-syntax-error
            'error "Unbound variable used in Accelerack 'use'" #'stx ))]
      ;; TODO - Check for identifier binding may have flaws
      ;; Atleast catches unbound vars correctly but may work weirdly with define on accelerack stuff
      [(use x:id t) (verify-type #'t)
       (if (identifier-binding #'x)
           #'x ;; The macro expands to this for the "regular Racket" execution.
           (raise-syntax-error
            'error "Unbound variable used in Accelerack 'use'" #'stx ))]
      ;;#'x] ;; FIXME!  Check that it is bound in the syntax table.

      ;; FIXME: use the acc-data syntax class:
      [(acc-array dat) #'(acc-array dat)]
      ;; Variable arity array dereference.  I.e. it's a special form:
      [(acc-array-ref e1 e2s ...)
       #`(acc-array-ref #,(loop #'e1) #,@(r:map loop (syntax->list #'(e2s ...))))]

      ;; Here these array ops are treated as special forms, not functions.
      [(generate f es ...)
       #`(generate #,(loop #'f) #,@(r:map loop (syntax->list #'(es ...))))]

      [(map f e) #`(map #,(loop #'f) #,(loop #'e))]
      [(zipwith f e1 e2) #`(zipwith #,(loop #'f) #,(loop #'e1) #,(loop #'e2))]
      [(fold f e1 e2)    #`(fold    #,(loop #'f) #,(loop #'e1) #,(loop #'e2))]
      [(stencil3x3 f e1 e2) #`(stencil3x3 #,(loop #'f) #,(loop #'e1) #,(loop #'e2))]

      [(if e1 e2 e3) #`(if #,(loop #'e1) #,(loop #'e2) #,(loop #'e3))]

      [#(e* ...)
       ; #`#( #,@(map loop (syntax->list #'(e* ...))) )
       (datum->syntax #'(e* ...) (list->vector (r:map loop (syntax->list #'(e* ...)))))]

      ;; Method one, don't match bad params:
      [(lambda (x:acc-lambda-param ...) e)
       ;; TEMP: for now strip the types in the expansion so Racket is happy.
       #`(lambda (x.name ...)
           #,(verify-acc-helper
              #'e (extend-env (syntax->list #'(x.name ...)) env)))]
#;
      ;; Method two, match bad params so we control the error:
      [(lambda (x ...) e)
       ; (printf "VERIFY, Got LAMBDA... ~a\n" (syntax->datum stx))
       (define binds (r:map
                      (lambda (bnd)
                        (syntax-parse bnd
                          [xt:acc-lambda-param
                           #'xt.name]
                          [oth
                           (raise-syntax-error
                            'error "Bad parameter in lambda:" #'oth)]))
                      (syntax->list #'(x ...))))
       ;; TEMP: for now strip the types in the expansion so Racket is happy.
       #`(lambda #,binds #,(verify-acc-helper
                                 #'e (extend-env (syntax->list #'(x.name ...)) env)))]

      ;; In principle, this limits the ability to come up with good error messages
      ;; if, for example, a bad type is used in a lambda param.  But that's not working
      ;; well now anyway...
      ;; ----------------------------------------
      [(lambda (x ...) )
         (raise-syntax-error 'error  "Missing body in lambda" stx)]
      [(lambda (x ...) bods ...)
         (raise-syntax-error 'error  "Too many bodies in lambda" stx)]
      [(lambda rest ...) (raise-syntax-error
                          'error  "\nBadly formed Accelerack lambda, probably fix the parameter list:" stx)]
      ;; ----------------------------------------

      [(let ( lb:acc-let-bind ...) ebod)
       (define xls (syntax->list #'(lb.name ...)))
       (define els (syntax->list #'(lb.rhs ...)))
       (define binds (for/list ([x xls] [e els])
                       (list x (loop e))))
       #`(let (#,@binds)
           #,(verify-acc-helper
              #'ebod (extend-env xls env)))]

      [(vector e ...)     #`(vector #,@(r:map loop (syntax->list #'(e ...))))]
      [(vector-ref e1 e2) #`(vector-ref #,(loop #'e1) #,(loop #'e2))]

      ;; We have to to be careful with how we influence the back-tracking search performed by
      ;; syntax-parse.
      [(p:acc-primop e ...)
       #`(p #,@(r:map loop (syntax->list #'(e ...))))]

      [(rator e ...)
       ;; It's never a good error message when we treat a keyword like a rator:
       #:when (not (memq (syntax->datum #'rator) acc-keywords-sexp-list))
       ; (printf "RECURRING ON APP, rator ~a ~a \n" #'rator (syntax->datum #'rator))
       #`(#,(loop #'rator) #,@(r:map loop (syntax->list #'(e ...))))]

      [p:acc-primop #'p]

      ;; If we somehow mess up the imports we can end up with one of the keywords UNBOUND:
      [keywd:id
       #:when (and (not (identifier-binding #'x))
                   (memq (syntax->datum #'keywd)
                         acc-keywords-sexp-list))
       (raise-syntax-error
        'error  "Accelerack keyword used but not imported properly.  Try (require accelerack)" #'keywd)]

     ;; --------------------------------------------------------------------------------
     ;; [2015.12.09] RRN: I ended up feeling I don't like these error messages as much.
     ;; In particular, I want a simple "undefined variable" error in the unbound case.
#|
     [(~and x:id (~fail #:unless (ormap (lambda (id) (free-identifier=? id #'x)) env)
                         "identifier with an Accelerack type"))
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
|#
     [x:identifier
      (cond
        [(ormap (lambda (id) (free-identifier=? id #'x)) env) #'x]
        [(not (identifier-binding #'x))
         (raise-syntax-error
          'error  "undefined variable used in Accelerack expression" #'x)]
        [else
         (raise-syntax-error
          'error
          (format "\n Regular Racket bound variable used in Accelerack expression: ~a.\n If it is an array variable, maybe you meant (use ~a)?"
                  (syntax->datum #'x) (syntax->datum #'x))
          )])]
      ;; --------------------------------------------------------------------------------
     )))
