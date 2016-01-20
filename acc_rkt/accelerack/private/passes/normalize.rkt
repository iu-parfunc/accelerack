#lang racket

;; ---------------------------------------------------------------
;; Defines a compiler pass which:
;;  (1) All array operators are directly followed by lambda
;;  (2) Should never throw errors hence can directly operate on sexp
;;      rather than rich syntax expression
;; ---------------------------------------------------------------

(require
 rackunit
 rackunit/text-ui         
 racket/match)
(require macro-debugger/expand)


(require racket/trace)
(require (for-syntax racket/trace))
(require accelerack/private/wrappers)
(require
 (for-syntax (except-in racket/base map))
 syntax/parse
 syntax/to-string
 scribble/srcdoc
 racket/trace
 (only-in accelerack/private/global_utils pass-output-chatter)
 accelerack/private/syntax
 (prefix-in r: racket/base))

(require (for-syntax syntax/parse))

(provide (contract-out
          (normalize (-> syntax? syntax?))))
(require ;; We use the identifiers from "wrappers" as our names for map/fold/etc
 (for-template
  accelerack/private/wrappers
  
  ;; Keyword symbols come from a mix of three places currently:
  (only-in accelerack/private/syntax acc-array)
  (only-in racket/base lambda let #%app if + * - / add1 sub1 vector vector-ref)
  (only-in accelerack/private/keywords : Array Int Bool Double use ->))

 ;; Temp: at every stage to make sure:
 ;(for-syntax (only-in accelerack/private/syntax :))
 ;(only-in accelerack/private/syntax :)
 )

(define (con s)
  (cond
    ((syntax? s) (syntax->datum s))
    ((list? s) (map con s))
    (else s)))

(define (concat xls yls)
  (let ((xls (con  xls))
        (yls (con yls)))    
    (cond
      ((null? xls) xls)
      (else (cons `(,(cadar xls) ,(cadar yls)) (concat (cdr xls) (cdr yls)))))
    ))

;; Implemented using syntax parse -- Should this be done using match ?
(define (normalize stx)  
  (let loop ((stx stx))
    (syntax-parse stx
      #:literals (acc-array acc-array-ref :
                            map zipwith fold stencil3x3 generate
                            lambda let if vector vector-ref)
      #:disable-colon-notation
      [(it (lambda(x...) e) e1...) #:when #`(memq #`it '(map zipwith)) #`(it (lambda(x...) e) e1...)]
      [(map f e) #`(map (lambda (x)  (#,(loop #'f) x))  #,(loop #'e))]
      [(zipwith e1 e2 e3) #`(zipwith (lambda(x y) (#,(loop #'e1) x y))
                                     #,(loop #'e2) #,(loop #'e3))]
      [((lambda(x ...) e) e1 ...) (let ((k (concat  #`(#'x ...) #`(#'e1 ...)))
                                        (e (loop #'e)))
                                    #`(let #,(datum->syntax #f k) #,(datum->syntax #f e)))]
      [n  #'n]
      )))

;; (display (syntax->datum (normalize (normalize (normalize #'(map add1 (list 12 3)))))))
;; (display "\n")
;; (display (syntax->datum (normalize #'((lambda(x y) (map x y)) add1 (list 1 2)) )))
;; (display "\n")
;; (display (syntax->datum (normalize #'((lambda (x y) (map x y)) add1 1) )))
;; (display "\n")
;; (display (syntax->datum (normalize #'(map x y) )))
