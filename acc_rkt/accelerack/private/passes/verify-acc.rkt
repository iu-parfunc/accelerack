#lang racket

;; ---------------------------------------------------------------
;; The syntax-capture and verification step.
;; ---------------------------------------------------------------

(provide verify-acc)

(require ; (for-syntax (except-in racket/base map))
         syntax/parse
         syntax/to-string
         )
(require ;; We use the identifiers from "wrappers" as our names for map/fold/etc
         (for-meta -1 accelerack/private/wrappers)
         (for-meta -1 (only-in racket/base lambda + * #%app))
         )

;; The identity compiler pass that simply checks the grammar.
;;
;; This must precisely follow the spec in accelerate_grammar.txt
(define (verify-acc stx env)
  (let loop ((stx stx))
   (syntax-parse stx
     #:literals (map zipwith fold lambda + *)
     ; (acc-array map zipwith fold lambda + *)
    ; [(acc-array dat) #'(acc-array dat)]
    [(map f e) #`(map #,(loop #'f) #,(loop #'e))]

    [(lambda (x:identifier ...) e)
     #`(lambda (x ...) #,(verify-acc #'e (cons #'e env)))]

    ;; TODO: syntax class for primitive functions:
    [(* e1 e2) #`(* #,(loop #'e1) #,(loop #'e2))]

    [x:identifier #'x] ;; TODO: check that it's bound...
    [n:integer #'n]
    )))
