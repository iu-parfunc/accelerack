#lang racket

;; ---------------------------------------------------------------
;; Defines a compiler pass which:
;;  * strips the syntax-object AST representation down to an SExp
;; ---------------------------------------------------------------

(provide (contract-out
          [strip-ast (-> syntax? list?)]
          ))

(require (only-in accelerack/private/utils pass-output-chatter))

;; Simple.  Just a placeholder.
(define (strip-ast a)
  (define res (syntax->datum a))
  (pass-output-chatter 'strip-ast res)
  res)
