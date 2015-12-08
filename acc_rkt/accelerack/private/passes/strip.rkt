#lang racket

;; ---------------------------------------------------------------
;; Defines a compiler pass which:
;;  * strips the syntax-object AST representation down to an SExp
;; ---------------------------------------------------------------

(provide (contract-out
          [strip-ast (-> syntax? list?)]
          ))

;; Simple.  Just a placeholder.
(define strip-ast syntax->datum)
