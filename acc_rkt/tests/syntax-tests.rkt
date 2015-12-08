#lang racket

(require rackunit
         accelerack/private/passes/syntax-capture)

(define-acc (sqr x) (* x x))
(define-acc ac 3)

(check-eq? ac 3)

(check-eq? 9 (sqr 3))

(define-acc num (* 3 (+ 4 (- 5 (add1 2)))))

; (symbol? (caar table))
; (syntax? (caar table))

;; (hash? table)
;; (map syntax? (hash-keys table))
;; (map syntax? (hash-values table))

;; (define k (car (hash-keys table)))
;; (printf "Using key: ~a\n " (syntax->datum k))
;; (define v (hash-ref table k))
;; (printf "Got value: ~a, list? ~a, syntax? ~a\n"
;;         v (list? v) (syntax? v))


(define table (persist-current-acc-syn-table))
