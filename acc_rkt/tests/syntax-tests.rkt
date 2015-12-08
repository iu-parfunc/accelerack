#lang racket

(require rackunit
         accelerack/private/passes/syntax-capture)

(define-acc (sqr x) (* x x))
(define-acc ac 3)

(check-eq? ac 3)

(check-eq? 9 (sqr 3))

(define-acc num (* 3 (+ 4 (- 5 (sub1 2)))))

(define-acc id1 (lambda (x) x))


;; TODO: unit tests for syntax failures:
;; ----------------------------------------
(define y 999)
; (define-acc id2 (lambda (x) y))

; (define-acc id3 (lambda (x) completely-unbound))
;; ----------------------------------------


(define table (persist-current-acc-syn-table))
