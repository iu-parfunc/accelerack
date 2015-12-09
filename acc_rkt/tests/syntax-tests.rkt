#lang racket

(require rackunit
         ;; See NOTE below:
         (only-in accelerack/private/wrappers acc-array-ref fold map zipwith)
         accelerack/private/passes/syntax-capture
         (only-in accelerack acc-array))

(define-acc (sqr x) (* x x))
(define-acc (sqr x) y)
(define-acc ac 3)

(check-eq? ac 3)

(check-eq? 9 (sqr 3))

(define-acc num (* 3 (+ 4 (- 5 (sqrt 2)))))

(define-acc id1 (lambda (x) x))

;; TODO: unit tests for syntax failures:
;; ----------------------------------------
(define y 999)
; (define-acc id2 (lambda (x) y))

; (define-acc id3 (lambda (x) completely-unbound))
;; ----------------------------------------

;; NOTE: This is a bit tricky because if, e.g., array-ref is ever
;; unbound... then we get a bad error message here:
;;
;; We should tweak verify-acc to check the identifier status of the
;; supposed special-forms like array-ref/acc-array/etc.
(define-acc test01 (lambda (x) (acc-array-ref (if #t x x) 0 3)))

;; Not working yet, need to sort out first class primitive handling:

(define-acc test02 (lambda (x)
                     (zipwith + ; * ;; FIXME: * does not work here... huh?
                              (map add1 (fold + 0 (acc-array (1 2 3 4))))
                              (acc-array (10 10 10 10)))
                     ))

(define table (persist-current-acc-syn-table))
