#lang racket

(provide x)

(require accelerack)
(require rackunit)

; (define-acc (sum x) (fold + 0 x))

(define-acc sum
  (: (lambda (x) (fold + 0 x))
     (-> (Array (add1 d) (Num t))
         (Array d        (Num t)))))

;; (define-acc (sum2 [ x : (Array (add1 d) (Num t)) ])
;;   : (Array d (Num t))
;;   (fold + 0 x))

;; Ideally we could have Haskell-style separate types:
;; (: sum
;;     (-> (Array (add1 d) (Num t))
;;         (Array d        (Num t))))

(check-equal? 2 (car (acc-array->sexp y)))
