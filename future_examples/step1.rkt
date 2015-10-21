#lang racket

(require accelerack)
; (require "accelerack.rkt") ;; Dummy exports to run this.
(require rackunit)

;; Debatable whether this should be called "array" or "acc-array"
;; It may be a good idiom to import the module with prefix "acc-"
(array 99)        ;; Zero dim array.
(array (1 2 3 4)) ;; One dim array
(array #(3 #f))   ;; Zero dim array of tuple.

(define x (array ((2 3)
                  (4 5)))) ;; Two dim array

(check-true (array? x))

;; Run Racket-side array computation:
(map add1 x)

;; But we don't lose map's normal functionality on lists:
(map add1 '(9 10 11))
