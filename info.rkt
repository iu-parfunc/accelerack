#lang info

(define collection 'multi)

(define compile-omit-paths
  (list "acc_hs"
        "acc_c"
;        "accelerack/tests_disabled"
        "info.rkt"))

(define test-omit-paths compile-omit-paths)

(define deps '("c-defs"
               ))
(define scribblings '(("manual.scrbl" ())))
