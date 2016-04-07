#lang info

(define collection 'multi)

(define compile-omit-paths
  (list "acc_hs"
        "acc_c"
        "acc_rkt/tests_disabled"))

(define test-omit-paths compile-omit-paths)

