#lang info

;; omit the following paths from testing during 'raco test'
 
(define test-omit-paths (list "info.rkt" "accelerack/main.rkt" "tests/ffi-tests.rkt"))
