#lang info

;; omit the following paths from testing during 'raco test'
 
(define test-omit-paths (list "info.rkt" "accelerack/main.rkt"))
(define deps '("c-defs"
               ("base" #:version "6.2.1")))
