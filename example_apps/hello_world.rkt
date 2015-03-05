#lang racket

;; TODO: replace with proper collections paths:
(require "../lib/accelerack.rkt")

;; Run a simple program in parallel:
(run-acc 
 (generate 10 (lambda (x) x))
 )

;; This will be hidden in the future:
acc-syn-table
