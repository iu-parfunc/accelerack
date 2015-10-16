#lang racket


(define-syntax-rule (acc a-exp)
  a-exp)

(acc (map sqr '(4 5 6)))

;expose constructor
;  via C wrapper
; return C-ptr to a pinned object
;pin value from constructor
; 

;gc issues

;racket-ffi can read header files?
; HsFFI.h

(begin
  (display 5)
  )

6