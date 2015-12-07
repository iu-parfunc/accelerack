#lang racket

;; This file provides the Racket-side implementation of core Accelerate operators.

;; These operations are not necessarily 

(require (only-in ffi/unsafe _int))
(require 
         ; ffi/unsafe/cvector
         accelerack/src/acc_header
         accelerack/src/acc_allocate
         accelerack/src/acc_arrayutils
         accelerack/src/acc_syntax
         ; (only-in '#%foreign ctype-scheme->c ctype-c->scheme)
         racket/contract
         )

(provide 
 (contract-out 
  [acc-map (-> procedure? acc-array? acc-array?)])
 )

(define x (car (array () _int 99)))
(define y (car (array (2) _int (98 99))))

;; Eventually: must take acc-manifest-array? or acc-deferred-array?

;; "arraySize" in Accelerate.  Convert camel case to hyphens:
(define (array-size arr) 
  (md_array-length (shape arr)))

(define (array-set!! arr ind val) (error 'finishme ""))
(define (!! arr ind) (error 'finishme ""))
(define (type arr) (acc-array-type arr))

(define (shape arr) 
  (readData (acc-array-shape arr)))

(define (acc-map fn arr)  
  ;; The acc-array is not mutable for end users, but for this library implementation 
  ;; we leverage a mutable representation internally.
  (let ([len (array-size arr)]
        [temp (acc_alloc (shape arr) (type arr))])
    ;; (assert (acc-array? temp))
    (for ([i (in-range 0 (sub1 len))])
      (array-set!! temp i (fn (!! arr i))))
    temp))

;; (define (generate sh fn) ...)

;; (define (fold ...) )

;; ---------------------------------------------
;; Temporary tests:

(readData* (acc-map add1 x))

(readData* (acc-map (lambda (x) x) y))

