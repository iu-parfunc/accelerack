#lang racket

;; This file provides the Racket-side implementation of core Accelerate operators.

;; These operations are not necessarily 

(require (except-in ffi/unsafe ->)
         ffi/unsafe/cvector
         accelerack/src/acc_header
         accelerack/src/acc_allocate
         accelerack/src/acc_arrayutils
         accelerack/src/acc_syntax
         (only-in '#%foreign ctype-scheme->c ctype-c->scheme)
         racket/contract
         )

(provide 
 (contract-out 
  [acc-map (-> procedure? acc-array? acc-array?)])
 )

;; Eventually: must take acc-manifest-array? or acc-deferred-array?

;; "arraySize" in Accelerate.  Convert camel case to hyphens:
(define (array-size arr) 
  (md_array-length (shape arr)))

;;(define (array-set!! arr ind val) (error 'finishme ""))
;;(define (!! arr ind) (error 'finishme ""))

;; returns the type of the given acc array
(define (type arr)
  (if (acc-array? arr) (segment-type (acc-array-data arr)) (segment-type arr)))

;; returns the shape of the given acc array
(define (shape arr) 
  (readData (acc-array-shape arr)))

;; returns the length of the given acc array
(define (acc-length arr) 
  (if (acc-array? arr) (segment-length (acc-array-data arr)) (segment-length arr)))

;; Execute the given function over the given acc array
;; Arguments -> input function, reference to the acc array
;; Return value -> reference to result array

(define (acc-map fn arr)  
  ;; The acc-array is not mutable for end users, but for this library implementation 
  ;; we leverage a mutable representation internally.
  (letrec ([len (array-size arr)]
           [type* (if (equal? ((ctype-scheme->c scalar) 'acc-payload-ptr) (type arr)) (get-tuple-type (unzip (readData* arr)) (shape arr)) (mapType (type arr)))]
           [temp (car (alloc-unit (shape arr) type*))])
    ;; (assert (acc-array? temp))
    (if (equal? ((ctype-scheme->c scalar) 'acc-payload-ptr) (type arr))
        (begin (tuple-array-set!! (acc-array-data temp) (acc-array-data arr) fn) temp)
        (begin
          (for ([i (in-range 0 len)])
            (array-set!! temp i (fn (array-get arr i))))
          temp))))

;; Set the value at given position in an acc array
;; Arguments -> reference to the acc array, offset, the value to set 
;; Return value -> void 

(define (array-set!! arr-ref offset value)
  (let ([type (mapType ((ctype-scheme->c scalar) (get-ctype value)))]
        [data (if (acc-array? arr-ref) (segment-data (acc-array-data arr-ref)) (segment-data arr-ref))])
    (ptr-set! data type offset value)))

;; Get the value at given position in an acc array
;; Arguments -> reference to the acc array, offset 
;; Return value -> value at given position

(define (array-get arr-ref offset)
  (let ([type (mapType (type arr-ref))]
        [data (if (acc-array? arr-ref) (segment-data (acc-array-data arr-ref)) (segment-data arr-ref))])
    (ptr-ref data type offset)))
                         
;; Execute the given function over the given acc tuple data
;; Arguments -> reference to result acc array ,reference to the input acc array, input function  
;; Return value -> void

(define (tuple-array-set!! arr-ref input-arr fn)
  (let ([acc-tuple ((ctype-scheme->c scalar) 'acc-payload-ptr)]
        [acc-int ((ctype-scheme->c scalar) 'c-int)]
        [acc-double ((ctype-scheme->c scalar) 'c-double)])
       (cond 
         [(equal? (type input-arr) acc-tuple) (let ([len (acc-length input-arr)])
                                                   (for ([i (in-range 0 len)])
                                                        (tuple-array-set!!
                                                          (ptr-ref (segment-data arr-ref) _segment-pointer i)
                                                          (ptr-ref (segment-data input-arr) _segment-pointer i)
                                                          fn)))]
         [(or (equal? (type input-arr) acc-int)
              (equal? (type input-arr) acc-double)) (let ([len (acc-length input-arr)])
                                                         (for ([i (in-range 0 len)])
                                                              (array-set!! arr-ref i (fn (array-get input-arr i)))))])))
