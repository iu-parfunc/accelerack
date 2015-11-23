#lang racket

;; This file provides the Racket-side implementation of core Accelerate operators.

;; These operations are not necessarily 

(require (except-in ffi/unsafe ->)
         ffi/unsafe/cvector
         accelerack/private/acc_header
         accelerack/private/acc_allocate
         accelerack/private/acc_arrayutils
         accelerack/private/acc_syntax
         (only-in '#%foreign ctype-scheme->c ctype-c->scheme)
         racket/contract
         )

(provide 
 (contract-out 
  [acc-map (-> procedure? acc-array? acc-array?)]
  [acc-zipwith (-> (-> number? number? number?) acc-array? acc-array? acc-array?)]
  [add (-> number? number? number?)]
  [sub (-> number? number? number?)]
  [mult (-> number? number? number?)]
  [div (-> number? number? number?)]
  [acc-fold (-> (->* (number?) () #:rest (listof number?) number?) number? acc-array? acc-array?)])
 )

;; Eventually: must take acc-manifest-array? or acc-deferred-array?

;; "arraySize" in Accelerate.  Convert camel case to hyphens:
(define (array-size arr) 
  (md-array-length (shape arr)))

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

;; add two numbers
(define (add x y)
  (+ x y))

;; subtract two numbers
(define (sub x y)
  (- x y))

;; multiply two numbers
(define (mult x y)
  (* x y))

;; divide two numbers
(define (div x y)
  (/ x y))


;; Find the length of the row in a payload
;; Arguments -> shape
;; Return value -> length

(define (row-length shape)
  (cond
    ((equal? 1 (length shape)) (car shape))
    ((null? (cadr shape)) (car shape))
    (else (row-length (cdr shape)))))


;; Perform zipwith accelerate function
;; Arguments -> binary function, reference to array 1,reference to array 2
;; Return value -> result array

(define (acc-zipwith fn arr1 arr2)
  (letrec ([type* (if (equal? ((ctype-scheme->c scalar) 'acc-payload-ptr) (type arr1)) (get-tuple-type (unzip (readData* arr1)) (shape arr1)) (mapType (type arr1)))]
           [shape* (find-shape (shape arr1) (shape arr2) '())]
           [temp* (car (alloc-unit shape* type*))]
           [len (array-size temp*)]
           [rlen1 (if (null? (shape arr1)) 1 (row-length (shape arr1)))]
           [rlen2 (if (null? (shape arr2)) 1 (row-length (shape arr2)))]
           [tlen  (if (null? (shape temp*)) 1 (row-length (shape temp*)))])
          (begin
            (zipwith-helper temp* arr1 arr2 0 0 0 0 1 1 1 1 tlen rlen1 rlen2 0 0 len fn (cdr (reverse (shape temp*))) (reverse (shape arr1)) (reverse (shape arr2)))
            temp*)))

(define (skip-by ls i itr itr*)
  (cond
    ((equal? i itr) itr*)
    (else (* (list-ref ls itr) (skip-by ls i (add1 itr) itr*)))))

;; Helper function for acc-zipwithh
;; Arguments -> reference to result array , reference to array 1, reference to array 2, index for result array, index for array 1,
;;              index for array 3, row length of result array, row length of array 1, row length of array 2, helper to increment array 1 index,
;;              helper to increment array 2 index, length of result array, binary function
;; Return value -> empty list / [side effect - sets the temp array]

(define (zipwith-helper temp arr1 arr2 i j k itr1 itr2 itr3 itr2* itr3* tlen rlen1 rlen2 t1 t2 len fn shp1 shp2 shp3)
  (cond
    ((equal? i len) '())
    ((zero? (remainder (+ i 1) tlen))
       (if (and (not (null? shp1)) (equal? (car shp1) (add1 itr1)))
           (begin
             (array-set!! temp i (fn (array-get arr1 j) (array-get arr2 k)))
             (zipwith-helper temp arr1 arr2 (add1 i) (skip-by shp2 (add1 itr2) 0 itr2*) (skip-by shp3 (add1 itr3) 0 itr3*) 0 (add1 itr2) (add1 itr3) (add1 itr2*) (add1 itr3*)
                             tlen rlen1 rlen2 (skip-by shp2 itr2 0 itr2*) (skip-by shp3 itr3 0 itr3*) len fn (cdr shp1) shp2 shp3))
           
           (begin
             (array-set!! temp i (fn (array-get arr1 j) (array-get arr2 k)))
             (zipwith-helper temp arr1 arr2 (add1 i) (+ t1 rlen1) (+ t2 rlen2) (add1 itr1) itr2 itr3 itr2* itr3* tlen rlen1 rlen2 (+ t1 rlen1) (+ t2 rlen2) len fn shp1 shp2 shp3))))
    (else (begin
            (array-set!! temp i (fn (array-get arr1 j) (array-get arr2 k)))
            (zipwith-helper temp arr1 arr2 (add1 i) (add1 j) (add1 k) itr1 itr2 itr3 itr2* itr3* tlen rlen1 rlen2 t1 t2 len fn shp1 shp2 shp3)))))

(define (add-ls ls)
  (cond
    ((null? ls) 0)
    (else (+ (car ls) (add-ls (cdr ls))))))

(define (mult-ls ls)
  (cond
    ((null? ls) 0)
    (else (+ (car ls) (mult-ls (cdr ls))))))


(define (acc-fold func def arr)
  (letrec ([type* (if (equal? ((ctype-scheme->c scalar) 'acc-payload-ptr) (type arr)) (error 'acc-fold "fold cannot be used on tuples") (mapType (type arr)))]
           [shape* (if (null? (shape arr)) '(1) (reverse (cdr (reverse (shape arr)))))] 
           [temp (car (alloc-unit shape* type*))]
           [len (array-size temp)]
           [rlen (if (null? (shape arr)) 1 (row-length (shape arr)))])
          (begin
            (acc-fold-helper func def arr temp len rlen 0 0)
            temp)))

(define (acc-fold-helper func def arr res len rlen i j)
  (cond
    ((equal? i len) '())
    (else (begin
            (array-set!! res i (apply-func func def arr j (+ j rlen)))
            (acc-fold-helper func def arr res len rlen (add1 i) (+ j rlen))))))

(define (apply-func func def arr i j)
  (cond
    ((equal? i j) def)
    (else (func (array-get arr i) (apply-func func def arr (add1 i) j)))))
