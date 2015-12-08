#lang racket

;; This file provides the Racket-side implementation of core Accelerate operators.

;; These operations are not necessarily 

(require (except-in ffi/unsafe ->)
         ffi/unsafe/cvector
         accelerack/private/header
         accelerack/private/allocate
         accelerack/private/arrayutils
         accelerack/private/global_utils
         ;;accelerack/private/syntax
	 ;;accelerack/private/sexp
         (only-in '#%foreign ctype-scheme->c ctype-c->scheme)
         racket/contract
         )

(provide 
 (contract-out
  [array-get (-> acc-manifest-array? exact-integer? any/c)]
  [acc-map (-> procedure? acc-manifest-array? acc-manifest-array?)]
  [acc-zipwith (-> (-> number? number? number?) acc-manifest-array? acc-manifest-array? acc-manifest-array?)]
  [acc-fold (-> (->* (number?) () #:rest (listof number?) number?) number? acc-manifest-array? acc-manifest-array?)]))

;; Eventually: must take acc-manifest-array? or acc-deferred-array?

;; "arraySize" in Accelerate.  Convert camel case to hyphens:
(define (array-size arr) 
  (md-array-length (shape arr)))

;; returns the length of the given acc array
(define (acc-length arr) 
  (if (acc-manifest-array? arr) (segment-length (acc-manifest-array-data arr)) (segment-length arr)))

;; Execute the given function over the given acc array
;; Arguments -> input function, reference to the acc array
;; Return value -> reference to result array

(define (acc-map fn arr)  
  ;; The acc-manifest-array is not mutable for end users, but for this library implementation 
  ;; we leverage a mutable representation internally.
  (letrec ([len (array-size arr)]
           [type* (if (equal? ((ctype-scheme->c scalar) 'acc-payload-ptr) (type arr))
                        (get-tuple-type (unzip (vector->list* (read-data* arr))) (shape arr))
                      (mapType (type arr)))]
           [temp (alloc-unit (shape arr) type*)])
    ;; (assert (acc-manifest-array? temp))
    (if (equal? ((ctype-scheme->c scalar) 'acc-payload-ptr) (type arr))
        (begin (tuple-array-set!! (acc-manifest-array-data temp) (acc-manifest-array-data arr) fn) temp)
        (begin
          (for ([i (in-range 0 len)])
            (array-set!! temp i (fn (array-get arr i))))
          temp))))

;; Set the value at given position in an acc array
;; Arguments -> reference to the acc array, offset, the value to set 
;; Return value -> void 

(define (array-set!! arr-ref offset value)
  (let ([type (mapType ((ctype-scheme->c scalar) (get-ctype value)))]
        [data (if (acc-manifest-array? arr-ref) (segment-data (acc-manifest-array-data arr-ref)) (segment-data arr-ref))])
    (ptr-set! data type offset value)))

;; Get the value at given position in an acc array
;; Arguments -> reference to the acc array, offset 
;; Return value -> value at given position

(define (array-get arr-ref offset)
  (let ([type (mapType (type arr-ref))]
        [data (if (acc-manifest-array? arr-ref) (segment-data (acc-manifest-array-data arr-ref)) (segment-data arr-ref))])
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


;; Find the length of the row in a payload
;; Arguments -> shape
;; Return value -> length

(define (row-length shape)
  (cond
    ((equal? 1 (length shape)) (car shape))
    ((null? (cadr shape)) (car shape))
    (else (row-length (cdr shape)))))


;; Accelerate zipwith (development) function
;; Arguments -> binary function, reference to array 1,reference to array 2
;; Return value -> result array

(define (acc-zipwith-dev fn arr1 arr2)
  (letrec ([type* (if (equal? ((ctype-scheme->c scalar) 'acc-payload-ptr) (type arr1)) (get-tuple-type (unzip (vector->list* (read-data* arr1))) (shape arr1)) (mapType (type arr1)))]
           [shape* (find-shape (shape arr1) (shape arr2) '())]
           [temp* (alloc-unit shape* type*)]
           [len (array-size temp*)]
           [new-arr1 (acc-alloc type* shape* (reshape shape* (read-data* arr1)))]
           [new-arr2 (acc-alloc type* shape* (reshape shape* (read-data* arr2)))])
          (begin
            (for ([i (in-range 0 len)])
              (array-set!! temp* i (fn (array-get new-arr1 i) (array-get new-arr2 i))))
            temp*)))

(define (reshape shp ls)
  (cond
    ((null? shp) ls)
    ((equal? (length ls) (car shp)) (map (lambda (x) (reshape (cdr shp) x)) ls))
    (else (map (lambda (x) (reshape (cdr shp) x)) (take ls (car shp))))))

(define (acc-zipwith fn arr1 arr2)
  (letrec ([type* (if (equal? ((ctype-scheme->c scalar) 'acc-payload-ptr) (type arr1)) (get-tuple-type (unzip (vector->list* (read-data* arr1))) (shape arr1)) (mapType (type arr1)))]
           [shape* (if (equal? (shape arr1) (shape arr2)) (shape arr1) (error 'acc-zipwith "shape of array 1 and array 2 not equal"))]
           [temp* (alloc-unit shape* type*)]
           [len (array-size temp*)])
          (begin
            (for ([i (in-range 0 len)])
              (array-set!! temp* i (fn (array-get arr1 i) (array-get arr2 i))))
            temp*)))

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
           [temp (alloc-unit shape* type*)]
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
