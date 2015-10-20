#lang racket

(require ffi/unsafe ffi/unsafe/cvector)
(require (only-in '#%foreign ctype-scheme->c ctype-c->scheme))
(require "acc_header.rkt")
(require "ArrayUtils.rkt")

(provide acc_alloc
         generatePayload
         readData
         readData*)

;; Helper to generatePayload function
;; Arguments -> (list containing the payload, type, initial empty list)
;; Return value -> pointer to c-vector containing the payload informations 

(define (generatePayload-helper data type payload)
  (cond
    ((null? type) (make-c-vector (length payload) 0 (cvector-ptr (list->cvector payload _c-vector-pointer))))
    ((equal? '_tuple (car type)) (generatePayload-helper data (cdr type) payload))
    ((pair? (car type)) (let ([payload* (generatePayload-helper (car data) (car type) '())])
                             (generatePayload-helper (cdr data) (cdr type) (append-end payload* payload))))
    (else (generatePayload-helper (cdr data) (cdr type) 
             (append-end (make-c-vector (length (car data)) 
                                     ((ctype-scheme->c scalar) (car type)) 
                                     (cvector-ptr (list->cvector (car data) (symbol->ctype (car type)))))
                          payload)))))

;; Stores the payload information into c-vector structure
;; Arguments -> (list containing the payload, type, initial empty list)
;; Return value -> pointer to c-vector containing the payload informations 

(define (generatePayload data type payload)
  (if (ctype? type)
      (make-c-vector 
        (length data) 
        ((ctype-scheme->c scalar) (ctype->symbol type)) 
        (cvector-ptr (list->cvector data type)))
      (generatePayload-helper data type payload)))


;; Helper to readData function
;; Arguments -> list containing c-vector pointers
;; Return value -> list with data read from memory pointed by c-vector pointers

(define (readData-helper ls)
  (cond
    ((null? ls) '())
    ((let ([element (ptr-ref (c-vector-data (car ls)) (mapType (c-vector-type (car ls))))])
          (and (not (boolean? element)) 
               (cpointer? element))) 
     (cons (readData-helper 
             (ptr-ref* 
               (c-vector-data (car ls)) 
               (mapType (c-vector-type (car ls))) 
               0 
               (c-vector-length (car ls))))
           (readData-helper (cdr ls))))
    (else (cons (ptr-ref* 
                  (c-vector-data (car ls)) 
                  (mapType (c-vector-type (car ls))) 
                  0 
                  (c-vector-length (car ls)))
                (readData-helper (cdr ls))))))


;; Read data from given memory location
;; Arguments -> c-vector pointer
;; Return value -> list with data read from given memory location

(define (readData cptr)
  (letrec ([len (c-vector-length cptr)]
           [cptr* (c-vector-data cptr)]
           [type (mapType (c-vector-type cptr))]
           [data (ptr-ref* cptr* type 0 len)])
          (if (ctype? type)
              (ptr-ref* cptr* type 0 len)
              (readData-helper data))))


;; Read data from given memory location
;; Arguments -> c-array pointer
;; Return value -> list with data read from given memory location

(define (readData* cptr)
  (letrec ([type (mapType (c-array-type cptr))]
           [data-ptr (c-array-data cptr)]
           [shape-ptr (c-array-shape cptr)]
           [data (readData data-ptr)]
           [shape (readData shape-ptr)])
          (if (equal? type '_scalar)
              (list->md_array data shape)
              (zip (readData-helper data)))))


;; Allocate memory for the payload
;; Arguments -> (type, shape,  payload, expression)
;; Return value -> pointer to allocated memory location

(define (acc_alloc _type _shape _data exp) 
    (let
      ([type (if (ctype? _type) ((ctype-scheme->c scalar) '_scalar) ((ctype-scheme->c scalar) '_tuple))]
       [shape-ptr (generatePayload _shape _int '())]
       [data-ptr (if (ctype? _type) (generatePayload (flatten _data) _type '()) (generatePayload (unzip _data) _type '()))]
       [expr exp])
      (make-c-array type shape-ptr data-ptr)))
     