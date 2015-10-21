#lang racket

(require ffi/unsafe 
         ffi/unsafe/cvector)
(require (only-in '#%foreign ctype-scheme->c ctype-c->scheme))
(require "acc_header.rkt")
(require "ArrayUtils.rkt")

(provide acc_alloc
         generatePayload
         readData
         readData*
         generatePayload-rkt
         readData-rkt
         readData*-rkt)

;; Helper to generatePayload function
;; Arguments -> (list containing the payload, type, initial empty list)
;; Return value -> pointer to c-vector containing the payload informations 

(define (generatePayload-helper data payload)
  (cond
    ((null? data) (make-c-vector (length payload)
                                 ((ctype-scheme->c scalar) '_c-vector-pointer) 
                                 (cvector-ptr (list->cvector payload _c-vector-pointer))))
    ((pair? (caar data)) (let ([payload* (generatePayload-helper (car data) '())])
                              (generatePayload-helper (cdr data) (append-end payload* payload))))
    (else (generatePayload-helper (cdr data) 
             (append-end (make-c-vector (length (car data)) 
                                        ((ctype-scheme->c scalar) (get-ctype (caar data)))
                                        (cvector-ptr (list->cvector (car data) (symbol->ctype (get-ctype (caar data))))))
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
      (generatePayload-helper data '())))

;; Helper to generatePayload function
;; Arguments -> (list containing the payload, type, initial empty list)
;; Return value -> pointer to c-vector containing the payload informations 

(define (generatePayload-helper-rkt data payload)
  (cond
    ((null? data) (make-rkt-vector (length payload)
                                 ((ctype-scheme->c scalar) '_rkt-vector-pointer) 
                                 payload))
    ((pair? (caar data)) (let ([payload* (generatePayload-helper-rkt (car data) '())])
                              (generatePayload-helper-rkt (cdr data) (append-end payload* payload))))
    (else (generatePayload-helper-rkt (cdr data) 
             (append-end (make-rkt-vector (length (car data)) 
                                        ((ctype-scheme->c scalar) (get-ctype (caar data)))
                                        (list->cvector (car data) (symbol->ctype (get-ctype (caar data)))))
                          payload)))))


;; Stores the payload information into c-vector structure
;; Arguments -> (list containing the payload, type, initial empty list)
;; Return value -> pointer to c-vector containing the payload informations 

(define (generatePayload-rkt data type payload)
  (if (ctype? type)
      (make-rkt-vector 
        (length data) 
        ((ctype-scheme->c scalar) (ctype->symbol type)) 
        (list->cvector data type))
      (generatePayload-helper-rkt data payload)))


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
              data
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

;; Helper to readData function
;; Arguments -> list containing c-vector pointers
;; Return value -> list with data read from memory pointed by c-vector pointers

(define (readData-helper-rkt ls)
  (cond
    ((null? ls) '())
    ((pair? (rkt-vector-data (car ls)))
     (cons (readData-helper-rkt (rkt-vector-data (car ls)))
           (readData-helper-rkt (cdr ls))))
     (else (cons (cvector->list (rkt-vector-data (car ls)))
                (readData-helper-rkt (cdr ls))))))


;; Read data from given memory location
;; Arguments -> c-vector pointer
;; Return value -> list with data read from given memory location

(define (readData-rkt cptr)
  (letrec ([len (rkt-vector-length cptr)]
           [cptr* (rkt-vector-data cptr)]
           [type (mapType (rkt-vector-type cptr))])
           ;;[data (cvector->list cptr*)])
          (if (ctype? type)
              (cvector->list cptr*)
              (readData-helper-rkt cptr*))))


;; Read data from given memory location
;; Arguments -> c-array pointer
;; Return value -> list with data read from given memory location

(define (readData*-rkt cptr)
  (letrec ([type (mapType (rkt-array-type cptr))]
           [data-ptr (rkt-array-data cptr)]
           [shape-ptr (rkt-array-shape cptr)]
           [data (readData-rkt data-ptr)]
           [shape (readData-rkt shape-ptr)])
          (if (equal? type '_scalar)
              (list->md_array data shape)
              (zip data))))


;; Allocate memory for the payload
;; Arguments -> (type, shape,  payload, expression)
;; Return value -> pointer to allocated memory location

(define (acc_alloc _type _shape _data exp) 
    (let
      ([type (if (ctype? _type) ((ctype-scheme->c scalar) '_scalar) ((ctype-scheme->c scalar) '_tuple))]
       [shape-ptr (generatePayload _shape _int '())]
       [shape-ptr-rkt (generatePayload-rkt _shape _int '())]
       [data-ptr (if (ctype? _type) (generatePayload (flatten _data) _type '()) (generatePayload (unzip _data) _type '()))]
       [data-ptr-rkt (if (ctype? _type) (generatePayload-rkt (flatten _data) _type '()) (generatePayload-rkt (unzip _data) _type '()))]
       [expr exp])
      ;;(make-c-array type shape-ptr data-ptr)))
      (list (make-c-array type shape-ptr data-ptr)
            (make-rkt-array type shape-ptr-rkt data-ptr-rkt))))
     