#lang racket

(require (except-in ffi/unsafe ->)
         ffi/unsafe/cvector
         accelerack/src/acc_header
         accelerack/src/acc_arrayutils
         racket/contract
         (only-in '#%foreign ctype-scheme->c ctype-c->scheme))

(provide
  (contract-out
    [acc_alloc (-> (or/c ctype? pair?) (or/c null? pair?) (or/c number? boolean? pair?) string? pair?)]
    [generatePayload (-> pair? (or/c ctype? symbol?) pair?)] 
    [alloc-unit (-> (or/c null? pair?) (or/c ctype? pair?) pair?)]
    [readData (-> segment? (or/c null? pair?))]
    [readData* (-> acc-array? pair?)]
    [readData-rkt (-> rkt-segment? pair?)]
    [readData*-rkt (-> rkt-acc-array? pair?)]))


;; Helper to generatePayload function
;; Arguments -> (list containing the payload, type, initial empty list)
;; Return value -> pointer to segment containing the payload informations 

(define (generatePayload-helper data payload-c payload-rkt)
  (cond
    ((null? data) (list (make-segment (length payload-c)
                                 ((ctype-scheme->c scalar) 'acc-payload-ptr) 
                                 (cvector-ptr (list->cvector payload-c _segment-pointer)))
                        (make-rkt-segment (length payload-rkt)
                                 ((ctype-scheme->c scalar) 'rkt-payload-ptr) 
                                 payload-rkt)))
    ((pair? (caar data)) (letrec ([payload* (generatePayload-helper (car data) '() '())]
                                  [payload*-c (car payload*)]
                                  [payload*-rkt (cadr payload*)])
                                 (generatePayload-helper (cdr data) (append-end payload*-c payload-c) (append-end payload*-rkt payload-rkt))))
    (else (let ([payload* (list->cvector (car data) (symbol->ctype (get-ctype (caar data))))])
               (generatePayload-helper (cdr data) 
                                       (append-end (make-segment (length (car data)) ((ctype-scheme->c scalar) (get-ctype (caar data)))
                                                                 (cvector-ptr payload*))
                                                    payload-c)
                                       (append-end (make-rkt-segment (length (car data)) 
                                                                     ((ctype-scheme->c scalar) (get-ctype (caar data)))
                                                                     payload*)
                                                    payload-rkt))))))


;; Stores the payload information into segment structure
;; Arguments -> (list containing the payload, type, initial empty list)
;; Return value -> pointer to segment containing the payload informations 

(define (generatePayload data type)
  (if (ctype? type)
      (let ([payload (list->cvector data type)])
           (list
             (make-segment
               (length data) 
               ((ctype-scheme->c scalar) (ctype->symbol type)) 
               (cvector-ptr payload))
             (make-rkt-segment 
               (length data) 
               ((ctype-scheme->c scalar) (ctype->symbol type)) 
               payload)))
      (generatePayload-helper data '() '())))


;; Helper to readData function
;; Arguments -> list containing segment pointers
;; Return value -> list with data read from memory pointed by segment pointers

(define (readData-helper ls)
  (cond
    ((null? ls) '())
    ((let ([element (ptr-ref (segment-data (car ls)) (mapType (segment-type (car ls))))])
          (and (not (boolean? element)) 
               (cpointer? element))) 
     (cons (readData-helper 
             (ptr-ref* 
               (segment-data (car ls)) 
               (mapType (segment-type (car ls))) 
               0 
               (segment-length (car ls))))
           (readData-helper (cdr ls))))
     (else (cons (ptr-ref* 
                  (segment-data (car ls)) 
                  (mapType (segment-type (car ls))) 
                  0 
                  (segment-length (car ls)))
                (readData-helper (cdr ls))))))


;; Read data from given memory location
;; Arguments -> segment pointer
;; Return value -> list with data read from given memory location

(define (readData cptr)
  (letrec ([len (segment-length cptr)]
           [cptr* (segment-data cptr)]
           [type (mapType (segment-type cptr))]
           [data (if (equal? type 'empty-type) '() (ptr-ref* cptr* type 0 len))])
          (if (and (ctype? type) (not (equal? _segment-pointer type))) 
              data
              (readData-helper data))))

;; Read data from given memory location
;; Arguments -> acc-array pointer
;; Return value -> list with data read from given memory location

(define (readData* cptr)
  (letrec ([type (mapType (acc-array-type cptr))]
           [data-ptr (acc-array-data cptr)]
           [shape-ptr (acc-array-shape cptr)]
           [data (readData data-ptr)]
           [shape (readData shape-ptr)])
          (if (equal? type 'scalar-payload)
              (list->md_array data shape)
              (zip data)))) ;; (readData-helper data)

;; Helper to readData function
;; Arguments -> list containing segment pointers
;; Return value -> list with data read from memory pointed by segment pointers

(define (readData-helper-rkt ls)
  (cond
    ((null? ls) '())
    ((pair? (rkt-segment-data (car ls)))
     (cons (readData-helper-rkt (rkt-segment-data (car ls)))
           (readData-helper-rkt (cdr ls))))
     (else (cons (cvector->list (rkt-segment-data (car ls)))
                (readData-helper-rkt (cdr ls))))))


;; Read data from given memory location
;; Arguments -> segment pointer
;; Return value -> list with data read from given memory location

(define (readData-rkt cptr)
  (letrec ([len (rkt-segment-length cptr)]
           [cptr* (rkt-segment-data cptr)]
           [type (mapType (rkt-segment-type cptr))])
          (if (ctype? type)
              (cvector->list cptr*)
              (readData-helper-rkt cptr*))))


;; Read data from given memory location
;; Arguments -> acc-array pointer
;; Return value -> list with data read from given memory location

(define (readData*-rkt cptr)
  (letrec ([type (mapType (rkt-acc-array-type cptr))]
           [data-ptr (rkt-acc-array-data cptr)]
           [shape-ptr (rkt-acc-array-shape cptr)]
           [data (readData-rkt data-ptr)]
           [shape (readData-rkt shape-ptr)])
          (if (equal? type 'scalar-payload)
              (list->md_array data shape)
              (zip data))))

;; Get a list corresponding to given type
;; Arguments -> type
;; Return value -> list corresponding to given type initialized with unit values

(define (getUnit-tuple type)
  (cond
    ((null? type) '())
    ((equal? '_tuple (car type)) (getUnit-tuple (cdr type)))
    ((equal? '_int (car type)) (cons 0 (getUnit-tuple (cdr type))))
    ((equal? '_double (car type)) (cons 0.0 (getUnit-tuple (cdr type))))
    ((equal? '_bool (car type)) (cons #f (getUnit-tuple (cdr type))))
    ((pair? (car type)) (cons (getUnit-tuple (car type)) (getUnit-tuple (cdr type))))))

;; Get a unit value corresponding to given type
;; Arguments -> type
;; Return value -> value corresponding to given type

(define (getUnit-scalar type)
  (cond
    ((equal? _int type) 0)
    ((equal? _double type) 0.0)
    ((equal? _bool type) #f)))


;; Allocates a result structure
;; Arguments -> (shape, type, payload)
;; Return value -> list initialized with unit values

(define (alloc-unit* shape type payload)
  (cond
    ((null? shape) payload)
    ((zero? (car shape))  (let ([shape* (if (null? (cdr shape)) '() (cons (sub1 (car (cdr shape))) (cdr (cdr shape))))]) 
                               (alloc-unit* shape* payload (list payload))))
    (else (alloc-unit* (cons (sub1 (car shape)) (cdr shape)) type (cons type payload)))))


;; Get a pointer to result structure
;; Arguments -> (shape, type)
;; Return value -> list with racket pointer and c pointer to result structure

(define (alloc-unit shape type)
  (letrec ([type-data (if (ctype? type) (getUnit-scalar type) (getUnit-tuple type))]
           [type* (if (ctype? type) ((ctype-scheme->c scalar) 'scalar-payload) ((ctype-scheme->c scalar) 'tuple-payload))]
           [shape* (if (null? shape) '(1) shape)]
           [shape** (generatePayload shape* _int)]
           [init-data (car (alloc-unit* (reverse shape*) type-data '()))]
           [data (if (ctype? type) (generatePayload (flatten init-data) type) (generatePayload (unzip init-data) type))])
          (list (make-acc-array type* (car shape**) (car data))
                (make-rkt-acc-array type* (cadr shape**) (cadr data)))))

;; Allocate memory for the payload
;; Arguments -> (type, shape,  payload, expression)
;; Return value -> pointer to allocated memory location

(define (acc_alloc _type _shape _data exp) 
    (letrec
      ([type (if (ctype? _type) ((ctype-scheme->c scalar) 'scalar-payload) ((ctype-scheme->c scalar) 'tuple-payload))] 
       [shape (generatePayload _shape _int)]
       [shape-ptr-c (car shape)]
       [shape-ptr-rkt (cadr shape)]
       [data (if (ctype? _type) (generatePayload (flatten _data) _type) (generatePayload (unzip _data) _type))]
       [data-ptr-c (car data)]
       [data-ptr-rkt (cadr data)]
       [expr exp])
      (list (make-acc-array type shape-ptr-c data-ptr-c)
            (make-rkt-acc-array type shape-ptr-rkt data-ptr-rkt))))
     
