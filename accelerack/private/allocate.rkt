#lang racket

;; Utilities for building acc-manifest-array data structures
;; as C pointers.

(require (except-in ffi/unsafe ->)
         ffi/unsafe/cvector
         accelerack/private/header
         accelerack/private/arrayutils
         accelerack/private/global_utils
         racket/contract
         (only-in '#%foreign ctype-scheme->c ctype-c->scheme))

(provide
 (contract-out
   ;; Manifest arrays:
  
   ;; TODO: improve contracts:  
   [list->manifest-array (-> (or/c ctype? pair?)   ;; type
                  (or/c null? pair?)    ;; shape
                  (or/c number? boolean? list?) ;; data
                  acc-manifest-array?)]
   [generatePayload (-> pair? (or/c ctype? symbol?) segment?)]
   [make-empty-manifest-array (-> (or/c null? pair?) (or/c ctype? pair?) acc-manifest-array?)]
   [read-data  (-> segment? (or/c null? pair?))]
   [read-data* (-> acc-manifest-array? any/c)]

   ;; Segments:
   
   [get-type (-> acc-manifest-array? integer?)]
   [get-shape (-> acc-manifest-array? (or/c null? pair?))]
   [get-result-array (-> acc-manifest-array? acc-manifest-array?)]
   [type (-> (or/c acc-manifest-array? segment?) integer?)]
   [shape (-> acc-manifest-array? (or/c null? pair?))])

 acc-manifest-array-flatref
 )


;; Helper to generatePayload function
;; Arguments -> (list containing the payload, type, initial empty list)
;; Return value -> pointer to segment containing the payload informations

(define (generatePayload-helper data payload-c)
  (cond
    ((null? data) (make-segment (length payload-c)
                                 ((ctype-scheme->c scalar) 'acc-payload-ptr)
                                 (cvector-ptr (list->cvector payload-c _segment-pointer))))
    ((pair? (caar data)) (letrec ([payload*-c (generatePayload-helper (car data) '())])
                                 (generatePayload-helper (cdr data) (append-end payload*-c payload-c))))
    (else (let ([payload* (list->cvector (car data) (symbol->ctype (get-ctype (caar data))))])
               (generatePayload-helper (cdr data)
                                       (append-end (make-segment (length (car data)) ((ctype-scheme->c scalar) (get-ctype (caar data)))
                                                                 (cvector-ptr payload*))
                                                    payload-c))))))


;; Stores the payload information into segment structure
;; Arguments -> (list containing the payload, type, initial empty list)
;; Return value -> pointer to segment containing the payload informations

(define (generatePayload data type)
  (if (ctype? type)
      (let ([payload (list->cvector data type)])
           (make-segment
             (length data)
             ((ctype-scheme->c scalar) (ctype->symbol type))
             (cvector-ptr payload)))
      (generatePayload-helper data '())))


;; Helper to read-data function
;; Arguments -> list containing segment pointers
;; Return value -> list with data read from memory pointed by segment pointers

(define (read-data-helper ls)
  (cond
    ((null? ls) '())
    ((let ([element (ptr-ref (segment-data (car ls)) (mapType (segment-type (car ls))))])
          (and (not (boolean? element))
               (cpointer? element)))
     (cons (read-data-helper
             (ptr-ref*
               (segment-data (car ls))
               (mapType (segment-type (car ls)))
               0
               (segment-length (car ls))))
           (read-data-helper (cdr ls))))
     (else (cons (ptr-ref*
                  (segment-data (car ls))
                  (mapType (segment-type (car ls)))
                  0
                  (segment-length (car ls)))
                (read-data-helper (cdr ls))))))


;; Read data from given memory location
;; Arguments -> segment pointer
;; Return value -> list with data read from given memory location
(define (read-data cptr)
  (letrec ([len (segment-length cptr)]
           [cptr* (segment-data cptr)]
           [type (mapType (segment-type cptr))]
           [data (if (equal? type 'empty-type) '() (ptr-ref* cptr* type 0 len))])
          (if (and (ctype? type) (not (equal? _segment-pointer type)))
              data
              (read-data-helper data))))

(define (segment-flatref seg type ind)
  (cond
    [(eq? type 'tuple-payload)
     (error 'segment-flatref "FINISHME: tuple case")]
    [(ctype? type)
     (ptr-ref (segment-data seg) type ind)]
    [else (error 'segment-flatref "unexpected type argument: ~a" type)]
  ))

(define (list->vector** ls)
  (cond
    ((null? ls) '())
    ((pair? (car ls)) (cons (list->vector (list->vector** (car ls))) (list->vector** (cdr ls))))
    (else (cons (car ls) (list->vector** (cdr ls))))))

(define (list->vector* ls shape)
  (cond
    ((null? shape) '())
    ((null? (cdr shape)) (map list->vector (map list->vector** ls)))
    ((equal? 1 (car shape)) (list (list->vector* (car ls) (cdr shape))))
    (else (cons (list->vector* (car ls) (cdr shape))
                (list->vector* (cdr ls) (cons (sub1 (car shape)) (cdr shape)))))))


;; Read data from given memory location
;; Arguments -> acc-manifest-array pointer
;; Return value -> list with data read from given memory location

(define (read-data* cptr)
  (letrec ([type (mapType (acc-manifest-array-type cptr))]
           [data-ptr (acc-manifest-array-data cptr)]
           [shape-ptr (acc-manifest-array-shape cptr)]
           [data  (read-data data-ptr)]
           [shape (read-data shape-ptr)])
          (if (equal? type 'scalar-payload)
              (if (null? shape)
                  (car (list->md-array data shape))
                  (list->md-array data shape))
              (list->vector* (zip data) shape)))) ;; (read-data-helper data)

;; Retrieve an element of an N-dimensional array using a 1-dimensional
;; index into its "row-major" repesentation.
(define (acc-manifest-array-flatref arr ind)
  (error 'acc-array-ref "FINISHME: acc-array-manifest-flatref unimplemented")
  (letrec ([type (mapType (acc-manifest-array-type arr))]
           [data-ptr      (acc-manifest-array-data arr)]
           )
    (segment-flatref data-ptr type ind)))

;; Get a list corresponding to given type
;; Arguments -> type
;; Return value -> list corresponding to given type initialized with unit values

(define (getUnit-tuple type)
  (cond
    ((null? type) '())
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


;; Helper function for make-empty-manifest-array.
;; Arguments: (shape, type, payload)
;; Return value: list initialized with unit values

(define (make-empty-manifest-array* shape type payload)
  (cond
    ((null? shape) payload)
    ((zero? (car shape))
     (let ([shape* (if (null? (cdr shape))
                       '()
                       (cons (sub1 (car (cdr shape)))
                             (cdr (cdr shape))))])
       (make-empty-manifest-array* shape* payload (list payload))))
    (else (make-empty-manifest-array* (cons (sub1 (car shape)) (cdr shape))
                       type (cons type payload)))))


;; Allocates a manifest array and fills it with default values.
;; Arguments: (shape, type)
;; Return value: list with racket pointer and c pointer to result structure

(define (make-empty-manifest-array shape type)
  (letrec ([type-data (if (ctype? type) (getUnit-scalar type) (getUnit-tuple type))]
           [type* (if (ctype? type)
                      ((ctype-scheme->c scalar) 'scalar-payload)
                      ((ctype-scheme->c scalar) 'tuple-payload))]
           [shape* (if (null? shape) '(1) shape)]
           [shape** (generatePayload shape* _int)]
           [init-data (car (make-empty-manifest-array* (reverse shape*) type-data '()))]
           [data (if (ctype? type)
                     (generatePayload (flatten init-data) type)
                     (generatePayload (unzip init-data) type))])
          (make-acc-manifest-array type* shape** data)))

;; Allocate memory for the payload
;; Arguments -> (type, shape,  payload, expression)
;; Return value -> pointer to allocated memory location

(define (list->manifest-array _type _shape _data)
    (letrec
      ([type (if (ctype? _type)
                 ((ctype-scheme->c scalar) 'scalar-payload)
                 ((ctype-scheme->c scalar) 'tuple-payload))]
       [shape (generatePayload _shape _int)]
       [data (if (ctype? _type)
                 (generatePayload (flatten _data) _type)
                 (generatePayload (unzip _data) _type))])
      (make-acc-manifest-array type shape data)))

(define (get-type arr)
  (if (acc-manifest-array? arr)
      (segment-type (acc-manifest-array-data arr))
      (segment-type arr)))

(define (get-shape arr)
  (read-data (acc-manifest-array-shape arr)))

(define (get-result-array input-arr)
  (letrec ([type* (if (equal? ((ctype-scheme->c scalar) 'acc-payload-ptr) (get-type input-arr))
                      (get-tuple-type (unzip (vector->list* (read-data* input-arr))) (get-shape input-arr))
                      (mapType (get-type input-arr)))]
           [temp (make-empty-manifest-array (get-shape input-arr) type*)])
          temp))

;; returns the type of the given acc array
(define (type arr)
  (if (acc-manifest-array? arr) (segment-type (acc-manifest-array-data arr)) (segment-type arr)))

;; returns the shape of the given acc array
(define (shape arr)
  (read-data (acc-manifest-array-shape arr)))
