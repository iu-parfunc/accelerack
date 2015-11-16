#lang racket

(require (except-in ffi/unsafe ->)
         accelerack/private/acc_header
         racket/contract
         (only-in '#%foreign ctype-scheme->c ctype-c->scheme))


(provide
  (contract-out
    [get-tuple-type (-> pair? pair? pair?)]
    [vector->list* (-> (or/c pair? vector?) pair?)]
    [unzip (-> (or/c null? pair?) (or/c null? pair?))]
    [zip (-> (or/c null? pair?) (or/c null? pair?) )]
    [zip-first (-> (or/c pair? null?) (or/c null? pair?) (or/c null? pair?))]
    [delete-first (-> (or/c pair? null?) (or/c null? pair?))]
    [create-ls (-> (or/c null? pair?) (or/c null? pair?))]
    [populate (-> (or/c null? pair?) pair? pair?)]
    [append-end (-> any/c (or/c pair? null?) pair?)]
    [scalar? (-> symbol? boolean?)]
    [string->ctype (-> string? ctype?)]
    [ctype->symbol (-> ctype? symbol?)]
    [symbol->ctype (-> symbol? ctype?)]
    [mapType (-> integer? (or/c ctype? symbol?))]
    [ptr-ref* (-> cpointer? ctype? integer? integer? any/c)]
    [list->md-array (-> (or/c null? pair?) (or/c null? pair?) (or/c null? pair?))]
    [md-array-length (-> (or/c null? pair?) integer?)]
    [get-ctype (-> any/c symbol?)]
    [get-type (-> acc-array? integer?)]
    [get-dimension (-> acc-array? segment?)]
    [get-data (-> acc-array? segment?)]))
  

(define (get-tuple-type-helper data type)
  (cond
    ((null? data) type)
    ((pair? (caar data)) (letrec ([type* (get-tuple-type-helper (car data) '())]
                                  [type** (cons '_tuple type*)])
                                 (get-tuple-type-helper (cdr data) (append-end type** type))))
    (else (letrec ([type* (get-ctype* (caar data))]
                   [type** (append-end type* type)])
                  (get-tuple-type-helper (cdr data) type**)))))

(define (get-tuple-type-helper* data shape)
  (cond
    ((null? (cdr shape)) data)
    (else (get-tuple-type-helper* (car data) (cdr shape)))))

(define (get-tuple-type data shape)
  (letrec ([data* (get-tuple-type-helper* data shape)]
           [type '(_tuple)]
           [tuple-type (get-tuple-type-helper data* type)])
          tuple-type))


;; Convert given vector to list recursively
;; Arguments -> vector or list with nested vectors
;; Return value -> list with all nested vectors converted to list

(define (vector->list* vec/ls)
  (cond
    ((vector? vec/ls) (vector->list* (vector->list vec/ls)))
    ((null? vec/ls) '())
    ((vector? (car vec/ls)) (cons (vector->list* (car vec/ls)) (vector->list* (cdr vec/ls))))
    (else (cons (car vec/ls) (vector->list* (cdr vec/ls))))))


;; Create empty list preserving structure of the data list 
;; Arguments -> (empty list, list containing the payload)
;; Return value -> list enclosing empty lists preserving the structure of data list

(define (create-ls data-ls)
  (cond
    ((null? data-ls) '())
    ((pair? (car data-ls)) (cons (create-ls (car data-ls)) (create-ls (cdr data-ls))))
    (else (cons '() (create-ls (cdr data-ls))))))


;; Insert to the end of the list
;; Arguments -> (element e, list l)
;; Return value -> list with e inserted at the end of l

(define (append-end x ls)
  (append ls (list x)))


;; Distribute data list elements into new list structure
;; Arguments -> (empty list, list containing the payload)
;; Return value -> list enclosing the data populated into new structures

(define (populate fin-ls data-ls)
  (cond
    ((null? data-ls) '())
    ((pair? (car data-ls)) (cons (populate (car fin-ls) (car data-ls)) (populate (cdr fin-ls) (cdr data-ls))))
    (else (cons (append-end (car data-ls) (car fin-ls)) (populate (cdr fin-ls) (cdr data-ls))))))


;; Flatten the tuple list
;; Arguments -> (list containing the payload)
;; Return value -> list enclosing the flattened tuple data

(define (unzip ls)
 (letrec ((fin-ls (create-ls (car ls))))
         (foldl (lambda (x y) (populate y x)) fin-ls ls)))


;; Zip the first element of each member in the data list
;; Arguments -> (empty list, list containing the payload)
;; Return value -> list enclosing first element of each member from data list zipped into new structure

(define (zip-first fin-ls data-ls)
  (cond
    ((null? data-ls) '())
    ((pair? (caar data-ls)) (cons (zip-first '() (car data-ls)) (zip-first fin-ls (cdr data-ls))))
    (else (cons (caar data-ls) (zip-first fin-ls (cdr data-ls))))))


;; Delete the first element of each member in the data list
;; Arguments -> (list containing the payload)
;; Return value -> list with first element removed from each member

(define (delete-first data-ls)
  (cond
    ((null? data-ls) '())
    ((pair? (caar data-ls)) (cons (delete-first (car data-ls)) (delete-first (cdr data-ls))))
    (else (cons (cdar data-ls) (delete-first (cdr data-ls))))))


;; Zip the list elements into original form from the flattened version
;; Arguments -> (list containing the payload in flattened form)
;; Return value -> list with data unraveled to original structure

(define (zip ls)
  (cond
    ((null? (flatten ls)) '())
    (else (cons (zip-first '() ls) (zip (delete-first ls))))))


;; Function to check whether given type is scalar
;; Arguments -> type
;; Return value -> true for base ctypes
;;                 false for other types

(define (scalar? type)
  (letrec ([check-type (lambda(i) (cond
                                    ((equal? i scalar-length) #f)
                                    ((equal? ((ctype-c->scheme scalar) i) type) #t)
                                    (else (check-type (add1 i)))))])
          (check-type 0)))


;; Converts from string (corresponding to haskell type) to racket ctype
;; Arguments -> string
;; Return value -> ctype

(define (string->ctype str)
  (cond
    ((equal? str "Double") _double)
    ((equal? str "Int") _int)
    ((equal? str "Bool") _bool)
    (else #f)))


;; Converts from ctype to symbol
;; Arguments -> ctype
;; Return value -> symbol

(define (ctype->symbol type)
  (cond
    ((equal? _double type) 'c-double)
    ((equal? _int type) 'c-int)
    ((equal? _bool type) 'c-bool)
    ((equal? _segment type) 'tuple-payload)
    ((symbol? type) type)))


;; Converts from symbol to ctype
;; Arguments -> symbol
;; Return value -> corresponding ctype

(define (symbol->ctype type)
  (cond
    ((equal? 'c-double type) _double)
    ((equal? 'c-int type) _int)
    ((equal? 'c-bool type) _bool)
    ((equal? 'c-ptr type) _gcpointer)
    ((ctype? type) type)))


;; Map the scalar enum value to corresponding ctype
;; Arguments -> scalar enum [int value]
;; Return value -> ctype

(define (mapType type)
  (cond
    ((equal? type 0) _int)
    ((equal? type 1) _double)
    ((equal? type 2) _bool)
    ((equal? type 3) _segment-pointer)
    ((equal? type 4) 'scalar-payload)
    ((equal? type 5) 'tuple-payload)
    ((equal? type 6) _gcpointer)
    ((equal? type 7) 'rkt-payload-ptr )
    (else 'empty_type)))


;; Map the actual type to scalar enum values
;; Arguments -> type
;; Return value -> scalar enum value

(define (get-ctype x)
  (cond 
    ((exact-integer? x) 'c-int)
    ((double-flonum? x) 'c-double)
    ((boolean? x) 'c-bool)))

(define (get-ctype* x)
  (cond 
    ((exact-integer? x) '_int)
    ((double-flonum? x) '_double)
    ((boolean? x) '_bool)))

;; Iteratively reads the given memory location for given length
;; Invalid length can lead to memory corruption and segmentation fault
;; Arguments -> (cpointer, type, initial index for iteration, length)
;; Return value -> list containing the values read from the memory location

(define (ptr-ref* cptr type itr length)
  (cond
    ((zero? length) '())
    (else (cons (ptr-ref cptr type itr) (ptr-ref* cptr type (add1 itr) (sub1 length))))))


;; Converts a flattened list into its original unflattened version
;; Arguments -> (flattened payload list, shape)
;; Return value -> list in its original unflattened form

(define (list->md-array data shape)
  (if (null? shape)
      data
      (if (= (car shape) 0)
          '()
          (let* ((len (length shape)) 
                 (func (if (= 1 (length shape))
                           append
                           cons)))
              (func (list->md-array (take data (/ (length data) (car shape)))
                                (cdr shape))
                    (list->md-array (drop data (/ (length data) (car shape)))
                                (cons (- (car shape) 1) (cdr shape))))))))


;; Calculate the length of a flattened nested list
;; Arguments -> shape
;; Return value -> length of the flattened version

(define md-array-length
  (lambda (ls)
    (cond
      ((null? ls) 1)
      (else (* (car ls) (md-array-length (cdr ls)))))))



;; Not used currently

;; Return type of acc-array pointer reference
;; Arguments -> pointer to acc-array
;; Return value -> type

;;(define (get-type c-ref)
  ;;(acc-array-type c-ref))


;; Return shape of acc-array pointer reference
;; Arguments -> pointer to acc-array
;; Return value -> shape

;;(define (get-dimension c-ref)
  ;;(acc-array-shape c-ref))

;; Return payload of acc-array pointer reference
;; Arguments -> pointer to acc-array
;; Return value -> data

;;(define (get-data c-ref)
  ;;(acc-array-data c-ref))
