#lang racket

(require (except-in ffi/unsafe ->)
         accelerack/acc-array/private/manifest-array/structs
         racket/contract
         (only-in accelerack/private/types acc-type?)
         (only-in '#%foreign ctype-scheme->c ctype-c->scheme))

(provide
  (contract-out
    [string->ctype (-> string? ctype?)]
    [ctype->symbol (-> lame-type? symbol?)]
    [symbol->ctype (-> symbol? ctype?)]
    [mapType (-> integer? lame-type?)]
    [lame-type? (-> any/c boolean?)]
    [acc-type->lame-type (-> acc-type? lame-type?)]
    [ptr-ref* (-> cpointer? ctype? integer? integer? any/c)]
    ))

;; This protocol is lame and isn't explained anywhere.  Kill it!
(define lame-type?
  (or/c 'scalar-payload ;; Why do we need this?
        'tuple-payload
        'rkt-payload-ptr ;; is this still used?        
        ctype?))

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
    ((equal? '_double type) 'c-double)
    ((equal? '_int type) 'c-int)
    ((equal? '_bool type) 'c-bool)
    ((equal? _segment type) 'tuple-payload)
    ((equal? _acc-manifest-array-pointer type) 'acc-manifest-array-ptr)
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
    ((equal? 'acc-manifest-array-ptr) _acc-manifest-array-pointer)
    ((ctype? type) type)))


;; Map the scalar enum value to corresponding ctype
;; Arguments -> scalar enum [int value]
;; Return value -> ctype

;; RENAME: decodeTypeEnum
(define (mapType type)
  (cond
    ((equal? type 0) _int)
    ((equal? type 1) _double)
    ((equal? type 2) _bool)
    ((equal? type 3) _segment-pointer)
    ((equal? type 4) 'scalar-payload)
    ((equal? type 5) 'tuple-payload)
    ((equal? type 6) _acc-manifest-array-pointer)
    ((equal? type 7) _gcpointer)
    ((equal? type 8) 'rkt-payload-ptr )
    (else (error 'mapType "invalid accelerack type enumeration: ~a" type))))

;; DEPRECATED
;; Remove this and the lame notion of (U ctype? symbol?)
(define (acc-type->lame-type t)
  (match t
    ['Int    _int]
    ['Double _double]
    ['Bool   _bool]
    [`#( ,_ ...) 'tuple-payload
     #| Uh, maybe?  The protocol is unclear. |#  ]
    ; [`(Array ,d ,elt) _acc-manifest-array-pointer]
    [else (error 'acc-type->lame-type "Cannot convert: ~a\n" t)]))


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

