#lang racket

(require ffi/unsafe ffi/unsafe/define ffi/unsafe/cvector ffi/vector)
(require (only-in '#%foreign ctype-scheme->c ctype-c->scheme))

(provide acc_alloc
         list->md_array
         getType
         getData
         getDimension
         cptr
         string->ctype
         ctype->symbol
         ptr-ref*
         scalar?)

(define-cstruct _c-array
  ([length _int] ;; length of the Dimension list
   [type _string]
   [dimension _gcpointer]
   [data _gcpointer]))

(define-cstruct _tuple
   ([length _int]
    [data _gcpointer]))

(define _scalar
  (_enum '(_double _int _bool)))

(define scalar-length 3)

(define _type
  (_union _tuple-pointer _scalar))

(define-struct type
  (tuple
   scalar))

;;(define types '(("Double" _double) ("Int" _int)))

(define libacclib (ffi-lib "libacclib"))
(define-ffi-definer define-libintegrator libacclib)
;;(define-libintegrator createStruct (_fun _pointer _pointer _string _string -> _c-array-pointer))
(define-libintegrator modify (_fun _c-array-pointer _int _string -> _string))
(define-libintegrator setType (_fun _tuple-pointer _scalar -> _type))

(define (scalar? type)
  (letrec ([check-type (lambda(i) (cond
                                    ((equal? i scalar-length) #f)
                                    ((equal? ((ctype-c->scheme _scalar) i) type) #t)
                                    (else (check-type (add1 i)))))])
          (check-type 0)))
  
(define (string->ctype str)
  (cond
    ((equal? str "Double") _double)
    ((equal? str "Int") _int)
    ((equal? str "Bool") _bool)
    (else #f)))

(define (ctype->symbol type)
  (cond
    ((equal? _double type) '_double)
    ((equal? _int type) '_int)
    ((equal? _bool type) '_bool)
    ((equal? _tuple type) '_tuple)))

;(define (create-tuple tuple cvec)
;  (cond
;    ((null? tuple) (make-tuple (length cvec) (cvector-ptr (list->cvector cvec _string))))
;    ((equal? (car tuple) '_tuple) (create-tuple (cdr tuple) cvec))
;    ((pair? (car tuple)) (cons 
(define types
  (list->cvector (map symbol->string '(_int _bool)) _string))

(define tuple-ptr
  (make-tuple 16 (cvector-ptr types)))

(define cvalue
  (setType tuple-ptr '_bool))
  ;;(ptr-ref (setType type_str '_bool) _type 0))

(define rvalue
  (make-type tuple-ptr '_bool))

(define union-types
  (lambda (types)
    (let ((type-ptr (cvector-ptr types))
          (tuple-ptr (make-tuple 16 (cvector-ptr types))))
         (letrec ((cval (setType tuple-ptr '_bool))
                  (rval (make-type  tuple-ptr '_bool)))
          #t))))

(define cptr "")

(define get-length
  (lambda (ls)
    (cond
      ((null? ls) 1)
      (else (* (car ls) (get-length (cdr ls)))))))

(define (ptr-ref* cptr type itr length)
  (cond
    ((zero? length) '())
    (else (cons (ptr-ref cptr type itr) (ptr-ref* cptr type (add1 itr) (sub1 length))))))

(define (acc_alloc _type _dimension _data exp) 
    (let
      ((vec (list->cvector (flatten _data) _type))
       (expr exp)
       (dimension (list->cvector _dimension _int)))
      (letrec
       ((c-ref (make-c-array (length _dimension) "Double" (cvector-ptr dimension) (cvector-ptr vec)))
        (type (if (equal? _type _int) "Int" "Double")))
       (begin
         (set! cptr c-ref)
         ;;(modify c-ref (get-length _dimension) type)
         (list->md_array (ptr-ref* (c-array-data c-ref) _double 0 (get-length _dimension)) _dimension)
         (list->md_array (ptr-ref* (getData c-ref) _double 0 (get-length _dimension)) _dimension)
         ))))

(define (getType c-ref)
  (c-array-type c-ref))

(define (getDimension c-ref)
  (c-array-dimension c-ref))

(define (getData c-ref)
  (c-array-data c-ref))

(define (list->md_array data dimension)
  (if (null? dimension)
      data
      (if (= (car dimension) 0)
          '()
          (let* ((len (length dimension)) 
                 (func (if (= 1 (length dimension))
                           append
                           cons)))
              (func (list->md_array (take data (/ (length data) (car dimension)))
                                (cdr dimension))
                    (list->md_array (drop data (/ (length data) (car dimension)))
                                (cons (- (car dimension) 1) (cdr dimension))))))))
