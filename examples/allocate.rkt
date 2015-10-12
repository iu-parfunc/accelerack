#lang racket

(require ffi/unsafe ffi/unsafe/define ffi/unsafe/cvector ffi/vector)

(provide acc_alloc)
(provide list->md_array)

(define-cstruct _c-array
  ([type _string]
   [data _pointer]
   [shape _pointer]))

(define-struct rkt-array
  (type
   data
   shape))

(define libacclib (ffi-lib "libacclib"))
(define-ffi-definer define-libintegrator libacclib)
(define-libintegrator createStruct (_fun _pointer _pointer _string _string -> _c-array-pointer))
(define-libintegrator modify (_fun _c-array-pointer _int _string -> _string))

(define get-length
  (lambda (ls)
    (cond
      ((null? ls) 1)
      (else (* (car ls) (get-length (cdr ls)))))))

(define (acc_alloc _type _shape _data exp) 
    (let
      ((vec (list->cvector (flatten _data) _type))
       (expr exp)
       (shape (list->cvector _shape _int)))
      (letrec
       ((rkt-ref (make-rkt-array "Double" vec shape))
        (c-ref (createStruct (cvector-ptr vec) (cvector-ptr shape) "Double" expr))
        (type (if (equal? _type _int) "Int" "Double")))
       (begin
         (modify c-ref (get-length _shape) type)
         (cvector->list (rkt-array-data rkt-ref))))))

(define (list->md_array data shape)
  (if (null? shape)
      data
      (if (= (car shape) 0)
          '()
          (let* ((len (length shape)) 
                 (func (if (= 1 (length shape))
                           append
                           cons)))
              (func (list->md_array (take data (/ (length data) (car shape)))
                                (cdr shape))
                    (list->md_array (drop data (/ (length data) (car shape)))
                                (cons (- (car shape) 1) (cdr shape))))))))
