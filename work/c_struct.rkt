#lang racket

(require ffi/unsafe ffi/unsafe/define ffi/unsafe/cvector ffi/vector ffi/cvector)

;(define libmylib (ffi-lib "libmylib"))

(define-cstruct _carray
  ([data _cvector]
   [type _string]
   [shape _f64vector]))

;(define-struct _carray
;  (data
;   type
;   shape))

(define xs '(((0.0 1.0 2.0 3.0)
             (4.0 5.0 6.0 7.0)
             (8.0 9.0 10.0 11.0))
             
             ((0.0 1.0 2.0 3.0)
              (4.0 5.0 6.0 7.0)
              (8.0 9.0 10.0 11.0))
             
             ((12.0 13.0 14.0 15.0)
              (16.0 17.0 18.0 19.0)
              (20.0 21.0 22.0 23.0))))

(define value
  (make-carray (list->cvector (flatten xs) _double)
               "_double"
               (list->f64vector '(3 3 4))))
