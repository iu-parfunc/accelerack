#lang racket/base

(require (for-syntax racket/base syntax/parse))

(provide)

;; (define-for-syntax (infer-shape d)
;;   (syntax-parse d

#|

scalar-type st
  = _int
  | _bool
  | _double
  | (_tuple st ...)

type t
  = st
  | (_tuple t ...)
  | (_array n t)
  | (-> t t)

n = non-negative integer

expression e
  = (array sh st a)
  | add1
  | sub1
  | (map e e)
  | (zipwith e e)
  | (fold e e e)
  | (let ((x e) ...) e)
  | x
  | (lambda (x ...) e)
  | (e e ...)
  | #(e ...)

value v
  = <boolean>
  | <integer>
  | <double>

array-data a
  = e
  | (a ...), all a have same length

shape sh = (n ...)

|#

(begin-for-syntax
  (define-syntax-class type
    #:attributes (verify)
    [pattern _bool
      #:with verify #'boolean?
      ]
    [pattern _int
      #:with verify #'integer?
      ]
    [pattern _double
      #:with verify #'flonum?
      ]
    [pattern (_tuple t:type ...)
      #:with l  (length (syntax->list #'(t ...)))
      ;; #:with ns 
      #:with verify
        #'(lambda (x)
            (and (vector? x)
                 (eqv? (vector-length x) l)
                 (t.verify (vector-ref x)) ...)) 
      ]
    ))

;;   (define-syntax-class scalar-val
;;   (define-syntax-class array
;;   #:attributes (shape type)
;;   [pattern v
;;     #:with shape (infer-shape #'v)
;;     #:with type  (infer-type #'v)]))

