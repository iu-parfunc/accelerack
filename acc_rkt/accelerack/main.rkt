#lang racket/base

(require accelerack/private/acc_allocate
         accelerack/private/acc_arrayutils
         (prefix-in acc: accelerack/private/acc_header)
         accelerack/private/acc_syntax
         accelerack/private/acc_parse
         accelerack/private/acc_racket_ops
         ffi/unsafe
         (prefix-in r: racket/base)
         (for-syntax racket/base syntax/parse))

#;
(provide (all-from-out accelerack/private/acc_allocate)
         (all-from-out accelerack/private/acc_arrayutils)
         (all-from-out accelerack/private/acc_header)
         (all-from-out accelerack/private/acc_syntax)
         (all-from-out accelerack/private/acc_parse)
         (all-from-out accelerack/private/acc_racket_ops))

(provide acc-array?
         map
         acc-array
         acc-array->list
         )

(define (map f x)
  (if (acc-array? x)
      (acc-map f x)
      (r:map f x)))

(define-for-syntax (infer-type d)
  (syntax-parse d
    [_:boolean #'_bool]
    [_:number (if (flonum? (syntax-e d)) #'_double #'_int)]
    [#(v ...) #`(_tuple #,@(map infer-type (syntax->list #'(v ...))))]
    [(v more ...) (infer-type #'v)]))


(define-for-syntax (infer-shape d)
  (syntax-parse d
    [(~or _:boolean _:number) #'()]
    [_
     #:when (vector? (syntax-e d))
     #'()]
    [(v more ...) 
     #:with rest (infer-shape #'v)
     #:with l (add1 (length (syntax->list #'(more ...))))
     #'(l . rest)]))

(define-syntax (acc-array stx)
  (syntax-parse stx
    [(_ data)
     #:with type (infer-type #'data)
     #:with shape (infer-shape #'data)
     #'(make-acc-array (car (array shape type data)))]))

(define-struct acc-array (val)
  #:guard (lambda (v _)
            (unless (or (acc-delayed-array? v) (acc:acc-array? v))
              (raise-argument-error 'acc-array "acc-array?" v))
            v)
  #:methods gen:custom-write
  [(define (write-proc v prt mode)
     ((if mode write print) 
      (if (acc-delayed-array? (acc-array-val v))
          (list 'acc-array "DELAYED ARRAY")
          (list 'acc-array (readData* (acc-array-val v))))
      prt))]
  #:transparent
  #:omit-define-syntaxes)

(struct acc-delayed-array ())

(define (acc-array->list x)
  (readData* (acc-array-val x)))

;(define x (acc-array (#(3 4) #(5 6))))x
;(acc-array? x)
;(acc-array->list x)
