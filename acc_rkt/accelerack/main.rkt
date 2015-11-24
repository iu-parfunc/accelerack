#lang racket/base

(require accelerack/private/acc_allocate
         accelerack/private/acc_arrayutils
         (prefix-in acc: (only-in accelerack/private/acc_header acc-array?))
         (except-in accelerack/private/acc_header acc-array? make-acc-array)
         ;;accelerack/private/acc_header
         accelerack/private/acc_syntax
         accelerack/private/acc_parse
         (prefix-in rkt: accelerack/private/acc_racket_ops)
         ffi/unsafe
         (prefix-in r: racket/base)
         (for-syntax racket/base syntax/parse))

(provide (all-from-out accelerack/private/acc_allocate) 
         (all-from-out accelerack/private/acc_arrayutils)
         (all-from-out accelerack/private/acc_header)
         (all-from-out accelerack/private/acc_syntax)
         (all-from-out accelerack/private/acc_parse)
         (all-from-out accelerack/private/acc_racket_ops)
         acc-array?
         map
         zipwith
         fold
         acc-zipwith
         acc-map
         acc-fold
         acc-array
         acc-array-val
         acc-array->list)

(define (map f x)
  (if (acc-array? x)
      (acc (acc:map f (acc-array-val x)))
      (r:map f x)))

(define (acc-map f x)
  (if (acc-array? x)
      (rkt:acc-map f (acc-array-val x))
      (error 'acc-map "acc-array expected")))

(define (acc-zipwith f x y)
  (if (and (acc-array? x) (acc-array? y))
      (rkt:acc-zipwith f (acc-array-val x) (acc-array-val y))
      (error 'acc-zipwith "acc-array expected")))

(define (acc-fold f def x)
  (if (acc-array? x)
      (rkt:acc-fold f def (acc-array-val x))
      (error 'acc-zipwith "acc-array expected")))

(define (fold f def x)
  (if (acc-array? x)
      (acc (acc:fold f def (acc-array-val x)))
      (error 'fold "acc-array expected")))

(define (zipwith f x y)
  (if (and (acc-array? x) (acc-array? y))
      (acc (acc:zipwith f (acc-array-val x) (acc-array-val y)))
      (error 'zipwith "acc-array expected")))

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


(begin-for-syntax
  (define-syntax-class acc-data
    #:attributes (shape type)
    [pattern v
             #:with shape (infer-shape #'v)
             #:with type (infer-type #'v)]))

(define-syntax (acc-array stx)
  (syntax-parse stx
    [(_ data:acc-data)
     #'(make-acc-array (car (array data.shape data.type data)))]))

(define-struct acc-array (val)
  #:guard (lambda (v _)
            (unless (or (acc-delayed-array? v) (acc:acc-array? v))
              (raise-argument-error 'acc-array "acc-array?" v))
            v)
  #:methods gen:custom-write
  [(define (write-proc v prt mode)
     ((if mode write print) 
      (if (acc-delayed-array? (acc-array-val v))
          (list 'acc-array "<DELAYED ARRAY>")
          (list 'acc-array (readData* (acc-array-val v))))
      prt))]
  #:transparent
  #:omit-define-syntaxes)

(struct acc-delayed-array ())

(define (acc-array->list x)
  (if (acc-array? x)
      (readData* (acc-array-val x))
      (readData* x)))

#|(define x (acc-array (#(3 4) #(5 6))))
x
(acc-array? x)
(acc-array->list x)|#
