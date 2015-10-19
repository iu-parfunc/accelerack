#lang racket

(require ffi/unsafe)
(require "allocate.rkt")
(require "unzip_tuple.rkt")

(provide verify-accelerack)

(define (verify-accelerack exp)
  (define check-tuple
      (lambda (type)
        (match type
          (`(_tuple ,x ...) (if (memv #f (map (lambda (y) (check-tuple y)) x)) #f #t))
          (`,x #:when (scalar? x) #t)
          (`,y #f))))

    (define check-type
      (lambda (type)
        (if (ctype? type) #t
            (if (list? type)
                (if (check-tuple type) #t #f)
                #f))))
    
    (define check-shape
      (lambda (shape)
        (cond
          ((null? shape) #t)
          ((exact-integer? (car shape)) (and #t (check-shape (cdr shape))))
          (else #f))))
    
    (define dbl_vector?
      (lambda (vec-list)
        (cond
          ((null? vec-list) #t)
          ((pair? (car vec-list)) (and (dbl_vector? (car vec-list)) (dbl_vector? (cdr vec-list))))
          ((double-flonum? (car vec-list)) (dbl_vector? (cdr vec-list)))
          (else #f))))

    (define int_vector?
      (lambda (vec-list)
        (cond
          ((null? vec-list) #t)
          ((pair? (car vec-list)) (and (int_vector? (car vec-list)) (int_vector? (cdr vec-list))))
          ((exact-integer? (car vec-list)) (int_vector? (cdr vec-list)))
          (else #f))))  

    (define bool_vector?
      (lambda (vec-list)
        (cond
          ((null? vec-list) #t)
          ((pair? (car vec-list)) (and (bool_vector? (car vec-list)) (bool_vector? (cdr vec-list))))
          ((boolean? (car vec-list)) (bool_vector? (cdr vec-list)))
          (else #f))))  
  
    (define check-length
      (lambda (vec-list shape)
        (cond
          ((null? vec-list) #t)
          ((null? shape) #f)
          ((and (pair? (car vec-list)) (equal? (length vec-list) (car shape))) (not (memv #f (map (lambda (x) (check-length x (cdr shape))) vec-list))))
          ((equal? (length vec-list) (car shape)) #t)
          (else #f))))
    
    (define check-tuple-expr
      (lambda (type flat-list)
        (match type
          (`(_tuple ,x ...) (if (memv #f (map (lambda (y ls) (check-tuple-expr y ls)) x flat-list)) #f #t))
          (`,x #:when (scalar? x) (if (equal? x '_int) (int_vector? flat-list)
                                      (if (equal? x '_bool) (bool_vector? flat-list)
                                          (if (equal? x '_double) (dbl_vector? flat-list) #f))))
          (`,y #f))))

    (define check-exp
      (lambda (exp shape type)
        (if (check-length exp shape)
            (if (if (ctype? type) 
                    (if (equal? type _int) (int_vector? exp) 
                        (if (equal? type _double) (dbl_vector? exp) 
                            #f))
                    (begin
                      (printf "comes here")
                    (check-tuple-expr type (unzip exp))))
                '(#t)
                '(#f "failed ! Invalid expression: type mismatch"))
            '(#f "failed ! Invalid expression: length mismatch"))))
    
    (match exp
      [`#(,type ,shape ,exp ...) #:when (list? shape)
         (let ((val (if (check-type type) 
                      (if (check-shape shape)
                          (let ((ret (check-exp (car exp) shape type)))
                               (if (car ret)
                                  '(#t)
                                   ret))
                         '(#f "failed ! Invalid Shape"))
                     '(#f "failed ! Invalid Type"))))
             (if (car val)
             (car exp)
            `(#f ,(cadr val))))]
      [`,no-match '(#f "failed ! Invalid expression ~a" no-match)]))