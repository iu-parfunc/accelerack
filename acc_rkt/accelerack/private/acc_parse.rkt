#lang racket

(require (except-in ffi/unsafe ->)
         racket/contract
         accelerack/private/acc_arrayutils)

(provide
  (contract-out
    [verify-accelerack (-> vector? pair?)]))

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
          ((null? shape) (if (equal? 1 (length vec-list)) #t #f))
          ((and (pair? (car vec-list)) (equal? (length vec-list) (car shape))) (not (memv #f (map (lambda (x) (check-length x (cdr shape))) vec-list))))
          ((equal? (length vec-list) (car shape)) #t)
          (else #f))))

    (define (check-tuple-expr-length type data)
      (cond
        ((null? type) (if (null? data) #t #f))
        ((null? data) (if (null? type) #t #f))
        ((equal? (car type) '_tuple) (check-tuple-expr-length (cdr type) data))
        ((pair? (car type)) (and (check-tuple-expr-length (car type) (car data)) (check-tuple-expr-length (cdr type) (cdr data))))
        (else (and #t (check-tuple-expr-length (cdr type) (cdr data))))))
  
    (define (build-type type ls len shape)
      (cond
        ((zero? len) (list->md_array ls shape))
        (else (build-type type (cons type ls) (sub1 len) shape))))

    (define (verify-type type data)
      (cond
        ((equal? 'c-int type) (exact-integer? data))
        ((equal? 'c-double type) (double-flonum? data))
        ((equal? 'c-bool type) (boolean? data))))
    
    (define (check-tuple-expr type data)
      (cond
        ((null? type) #t)
        ((equal? (car type) '_tuple) (check-tuple-expr (cdr type) data))
        ((pair? (car type)) (and (check-tuple-expr (car type) (car data)) (check-tuple-expr (cdr type) (cdr data))))
        (else (and (verify-type (car type) (car data)) (check-tuple-expr (cdr type) (cdr data))))))

    (define check-exp
      (lambda (exp shape type)
            (if (ctype? type)
                (if (check-length exp shape)
                    (if (if (equal? type _int) (int_vector? exp) 
                            (if (equal? type _double) (dbl_vector? exp)
                                (if (equal? type _bool) (bool_vector? exp)
                                    #f)))
                        '(#t)
                       '(#f "failed ! Invalid expression: type mismatch"))
                   '(#f "failed ! Invalid expression: length mismatch"))
                (if (check-tuple-expr-length (build-type type '() (md_array-length shape) shape) exp)
                    (if (check-tuple-expr (build-type type '() (md_array-length shape) shape) exp)
                        '(#t)
                       '(#f "failed ! Invalid expression: type mismatch"))
                   '(#f "failed ! Invalid expression: length mismatch")))))
    
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
