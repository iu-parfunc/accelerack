#lang racket

(provide verify-accelerack)

(define (verify-accelerack exp)
  ;(lambda (exp)
    (define types '(_double _Double _int _Int))
    
    (define check-type
      (lambda (type)
        (if (member type types) #t #f)))
    
    (define check-shape
      (lambda (shape)
        (cond
          ((null? shape) #t)
          ((exact-integer? (car shape)) (and #t (check-shape (cdr shape))))
          (else #f))))
    
    (define dblvector?
      (lambda (vec-list)
        (cond
          ((null? vec-list) #t)
          ((pair? (car vec-list)) (and (dblvector? (car vec-list)) (dblvector? (cdr vec-list))))
          ((double-flonum? (car vec-list)) (dblvector? (cdr vec-list)))
          (else #f))))
    
    (define check-length
      (lambda (vec-list shape)
        (cond
          ((null? vec-list) #t)
          ((null? shape) #f)
          ((and (pair? (car vec-list)) (equal? (length vec-list) (car shape))) (not (memv #f (map (lambda (x) (check-length x (cdr shape))) vec-list))))
          ((equal? (length vec-list) (car shape)) #t)
          (else #f))))
    
    (define check-exp
      (lambda (exp shape type)
        (if (check-length exp shape)
            (if (dblvector? exp)
                #t
                (error 'verify-accelerack "failed ! Invalid expression: double expected"))
            (error 'verify-accelerack "failed ! Invalid expression: length mismatch"))))
    
    (match exp
      [`(use (vector ,type ,shape ,exp ...)) #:when (list? shape)
         (if (and (check-type type) (check-shape shape) (check-exp exp shape type))
             exp
             (error 'verify-accelerack "failed ! Invalid expression ~a" exp))]
      [`,no-match (error 'verify-accelerack "failed ! Invalid expression ~a" no-match)]))