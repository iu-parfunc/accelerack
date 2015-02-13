#lang racket

(provide acc)

(begin-for-syntax
  (define ht (make-hash))
  )



(define-syntax (acc stx)
  (syntax-case stx ()
    [(_ (f (fn x) body)) (begin (hash-set! ht (syntax->datum (syntax fn)) (syntax->datum (syntax body)))
                                     #'(f (fn x) body))]  ;<--separate handling for fn defn...todo
    [(_ (f x exp)) (begin  (display ht) #'(define x exp))]
    [(_ exp) (begin (printf "uncaught:~a~n" (syntax->datum stx)) #'(void))]
    [(_ exp exp2 x ...) #'(begin (acc exp) (acc exp2 x ...))]
    [(_) (datum->syntax #'acc ht)]
    ))
