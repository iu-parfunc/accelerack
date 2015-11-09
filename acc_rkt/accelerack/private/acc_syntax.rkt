#lang racket

(require ffi/unsafe
         ffi/unsafe/define
	 racket/runtime-path
         accelerack/private/acc_parse
         accelerack/private/acc_allocate
         accelerack/private/acc_arrayutils
         accelerack/private/acc_header)

(provide acc
         array
         _tuple
         generate)

(define generate build-list)

(begin-for-syntax
  (define ht (make-hash))
  (define UNDEFINED 'accrack-undefined)
  (define (undefined? id)
    (equal? (hash-ref ht id UNDEFINED) UNDEFINED))
  )

;; TODO - The calls to functions in libacclib need to be replaced by calls to haskell functions

(define-runtime-path libacc "../../../acc_c/libacc.so")
(define libacclib (ffi-lib libacc))
(define-ffi-definer define-libintegrator libacclib)
(define-libintegrator rkt_handler (_fun _acc-array-pointer _string -> _void))

;; TODO - Need to rework the macros

(define (process-data data)
  (match data
    ((list (list x ...) ...) (map process-data x))
    (`(,x ...) (map process-data x))
    (`,x (if (or (pair? x) (vector? x)) (vector->list* x) x))))

(define-syntax (array stx)
  (syntax-case stx ()

    [(array (shape ...) type (data ...))
                           #'(letrec ((data* (process-data (syntax->datum (syntax (data ...)))))
                                                                 (ret (verify-accelerack (vector type (syntax->datum (syntax (shape ...))) data*))))
                                                (if (car ret)
                                                      (acc_alloc type (syntax->datum (syntax (shape ...))) data*)
                                                      (error 'verify-accelerack (cadr ret))))]
    [(array (shape ...) type data)
                           #'(let ((ret (verify-accelerack (vector type (syntax->datum (syntax (shape ...))) (flatten data)))))
                                                (if (car ret)
                                                     (acc_alloc type (syntax->datum (syntax (shape ...))) data)
                                                     (error 'verify-accelerack (cadr ret))))]))

(define map-type
  (lambda (x)
    (cond
      ((ctype? x) (ctype->symbol x))
      ((pair? x) (cons (map-type (car x)) (map-type (cdr x))))
      (else x))))

;; TODO - Need to rework the definition of tuple syntax
(define-syntax (_tuple stx)
  (syntax-case stx ()
    [(_ type ...) #'(cons '_tuple (map map-type (list type ...)))]))

(define-syntax (acc stx)
  (syntax-case stx (define view load run get)
    
    ;Redefinitions are ignored.  Should throw an error.
    ; - but DrRacket's error-handling of redefinitions works well.  Comment out the guard and see.
    ;[(acc (define y (vector type #(shape ...) data))) (identifier? #'x) #'(define y (list shape ...))]
    [(acc (define x (vector type shape data))) (identifier? #'x)
                                #'(define x (let ((ret (verify-accelerack (vector type shape (vector->list* data)))))
                                                 (if (car ret)
                                                     ;;(list->md_array (acc_alloc type shape data "use") shape)
                                                     (acc_alloc type shape (vector->list* data) "use")
                                                     (error 'verify-accelerack (cadr ret)))))]
                                ;#'(verify-accelerack (vector type shape data))
    [(acc (map exp data))  #'(cpointer? data)
                           #'(if (equal? exp 'add1)
                                 (rkt_handler data "add1")
                                 (if (equal? exp 'sub1) 
                                     (rkt_handler data "sub1")
                                     (error "function not defined")))]
    ; Variable definition    
    [(acc (define x exp)) (and (identifier? #'x)
                               ; (undefined? (syntax->datum #'x))
                               )
                          (begin (hash-set! ht (syntax->datum (syntax x)) (syntax->datum (syntax exp)))
                                 #'(define x exp))]
    
    ; Function definition
    [(acc (define (fn x ...) body)) ;(undefined? (syntax->datum #'fn))
     (begin
       (hash-set! ht (syntax->datum (syntax fn)) (syntax->datum (syntax (λ (x ...) body))))
       #'(define (fn x ...) body))]
    
    ; Generic use of higher order function, not well-understood in here yet
    ;[(acc (f (fn x) body)) (begin (hash-set! ht (syntax->datum (syntax fn)) (syntax->datum (syntax body)))
    ;                                 #'(f (fn x) body))]  ;<--separate handling for fn defn...todo
    
    ; Designed to be called in Definitions Window to create a run-time binding to the AccRack hashtable
    [(acc) (datum->syntax #'acc ht)]
    [(acc view) (begin (printf "~a~n" ht) #'(void))]
    [(acc get) #'ht]
    
    ; Designed to be called in Interactions Windows to reset the REPL compilation environment's hashtable
    [(acc load ht3) (begin (set! ht (syntax->datum (syntax ht3))) #'(display ht3))]
    
    ; Placeholder for run command.  Just display the hashtable contents.
    [(acc run) #'(begin (printf "Wish I could run: ~a~n" (acc)))]
    
    ; Catch unrecognized commands
    [(acc exp) (begin (printf "uncaught:~a~n" (syntax->datum stx)) #'(void))]
    
    ; Handles multiple commands
    [(acc exp exp2 x ...) #'(begin (acc exp) (acc exp2 x ...))]
    ))
