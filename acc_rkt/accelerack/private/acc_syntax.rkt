#lang racket

(require ffi/unsafe
         ffi/unsafe/define
	 racket/runtime-path
         accelerack/private/acc_parse
         accelerack/private/acc_allocate
         accelerack/private/acc_arrayutils
         accelerack/private/acc_header
         (only-in '#%foreign ctype-scheme->c ctype-c->scheme))

(provide ++
         --
         **
         acc
         array
         _tuple
         cmap
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

(define-runtime-path librachs "../../../acc_hs/librachs.so")
(define lib-hs (ffi-lib librachs))
(define-ffi-definer define-hs lib-hs)

(define-runtime-path librts "../../../acc_c/librts.so")
(define lib-rts (ffi-lib librts))
(define-ffi-definer define-rts lib-rts)

(define-rts ark_init (_fun -> _void))
(define-rts ark_exit (_fun -> _int))

(ark_init)

(define-hs accelerateMap (_fun _acc-array-pointer _acc-array-pointer _int _int -> _void))
(define-hs accelerateZipWith (_fun _acc-array-pointer _acc-array-pointer _acc-array-pointer _int -> _void))
(define-hs accelerateFold (_fun _acc-array-pointer _acc-array-pointer _int _int -> _void))


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
                                         (acc-alloc type (syntax->datum (syntax (shape ...))) data*)
                                         (error 'verify-accelerack (cadr ret))))]
    [(array (shape ...) type data)
                           #'(let ((ret (verify-accelerack (vector type (syntax->datum (syntax (shape ...))) (flatten data)))))
                                  (if (car ret)
                                      (acc-alloc type (syntax->datum (syntax (shape ...))) data)
                                      (error 'verify-accelerack (cadr ret))))]))

(define-syntax (cmap stx)
  (syntax-case stx ()
    [(cmap exp data) #'(cpointer? data)
                           #'(if (equal? exp 'add1)
                                 (rkt_handler data "add1")
                                 (if (equal? exp 'sub1) 
                                     (rkt_handler data "sub1")
                                     (error "function not defined")))]))


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

(define (process-function exp)
  (match exp
    (`(+ ,x) (list 0 x))
    (`(++) 0)
    (`(- ,x) (list 0 (* -1 x)))
    (`(--) 1)
    (`(* ,x) (list 1 x))
    (`(**) 2)
    (`(^ ,x) (list 2 x))))

(define-syntax (** stx)
  (syntax-case stx ()
    [(** x) #''(* x)]
    [(**) #''(**)]))

(define-syntax (++ stx)
  (syntax-case stx ()
    [(++ x) #''(+ x)]
    [(++) #''(++)]))

(define-syntax (-- stx)
  (syntax-case stx ()
    [(-- x) #''(- x)]
    [(--) #''(--)]))

(define-syntax (acc stx)
  (syntax-case stx (define acc:map acc:zipwith acc:fold)
    [(acc (acc:map exp data))  #'(cpointer? data)
                           #'(letrec ([result-arr (get-result-array data)]
                                      [value-ls (if (equal? exp add1) (list 0 1)
                                                    (if (equal? exp sub1) (list 0 -1)
                                                        (process-function exp)))]
                                      [opr (car value-ls)]
                                      [value (cadr value-ls)])
                                     (begin
                                       (accelerateMap data result-arr value opr)
                                       result-arr))]

    [(acc (acc:zipwith exp data1 data2))  #'(and (cpointer? data1) (cpointer? data2))
                           #'(letrec ([type (if (equal? ((ctype-scheme->c scalar) 'acc-payload-ptr) (get-type data1))
                                                 (error "TODO : Currently not supporting tuple types for zipwith")
                                                 (mapType (get-type data1)))]
                                      [shape (find-shape (get-shape data1) (get-shape data2) '())]
                                      [result-arr (car (alloc-unit shape type))]
                                      [opr (process-function exp)])
                                     (begin
                                       (accelerateZipWith data1 data2 result-arr opr)
                                       result-arr))]

    [(acc (acc:fold exp value data))  #'(cpointer? data)
                           #'(letrec ([type (if (equal? ((ctype-scheme->c scalar) 'acc-payload-ptr) (get-type data))
                                                 (error "TODO : Currently not supporting tuple types for zipwith")
                                                 (mapType (get-type data)))]
                                      [shape* (if (null? (shape data)) '(1) (reverse (cdr (reverse (shape data)))))]
                                      [result-arr (car (alloc-unit shape* type))]
                                      [opr (process-function exp)])
                                     (begin
                                       (accelerateFold data result-arr value opr)
                                       result-arr))]

    ; Variable definition    
    [(acc (define x exp)) (and (identifier? #'x)) #'(define x (car exp))]
    
                
    ; Catch unrecognized commands
    [(acc exp) (begin (printf "uncaught:~a~n" (syntax->datum stx)) #'(void))]
    
    ; Handles multiple commands
    [(acc exp exp2 x ...) #'(begin (acc exp) (acc exp2 x ...))]
    ))
