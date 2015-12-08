#lang racket

;; This is Paventhan's prototype for capturing expressions
;; and launching them over to Haskell.

(require ffi/unsafe
         ffi/unsafe/define
	 racket/runtime-path
         accelerack/private/parse
         accelerack/private/allocate
         accelerack/private/arrayutils
         accelerack/private/global_utils
         (prefix-in rkt: accelerack/private/racket_ops)
         (except-in accelerack/private/header acc-array? make-acc-array)
         (prefix-in acc: (only-in accelerack/private/header acc-array?))
         (prefix-in acc: (only-in accelerack/private/header make-acc-array))
         (only-in '#%foreign ctype-scheme->c ctype-c->scheme)
         (for-syntax racket/base syntax/parse)
         (prefix-in r: racket/base)

         accelerack/private/types
         )

(provide acc
         cmap
         acc-map
         acc-zipwith
         acc-fold
;         generate

         segment-length
         segment-data
         scalar
         _segment-pointer
         build-sexp
         append-string*

         ;; wrappers:
         map fold zipwith
         )

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

(define-runtime-path libsxp "../../../acc_hs/libsxp.so")
(define lib-sxp (ffi-lib libsxp))
(define-ffi-definer define-sxp lib-sxp)

(define-runtime-path librts "../../../acc_c/librts.so")
(define lib-rts (ffi-lib librts))
(define-ffi-definer define-rts lib-rts)

(define-rts ark_init (_fun -> _void))
(define-rts ark_exit (_fun -> _int))

(ark_init)

(define-hs accelerateMap (_fun _acc-array-pointer _acc-array-pointer _int _int -> _void))
(define-hs accelerateZipWith (_fun _acc-array-pointer _acc-array-pointer _acc-array-pointer _int -> _void))
(define-hs accelerateFold (_fun _acc-array-pointer _acc-array-pointer _int _int -> _void))
(define-sxp runAcc (_fun _string _acc-array-pointer _acc-array-pointer -> _void))
(define run-acc runAcc)

;; Merging acc_sexp.rkt and ../main.rkt to avoid cycles while loading modules
;;----------------------------------------------------------------------------


;; RRN: No, the racket ops should use the acc-array representation FROM THE START:
(define (acc-map f x)
  (if (acc-array? x)
      (make-acc-array (rkt:acc-map f (acc-array-val x)))
      (error 'acc-map "acc-array expected")))

(define (acc-zipwith f x y)
  (if (and (acc-array? x) (acc-array? y))
      (make-acc-array (rkt:acc-zipwith f (acc-array-val x) (acc-array-val y)))
      (error 'acc-zipwith "acc-array expected")))

(define (acc-fold f def x)
  (if (acc-array? x)
      (make-acc-array (rkt:acc-fold f def (acc-array-val x)))
      (error 'acc-zipwith "acc-array expected")))



;---------------

(define (procedure->symbol proc)
  (cond
    ((equal? proc add1) 'add1)
    ((equal? proc sub1) 'sub1)
    ((equal? proc map) 'map)
    ((equal? proc zipwith) 'zipwith)
    ((equal? proc fold) 'fold)
    ((equal? proc '(++)) '(++))
    ((equal? proc '(--)) '(--))
    ((equal? proc '(**)) '(**))))
    ;; (else proc)))

(define (symbol->procedure sym)
  (cond
    ((equal? sym 'add1) add1)
    ((equal? sym 'sub1) sub1)
    ((equal? sym 'map) map)
    ((equal? sym 'zipwith) zipwith)
    ((equal? sym 'fold) fold)
    ((equal? sym '(++)) '(++))
    ((equal? sym '(--)) '(--))
    ((equal? sym '(**)) '(**))))
    ;; (else sym)))

(define (procedure->string proc)
  (cond
    ((equal? proc add1) "(+1)")
    ((equal? proc sub1) "(+ (-1))")
    ((equal? proc map) "map")
    ((equal? proc zipwith) "zipwith")
    ((equal? proc fold) "fold")
    ((equal? proc '(++)) "(+)")
    ((equal? proc '(--)) "(-)")
    ((equal? proc '(**)) "(*)")
    ((equal? proc '+) "+")
    ((equal? proc '-) "-")
    ((equal? proc '*) "*")))

(define (list->string* ls)
  (cond
    ((null? ls) '())
    ((or (equal? (car ls) '(++)) (equal? (car ls) '(--)) (equal? (car ls) '(**)) (procedure? (car ls)))
       (cons (procedure->string (car ls)) (list->string* (cdr ls))))
    ((pair? (car ls)) (cons (list->string* (car ls)) (list->string* (cdr ls))))
    ((number? (car ls)) (cons (number->string (car ls)) (list->string* (cdr ls))))
    ((symbol? (car ls)) (if (procedure? (symbol->procedure (car ls)))
                            (cons (procedure->string (symbol->procedure (car ls))) (list->string* (cdr ls)))
                            (cons (symbol->string (car ls)) (list->string* (cdr ls)))))))

(define (append-strings ls str)
  (cond
    ((null? ls) str)
    ((pair? (car ls)) (letrec ([newstr (if (null? (cdr ls)) (string-append str " " "(" (append-strings (car ls) "") ")")
                                           (string-append str "(" (append-strings (car ls) "") ")"))])
                              (append-strings (cdr ls) newstr)))
    (else (letrec ([newstr (if (null? (cdr ls)) (string-append str (car ls)) (string-append str (car ls) " "))])
                  (append-strings (cdr ls) newstr)))))

(define (append-string* ls)
  (string-append "(" (append-strings (list->string* ls) "") ")"))

(define (get-id data data-ls inc)
  (cond
    ((member data data-ls) (values inc (list 'id (- (length data-ls) (length (member data data-ls))))))
    (else (values (+ inc 1) (list 'id inc)))))

(define (get-op data)
  (cond
    ((null? data) '())
    ((and (pair? (car data)) (equal? 'id (caar data))) '())
    (else (cons (car data) (get-op (cdr data))))))

(define (build-id-list data-ls data-ls* inc)
  (cond
    ((null? data-ls) (values inc '()))
    ((number? (car data-ls)) (letrec-values ([(inc* id-list) (build-id-list (cdr data-ls) data-ls* inc)])
                                            (values inc* (cons (car data-ls) id-list))))
    ((pair? (car data-ls))
       (letrec-values ([(inc* id-list) (build-id-list (read-data* (car (cdr data-ls))) data-ls* inc)]
                       [(inc** id-list*) (build-id-list (cdr (cdr data-ls)) data-ls* inc*)])
                      (values inc** (cons (append (get-op (car data-ls)) id-list) id-list*))))
    (else (letrec-values ([(dummy data*) (values '() (if (acc-array? (car data-ls)) (acc-array-val (car data-ls)) (car data-ls)))]
                          [(inc* new-id) (get-id (read-data* data*) data-ls* inc)]
                          [(inc** id-list) (build-id-list (cdr data-ls) data-ls* inc*)])
                         (values inc** (cons new-id id-list))))))

(define (remove-duplicate ls1 ls2)
  (cond
    ((null? ls1) '())
    ((member (read-data* (car ls1)) ls2) (remove-duplicate (cdr ls1) ls2))
    (else (cons (car ls1) (remove-duplicate (cdr ls1) ls2)))))

(define (group-data data-ls data-ls*)
  (cond
    ((null? data-ls) '())
    ((pair? (car data-ls)) (append (remove-duplicate (read-data* (car (cdr data-ls))) data-ls*)
                                   (group-data (cdr (cdr data-ls)) data-ls*)))
    (else (cons (car data-ls) (group-data (cdr data-ls) data-ls*)))))

(define (get-data-ptrs data-ls)
  (cond
    ((null? data-ls) '())
    ((acc-array? (car data-ls)) (cons (acc-array-val (car data-ls)) (get-data-ptrs (cdr data-ls))))
    ((acc:acc-array? (car data-ls)) (letrec ([payload (read-data* (car data-ls))])
                                      (append payload (get-data-ptrs (cdr data-ls)))))
    (else (get-data-ptrs (cdr data-ls)))))

(define (get-data-list data-ls)
  (cond
    ((null? data-ls) '())
    ((acc-array? (car data-ls)) (cons (read-data* (acc-array-val (car data-ls))) (get-data-list (cdr data-ls))))
    ((acc:acc-array? (car data-ls)) (letrec ([payload (read-data* (car data-ls))]
                                             [payload* (if (pair? payload) (if (acc:acc-array? (car payload)) (r:map read-data* payload) (list payload)) (list payload))])
                                      (append payload* (get-data-list (cdr data-ls)))))
    (else (get-data-list (cdr data-ls)))))

(define (build-sexp func opr . data)
  (letrec-values ([(dummy data-ls*) (values '() (remove-duplicates (get-data-list data)))]
                  [(dummy* data*) (values '() (if (and (not (null? data)) (memv #t (r:map pair? data)))
                                                  (remove-duplicates (get-data-ptrs data))
                                                  data))]
                  [(inc id-list) (build-id-list data data-ls* 0)]
                  [(dummy** data**) (values '() (filter (lambda (x) (or (acc-array? x) (cpointer? x))) data*))]
                  [(dummy*** acc-payload) (values '() (car (acc-alloc _acc-array-pointer (list (length data**)) (r:map (lambda (x) (if (acc-array? x) (acc-array-val x) x)) data**))))])
                 (values (append (list (if (procedure? func) (procedure->symbol func) func) (if (procedure? opr) (procedure->symbol opr) opr)) id-list) acc-payload)))

(define (convert-ptr x)
  (if (acc:acc-array? x) (make-acc-array x) x))


(define (sexp-interpreter sexp payload)
  (match sexp
    [`(,func ,opr ,arg) #:when (procedure? (symbol->procedure func))
                        (begin
                          ;;(printf "arg ~s\n" arg)
                          ((symbol->procedure func) (symbol->procedure opr) (sexp-interpreter arg payload)))]
    [`(,func ,opr ,value ,arg2) #:when (and (procedure? (symbol->procedure func)) (number? value))
                        (begin
                          ;;(printf "arg1 ~s and arg2 ~s\n" arg1 arg2)
                          ((symbol->procedure func) (symbol->procedure opr) value (sexp-interpreter arg2 payload)))]
    [`(,func ,opr ,arg1 ,arg2) #:when (procedure? (symbol->procedure func))
                        (begin
                          ;;(printf "arg1 ~s and arg2 ~s\n" arg1 arg2)
                          ((symbol->procedure func) (symbol->procedure opr) (sexp-interpreter arg1 payload) (sexp-interpreter arg2 payload)))]
    [`(id ,val)
     (begin
       ;;(printf "val ~s and payload ~s\n" val (read-data* (rkt:array-get payload val)))
       (convert-ptr (rkt:array-get payload val)))]
    [x (error "No matching pattern found")]))


(define-syntax (cmap stx)
  (syntax-case stx ()
    [(cmap exp data) #'(cpointer? data)
                           #'(if (equal? exp 'add1)
                                 (rkt_handler data "add1")
                                 (if (equal? exp 'sub1)
                                     (rkt_handler data "sub1")
                                     (error "function not defined")))]))

(define (process-function exp)
  (match exp
    (`(+ ,x) `(+ ,x))
    (`(++) `(++))
    (`(- ,x) `(+ (,(* -1 x))))
    (`(--) `(--))
    (`(* ,x) `(* ,x))
    (`(**) `(**))
    (`(^ ,x) `(^ ,x))))

(define-syntax (acc stx)
  (syntax-case stx (define acc:map acc:zipwith acc:fold)
    [(acc (acc:map exp data))  #'(cpointer? data)
                           #'(letrec-values ([(dummy res-ptr) (values '() (get-result-array data))]
                                             [(dummy* opr) (values '() (if (equal? exp add1) add1
                                                                           (if (equal? exp sub1) sub1
                                                                               (process-function exp))))]
                                             [(sexp payld) (build-sexp map opr data)])
                                            (begin
                                              (run-acc (append-string* sexp) payld res-ptr)
                                              res-ptr))]

    [(acc (acc:zipwith exp data1 data2))  #'(and (cpointer? data1) (cpointer? data2))
                           #'(letrec-values ([(dummy res-ptr) (values '() (get-result-array data1))]
                                             [(dummy* opr) (values '() (process-function exp))]
                                             [(sexp payld) (build-sexp zipwith opr data1 data2)])
                                            (begin
                                              (run-acc (append-string* sexp) payld res-ptr)
                                              res-ptr))]

    [(acc (acc:fold exp value data))  #'(cpointer? data)
                           #'(letrec-values ([(dummy type*) (values '() (if (equal? ((ctype-scheme->c scalar) 'acc-payload-ptr) (type data))
                                                                            (error 'acc-fold "fold cannot be used on tuples") (mapType (type data))))]
                                             [(dummy* shape*) (values '() (if (null? (shape data)) '(1) (reverse (cdr (reverse (shape data))))))]
                                             [(dummy** res-ptr) (values '() (car (alloc-unit shape* type*)))]
                                             [(dummy*** data*) (values '() (if (acc-array? data) (acc-array-val data) data))]
                                             [(dummy**** shape**) (values '() (reverse (read-data (acc-array-shape data*))))]
                                             [(dummy***** shape***) (values '() (car (generatePayload shape** _int)))]
                                             [(dummy****** data**) (values '() (make-acc-array (acc:make-acc-array (acc-array-type data*) shape*** (acc-array-data data*))))]
                                             [(dummy******* opr) (values '() (process-function exp))]
                                             [(sexp payld) (build-sexp fold opr value data**)])
                                            (begin
                                              (run-acc (append-string* sexp) payld res-ptr)
                                              res-ptr))]


    ; Variable definition
    [(acc (define x exp)) (and (identifier? #'x)) #'(define x (car exp))]


    ; Catch unrecognized commands
    [(acc exp) (begin (printf "uncaught:~a~n" (syntax->datum stx)) #'(void))]

    ; Handles multiple commands
    [(acc exp exp2 x ...) #'(begin (acc exp) (acc exp2 x ...))]
    ))

;(define x (acc-array (1 2 3 4 5 6 7 8)))
;(define y (acc-array (10 20 30 40 50 60 70 80)))
;(define z (acc-array (100 200 300 400 500 600 700 800)))

;(define y (acc-array (12.34 56.78 34.12 45.32 65.45)))
;(define z (acc-array (71.50 45.12 45.68 76.34 57.90)))
;(define z1 (acc-array (10.009 20.009 30.009 40.009 50.009)))

#|(define (tester1)
  (letrec-values (;[(sexp payld) (build-sexp zipwith (++) x y)]
                  [(sexp* payld*) (build-sexp map add1 x)]) ;;sexp payld)])
    (begin
      (printf "function (map add1 (zipwith (++) x y)\n")
      (printf "s-exp ~s\n" sexp*)
      (printf "x ~s\n" x)
      (printf "y ~s\n" y)
      (sexp-interpreter sexp* payld*))))

(tester1)

(map add1 x)|#

;; (define x (acc-array ((1.11 2.22 3.33) (4.44 5.55 6.66))))
;; (read-data (acc-array-shape (acc-array-val x)))



;; These have been replace by wrappers.rkt:
;; --------------------------------------------------------------------------------

(define (map f x)
  (if (acc-array? x)
      (make-acc-array (acc (acc:map f (acc-array-val x))))
      (if (acc:acc-array? x)
          (make-acc-array (acc (acc:map f x)))
             (r:map f x))))

(define (fold f def x)
  (if (acc-array? x)
      (make-acc-array (acc (acc:fold f def (acc-array-val x))))
      (if (acc:acc-array? x)
          (make-acc-array (acc (acc:fold f def x)))
          (error 'fold "acc-array expected"))))

(define (zipwith f x y)
  (if (and (acc-array? x) (acc-array? y))
      (make-acc-array (acc (acc:zipwith f (acc-array-val x) (acc-array-val y))))
      (if (and (acc:acc-array? x) (acc:acc-array? y))
          (make-acc-array (acc (acc:zipwith f x y)))
          (error 'zipwith "acc-array expected"))))
