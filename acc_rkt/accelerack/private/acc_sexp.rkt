#lang racket

(require (except-in ffi/unsafe ->)
         ffi/unsafe/cvector
         ;accelerack/private/acc_racket_ops
         accelerack/private/acc_allocate
         accelerack/private/acc_arrayutils
         accelerack/private/acc_syntax
         accelerack/main
         (only-in '#%foreign ctype-scheme->c ctype-c->scheme)
         racket/contract
         )

(define (procedure->symbol proc)
  (cond
    ((equal? proc add1) 'add1)
    ((equal? proc sub1) 'sub1)
    ((equal? proc map) 'map)
    ((equal? proc zipwith) 'zipwith)
    ((equal? proc fold) 'fold)
    (else proc)))

(define (symbol->procedure sym)
  (cond
    ((equal? sym 'add1) add1)
    ((equal? sym 'sub1) sub1)
    ((equal? sym 'map) map)
    ((equal? sym 'zipwith) zipwith)
    ((equal? sym 'fold) fold)
    (else sym)))

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
    #|((and (pair? (car data-ls)) (equal? 'id (caar data-ls)))
       (letrec-values ([(inc* new-id) (get-id (read-data* (car data-ls)) data-ls* inc)]
                       [(inc** id-list) (build-id-list (cdr data-ls) data-ls* inc*)])
                      (values inc** (cons new-id id-list)))) 
    ((symbol? (car data-ls))
       (begin (printf "symbol is ~s\n" (car data-ls))
       (letrec-values ([(inc* id-list) (build-id-list (cdr data-ls) data-ls* inc)])
                      (values inc* (cons (car data-ls) id-list)))))|#
    ((pair? (car data-ls))
       (begin ;(printf "pair is ~s\n" (car data-ls))
       (letrec-values ([(inc* id-list) (build-id-list (read-data* (car (cdr data-ls))) data-ls* inc)]
                       [(inc** id-list*) (build-id-list (cdr (cdr data-ls)) data-ls* inc*)])
                      (values inc** (cons (append (get-op (car data-ls)) id-list) id-list*)))))
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
                                             ;;[payload* (if (pair? payload) (map read-data* payload) (list payload))])
                                      (begin ;;(printf "payld ~s and payld* ~s\n" payload payload*)
                                      (append payload (get-data-ptrs (cdr data-ls))))))
    (else (get-data-ptrs (cdr data-ls)))))

(define (get-data-list data-ls)
  (cond
    ((null? data-ls) '())
    ((acc-array? (car data-ls)) (cons (read-data* (acc-array-val (car data-ls))) (get-data-list (cdr data-ls))))
    ((acc:acc-array? (car data-ls)) (letrec ([payload (read-data* (car data-ls))]
                                             [payload* (if (pair? payload) (map read-data* payload) (list payload))])
                                      (begin ;;(printf "payld ~s and payld* ~s\n" payload payload*)
                                      (append payload* (get-data-list (cdr data-ls))))))
    (else (get-data-list (cdr data-ls)))))
  
(define (build-sexp func opr . data)
  (letrec-values ([(dummy data-ls*) (values '() (remove-duplicates (get-data-list data)))]
                  [(dummy* data*) (values '() (if (and (not (null? data)) (memv #t (map pair? data)))
                                                  (remove-duplicates (get-data-ptrs data))
                                                  data))]
                  [(inc id-list) (build-id-list data data-ls* 0)]
                  [(dummy** acc-payload) (values '() (car (acc-alloc _acc-array-pointer (list (length data*)) (map (lambda (x) (if (acc-array? x) (acc-array-val x) x)) data*))))])
                  (begin ;;(printf "data* is ~s\n" data*)
                 (values (append (list (procedure->symbol func) (if (procedure? opr) (procedure->symbol opr) opr)) id-list) acc-payload))))

(define (convert-ptr x)
  (if (acc:acc-array? x) (make-acc-array x) x))

(define (sexp-interpreter sexp payload)
  (match sexp
    [`(,func ,opr ,arg) #:when (procedure? (symbol->procedure func))
                        (begin
                          ;;(printf "arg ~s\n" arg)
                          ((symbol->procedure func) (symbol->procedure opr) (sexp-interpreter arg payload)))]
    [`(,func ,opr ,arg1 ,arg2) #:when (procedure? (symbol->procedure func))
                        (begin
                          ;;(printf "arg1 ~s and arg2 ~s\n" arg1 arg2)
                          ((symbol->procedure func) (symbol->procedure opr) (sexp-interpreter arg1 payload) (sexp-interpreter arg2 payload)))]
    [`(id ,val)
     (begin
       ;;(printf "val ~s and payload ~s\n" val (read-data* (rkt:array-get payload val)))
       (convert-ptr (rkt:array-get payload val)))]
    [x (error "No matching pattern found")]))

(define x (acc-array (1 2 3 4 5 6 7 8)))
(define y (acc-array (10 20 30 40 50 60 70 80)))
(define z (acc-array (100 200 300 400 500 600 700 800)))

;(define y (acc-array (12.34 56.78 34.12 45.32 65.45)))
;(define z (acc-array (71.50 45.12 45.68 76.34 57.90)))
;(define z1 (acc-array (10.009 20.009 30.009 40.009 50.009)))

(define (tester1)
  (letrec-values ([(sexp payld) (build-sexp zipwith (++) x y)]
                  [(sexp* payld*) (build-sexp map add1 sexp payld)])
    (begin
      (printf "function (map add1 (zipwith (++) x y)\n")
      (printf "s-exp ~s\n" sexp*)
      (printf "x ~s\n" x)
      (printf "y ~s\n" y)
      (sexp-interpreter sexp* payld*))))

(define (tester2)
  (letrec-values ([(sexp payld) (build-sexp zipwith (++) x y)]
                  [(sexp* payld*) (build-sexp zipwith (++) z sexp payld)])
    (begin
      (printf "function (zipwith z (zipwith (++) x y)\n")
      (printf "s-exp ~s\n" sexp*)
      (printf "x ~s\n" x)
      (printf "y ~s\n" y)
      (printf "z ~s\n" z)
      (sexp-interpreter sexp* payld*))))

(define (tester3)
  (letrec-values ([(sexp payld) (build-sexp zipwith (++) x y)]
                  [(sexp* payld*) (build-sexp zipwith (++) x sexp payld)])
    (begin
      (printf "function (zipwith (++) x (zipwith (++) x y)\n")
      (printf "s-exp ~s\n" sexp*)
      (printf "x ~s\n" x)
      (printf "y ~s\n" y)
      (sexp-interpreter sexp* payld*))))

(printf "Test case 1\n")
(tester1)
(printf "\nTest case 2\n")
(tester2)
(printf "\nTest case 3\n")
(tester3)