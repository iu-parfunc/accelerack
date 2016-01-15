#lang racket

;; ---------------------------------------------------------------
;; Defines a compiler pass which:
;;  (1) All array operators are directly followed by lambda
;;  (2) Should never throw errors hence can directly operate on sexp
;;      rather than rich syntax expression
;; ---------------------------------------------------------------

(require ffi/unsafe
         ffi/unsafe/define
         rackunit
         rackunit/text-ui
         racket/runtime-path
	 racket/match
         (only-in '#%foreign ctype-scheme->c ctype-c->scheme))

;; Validate if output matches the expected output
;; ---------------------------------------------------------------
;; TODO - needs changes according to output of typecheck
;; Should throw an error for incorrect input 
(define (check-if-lambda exp)
  (let ((check-e check-if-array-ops-followed-by-lamda))
    (begin      
      (check-match exp `(lambda (,x ...) ,e))
      (match exp
	    (`(lambda (,x ...) ,e) (check-e e))
	    (`,x (void))))))

;; TODO - This currently supresses any errors in input - but input should match output of typecheck
(define (check-if-array-ops-followed-by-lamda exp)
  (let ((check check-if-array-ops-followed-by-lamda))
    (match exp
      (`(map ,l ,e2) (and (check-if-lambda l) (check e2)))
      (`(zipwith ,l ,e1 ,e2) (begin
			       (check-if-lambda l)
			       (check e1)
			       (check e2)))
      (`(fold ,l ,e1 ,e2) (begin
			       (check-if-lambda l)
			       (check e1)
			       (check e2)))
      (`(generate ,e ....) (map check e))
      (`(stencil3x3 ,l ,b ,e) (begin
			       (check-if-lambda l)
			       (check e)))      
      (`(let (,x ...) ,e) (check e))
      (`(lambda (,x ...) ,e) (check e))
      (`(if ,e1 ,e2 ,e3) (begin
			   (check e1) (check e2) (check e3)))
      (`(acc-array ,a) (void))
      ;; General application is removed - Only primitives allowed
      (`(,p ,e ...) (memq p '(acc-array-ref vector vector-ref)) (map check e))
      (`,x (void)))))

(check-if-array-ops-followed-by-lamda `(begin
					 (map (lambda(x) 1) (list (1 23 4)))))


