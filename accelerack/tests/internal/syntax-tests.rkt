#lang racket

(require rackunit
         ;; See NOTE below:
         (only-in accelerack acc-array acc-array-ref fold map zipwith generate :
                  acc-array?
                  ;; If you leave these out, the error messages can be bad:
                  Int Bool Double use
                  Array)
         accelerack/private/syntax-capture
         (only-in accelerack/acc-array/private/delayed acc-delayed-array?)
         )

(printf "Running syntax tests that use define-acc\n")


(test-case "defs1"  
  (define-acc num2 (+ (: 3 Int) 4))

  ; (check-equal? num2 7)
  (void)
)

; (define plain 34)
; (define-acc num3 (+ 1 (use plain)))

(test-case "pred1"
  (define-acc ac 3)
  (define-acc foo (+ ac 4)) ;; Use a previous binding.
  (check-pred acc-array? ac) ;; TEMP: this needs to be a scalar.
) 

(test-case "run sqr"
  (define-acc (sqr x) (* x x))
  
  (check-eq? 9 (sqr 3)))

(test-case "numa"
  (define-acc num (* 3 (+ 4 (- 5 (add1 2)))))
  (void))

;; (define-acc id1 (lambda (x) x))


;; TODO: unit tests for syntax failures:
;; ----------------------------------------
(define y 999)
; (define-acc id2 (lambda (x) y))

; (define-acc z (* y y)) ;; Good errors here now [2015.12.09]

; (define-acc id3 (lambda (x) completely-unbound-var))

;; ----------------------------------------

(test-case "test01"
;; NOTE: This is a bit tricky because if, e.g., array-ref is ever
;; unbound... then we get a bad error message here:
;;
;; We should tweak verify-acc to check the identifier status of the
;; supposed special-forms like array-ref/acc-array/etc.
  (define-acc test01 (lambda (x) (acc-array-ref (if #t x x) 0 3)))
  (void))

;; (define-acc test01B (test01 (acc-array ((1 2 3 4 5 6)))))
(test-case "test02"
  (define-acc test02 (lambda (x)
                     (zipwith + ; * ;; FIXME: * does not work here... huh?
                              (map add1 (fold + 0 (acc-array (1 2 3 4))))
                              ;; (acc-array (10 10 10 10))
                              ;; TODO: ^^ improve the type error for this line.
                              (acc-array 10))
                     ))
  (void))

;; (acc-array->sexp test02)
;; (run-eval test02)

(test-case "test03"
  (define-acc test03 (vector-ref (vector 1 2 3) 0))
  (void))

(test-case "test04-06"
  (define-acc test04 (generate (lambda () (+ 3 4))))
  (define-acc test05 (generate (lambda (x) (+ x 3)) 100))
  (define-acc test06 (generate (lambda (x y) (+ x y)) 100 100))

  (check-pred acc-array? test04)
  (check-pred acc-array? test05)
  (check-pred acc-array? test06))

(test-case "test07-09"
  (define-acc test07 (lambda ((x : Int) (y : Bool)) (if y x 3)))
  (define-acc test08 (lambda ((y : Int)) 3))
  (define-acc test09 : (-> Double Double Double)
    (lambda ((y : Double) z) (+ y z)))
  
  (check-pred procedure? test07)
  (check-pred procedure? test08)
  (check-pred procedure? test09))

(test-case "test10-13"
  (define-acc test10 : Int (let ([x 3] [y : Int 4]) (+ x y )))

  (define-acc test11 (: (lambda (x) x) (-> Double Double)))
  (define-acc test12 (: (lambda ((x : Double)) x) (-> Double Double)))
  (define-acc test13 (lambda ((x : Double)) (: x Double)))
  
  ; (check-pred procedure? test11)  ;; FIXME... we need to be smart about how we handle these.
  ; (check-pred procedure? test12)
  (check-pred procedure? test13))

(test-case "test14"
  (define rktarr (acc-array (1 2 3)))
  (define-acc test14 (map add1 (use rktarr (Array 1 Int))))
  (check-pred acc-array? test14))

;; TODO: the story for scalars is not established yet.
; (check-pred acc-delayed-scalar? test10)

;;(define table (snapshot-current-acc-syn-table))
