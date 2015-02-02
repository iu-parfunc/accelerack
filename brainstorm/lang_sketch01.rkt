#lang racket
;; Typed racket?  lang accelerate?
;; (require accelerate) ;; Get both staged and Racket bindings

;; Dummy wrappers:
(define generate build-list)
(define (unindex1 x) x)
(define (index1 x) x)
;; TODO replace above identity functions with ADT.
; (define-struct shape ... ? ? )

(define-syntax acc
  (syntax-rules ()
    [(acc x ...) (begin x ...)]))
(define-syntax run-acc
  (syntax-rules ()
    [(run-acc x) (begin x)]))
  

;; ---------------------------------
;; Core ops: generate, map, fold

(acc 
 (define (sqr x) (* x x))
 )

;; If I define that helper function, it would be really nice
;; if it were available BOTH to Racket code and Acc code.
(sqr 3)
;; This is a big win over what Accelerate/Haskell can do.

;;; Don't forget, indices are not just numbers:
;; Z :. Int
;; Z :. 3 


;; I think we should have two namespaces.. like a lisp-2...
;; But we should be able 
(acc 
 ;; Will probably need type annotation: (:: e t) 
 (define x (generate (index1 10) 
                     (lambda (x) (* 2 (unindex1 x)))))
 (define y (map sqr x))
 ; (define y (map (if #t sqr acc) x))
 )

;; Printing "y" should just print an AST.
;; Running y on the other hand actually goes to the GPU:
(run-acc y)
