#lang racket

(require accelerack)
(require accelerack/private/passes/typecheck)
(require accelerack/private/front-end)
(require rackunit)

(define (typeit x)
  (define-values (ty _) (typecheck-expr (snap-as-list) x))
  ty)

(check-equal? (typeit #'(lambda ((x : #(Int Double Bool))) (vector-ref x 2)))
              '(-> #(Int Double Bool) Bool))

(test-case ""
           (match (typeit #'(lambda ((x : #(a b c))) (vector-ref x 2)))
             [`(-> #(,a ,b ,c1) ,c2) #:when (eq? c1 c2)
                                   (void)]
             [else (error 'test "bad result")]))

;; This one currently should fail:
; (typecheck-expr (snap-as-list) #'(lambda (x) (vector-ref x 2)))
; typecheck: This is expected to have a vector type of known length, instead found: a1 in: x


;; Oops looks like there are bugs in instantiation.  These should work:

; (typecheck-expr (snap-as-list) #'(lambda ((x : #(a a a))) (+ 1 (vector-ref x 2))))

; (typecheck-expr (snap-as-list) #'(lambda ((x : #(a b c))) (+ 1 (vector-ref x 2))))
#|
unify-types: Conflicting types.
Found: Int
Expected: c3
 in: (+ 1 (vector-ref x 2))
|#


; (typecheck-expr (snap-as-list) #'(lambda (x) (+ 5 x)))


