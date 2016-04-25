#lang racket

(require accelerack
         accelerack/private/passes/typecheck
         accelerack/private/wrappers
         accelerack/private/syntax-table
         accelerack/private/types
         accelerack/private/front-end)
(require rackunit
         syntax/macro-testing)

(define (typeit x)
  (define-values (ty _) (typecheck-expr (snap-as-list) x))
  ty)

(check-equal? (typeit #'(lambda ((x : #(Int Double Bool))) (vector-ref x 2)))
              '(-> #(Int Double Bool) Bool))

(test-true "polymorphic vector ref 1"
  (match (typeit #'(lambda ((x : #(a b c))) (vector-ref x 2)))
    [`(-> #(,a ,b ,c1) ,c2) #:when (eq? c1 c2)
     #t]))

(test-true "polymorphic vector ref 2"
   (match (typeit #'(lambda ((x : #(a a a))) (vector-ref x 2)))
     [`(-> #(,a ,a ,a) ,a) #t]))

(test-exn "ambiguous vector len"
   #rx"of known length"
   (lambda ()
     (convert-compile-time-error
      (let ()
        (typeit  #'(lambda (x) (vector-ref x 2)))))))

(check-exn
 #rx"rigid"
 (lambda ()
   (convert-compile-time-error
    (typeit #'(lambda ((x : #(a a a))) (+ 1 (vector-ref x 2)))))))

(test-equal? "monomorphic lambda"
             (typeit #'(lambda (x) (+ 5 x)))
             '(-> Int Int))

(test-true "polymorphic lambda"
  (match (typeit #'(lambda (x) (+ x x)))
    [`(-> ,n ,n) #:when (numeric-type-var? n) #t]))

(check-equal? (typeit #'(let ([x 3]) (+ 5 x)))     'Int)
(check-equal? (typeit #'(let ([x #t]) (+ 5 9)))    'Int)
(check-equal? (typeit #'(if #t 5 9))               'Int)
(check-equal? (typeit #'(let ([x #t]) (if x x x))) 'Bool)
(check-equal? (typeit #'(let ([x : Bool #t]) (if x x x))) 'Bool)

(check-equal? (typeit #'(acc-array-ref (acc-array ((9.9))) 0 0))
              'Double)

; (acc-echo-types)
(test-case "check type inferred for colarray->vec4array"
  (define-acc (col-r (v : #(Int Int Int))) (vector-ref v 0))
  (define-acc (col-g (v : #(Int Int Int))) (vector-ref v 1))
  (define-acc (col-b (v : #(Int Int Int))) (vector-ref v 2))
  (define-acc (colarray->vec4array arr)
    (let ([helper2
           (lambda ((c : #(Int Int Int)))
             (vector (col-r c)
                     (col-g c)
                     (col-b c)
                     255))])
      (map helper2 arr)))
  (match (type-of colarray->vec4array)
    [`(-> (Array ,n #(Int Int Int)) (Array ,n #(Int Int Int Int)))
     (void)]
    [oth (error 'failed-test "unexpected result: ~a" oth)]))

(test-case "dot"  
  (define-acc (dot (v1 : #(Double Double Double))
                   (v2 : #(Double Double Double)))
    (+ (* (vector-ref v1 0)
          (vector-ref v2 0))
       (+ (* (vector-ref v1 1)
             (vector-ref v2 1))
          (* (vector-ref v1 2)
             (vector-ref v2 2)))))
  (check-equal? (type-of dot)
                '(-> #(Double Double Double) #(Double Double Double) Double)))

(test-case "fold2d"
  (define-acc (fold2d f z (a : (Array 2 Double)))
    (fold f z (fold f z a)))
  (check-equal?
   (type-of fold2d)
   '(-> (-> Double Double Double) Double (Array 2 Double) (Array 0 Double))))
