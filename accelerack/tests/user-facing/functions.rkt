#lang racket
(require accelerack
         ; (for-syntax accelerack )
         rackunit)

(test-case "identity function definition"
  (define-acc (f x) x)
  (check-true (procedure? f))
  (check-equal? 3 (f 3)))

(test-case "plus 5 function definition"
  (define-acc (f x) (+ x 5))
  (check-true (procedure? f))
  (check-equal? 8 (f 3)))

; (acc-echo-types)

(test-case "vec+"
  (define-acc vec+
    (lambda ((v1 : #(Int Int Int))
             (v2 : #(Int Int Int)))
      (vector (+ (vector-ref v1 0)
                 (vector-ref v2 0))
              (+ (vector-ref v1 1)
                 (vector-ref v2 1))
              (+ (vector-ref v1 2)
                 (vector-ref v2 2)))))
  
  (check-true (procedure? vec+))
  )

(test-case "vec+ 2"
  (define-acc (vec+ (v1 : #(Int Int Int))
                    (v2 : #(Int Int Int)))
    (vector (+ (vector-ref v1 0)
               (vector-ref v2 0))
            (+ (vector-ref v1 1)
               (vector-ref v2 1))
            (+ (vector-ref v1 2)
               (vector-ref v2 2))))
  (void))



