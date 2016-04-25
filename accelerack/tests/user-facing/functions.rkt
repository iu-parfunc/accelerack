#lang racket
(require accelerack
         ; (for-syntax accelerack )
         rackunit)

(test-case "identity function definition"
  (define-acc (f x) x)
  (check-true (procedure? f))
  (check-equal? 3 (f 3)))

(test-case "plus 5 function definition"
  (: f (-> Int Int))
  (define-acc (f x) (+ x 5))
  (check-true (procedure? f))
  (check-equal? 8 (f 3)))

(test-case "add1 function definition"
           "add1 function definition"
  (define-acc (f x) (add1 x))
  (check-true (procedure? f))
  (check-equal? 4 (f 3)))

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


; (acc-echo-types)

(test-case "app sqr"
  ;; FIXME: this one should get a "rigid type variable error" (CC #51)
  ;; (: sqr (-> num_a Int))
  (: sqr (-> num_a num_a))
  (: y Int)
  (define-acc (sqr x) (* x x))
  (define-acc y (sqr 3))
  ; (define-acc z (generate (lambda () (sqr #t))))
  ;; FIXME!: 
  ; (check-equal? (format "~a" y) "3")
  ; (check-false (acc-array? y))
  (void)
  )


;; FIXME: see issue #55
; (test-case "resolve ambiguity with top-level ascription"
;   (: col-r (-> #(Int Int Int) Int))
;   (define-acc (col-r v) (vector-ref v 0))
;   (col-r (vector 1 2 3)))

