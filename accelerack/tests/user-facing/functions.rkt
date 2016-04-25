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


;; FIXME: here too weak of a type was inferred:
;;  (-> (Array n8 a6) (Array n8 #(res39 res410 res511 Int)))
;; For the definition:
; (acc-echo-types)
#; 
(test-case ""
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
      (map helper2 arr))))
