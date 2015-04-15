#lang racket

(require rackunit)
(require "../lib/types.rkt")
(require "../lib/accelerack.rkt")
(require/expose "../lib/accelerack.rkt" (zero-array gen-indices plty-default))

;; Basic in-Racket Array operations:
;; -----------------------------------------------------
(define arr1 (r-arr (Z 4) (array* 1 'Word64)
                    (list (u64vector 0 10 20 30))))
(define arr2 (r-arr (Z 1 5)
                    (array* 2 '#(Word64 Word64 Word64))
                    (list (u64vector 0 1 2 3 4)
                          (u64vector 5 6 7 8 9)
                          (u64vector 10 11 12 13 14))))
(define arr3 (r-arr (Z 2 3)
                    (array* 2 '#(Word64 Word64 Word64))
                    (list (u64vector 0 1 2 3 4 5)
                          (u64vector 6 7 8 9 10 11)
                          (u64vector 12 13 14 15 16 17))))
(define arr4 (r-arr (Z 2 2) (array* 2 '#(Word64 Float Word64))
                    (list (u64vector 4 5 6 7)
                          (f64vector 10 11 12 13)
                          (u64vector 22 33 44 55))))
;rget
(check-eqv? (rget arr1 (Z 2)) 20)
(check-equal? (rget arr2 (Z 0 0)) #(0 5 10))
(check-equal? (rget arr2 (Z 0 3)) #(3 8 13))
(check-exn #rx"Invalid index for array" (λ ()
                                          (rget arr1 (Z 4))))
(check-exn exn:fail? (λ ()
                       (rget arr3 (Z 0 3))))
;rput
(check-eqv? (begin (rput arr1 (Z 2) 25) (rget arr1 (Z 2)))
            25)
(check-equal? (begin (rput arr2 (Z 0 3) #(103 108 113)) (rget arr2 (Z 0 3)))
              #(103 108 113))

;; Acc-Fn
;(rget arr2 (Z))


; zero-array
;singletons
(check-equal? (zero-array (Z) 'Word64) (r-arr (Z) (array* 0 'Word64) (list #(0))))
(check-equal? (zero-array (Z) '#(Word64)) (r-arr (Z) (array* 0 '#(Word64)) (list #(0))))
(check-equal? (zero-array (Z) '#(Word64 Word64)) (r-arr (Z) (array* 0 '#(Word64 Word64)) (list #(0) #(0))))
;(check-equal? (zero-array (Z) '#(Word64 Float)) (r-arr (Z) (array* 0 '#(Word64 Float)) (list #(0) #(0.0))))
(check-equal? (zero-array (Z 1) '#(Word64)) (r-arr (Z 1) (array* 1 '#(Word64)) (list #(0))))
(check-equal? (zero-array (Z 2) '#(Word64)) (r-arr (Z 2) (array* 1 '#(Word64)) (list #(0 0))))
(check-equal? (zero-array (Z 2 3) '#(Word64)) (r-arr (Z 2 3) (array* 2 '#(Word64))
                                                     (list #(0 0 0 0 0 0))))
(check-equal? (zero-array (Z 2 3) '#(Word64 Word64)) (r-arr (Z 2 3) (array* 2 '#(Word64 Word64))
                                                            (list #(0 0 0 0 0 0)
                                                                  #(0 0 0 0 0 0))))
#;(check-equal? (zero-array (Z 2 3) '#(Word64 Float)) (r-arr (Z 2 3) (array* 2 '#(Word64 Float))
                                                             (list #(0 0 0 0 0 0)
                                                                   #(0.0 0.0 0.0 0.0 0.0 0.0))))
; gen-indices
(check-equal? (gen-indices (Z)) (list (Z)))
(check-equal? (gen-indices (Z 1)) (list (Z 0)))
(check-equal? (gen-indices (Z 3)) (list (Z 0) (Z 1) (Z 2)))
(check-equal? (gen-indices (Z 1 1 1)) (list (Z 0 0 0)))
(check-equal? (gen-indices (Z 2 2 5)) (list (Z 0 0 0) (Z 0 0 1) (Z 0 0 2) (Z 0 0 3) (Z 0 0 4)
                                            (Z 0 1 0) (Z 0 1 1) (Z 0 1 2) (Z 0 1 3) (Z 0 1 4)
                                            (Z 1 0 0) (Z 1 0 1) (Z 1 0 2) (Z 1 0 3) (Z 1 0 4)
                                            (Z 1 1 0) (Z 1 1 1) (Z 1 1 2) (Z 1 1 3) (Z 1 1 4)))

(check-equal? (gen-indices (Z 0)) empty)
(check-equal? (gen-indices (Z 2 0 5)) empty)

; helper for checking simplified API
(define (check-simplified sarr rarr)
  (begin (check-equal? sarr (simplify rarr))
         (check-equal? (complicate sarr)
                       rarr)))

;generate/a
;TODO: empty arrays
;TODO: see if singleton vs 1-tuple can be worked out in SArray
; 1-D singleton
(check-equal? (generate/a (Z 11) (r-gen-fn 1 'Word64 (make-gen-fn sqr)))
              (r-arr (Z 11) (array* 1 'Word64) '(#(0 1 4 9 16 25 36 49 64 81 100))))
; 1-D 1-tuple
(check-equal? (generate/a (Z 11) (r-gen-fn 1 '#(Word64) (make-gen-fn-ls sqr)))
              (r-arr (Z 11) (array* 1 '#(Word64)) '(#(0 1 4 9 16 25 36 49 64 81 100))))
(check-simplified (generate '(11) sqr)
                  (generate/a (Z 11) (r-gen-fn 1 '#(Float) (make-gen-fn (λ (x)
                                                                          (list (sqr x)))))))
; 2-D singleton
(check-equal? (generate/a (Z 5 5) (r-gen-fn 2 'Word64 (make-gen-fn expt)))
              (r-arr
               (Z 5 5)
               (array* 2 'Word64)
               '(#(1 0 0 0 0 1 1 1 1 1 1 2 4 8 16 1 3 9 27 81 1 4 16 64 256))))
(check-simplified (generate '(5 5) expt)
                  (generate/a (Z 5 5) (r-gen-fn 2 '#(Float) (make-gen-fn-ls expt))))
; 1-D 3-tuple
(check-equal? (generate/a (Z 5) (r-gen-fn 1 '#(Word64 Word64 Word64)
                                          (λ (index) (match-let ([`(Z ,i) index])
                                                       (vector i (sqr i) (* i i i))))))
              (r-arr
               (Z 5)
               (array* 1 '#(Word64 Word64 Word64))
               (list #(0 1 2 3 4) #(0 1 4 9 16) #(0 1 8 27 64))))
(check-simplified (generate '(5) (λ (i) (list i (sqr i) (* i i i))))
                  (generate/a (Z 5) (r-gen-fn 1 '#(Float Float Float)
                                              (λ (index) (match-let ([`(Z ,i) index])
                                                           (vector i (sqr i) (* i i i)))))))
; 2-D 2-tuple
(check-equal? (generate/a (Z 5 5) (r-gen-fn 2 '#(Float Float)
                                            (λ (index) (match-let ([`(Z ,i ,j) index])
                                                         (vector (+ i (/ j 10.0))
                                                                 (+ i (quotient j 10)))))))
              (r-arr
               '(Z 5 5)
               (array* 2 '#(Float Float))
               (list #(0 0.1 0.2 0.3 0.4 1 1.1 1.2 1.3 1.4 2 2.1 2.2
                         2.3 2.4 3 3.1 3.2 3.3 3.4 4 4.1 4.2 4.3 4.4)
                     #(0 0 0 0 0 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4))))
(check-simplified (generate '(5 5) (λ (i j) (list (+ i (/ j 10.0))
                                                  (+ i (quotient j 10)))))
                  (generate/a (Z 5 5) (r-gen-fn 2 '#(Float Float)
                                                (λ (index) (match-let ([`(Z ,i ,j) index])
                                                             (vector (+ i (/ j 10.0))
                                                                     (+ i (quotient j 10))))))))
;; FIXME I can't get this to be equal
#;(check-equal? (generate/a (Z 5 5) (r-gen-fn 2 '#(Float Float)
                                              (λ (index) (match-let ([`(Z ,i ,j) index])
                                                           (vector (+ i (/ j 10.0))
                                                                   (/ i (add1 j)))))))
                (r-arr
                 '(Z 5 5)
                 (array* 2 '#(Float Float))
                 (list #(0 0.1 0.2 0.3 0.4 1 1.1 1.2 1.3 1.4 2 2.1 2.2
                           2.3 2.4 3 3.1 3.2 3.3 3.4 4 4.1 4.2 4.3 4.4)
                       #(0 0 0 0 0 1 (/ 1 2) (/ 1 3) (/ 1 4) (/ 1 5) 2
                           1 (/ 2 3) (/ 2 4) (/ 2 5) 3 (/ 3 2) 1 (/ 3 4)
                           (/ 3 5) 4 2 (/ 4 3) 1 (/ 4 5)))))
;plty-default
(check-equal? (plty-default 1) '#(Float))
(check-equal? (plty-default 2) '#(Float Float))
(check-equal? (plty-default 3) '#(Float Float Float))

;map/a
; 1-D singleton without need for helper on fn argument
(check-equal? (map/a (r-map-fn 1 'Word64 sqr)
                     (r-arr (Z 11) (array* 1 'Word64) '(#(0 1 2 3 4 5 6 7 8 9 10))))
              (r-arr (Z 11) (array* 1 'Word64) '(#(0 1 4 9 16 25 36 49 64 81 100))))
; 1-D singleton showing use of helper on fn argument
(check-equal? (map/a (r-map-fn 1 'Word64 (make-map-fn sqr))
                     (r-arr (Z 11) (array* 1 'Word64) '(#(0 1 2 3 4 5 6 7 8 9 10))))
              (r-arr (Z 11) (array* 1 'Word64) '(#(0 1 4 9 16 25 36 49 64 81 100))))
(check-equal? (map/a (r-map-fn 1 '#(Word64) (make-map-fn-ls sqr))
                     (r-arr (Z 11) (array* 1 '#(Word64)) '(#(0 1 2 3 4 5 6 7 8 9 10))))
              (r-arr (Z 11) (array* 1 '#(Word64)) '(#(0 1 4 9 16 25 36 49 64 81 100))))
(check-simplified (amap sqr '((0) (1) (2) (3) (4) (5) (6) (7) (8) (9) (10)))
                  (map/a (r-map-fn 1 '#(Float) (make-map-fn-ls sqr))
                         (r-arr (Z 11) (array* 1 '#(Float)) '(#(0 1 2 3 4 5 6 7 8 9 10)))))
; 2-D singleton without helper
(check-equal? (map/a (r-map-fn 2 'Word64 sqr)
                     (r-arr (Z 3 3) (array* 2 'Word64)
                            '(#(0 1 2 3 4 5 6 7 8))))
              (r-arr (Z 3 3) (array* 2 'Word64)
                     '(#(0 1 4 9 16 25 36 49 64))))
; 2-D singleton with helper
(check-equal? (map/a (r-map-fn 2 'Word64 (make-map-fn sqr))
                     (r-arr (Z 3 3) (array* 2 'Word64)
                            '(#(0 1 2 3 4 5 6 7 8))))
              (r-arr (Z 3 3) (array* 2 'Word64)
                     '(#(0 1 4 9 16 25 36 49 64))))
(check-equal? (map/a (r-map-fn 2 '#(Word64) (make-map-fn-ls sqr))
                     (r-arr (Z 3 3) (array* 2 '#(Word64))
                            '(#(0 1 2 3 4 5 6 7 8))))
              (r-arr (Z 3 3) (array* 2 '#(Word64))
                     '(#(0 1 4 9 16 25 36 49 64))))
(check-simplified (amap sqr '(((0) (1) (2))
                              ((3) (4) (5))
                              ((6) (7) (8))))
                  (map/a (r-map-fn 2 '#(Float) (make-map-fn-ls sqr))
                         (r-arr (Z 3 3) (array* 2 '#(Float))
                                '(#(0 1 2 3 4 5 6 7 8)))))
; 1-D 3-tuple without helper
(check-equal? (map/a (r-map-fn 1 '#(Word64 Word64 Word64)
                               (λ (tuple) (match-let ([(vector x y z) tuple])
                                            (vector x (sqr y) (* z 3)))))
                     (r-arr (Z 5) (array* 1 '#(Word64 Word64 Word64))
                            (list #(10 11 12 13 14) #(0 1 2 3 4) #(11 12 13 14 15))))
              (r-arr (Z 5) (array* 1 '#(Word64 Word64 Word64))
                     (list #(10 11 12 13 14) #(0 1 4 9 16) #(33 36 39 42 45))))
; 2-D 2-tuple with helper
(check-equal? (map/a (r-map-fn 2 '#(Float Float)
                               (make-map-fn (λ (x y)
                                              (list y x))))
                     (r-arr '(Z 3 3) (array* 2 '#(Float Float))
                            (list #(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0)
                                  #(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9))))
              (r-arr '(Z 3 3) (array* 2 '#(Float Float))
                     (list #(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9)
                           #(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0))))
(check-simplified (amap (λ (x y) (list y x)) '(((1.0 0.1) (2.0 0.2) (3.0 0.3))
                                               ((4.0 0.4) (5.0 0.5) (6.0 0.6))
                                               ((7.0 0.7) (8.0 0.8) (9.0 0.9))))
                  (map/a (r-map-fn 2 '#(Float Float)
                                   (make-map-fn (λ (x y)
                                                  (list y x))))
                         (r-arr '(Z 3 3) (array* 2 '#(Float Float))
                                (list #(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0)
                                      #(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9)))))
                  
                  