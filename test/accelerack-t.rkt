#lang racket

(require rackunit)
(require "../lib/accelerack.rkt")

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

;; Testing Accelerack internals
(require/expose "../lib/accelerack.rkt" (zero-array gen-indices))
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
                                            
(check-exn exn:fail? (thunk (gen-indices (Z 0))) "gen-indices: does not accept empty shapes")
(check-exn exn:fail? (thunk (gen-indices (Z 2 0 5))) "gen-indices: does not accept empty shapes")
