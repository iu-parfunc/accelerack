#lang racket

(require rackunit)
(require "../lib/accelerack.rkt")

;; Basic in-Racket Array operations:
;; -----------------------------------------------------

;; Test Rack-Array constructor guard
;; TODO: test for expected error message
;;  - which means externalizing the strings, TODO
;; Test dimensionality
(check-exn exn:fail? (lambda ()
                       (r-arr (Z) '(Array (Z Int) #(Word64))
                              (list (u64vector 0 10 20 30)))))

(check-exn exn:fail? (lambda ()
                       (r-arr (Z 1 ) '(Array (Z Int) #(Word64))
                              (list (u64vector 0 10 20 30)))))
(check-exn exn:fail? (lambda ()
                    (r-arr (Z 1 5 1)
                           '(Array (Z Int Int) #(Word64 Word64 Word64))
                           (list (u64vector 0 1 2 3 4)
                                 (u64vector 5 6 7 8 9)
                                 (u64vector 10 11 12 13 14)))))
;; Test vector types
(check-exn exn:fail? (lambda ()
                       (r-arr (Z 4) '(Array (Z Int) #(Float))
                              (list (u64vector 0 10 20 30)))))
(check-exn exn:fail? (lambda ()
                    (r-arr (Z 1 5)
                           '(Array (Z Int Int) #(Word64 Word64 Word64))
                           (list (u64vector 0 1 2 3 4)
                                 (f64vector 5 6 7 8 9)
                                 (u64vector 10 11 12 13 14)))))
;; Test vector lengths
(check-exn exn:fail? (lambda ()
                       (r-arr (Z 4) '(Array (Z Int) #(Word64))
                              (list (u64vector 0 10 20)))))
(check-exn exn:fail? (lambda ()
                       (r-arr (Z 4) '(Array (Z Int) #(Word64))
                              (list (u64vector 0 10 20 30 40)))))
(check-exn exn:fail? (lambda ()
                    (r-arr (Z 1 5)
                           '(Array (Z Int Int) #(Word64 Word64 Word64))
                           (list (u64vector 0 1 2 3 4)
                                 (u64vector 5 6 7 8 9)
                                 (u64vector 10)))))

(define arr1 (r-arr (Z 4) '(Array (Z Int) Word64)
                    (list (u64vector 0 10 20 30))))


(define arr2 (r-arr (Z 1 5)
                    '(Array (Z Int Int) #(Word64 Word64 Word64))
                    (list (u64vector 0 1 2 3 4)
                          (u64vector 5 6 7 8 9)
                          (u64vector 10 11 12 13 14))))

(define arr3 (r-arr (Z 2 3)
                    '(Array (Z Int Int) #(Word64 Word64 Word64))
                    (list (u64vector 0 1 2 3 4 5)
                          (u64vector 6 7 8 9 10 11)
                          (u64vector 12 13 14 15 16 17))))
(check-exn exn:fail? (lambda ()
                       (r-arr (Z 2 3)
                              '(Array (Z Int Int) #(Word64 Word64 Word64))
                              (list (u64vector 0 1 2 3 4 5)
                                    (u64vector 6 7 8 9 10 11)
                                    (u64vector 12 13 14 15 16 17 18)))))
(define arr4 (r-arr (Z 2 2) '(Array (Z Int Int) #(Word64 Float Word64))
                    (list (u64vector 4 5 6 7)
                          (f64vector 10 11 12 13)
                          (u64vector 22 33 44 55))))
                     
                     ;; (Z 2 0 2) allowed?

;; FINISHME:
(check-equal? (rget arr2 (Z 0 0)) #(0 5 10))
(check-equal? (rget arr2 (Z 0 3)) #(3 8 13))

(check-eqv? (rget arr1 (Z 2)) 20)