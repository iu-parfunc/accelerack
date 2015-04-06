#lang htdp/isl+

(require "../lib/accelerack.rkt")

(generate (Z 11) (r-fn 1 'Word64 sqr))

(generate (Z 5 5) (r-fn 2 'Word64 expt))

;FIXME student users should not need vectors
(require lang/htdp-advanced)
(generate (Z 5) (r-fn 1 '#(Word64 Word64 Word64) (λ (i) (vector i (sqr i) (* i i i)))))
(generate (Z 5 5) (r-fn 2 '#(Float Float) (λ (i j) (vector (+ i (/ j 10.0)) (/ i (add1 j))))))

