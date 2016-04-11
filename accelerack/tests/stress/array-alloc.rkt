#lang racket

(require accelerack)
(require rackunit)

;; Prakash and Ryan can't get this version to crash [2016.04.11]:
(test-case "Repeated alloc.  Keep 100 at a time, allocate a million."
  (define result #f)
  (for ((i (range 5000)))
    (set! result (map (lambda(x) (acc-array 1)) (range 100)))
    )
  (length result))

(test-case "Exercise acc-array print code, to a string"
  (define arrs (map (lambda(x) (acc-array 1)) (range 100)))
  (define str "")
  (for ((i (range 200)))
    (set! str (with-output-to-string (lambda () (display arrs)))))
  (string-length str))

#;
(test-case "Allocate a big array."
  (define arr (new-acc-array (* 10 1000 1000)))
  (acc-array-ref arr 1000))
