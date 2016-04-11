#lang racket

(require accelerack)
(require rackunit)

(define result #f)

#;
;; Prakash and Ryan can't get this version to crash [2016.04.11]:
(test-case "repeated alloc.  Keep 100 at a time, allocate a million."
  (for ((i (range 10000)))
    (set! result (map (lambda(x) (acc-array 1)) (range 100)))
    ))


#;
;; The key here is not the allocation, but the PRINTING:
(test-case "repeated alloc, create a massive list-of-lists of acc-arrays"
  (map (lambda(x) (map (lambda(x) (acc-array 1))
                       (range 100)))
       ;; -----------------------------------------
       ;; ON MAC OS, Racket 6.4:
       ;; -----------------------------------------
       ;; (range 100) ;; Can't get this to crash.
       ;; (range 125)  ;; Can't get this to crash on mac laptop.
       ; (range 150) ;; Can contract-violate when printing this many (on mac laptop).
       (range 200) ;; Crashes promptly.  With inner loop ??
#|

I've seen this failure:       
  car: contract violation
    expected: pair?
    given: '()
    context...:
     accelerack/accelerack/private/allocate.rkt:137:0: read-data*

Or memory corruption can manifest elsewhere:
take: contract violation
  expected: exact-nonnegative-integer?
  given: 13/12
  argument position: 2nd
  other arguments...:
   '(60 0 507059824 32553 69 0 8389504 0 -2134638497 0 10066464 0 10066464 0 576345128 32553

But sometimes it just segfaults.
       |#

       ;; ----------------------------------------------
       ;; On Linux, Racket 6.3
       ;; ----------------------------------------------
       ;; 
       ;; (range 125) ;; On linux hit segfault or:  mapType: invalid accelerack type enumeration: 32759
       ;; (range 200) ;; promptly hits contract violation
       ))

;; FIXME: DEBUG THIS ONE:
#;
(test-case "Exercise acc-array print code, to a string"
  (define arrs (map (lambda(x) (acc-array 1)) (range 100)))
  (define str "")
  (for ((i (range 200))) ;; Crashes consistently on mac, reproducing the behavior above.
    (set! str (with-output-to-string (lambda () (display arrs)))))
  (string-length str))

#; 
;; This one segfaults easily on Mac OS:
(test-case "repeated alloc, create a massive list-of-lists of acc-arrays"
  (time (map (lambda(x) (acc-array 1))
             (range (* 1000 1000))))
  )
