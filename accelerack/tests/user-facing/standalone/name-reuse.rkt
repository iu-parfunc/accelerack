#lang racket

(require accelerack rackunit)

; (acc-echo-types)

(test-case "use 1"
  (: foo Bool)
  (define-acc foo #t)
  (void)
  )

(test-case "use 2"
  (: foo Int)
  (define-acc foo 4)
  (define-acc bar (generate (lambda () foo)))
  (void)
  )
