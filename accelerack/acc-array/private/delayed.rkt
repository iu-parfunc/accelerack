#lang racket

;; Types and functions for working with delayed array representations:

(provide
 ;; delayed scalars are not fully implemented yet [2016.04.11]:
 acc-delayed-array?  acc-delayed-array  acc-delayed-array-thunk
 )

;; The datatype for delayed arrays that are not yet computed by either
;; Racket/Accelerack or Haskell/Accelerate.
;;
;; TODO: If we eventually want to make (define x (acc y)) equivalent to (define-acc x y)
;; then we will need to store the captured syntax in here also, because we will not
;; have top-level bindings entered in the table.
(struct acc-delayed-array (thunk)
  #:guard (lambda (th _)
            (unless (procedure? th)
              (raise-argument-error 'acc-delayed-array "procedure?" th))
            th))
