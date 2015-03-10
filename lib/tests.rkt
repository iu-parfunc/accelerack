#lang racket

(require rackunit)
(require "accelerack.rkt")


(check-true (acc-type? 'Int))

(check-true (acc-type? '(Array (Z Int) Float)))

(check-true (acc-type? '(Z Int Int Int Int)))
(check-true (acc-shape-type? '(Z Int Int Int Int)))

(check-true (acc-type? '#(Int Float)))


(check-false (acc-type? '(Int Float)))
(check-false (acc-type? '(Z Int Float)))
