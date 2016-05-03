#lang racket

;; Macros for working with Accelerack data.
;; Syntax helpers for defining core Accelerate macros and compiler passes.

(require (only-in ffi/unsafe ctype? _int _double _bool) ;; FIXME: remove _*
         rackunit
         (except-in racket/base map sqrt round ceiling floor)
         (only-in racket/contract ->)
         accelerack/private/keywords
         accelerack/private/wrappers
         accelerack/private/prim-redefinitions
         (only-in accelerack/acc-array
                  acc-array-size acc-array-dimension)
         (for-template (except-in racket/base map sqrt round ceiling floor)
                       (only-in racket/contract ->)
                       accelerack/private/keywords
                       accelerack/private/wrappers
                       accelerack/private/prim-redefinitions
                       (only-in accelerack/acc-array
                                acc-array-size acc-array-dimension)
                       )
         (for-syntax (except-in racket/base map sqrt round ceiling floor)
                     (only-in racket/contract ->)
                       accelerack/private/keywords
                       accelerack/private/wrappers
                       accelerack/private/prim-redefinitions
                       (only-in accelerack/acc-array
                                acc-array-size acc-array-dimension))
         )

(provide 
         ;; Global constants:
         acc-primop-lits
         acc-primop-types
         acc-scalar-lits
         acc-keyword-lits
         acc-all-bound-syms
         )

;; Maps symbols -> acc-type?
(define acc-primop-types
  (dict-set*
   (make-immutable-custom-hash free-identifier=?)
   #'+    '(-> num_a num_a num_a)
   #'-    '(-> num_a num_a num_a)
   #'*    '(-> num_a num_a num_a)
   #'/    '(-> Double Double Double)
   #'expt '(-> num_a num_a num_a)

   #'=    '(-> num_a num_a Bool)
   #'<    '(-> num_a num_a Bool)
   #'<=   '(-> num_a num_a Bool)
   #'>=   '(-> num_a num_a Bool)   
   #'>    '(-> num_a num_a Bool)

   ;; FIXME: Should probably provide prim-redefinitions for these
   ;; OR simply make them syntax.
   #'and  '(-> Bool Bool Bool)
   #'or   '(-> Bool Bool Bool)
   #'not  '(-> Bool Bool)
   
   #'add1 '(-> num_a num_a)
   #'sub1 '(-> num_a num_a)
   #'abs  '(-> num_a num_a)
   #'sqrt '(-> num_a Double)
   
   #'min  '(-> num_a num_a num_a)
   #'max  '(-> num_a num_a num_a)

   #'modulo    '(-> Int Int Int)
   #'quotient  '(-> Int Int Int)
   #'remainder '(-> Int Int Int)

   ;; TODO Trig functions.
   
   #'round   '(-> Double Int)
   #'ceiling '(-> Double Int)
   #'floor   '(-> Double Int)
   #'exact->inexact  '(-> Int Double)
   
   #'map  '(-> (-> a b) (Array n a) (Array n b))

   #'zipwith '(-> (-> a b c) (Array n a) (Array n b) (Array n c))

   #'acc-array-size      '(-> (Array n a) Int)
   #'acc-array-dimension '(-> (Array n a) Int)   
   
   ))

    ;; ;; Shorthands for convenience and simplicity:
    ;; [fold1 (-> (-> a a a) a (Array 1 a) (Array 0 b))]
    ;; [fold2 (-> (-> a a a) a (Array 2 a) (Array 1 b))]

    ;; ;; Psuedo-syntax for the type:
    ;; [generate (-> (-> Int_1 ... Int_n a) Int_1 ... Int_n (Array n a))]



(define acc-primop-lits
  (dict-keys acc-primop-types))

(define acc-keyword-lits
  (list
   #'if #'let #'acc-array-ref #'acc-array
   #'-> #': #'use
   #'Array
   #'map #'zipwith #'fold
   #'generate  #'stencil3x3
   #'replicate #'until
   ))

(define acc-scalar-lits
  (list #'Bool #'Int #'Double))

;; All the symbols that we expect to be a-priori bound in acc blocks.
(define acc-all-bound-syms
  (append acc-primop-lits
          acc-scalar-lits
          acc-keyword-lits))

;; Each of the above #'foo uses occurs at template stage relative to us.
;; Test that these identifiers are all bound in other modules.
(module+ test

  (define (at-least-one-bind? id)    
    (or (identifier-binding id)
        (identifier-template-binding id)
        (identifier-template-binding id)))
  
 (for ([p acc-primop-lits])
   (check-pred at-least-one-bind? p))
 (for ([p acc-scalar-lits])
   (check-pred at-least-one-bind? p)))

