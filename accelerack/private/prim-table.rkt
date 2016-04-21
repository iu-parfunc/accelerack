#lang racket

;; Macros for working with Accelerack data.
;; Syntax helpers for defining core Accelerate macros and compiler passes.

(require (only-in ffi/unsafe ctype? _int _double _bool) ;; FIXME: remove _*
         rackunit

         (except-in racket/base map)
         (only-in racket/contract ->)
         accelerack/private/keywords
         accelerack/private/wrappers
         (for-template (except-in racket/base map)
                       (only-in racket/contract ->)
                       accelerack/private/keywords
                       accelerack/private/wrappers)
         (for-syntax (except-in racket/base map)
                       (only-in racket/contract ->)
                       accelerack/private/keywords
                       accelerack/private/wrappers)
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
   #'/    '(-> num_a num_a num_a)
   
   #'add1 '(-> num_a num_a)
   #'sub1 '(-> num_a num_a)
   #'sqrt '(-> num_a num_a)
   #'abs  '(-> num_a num_a)
   
   #'map  '(-> (-> a b) (Array n a) (Array n b))

   #'zipwith '(-> (-> a b c) (Array n a) (Array n b) (Array n c))
   
   ; #'sub1 #'+ #'* #'/ #'-    
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
   #'-> #': #'use
   #'Array
   #'generate  #'fold
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
(test-begin

  (define (at-least-one-bind? id)    
    (or (identifier-binding id)
        (identifier-template-binding id)
        (identifier-template-binding id)))
  
 (for ([p acc-primop-lits])
   (check-pred at-least-one-bind? p))
 (for ([p acc-scalar-lits])
   (check-pred at-least-one-bind? p)))

