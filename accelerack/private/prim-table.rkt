#lang racket

;; Macros for working with Accelerack data.
;; Syntax helpers for defining core Accelerate macros and compiler passes.

(require (only-in ffi/unsafe ctype? _int _double _bool) ;; FIXME: remove _*
         accelerack/private/parse
         accelerack/acc-array/private/manifest-array
         rackunit
         (prefix-in r: racket/base)

         syntax/parse
         (only-in accelerack/acc-array/private make-acc-array)

         ;; Regular require, careful of phasing of these identifiers:
         accelerack/private/keywords
         (for-template (except-in racket/base map)
                       (only-in racket/contract ->)
                       accelerack/private/keywords
                       (only-in accelerack/private/wrappers map fold
                                zipwith generate stencil3x3))
         (for-syntax racket/base syntax/parse accelerack/private/parse)
         (only-in accelerack/private/utils vector->list*)
         (only-in accelerack/private/types type-var-id?)
         (only-in rackunit check-not-false)
         )

(provide 
         ;; Global constants:
         acc-primop-lits
         acc-primop-types
         acc-scalar-lits
         
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

(define acc-scalar-lits
  (list #'Bool #'Int #'Double))


;; Each of the above #'foo uses occurs at template stage relative to us.
;; Test that these identifiers are all bound in other modules.
(test-begin
 (for ([p acc-primop-lits])
   (check-pred identifier-template-binding p))
 (for ([p acc-scalar-lits])
   (check-pred identifier-template-binding p)))
