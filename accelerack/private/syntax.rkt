#lang racket

;; Macros for working with Accelerack data.
;; Syntax helpers for defining core Accelerate macros and compiler passes.

(require (only-in ffi/unsafe ctype? _int _double _bool) ;; FIXME: remove _*
         accelerack/private/parse
         accelerack/acc-array/private/manifest-array
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
         (only-in rackunit check-not-false)
         )

(provide acc-array
         acc-primop
         acc-primop-lits
         acc-primop-types
         ; acc-primop-identifier?
         acc-lambda-param
         
         acc-scalar-lits
         acc-element-type

         infer-element-type
         )
(provide (all-from-out accelerack/private/keywords))

(define-for-syntax (infer-shape d)
  (syntax-parse d
    [(~or _:boolean _:number) #'()]
    [_
     #:when (vector? (syntax-e d))
     #'()]
    [(v more ...)
     #:with rest (infer-shape #'v)
     #:with l (add1 (length (syntax->list #'(more ...))))
     #'(l . rest)]))

(begin-for-syntax
  (define-syntax-class acc-data
    #:attributes (shape type)
    [pattern v
             #:with shape (infer-shape #'v)
             #:with type (infer-element-type #'v)
             ])
  )

;; Maps symbols -> acc-type?
(define acc-primop-types
  (dict-set*
   (make-immutable-custom-hash free-identifier=?)
   #'+    '(-> num_a num_a num_a)
   #'add1 '(-> num_a num_a)
   
   #'map  '(-> (-> a b) (Array n a) (Array n b))
   
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

;; [2015.12.11] Huh.. these work when I load this modulue, but fail
;; when this moudle is imported by syntax-tests.rkt:
(check-not-false (andmap identifier-binding acc-primop-lits))
(check-not-false (andmap identifier-binding acc-scalar-lits))

(define (acc-primop-identifier? id)
  (and (identifier-binding id) ;; could throw an error for this.
       (member id acc-primop-lits free-identifier=?)))

(define (acc-scalar-identifier? id)
  (and (identifier-binding id) ;; could throw an error for this.
       (member id acc-scalar-lits free-identifier=?)))

(define-syntax-class acc-primop
  #:description "a primitive function supported by Accelerack (such as +, -, *, map, etc)"
  (pattern p:id #:when (acc-primop-identifier? #'p)))

(define-syntax-class acc-element-type
  #:description "a type for element data that can go inside an array"
  (pattern p:id #:when (acc-scalar-identifier? #'p))
  (pattern #(t:acc-element-type ...)))

(define-syntax-class acc-lambda-param
  #:description "an Accelerack lambda parameter with optional type"
  #:literals (:)
  #:attributes (name type)
  (pattern x:id
           #:with name #'x
           #:with type #f)
  (pattern (x:id : t:acc-type)
           #:with name #'x
           #:with type (syntax->datum #'t)))


;; A convenient syntax for literal arrays, which does not require the
;; user to provide type/shape information.
(define-syntax (acc-array stx)  
  (syntax-parse stx
    [(_ data:acc-data)
     #;
     (printf "ACC-ARRAY MACRO, finishme: ~a ~a ~a\n"
            (syntax->datum #'data)
            (syntax->datum #'data.type)
            (syntax->datum #'data.shape))
     (let* ([typ (syntax->datum #'data.type)]
            [shp (syntax->datum #'data.shape)]
            [dat (syntax->datum #'data)]
            [ver (validate-literal typ shp dat)])
       (if (eq? ver #t)
           #`(make-acc-array (list->manifest-array '#,typ (list->vector '#,shp) '#,dat))
           (raise-syntax-error 'acc-array
                               (string-append "bad array literal.\n" ver) stx))
           )]))



