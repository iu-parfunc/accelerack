#lang racket

;; Macros for working with Accelerack data.
;; Syntax helpers for defining core Accelerate macros and compiler passes.

(require (only-in ffi/unsafe ctype? _int _double _bool) ;; FIXME: remove _*
         accelerack/private/parse
         accelerack/private/prim-table
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

(provide ;; Functions and macros
         acc-array
         ; acc-primop-identifier?
         infer-element-type
         
         ;; Global constants:
         acc-primop-lits
         acc-primop-types
         acc-scalar-lits
         
         ;; Syntax classes:
         acc-primop
         acc-lambda-param
         acc-let-bind
         acc-type
         acc-element-type
         acc-element-literal
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


(define (acc-primop-identifier? id)
  (and (identifier-binding id) ;; could throw an error for this.
       (member id acc-primop-lits free-identifier=?)))

(define (acc-scalar-identifier? id)
  (and (identifier-binding id) ;; could throw an error for this.
       (member id acc-scalar-lits free-identifier=?)))

(define-syntax-class acc-primop
  #:description (string-append "a primitive function supported by Accelerack.\n"
                               ; "Examples include +, -, *, map, etc.\n"
                               ; "Evaluate 'acc-prims' for the full list."
                               (format "The full list of primitives is:\n  ~a\n"
                                       (for/list ([(k _) (in-dict acc-primop-types)])
                                         (syntax->datum k)))
                               )
  (pattern p:id #:when (acc-primop-identifier? #'p)))


(define-syntax-class acc-type-variable
  #:description "a type variable, which starts with a lower-case letter"
  (pattern p:id #:when (type-var-id? #'p)))

(define-syntax-class acc-element-type
  #:description "a type for element data that can go inside an array"
  (pattern p:id #:when (acc-scalar-identifier? #'p))
  (pattern p:acc-type-variable)
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

(define-syntax-class acc-type
  #:description "an Accelerack type"
  #:literals (-> Array)
  (pattern (-> opera:acc-type ...))
  (pattern (Array n:integer           elt:acc-element-type))
  (pattern (Array v:acc-type-variable elt:acc-element-type))
  (pattern t:acc-element-type))

(test-true "parse type 1"
           (syntax-parse #'Int
             [t:acc-type #t]
             [else #f]))

(test-true "parse array type"
            (syntax-parse #'(Array 3 Int)
              [t:acc-type #t]
              [else #f]))

;; Syntactic class for literal data that goes inside an array.
(define-syntax-class acc-element-literal
  #:description "Accelerack element data (which can go in an array)"
  (pattern _:boolean)
  (pattern ns:number #:when
           (let ((n (syntax->datum #'ns)))
             (or (flonum? n) (exact-integer? n))))
  (pattern #( _:acc-element-literal ...)))


(define-syntax-class acc-let-bind
  #:description "an Accelerack let-binding with optional type"
  #:literals (:)
  #:attributes (name type rhs)
  (pattern (x:id expr)
           #:with name #'x
           #:with type #f
           #:with rhs #'expr)
  (pattern (x:id : t:acc-type expr)
           #:with name #'x
           #:with type (syntax->datum #'t)
           #:with rhs #'expr
           ) ;; TODO: Could use acc-expr class.  Transform verify-acc into it?
  )

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



