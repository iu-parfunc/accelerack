#lang racket

;; ---------------------------------------------------------------
;; Defines a compiler pass which:
;;  (1) Adds type annotations (whereever missing) for all binders.
;;  (2) Throws an exception localized to the right syntax object
;;      for any type errors.
;; This does not work as a macro but instead takes in a syntax object
;; and works on it at the syntax phase level
;; ---------------------------------------------------------------

(provide
 unify-types
 typecheck-expr
 )
(require (for-syntax racket/base
                     syntax/parse)
         ; syntax/parse
         racket/trace
         syntax/to-string
         rackunit rackunit/text-ui
         ;; accelerack/private/passes/typeCheckerDemo
         (only-in accelerack/private/global_utils pass-output-chatter)
         (only-in accelerack/private/types acc-scalar? acc-int? acc-type? acc-syn-entry-type)
         )

;; The full type-checking pass.
;; Returns two values:
;;   (1) principal type of expression
;;   (2) fully annotated expression
(define (typecheck-expr syn-table e)
  (pass-output-chatter 'typecheck-expr e)
  ;; TODO:
  ;; (p-infer e syn-table)
  )

(define (unify-types ty1 ty2)
  ;; FINISHME
  #t)

;; Typing environment:
(define type-env
  '(
    [map  (-> (-> a b) (Array (Num n) a) (Array (Num n) b))]
    ;; ^ PLUS side condition that a/b don't contain Array
    [fold (-> (-> a a a) a
	      (Array (add1 (Num n)) a)
	      (Array (Num n) b))]

    ;; Shorthands for convenience and simplicity:
    [fold1 (-> (-> a a a) a (Array 1 a) (Array 0 b))]
    [fold2 (-> (-> a a a) a (Array 2 a) (Array 1 b))]

					; (generate (lambda () 99))
					; (generate 3 (lambda (i) i))
					; (generate 3 4 (lambda (x y) (+ x y)))
    ;; Psuedo-syntax for the type:
    [generate (-> Int_1 ... Int_n (-> Int_1 ... Int_n a) (Array n a))]

    ))


;; ---------------- Persistent variables and typevariable related stuff ---------------------
(define var-cnt 0)
(define (reset-var-cnt) (set! var-cnt 0))
(define (fresh var)
  (set! var-cnt (+ var-cnt 1))
  (string-append (if (symbol? var)
                     (symbol->string var)
                     (if (syntax? var) (symbol->string (syntax->datum var)) var))
                 (number->string var-cnt)))
;; free variables: type -> set
;; Fetches all the variables in the input given
(define (free_vars t)
  (cond [(or (type_array? t) (type_var? t)) (set t)]
        [(type_fun? t) (let ([in-types (drop-right (cdr t) 1)]
                             [ret-type (last t)])
                         (set-union (list->set (map free_vars in-types))
                                    (free_vars ret-type)))]
        [(type_con? t) (set)]
        [else (raise-syntax-error 'free_vars (format "Unknown type for ~s" t))]))


;; active variables: constraints -> set(type var)
(define (active_vars constraints)
  ;(print constraints)
  (foldl (lambda (constraint res)
           (match constraint
             [`(== ,v1 ,v2) (set-union (free_vars v1) (free_vars v2) res)]
             [`(implicit ,v1 ,v2 ,v3) (set-union (free_vars v1) (set-intersect v3 (free_vars v2)) res)]
             [`(explicit ,v1 ,v2) (set-union (free_vars v1) (free_vars v2) res)]))
         (set) constraints))
;; ---------------- Struct Record ----------------------------
(struct infer-record ([assumptions #:mutable]
                      [contraints #:mutable]
                      type
                      type-expr)
  #:transparent
  #:guard (lambda (as con t te type-name)
            (cond
              [(not (set-mutable? as))
               (raise-syntax-error type-name "Assumptions: ~e has to be of the type mutable-set" as)]
              [(not (set-mutable? con))
               (raise-syntax-error type-name "Constraints: ~e has to be of the type mutable-set" con)]
              [else (values as con t te)])))
;; ------------------------ Classifiers ------------------------
(define type_var? string?)
(define type_con? symbol?)
(define type_array? (lambda (x)
                      (match x
                        [`(Array ,len ,ls) #t]
                        [else #f])))
(define (type_fun? type)
  (match type
    [`(-> . ,t1) #t]
    [else #f]))
(define (type_scheme? type) (and (pair? type) (eq? (car type) 'scheme)))
;; ------------------------ Actual Type inference ------------------------
(define (infer-lit exp)
  (match exp
    [(? acc-int?) 'Int]
    [(? flonum?) 'Double]
    [(? boolean?) 'Bool]
    [`(acc-array ,ls) (if (null? ls)
                          (raise-syntax-error 'infer-lit "Empty list in acc-array not permitted " exp)
                          (let ((type (foldr (lambda (x y) (if (equal? x y) x #f))
                                             (infer-lit (car ls))
                                             (map infer-lit (cdr ls)))))
                            (if type
                                `(Array ,(length ls) ,type)
                                (raise-syntax-error 'infer-lit "All-vars of Acc-array should be of same type: ~a " exp))))]
    ;; Supporting tuples
    [`(,x ...) (map infer-lit x)]
    [else (raise-syntax-error 'infer-lit "This literal is not supported yet: ~a " exp)]))

;; Infer type for if statement
(define (infer-cond e env syn-table)
  (match-define `(if ,cnd ,thn ,els) e)
  (match-define (infer-record ac cc tc tec) (infer-types cnd env syn-table))
  (match-define (infer-record at ct tt tet) (infer-types thn env syn-table))
  (match-define (infer-record ae ce te tee) (infer-types els env syn-table))
  (set-union! ac at ae)
  (set-union! cc ct ce (set `(== ,tt ,te)))
  (infer-record ac cc tt `(if ,tec ,tet ,tee)))

(define (get-vars ls vars env)
  (cond
    [(< (length ls) 3) (list (append ls vars) env)]
    [else (if (eq? ': (cadr ls))
              (get-vars (cdddr ls)
                        (cons (car ls) vars)
                        (cons `(,(car ls) . ,(caddr ls)) env))
              (get-vars (cdr ls)
                        (cons (car ls) vars)
                        env))]))

(define (infer-lambda exp env syn-table)
  (match-define `(lambda ,args ,body) exp)
  (let* ((arg-vals (get-vars args '() '()))
         (arg-env (map (lambda (arg)
                         (let ([env-val (assoc arg (last arg-vals))])
                          (cons arg (if env-val
                                        (cdr env-val)
                                        (fresh "arg"))))) (car arg-vals)))
         (arg-vars (map cdr arg-env))
         (c (mutable-set))
         (a2 (mutable-set)))
    (match-define (infer-record a c t e) (infer-types body (set-union env (list->set (car arg-vals))) syn-table))
    (set-for-each a (lambda (y)
                      (let ((lkp (assoc (car y) arg-env)))
                        (if lkp
                            (set-add! c `(== ,(cdr y) ,(cdr lkp)))
                            (set-add! a2 y)))))
    ;(displayln c)
    (set-union! a2 a)
    (define ret-type `(-> ,@arg-vars ,t))
    (infer-record a2 c ret-type `(lambda ,(foldr (lambda (x res)
                                                    (append `(,(list (car x) ': (cdr x))) res))
                                                 '() arg-env) ,e))
    ))

(define environment
  '((+ . (-> Int Int Int))
    (add1 . (-> Int Int))
    (- . (-> Int Int Int))
    (sub1 . (-> Int Int))
    (* . (-> Int Int Int))
    (/ . (-> Int Int Int))
    (< . (-> Int Int Bool))
    (= . (-> Int Int Bool))
    (eq? . (-> Int Int Bool))))

;; ------------------------ SOLVER and UNIFYIER related stuff ------------------------
;; Var Type -> ((var . type)...)
(define (occurs-check var type)
  (cond
    [(equal? var type) '()]
    ;This is an infinite type. Send an error back
    [(set-member? (free_vars type) var)
     (raise-syntax-error 'occurs-check "Occurs check failed, ~a occurs in ~a\n" var type)]
    [else `(,(cons var type))]))
;; Substitution Type -> Type
(define (substitute s type)
  (cond
    [(type_con? type) type]
    [(type_var? type) (dict-ref s type type)]
    [(type_array? type) `(,(car type) ,(cadr type) ,(substitute s (last type)))]
    [(type_fun? type) `(-> ,@(map (curry substitute s) (cdr type)))]
    [else (raise-syntax-error 'substitute (format "unknown type: ~a" type))]))
;; unify : type type -> ?
(define (unify t1 t2)
  (cond
    [(and (pair? t1) (pair? t2))
     (match-let ((`(-> . ,t1-types) t1)
                 (`(-> . ,t2-types) t2))
       (if (not (eq? (length t1-types) (length t2-types)))
           (error "Types ~a and ~a are incompatible" t1 t2)
           (foldl (lambda (p1 p2 s)
                    (set-union (unify (substitute s p1) (substitute s p2)) s))
                  '() t1-types t2-types)))]
    [(equal? t1 t2) '()]
    [(type_var? t1) (occurs-check t1 t2)]
    [(type_var? t2) (occurs-check t2 t1)]
    [else (raise-syntax-error 'unify (format "Can't Unify t1: ~s and t2: ~s" t1 t2))]))

(define (subs-union subs1 subs2)
  (let ((s (map (lambda (v)
                  (cons (car v) (substitute subs1 (cdr v)))) subs2)))
    (foldl (lambda (v res)
             (when (dict-ref subs2 (car v) #f)
               (raise-syntax-error 'subs-union "Substitutions with same type vars"))
             (set! s (cons v s))) '() subs1) s))
;;  substitution -> constraint -> constraint
(define (sub_constraint s constraint)
  (match constraint
    [`(== ,v1 ,v2) `(== ,(substitute s v1) ,(substitute s v2))]
    [`(implicit ,v1 ,v2 ,v3) `(implicit
                               ,(substitute s v1)
                               ,(substitute s v2)
                               ,(for/set ([var v3])
                                  (dict-ref s var var)))]
    [`(explicit ,v1 ,v2) `(explicit ,(substitute s v1) ,(substitute s v2))]))

;; generalize: set(type var) -> type -> scheme
(define (generalize monos type)
  (list 'scheme (set-subtract (free_vars type) monos) type))

;; instantiate: scheme -> type
(define (instantiate scheme)
  (match-define `(,_ ,qs ,type) scheme)
  (substitute (for/list ([q qs]) (cons q (fresh "I"))) type))

(define (solve constraints)
  (cond
    [(empty? constraints) '()]
    [else (let ((constraint (car constraints)))
            (match constraint
              [`(== ,t1 ,t2) (let ((s (unify t1 t2)))
                               (subs-union (solve (map (curry sub_constraint s) (cdr constraints))) s))]
              [`(implicit ,t1 ,t2 ,monos) (if (set-empty? (set-intersect
                                                           (set-subtract (free_vars t2) monos)
                                                           (active_vars (cdr constraints))))
                                              (solve (cons `(explicit ,t1 ,(generalize monos t2))
                                                           (cdr constraints)))
                                              (solve (append (cdr constraints) `(,constraint))))]
              [`(explicit ,t ,s) (solve (cons `(== ,t ,(instantiate s)) (cdr constraints)))]))]))

;; ------------------------ END SOLVER STUFF ----------------------------
; [Var]
; infer-var: Variable -> InferRecord
(define (infer-var x syn-table)
  (let ((simple-type (or (dict-ref environment x #f)
                         (let ([prev-acc (dict-ref (unbox syn-table) x #f)])
                           (if prev-acc
                               (acc-syn-entry-type prev-acc)
                               #f)))))
    (if simple-type
        (infer-record (mutable-set) (mutable-set) simple-type x)
        (let ([var (fresh x)])
          (infer-record (mutable-set (cons x var))
                        (mutable-set)
                        var
                        x)))))

(define (str->sym val)
  (match val
    [(? string?) (string->symbol val)]
    [(? list?) (map str->sym val)]
    [else val]))
          
(define (annotate-type ty subs)
  (match ty
    [`(-> . ,types) `(-> . ,(map (curryr annotate-type subs) types))]
    [else (let ([f (assoc ty subs)])
            (if f (str->sym (cdr f)) (str->sym ty)))]))

(define (annotate-expr type-expr subs)
  (match type-expr
    [x #:when (or (symbol? x) (number? x) (boolean? x)) type-expr]
    [`(,x : ,ty) `(,(annotate-expr x subs) : ,(annotate-type ty subs))]
    [`(lambda ,x ,b) `(lambda ,(foldr (lambda (val res)
                                         (append (annotate-expr val subs) res)) '() x)
                        ,(annotate-expr b subs))]
    [`(let ,vars ,b) `(let ,(foldr (lambda (var res)
                                     (match-let ([`(,x ,e) var])
                                       (append `((,@(annotate-expr x subs)
                                                  ,(annotate-expr e subs))) res))) '() vars)
                        ,(annotate-expr b subs))]
    [`(map ,fun ,arr) `(map ,(annotate-expr fun subs) ,(annotate-expr arr subs))]
    [`(fold ,fun ,res ,arr) `(fold ,(annotate-expr fun subs) ,(annotate-expr res subs) ,(annotate-expr arr subs))]
    [`(,rator . ,rand) `(,(annotate-expr rator subs)
                         ,@(map (curryr annotate-expr subs) rand))]
    [else (error 'error type-expr)]))


;; syntax -> box list -> type
(define (infer-types e env syn-table)
  (match e
    [(? symbol?) (infer-var e syn-table)]
    [`(lambda ,x ,b) (infer-lambda e env syn-table)]
    ;; [`(let ,vars ,b) (infer-let e env syn-table)]
    ;; [`(map ,fun ,arr) (infer-map e env syn-table)]
    ;; [`(fold ,fun ,res ,arr) (infer-fold e env syn-table)]
    ;; [`(: ,e ,t0) (infer-asc e t0 env syn-table)]
    ;; [`(use ,e ,t0) (infer-use e t0 env syn-table)]
    [`(if ,cnd ,thn ,els) (infer-cond e env syn-table)]
    [`(acc-array ,ls) (infer-record (mutable-set) (mutable-set) (infer-lit e) e)]
    [(? acc-scalar?) (infer-record (mutable-set) (mutable-set) (infer-lit e) e)]
    ;; [`(,rator . ,rand) (infer-app e env syn-table)]
    [else (raise-syntax-error 'infer-types "unhandled syntax: ~a" e)]))


(define (infer e syn-table)
  (match-define
    (infer-record assumptions constraints type type-expr)
    (infer-types (if (syntax? e)
                     (syntax->datum e)
                     e)
                 (set) syn-table))
  (define substitutions (solve (set->list constraints)))
  (infer-record assumptions constraints (substitute substitutions type) (annotate-expr type-expr substitutions)))
;; ---------------------------- TEST RELATED funcs ----------------------------
(define (inf e)
  (infer-types e (set) 	(box '())))

(define (inf_r e)
  (infer e (box '())))



;;;; SOME TESTS FOR typechecker - MOVE to internal once done
;; Test for infer lit
(check-equal? (infer-lit '9) 'Int)
(check-equal? (infer-lit '#t) 'Bool)
(check-equal? (infer-lit '#f) 'Bool)
(check-equal? (infer-lit '9.333) 'Double)
(check-equal? (infer-lit '(acc-array (1.1 2.1 3.1))) '(Array 3 Double))
(check-equal? (infer-lit '(acc-array (1.1 2.1 3.1))) '(Array 3 Double))
(check-equal? (infer-lit '(acc-array ((1 2) (2 3) (2 2)))) '(Array 3 (Int Int)))
(check-equal? (infer-lit '(acc-array ((1 2.1) (2 3.22) (2 2.33)))) '(Array 3 (Int Double)))

(define (check-record-t f record mtch)
  (match-define (infer-record a b type k) record)
  (f type mtch))
(define (check-ls-t f ls mtch)
  (f (list-ref ls 2) mtch))

;; Lets check some infer-records now
(check-record-t check-equal? (inf '9) 'Int)
(check-record-t check-equal? (inf '#t) 'Bool)
(check-record-t check-equal? (inf '(acc-array (1 2))) '(Array 2 Int))
(check-record-t check-equal? (inf '(if 1 2 3)) 'Int)
(check-record-t check-equal? (inf '(if 1 (acc-array (1 2)) (acc-array (2 3)))) '(Array 2 Int))
;; FIXME - Record matcher should try to ignore type variable if possible - MAYBE we shouldn't just have such test cases
(check-record-t check-equal? (inf '(lambda (x) 1)) '(-> "arg1" Int))
(check-record-t check-equal? (inf '(lambda (x) 1)) '(-> "arg2" Int))





(check-record-t check-equal? (inf_r '(lambda (x) x)) '(-> "arg3" "arg3"))

;; TODO What should happen if 2 arrays are of different size ?????

;; (check-exn (infer-lit '(acc-array ())) '(Array 0 Int))
;; DUMMY
;; (check-equal? '(1 2 3) '(1 2 3))
