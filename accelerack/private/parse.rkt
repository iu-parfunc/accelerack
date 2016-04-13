#lang racket

(require (except-in ffi/unsafe ->)
         racket/contract
         (only-in accelerack/private/types acc-element-type? acc-scalar? acc-int? acc-element?)
         )

(provide
  (contract-out
   [validate-literal (-> acc-element-type?
                         (listof exact-nonnegative-integer?)
                         any/c (or/c #t string?))]
   ))

;; Returns #t if everything checks out.  Otherwise returns an
;; explanation of the problem in a string.
(define (validate-literal typ shp dat)
  (define (mkpred ty)
    (match ty
      ['Bool boolean?]
      ['Int  acc-int?]
      ['Double flonum?]
      [`#( ,tys ...)
       (let ((preds (map mkpred tys)))
         (if (andmap procedure? preds)             
             (lambda (x)
               (if (vector? x)
                   (format "Expected tuple of type ~a, found: ~a\n" ty x)
                   (squish (map (lambda (f y) (f y))
                                preds (vector->list x)))))
             (squish (filter string? preds))))]
      [else (format "Unexpected type for array element: ~a\n" ty)]))

  ;; This is tedious because it tries to avoid throwing an exception:
  (define (lenmatch l s)
    (cond
      [(acc-element? l)
       (if (null? s) #t
           (format "Wrong nesting depth.  Expected something of shape ~a, found ~a.\n" s l))]
      [(null? l) #t] ;; zero-length dim is always ok.      
      [(list? l)
       (if (= (length l) (car s))
           (squish (map (lambda (x) (lenmatch x (cdr s))) l))
           (format "Literal array data of wrong length.  Expected ~a things, found ~a, in:\n ~a\n"
                   (car s) (length l) l))]
      [else (format "Unexpected expression where array data was expected: ~a\n"
                    l)]))

  ;; Apply to each element, disregarding nesting level.
  (define (deep-map f dat)
    (cond
      [(pair? dat) (map (lambda (x) (deep-map f x)) dat)]
      [else (f dat)]))
  ;; Take a mix of #t's and strings in an arbitrary sexp.  Append the
  ;; strings separated by newlines.  If no strings found, return #t.
  (define (squish x)
    (cond
      [(eq? #t x) #t]
      [(string? x) x]
      [(null? x) #t]
      [(pair? x)
       (let ((fst (squish (car x)))
             (snd (squish (cdr x))))
         (if (eq? fst #t) snd
             (if (eq? snd #t) fst
                 (string-append fst snd))))]
      [else (error 'validate-literal "internal error.  Squish function got: ~a\n" x)]))
  
  (let ((len-check (lenmatch dat shp))
        (pred (mkpred typ)))
    (if (not (procedure? pred))
        pred
        (if (eq? len-check #t)
            (squish (deep-map (lambda (el)
                                (if (pred el) #t
                                    (format "Array element ~a does not match expected type ~a\n"
                                            el typ)))
                              dat))
            len-check))))

