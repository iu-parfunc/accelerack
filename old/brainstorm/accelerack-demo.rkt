#lang racket

(begin-for-syntax
  (define ht (make-hash))
  )

(define-syntax (acc stx)
  (syntax-case stx ()
    [(_ (f (fn x) body)) (begin
                           ; Record what was defined within the acc block
                           (hash-set! ht
                                      (syntax->datum (syntax fn))
                                      (syntax->datum (syntax body)))
                           ; Define it in the real evironment
                           #'(f (fn x) body))]
    [(_) (datum->syntax #'acc ht)]
    ))

; Define a sqr function
(acc 
 (define (sqr2 x) (* x x))
 )

; Try the sqr function
(sqr2 5)

; See what was recorded by the macro
(acc)

; Bind a run-time variable to the macro's hashtable
(define ht2 (acc))


#| Interactions Dialogue

25
'#hash((sqr2 . (* x x)))
> (sqr2 5)
25
> (acc)
'#hash()
> ht2
'#hash((sqr2 . (* x x)))
> (acc (define (sqr3 x) (* x x)))
> (acc)
'#hash((sqr3 . (* x x)))
> ht2
'#hash((sqr2 . (* x x)))
> (acc) ; the hashtable is empty
'#hash((sqr3 . (* x x)))

|#

#|
Thoughts:

Could rig macro to create a run-time binding to the ht.
That run-time binding would persist into the Interactions Window.
An acc helper (acc reload) would reset the re-compiled ht to the persisted value,
which would enable the Interactions Window to continue on with the same run-time
environment and macro-used ht.  The macro in the second compilation would then be
pre-loaded with the final ht from the expansion of the .rkt file

|#
