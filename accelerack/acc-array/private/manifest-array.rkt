#lang racket

;; This is the public interface for manifest arrays in Accelerack.
;; End users should not need to use this.

;; Implementors should use this rather than violating manifest-array's
;; abstraction.  (Internally, it is implemented based on a tree of C
;; pointers.)

(require (except-in ffi/unsafe ->)
         ffi/unsafe/cvector
         accelerack/acc-array/private/manifest-array/structs
         accelerack/acc-array/private/arrayutils
         accelerack/private/utils

         (only-in accelerack/private/types
                  acc-element? acc-shape? acc-type? acc-scalar-type? acc-element-type?
                  acc-sexp-data? acc-sexp-data-shallow?)
         ; (only-in accelerack/private/paven_old/global_utils vector->list*)
         racket/contract
         (only-in '#%foreign ctype-scheme->c ctype-c->scheme))
(require racket/trace)

(provide
 manifest-array-ref
 ; manifest-array-set!
 manifest-array-flatref
 manifest-array-flatset!
 (contract-out
  ;; Manifest arrays:
  [list->manifest-array (-> acc-type? acc-shape? acc-sexp-data?
                            acc-manifest-array?)]
  [make-empty-manifest-array (-> acc-shape? acc-type? acc-manifest-array?)]
  [manifest-array-shape (-> acc-manifest-array? (vectorof exact-nonnegative-integer?))]
  [manifest-array-size  (-> acc-manifest-array? exact-nonnegative-integer?)]
  [manifest-array-dimension (-> acc-manifest-array? exact-nonnegative-integer?)]
  ;; Disabling for performance:
  ;; [manifest-array-flatref (-> acc-manifest-array? exact-nonnegative-integer?
  ;;                             acc-element?)]
  ;; [manifest-array-flatset! (-> acc-manifest-array? exact-nonnegative-integer?
  ;;                              acc-element? void?)]
  [manifest-array-set! (-> acc-manifest-array? (listof exact-nonnegative-integer?)
                           acc-element? void?)]
  
  [manifest-array->sexp (-> acc-manifest-array? acc-sexp-data-shallow?)]
  [manifest-array-type  (-> acc-manifest-array? acc-type?)]

  ;; DEPRECATED / rename or remove:
  [type (-> (or/c acc-manifest-array? segment?) integer?)]
  [make-empty-manifest-array-lame (-> (listof number?) lame-type? acc-manifest-array?)]
 ))


;; Helper to generatePayload function
;; Arguments -> (list containing the payload, type, initial empty list)
;; Return value -> pointer to segment containing the payload informations

(define (generatePayload-helper data payload-c)
  (cond
    ((null? data)
     (make-segment
      (length payload-c)
      ((ctype-scheme->c scalar) 'acc-payload-ptr)
      (cvector-ptr (list->cvector payload-c _segment-pointer))))
    ((pair? (caar data))
     (letrec ([payload*-c (generatePayload-helper (car data) '())])
       (generatePayload-helper (cdr data)
                               (append-end payload*-c payload-c))))
    (else (let ([payload* (list->cvector
                           (car data)
                           (symbol->ctype (get-ctype (caar data))))])
            (generatePayload-helper
             (cdr data)
             (append-end (make-segment (length (car data))
                                       ((ctype-scheme->c scalar) (get-ctype (caar data)))
                                       (cvector-ptr payload*))
                         payload-c))))))


(define (list->segment-tree ty data)
  (cond
    [(acc-scalar-type? ty)
     (list->segment (acc-type->lame-type ty) data)]
    [(vector? ty)
     (let ([segs (for/list ((i (range (vector-length ty))))
                   (list->segment-tree (vector-ref ty i)
                                       (map (lambda (v) (vector-ref v i)) data)))])
       (make-segment
        (length segs)
        ((ctype-scheme->c scalar) 'acc-payload-ptr)
        (cvector-ptr (list->cvector segs _segment-pointer))))]))


;; Stores the payload information into segment structure
;; Arguments -> (list containing the payload, type, initial empty list)
;; Return value -> pointer to segment containing the payload informations


;; DEPRECATED:
; [generatePayload (-> pair? (or/c ctype? symbol?) segment?)]
(define (generatePayload data type)
  (if (ctype? type)
      (let ([payload (list->cvector data type)])
           (make-segment
             (length data)
             ((ctype-scheme->c scalar) (ctype->symbol type))
             (cvector-ptr payload)))
      (generatePayload-helper data '())))

(define (list->segment cty data)
  (let ([payload (list->cvector data cty)])
    (make-segment
     (length data)
     ((ctype-scheme->c scalar) (ctype->symbol cty))
     (cvector-ptr payload))))


;; Retrieve an acc-element? from a tree of segments, using linear indexing.
(define (segment-flatref seg ind)  
  (define type (mapType (segment-type seg)))
  (cond
    ;; FIXME: need good scalar type pred here:
    [(or (equal? type _int) (equal? type _double) (equal? type _bool))
     (if (< ind (segment-length seg))
         (ptr-ref (segment-data seg) type ind)
         (error (format "flatref: out of bounds access to array, index ~a, but array length is ~a"
                        ind (segment-length seg))))]
;    [(eq? type 'scalar-payload) (error "what the heck is this needed for?")]
    [(smells-like-interior-node? type)
     (let ((segs (ptr-ref* (segment-data seg)
                           type 0 (segment-length seg))))
       (list->vector
        (map (lambda (s) (segment-flatref s ind))
             segs)))]
    [else (error 'acc-manifest-array-data
                 "unexpected type inside segment: ~a" type)]))

(define (segment-flatset! seg ind val)
  (define type (mapType (segment-type seg)))
  ; (printf "FLATSET type ~a\n" type)
  (cond
    ;; FIXME: need good scalar type pred here:
    [(or (equal? type _int) (equal? type _double) (equal? type _bool))
     (if (< ind (segment-length seg))
         (ptr-set! (segment-data seg) type ind val)
         (error (format "flatref: out of bounds access to array, index ~a, but array length is ~a"
                        ind (segment-length seg))))]
;    [(eq? type 'scalar-payload) (error "what the heck is this needed for?")]
    [(smells-like-interior-node? type)
     (let ((segs (ptr-ref* (segment-data seg)
                           type 0 (segment-length seg))))
       (for ((i (length segs)))
         (segment-flatset! (list-ref segs i) ind (vector-ref val i)))
       )]
    [else (error 'acc-manifest-array-data
                 "unexpected type inside segment: ~a" type)]))

(define (list->vector** ls)
  (cond
    ((null? ls) '())
    ((pair? (car ls)) (cons (list->vector (list->vector** (car ls)))
                            (list->vector** (cdr ls))))
    (else (cons (car ls) (list->vector** (cdr ls))))))

(define (list->vector* ls shape)
  (cond
    ((null? shape) '())
    ((null? (cdr shape)) (map list->vector (map list->vector** ls)))
    ((equal? 1 (car shape)) (list (list->vector* (car ls) (cdr shape))))
    (else (cons (list->vector* (car ls) (cdr shape))
                (list->vector* (cdr ls) (cons (sub1 (car shape)) (cdr shape)))))))



;; Convert the tree structured (unzipped) manifest array
;; into a fully-zipped, nested-list S-Expression.
(define (manifest-array->sexp a)
  (define len (manifest-array-size a))
  (reshape-list (vector->list (manifest-array-shape a))
                (segment->list (acc-manifest-array-data a) len)
                len))

(define (segment->list seg len)
  (for/list ((i len))
    (segment-flatref seg i)))

(define (reshape-list shp ls len)
  (cond [(null? shp)
         (if (= (length ls) 1)
             (car ls)
             (error 'reshape-list "Shape was zero-dim but list: ~a" ls))]
        [(null? (cdr shp)) ls]
        [else
         (define chunksize (quotient len (car shp)))
         (map (lambda (g) (reshape-list (cdr shp) g chunksize))
              (groups chunksize ls))]))

;; Maybe this is in the standard lib somewhere.
(define (groups n ls)
  (cond [(null? ls) ls]
        [else (let-values ([(a b) (split-at ls n)])
                (cons a (groups n b)))]))

;; Retrieve an element of an N-dimensional array using a 1-dimensional
;; index into its "row-major" repesentation.
(define (manifest-array-flatref arr ind)
  (when (< ind 0)
    (error 'manifest-array-flatref "received negative index: ~a" ind))
  (letrec ([type (mapType (acc-manifest-array-type arr))]
           [seg  (acc-manifest-array-data arr)])
    (segment-flatref seg ind)))

;; Multi-dimensional access.
(define (manifest-array-ref arr . inds)
   (let ((offset (ND->1D-index (manifest-array-shape arr) inds)))
     (manifest-array-flatref arr offset)))

(define (manifest-array-set! arr inds val)
   (let ((offset (ND->1D-index (manifest-array-shape arr) inds)))
     (manifest-array-flatset! arr offset val)))

;; Set an element of an N-dimensional array using a 1-dimensional
;; index into its "row-major" repesentation.
(define (manifest-array-flatset! arr ind val)
  (letrec ([type (mapType (acc-manifest-array-type arr))]
           [seg  (acc-manifest-array-data arr)])
    (segment-flatset! seg ind val)))


;; ====================================================================================================
;; DELETEME:
;; ====================================================================================================
;; Get a list corresponding to given type
;; Arguments -> type
;; Return value -> list corresponding to given type initialized with unit values

(define (getUnit-tuple type)
  (cond
    ((null? type) '())
    ((equal? '_int (car type)) (cons 0 (getUnit-tuple (cdr type))))
    ((equal? '_double (car type)) (cons 0.0 (getUnit-tuple (cdr type))))
    ((equal? '_bool (car type)) (cons #f (getUnit-tuple (cdr type))))
    ((pair? (car type)) (cons (getUnit-tuple (car type)) (getUnit-tuple (cdr type))))))

;; Get a unit value corresponding to given type
;; Arguments -> type
;; Return value -> value corresponding to given type

(define (getUnit-scalar type)
  (cond
    ((equal? _int type) 0)
    ((equal? _double type) 0.0)
    ((equal? _bool type) #f)))

;; Helper function for make-empty-manifest-array.
;; Arguments: (shape, type, payload)
;; Return value: list initialized with unit values

(define (make-empty-manifest-array* shape type payload)
  (cond
    ((null? shape) payload)
    ((zero? (car shape))
     (let ([shape* (if (null? (cdr shape))
                       '()
                       (cons (sub1 (car (cdr shape)))
                             (cdr (cdr shape))))])
       (make-empty-manifest-array* shape* payload (list payload))))
    (else (make-empty-manifest-array*
           (cons (sub1 (car shape)) (cdr shape))
           type (cons type payload)))))

;; Allocates a manifest array and fills it with default values.
;; Arguments: (shape, type)
;; Return value: list with racket pointer and c pointer to result structure

(define (make-empty-manifest-array-lame shape type)
  (letrec ([type-data (if (ctype? type) (getUnit-scalar type) (getUnit-tuple type))]
           [type* (if (ctype? type)
                      ((ctype-scheme->c scalar) 'scalar-payload)
                      ((ctype-scheme->c scalar) 'tuple-payload))]
           [shape* (if (null? shape) '(1) shape)]
           [shape** (generatePayload shape _int)] ;; RRN: change to empty shape for 0D.
           [init-data (car (make-empty-manifest-array* (reverse shape*) type-data '()))]
           [data (if (ctype? type)
                     (generatePayload (flatten init-data) type)
                     (generatePayload (unzip init-data) type))])
          (make-acc-manifest-array type* shape** data)))
;; ====================================================================================================


(define (make-empty-manifest-array _shape type)
  (define shape (vector->list _shape))
  (match type
    [`(Array ,_ ,elt) (new-manifest-array elt shape)]
    ;; TEMP: Remove this behavior or the other:
    [elt #:when (acc-element-type? elt)
         (new-manifest-array elt shape)]
    [else (error 'make-empty-manifest-array "expected an array type, got: ~a" type)]))

;; Extremely similar to the list-> versions:
;;--------------------------------------------------------------------------------
(define (new-manifest-array elt shapels)
  (make-acc-manifest-array (remove-type-field-from-manifest-array elt)
                           (list->segment _int shapels)
                           (new-segment-tree elt (apply * shapels))))

(define (new-segment-tree ty len)
  (cond
    [(acc-scalar-type? ty)
     (new-segment (acc-type->lame-type ty) len)]
    [(vector? ty)
     (let ([segs (for/list ((i (range (vector-length ty))))
                   (new-segment-tree (vector-ref ty i) len))])
       (make-segment
        (length segs)
        ((ctype-scheme->c scalar) 'acc-payload-ptr)
        (cvector-ptr (list->cvector segs _segment-pointer))))]))

(define (new-segment cty len)
  (define vec (make-cvector cty len))
  (define init-val (cond
                     [(equal? cty _bool) #f]
                     [(equal? cty _int)   0]
                     [(equal? cty _double) 0.0]
                     [else 'new-segment "internal-error, unexpected type: ~a" cty]))
  ;; Some kind of calloc would be better:
  (for ((i (range len))) (cvector-set! vec i init-val))
  (make-segment len
                ((ctype-scheme->c scalar) (ctype->symbol cty))
                (cvector-ptr vec)))

;; TODO: This could be more efficient by blasting the segments one at a time:
(define (zero-manifest-array! arr)
  (define ty  (manifest-array-type arr))
  (define len (manifest-array-size arr))
  (define zer
    (match ty
      ['Bool  #f]
      ['Int    0]
      ['Double 0.0]
      [else 'zero-manifest-array! "internal-error, unexpected type: ~a" ty]))
  (for ((i (range len)))
    (manifest-array-flatset! arr i zer)))

;;--------------------------------------------------------------------------------


;; Allocate memory for the payload
;; Arguments -> (type, shape,  payload, expression)
;; Return value -> pointer to allocated memory location

(define (list->manifest-array type shape data)
  (make-acc-manifest-array (remove-type-field-from-manifest-array type)
                           (list->segment _int (vector->list shape))
                           (list->segment-tree type (flatten data))))

;; TODO: Remove this by removing the field that contains it:
(define (remove-type-field-from-manifest-array type)
  ;; This replicates the old/strange protocol.  Unclear that it is necessary.
  (if (ctype? (acc-type->lame-type type))
      ((ctype-scheme->c scalar) 'scalar-payload)
      ((ctype-scheme->c scalar) 'tuple-payload)))

;; RRN: Get rid of functions that are unnecessarily overloaded over
;; segment or acc-manifest-array.  That's sloppy:

;; returns the type of the given acc array
(define (type arr)
  (if (acc-manifest-array? arr)
      (segment-type (acc-manifest-array-data arr))
      (segment-type arr)))

;; returns the shape of the given acc array
(define (manifest-array-shape arr)
  (list->vector (segment->list (acc-manifest-array-shape arr)
                               (manifest-array-dimension arr))))

(define (manifest-array-size arr)
  (let ((ls (segment->list (acc-manifest-array-shape arr)
                           (manifest-array-dimension arr))))
    ;; Note: This is 1 if list is null.
    (apply * ls)))

(define (manifest-array-dimension a)
  (segment-length (acc-manifest-array-shape a)))

;; This must recursively read the tree of segments to reconstruct the
;; full type.
(define (manifest-array-type a)
  `(Array ,(manifest-array-dimension a)
          ,(segtree-type (acc-manifest-array-data a))))

;; DEPRECATED: Fix this strange protocol:
(define (smells-like-interior-node? type)
  (or (eq? type 'tuple-payload)
      (eq? type 'scalar-payload)
      (equal? type _segment-pointer)))

(define (segtree-type seg)
  (let ((ty (mapType (segment-type seg))))
    (cond
      [(equal? ty _int)    'Int]
      [(equal? ty _double) 'Double]
      [(equal? ty _bool)   'Bool]
      [(smells-like-interior-node? ty)
       (let ((segs (ptr-ref* (segment-data seg)
                             type 0 (segment-length seg))))
         (list->vector (map segtree-type segs)))]
      [(equal? ty _acc-manifest-array-pointer)
       (error 'segtree-type "What to do with _acc-manifest-array-pointer")]
      [(equal? ty _gcpointer)
       (error 'segtree-type "What to do with _gcpointer")]
      [(equal? ty 'rkt-payload-ptr)
       (error 'segtree-type "What to do with rkt-payload-ptr")]
      [else (error 'segtree-type "Unexpected type: ~a" ty)])))
