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

         (only-in accelerack/private/types
                  acc-element? acc-shape? acc-type? acc-scalar-type?
                  acc-sexp-data? acc-sexp-data-shallow?)
         ; (only-in accelerack/private/paven_old/global_utils vector->list*)
         racket/contract
         (only-in '#%foreign ctype-scheme->c ctype-c->scheme))
(require racket/trace)

(provide
 (contract-out
   ;; Manifest arrays:
  [list->manifest-array (-> acc-type? acc-shape? acc-sexp-data?
                            acc-manifest-array?)]
  [make-empty-manifest-array (-> acc-shape? acc-type? acc-manifest-array?)]
  [manifest-array-shape (-> acc-manifest-array? (vectorof exact-nonnegative-integer?))]
  [manifest-array-size  (-> acc-manifest-array? exact-nonnegative-integer?)]
  [manifest-array-dimension (-> acc-manifest-array? exact-nonnegative-integer?)]
  [manifest-array-flatref (-> acc-manifest-array? exact-nonnegative-integer?
                              acc-element?)]
  [manifest-array-flatset! (-> acc-manifest-array? exact-nonnegative-integer?
                               acc-element? void?)]

  [manifest-array->sexp (-> acc-manifest-array? acc-sexp-data-shallow?)]
  [manifest-array-type  (-> acc-manifest-array? acc-type?)]

  ;; DEPRECATED / rename or remove:
  [type (-> (or/c acc-manifest-array? segment?) integer?)]
  [make-empty-manifest-array-lame (-> acc-shape? lame-type? acc-manifest-array?)]
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


;; Helper to segment->sexp function
;; Arguments -> list containing segment pointers
;; Return value -> list with data read from memory pointed by segment pointers

(define (read-data-helper ls)
  (cond
    ((null? ls) '())
    ((let ([element (ptr-ref (segment-data (car ls)) (mapType (segment-type (car ls))))])
          (and (not (boolean? element))
               (cpointer? element)))
     (cons (read-data-helper
             (ptr-ref*
               (segment-data (car ls))
               (mapType (segment-type (car ls)))
               0
               (segment-length (car ls))))
           (read-data-helper (cdr ls))))
     (else (cons (ptr-ref*
                  (segment-data (car ls))
                  (mapType (segment-type (car ls)))
                  0
                  (segment-length (car ls)))
                (read-data-helper (cdr ls))))))


;  segment->sexp: (-> segment? acc-sexp-data-shallow?)
;; Read data from a segment or tree of segments.
(define (segment->sexp cptr)
  (letrec ([len (segment-length cptr)]
           [cptr* (segment-data cptr)]
           [type (mapType (segment-type cptr))]
           [data (if (equal? type 'empty-type)
                     '() (ptr-ref* cptr* type 0 len))])
    (if (and (ctype? type) (not (equal? _segment-pointer type)))
        data
        (read-data-helper data))))

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
       (list->vector
        (map (lambda (s) (segment-flatset! s ind val))
             segs)))]
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


;; Read data from given memory location
;; Arguments -> acc-manifest-array pointer
;; Return value -> list with data read from given memory location

(define (manifest-array->sexp cptr)
  (letrec ([type (mapType (acc-manifest-array-type cptr))]
           [data-ptr  (acc-manifest-array-data cptr)]
           [shape-ptr (acc-manifest-array-shape cptr)]
           [data  (segment->sexp data-ptr)]
           [shape (segment->sexp shape-ptr)])
    (if (equal? type 'scalar-payload)
        (if (null? shape)
            (car (list->md-array data shape))
            (list->md-array data shape))
        (list->vector* (zip data) shape)))) ;; (read-data-helper data)

;; Retrieve an element of an N-dimensional array using a 1-dimensional
;; index into its "row-major" repesentation.
(define (manifest-array-flatref arr ind)
  (letrec ([type (mapType (acc-manifest-array-type arr))]
           [seg  (acc-manifest-array-data arr)])
    (segment-flatref seg ind)))

;; Set an element of an N-dimensional array using a 1-dimensional
;; index into its "row-major" repesentation.
(define (manifest-array-flatset! arr ind val)
  (letrec ([type (mapType (acc-manifest-array-type arr))]
           [seg  (acc-manifest-array-data arr)])
    (segment-flatset! seg ind val)))

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

(define (make-empty-manifest-array shape type)
  ;; TODO: replace this:
  (make-empty-manifest-array-lame shape (acc-type->lame-type type)))

;; Allocate memory for the payload
;; Arguments -> (type, shape,  payload, expression)
;; Return value -> pointer to allocated memory location

(define (list->manifest-array type shape data)
  ;; In terms of layout, 0D arrays are the same as singleton 1D arrays:
  (let () ; ([shape* (if (null? shape) '(1) shape)])
    (make-acc-manifest-array (remove-type-field-from-manifest-array type)
                             (list->segment _int shape)
                             (list->segment-tree type (flatten data)))))

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
  (list->vector (segment->sexp (acc-manifest-array-shape arr))))

(define (manifest-array-size arr)
  (let ((ls (segment->sexp (acc-manifest-array-shape arr))))
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
