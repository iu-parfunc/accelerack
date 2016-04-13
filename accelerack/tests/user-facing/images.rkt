#lang racket
(require accelerack rackunit
         2htdp/image
         )

(define x (circle 3 "solid" "red"))

(test-case "Convert image->array"
  (define y (image->color-list x))
  (define z (image->acc-array x))
  (define l (acc-array->sexp z))
  (test-case "right length"
    (check-equal? (length (flatten l)) (length y)) )
  (test-case "exact image match"
    (check-equal? (color-list->bitmap y (image-width x) (image-height x))
                  (acc-array->image z)))
  )

;; Saturating arithmetic:
(define (add-bytes b1 b2)
  (min 255 (+ b1 b2)))

(define (add-cols c1 c2)
  (vector (add-bytes (vector-ref c1 0) (vector-ref c2 0))
          (add-bytes (vector-ref c1 1) (vector-ref c2 1))
          (add-bytes (vector-ref c1 2) (vector-ref c2 2))
          (add-bytes (vector-ref c1 3) (vector-ref c2 3))))

;; FINISHME: memory corruption problems atm:
#;
(test-case "Convert image->array"
  (define x (image->acc-array (circle 30 "solid" "red")))
  (define y (image->acc-array (circle 40 "solid" "green")))
  (printf "Image 1 shape ~a, image 2 shape ~a\n" (acc-array-shape x) (acc-array-shape y))
  (define z (zipwith add-cols x y))  
  (image-width (acc-array->image z))
  )


(test-case "Image properties"
  (define (shape-prop img)
    ; (check-true (image? img))
    (check-equal? (acc-array-shape (image->acc-array img))
                  (vector (image-width img)
                          (image-height img))))

  (test-case "shape-prop 1" (shape-prop x))
  (test-case "shape-prop 1" (shape-prop (circle 50 "solid" "red")))
  (test-case "shape-prop 1" (shape-prop (empty-scene 100 100)))
  )
