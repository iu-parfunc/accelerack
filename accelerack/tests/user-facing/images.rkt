#lang racket
(require accelerack rackunit
         2htdp/image
         )

(test-case "Convert image->array"
  (define x (circle 3 "solid" "red"))
  (define y (image->color-list x))
  (define z (image->acc-array x))
  (define l (acc-array->sexp z))
  (test-case "right length"
    (check-equal? (length (flatten l)) (length y)) )
  (test-case "exact image match"
    (check-equal? (color-list->bitmap y (image-width x) (image-height x))
                  (acc-array->image z)))
  )

;; FINISHME
#;
(test-case "Convert image->array"
  (define x (image->acc-array (circle 3 "solid" "red")))
  (define y (image->acc-array (circle 4 "solid" "green")))
  (define z (map (lambda (x) (min x 255))
                 (zipwith + x y)))
  ;(acc-array->image z)
  (length (acc-array->sexp y))
  )

