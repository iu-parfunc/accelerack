#lang racket


(require accelerack
         2htdp/image
         rackunit)

(test-begin

 (define img (bitmap/file "racket-logo.jpg"))

 (define (fn col)
   (match col
    [`#(,r ,g ,b ,a) 
     (vector b g r a)]))
 
 (test-case "pixel map"
            (define img2
              (time (acc-array->image
                     (map fn (image->acc-array img)))))
            
            (void)))

