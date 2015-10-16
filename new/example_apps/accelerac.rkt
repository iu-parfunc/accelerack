
(require ffi/unsafe)

(ffi-lib "libparser.so")

(set-ffi-obj! "acc_str" "libparser.so" _string "Racket String")

;;(define acc_str
;;  (get-ffi-obj "acc_str" "libparser.so" _string))

;;(define acc_argc
;;  (get-ffi-obj "argc_value" "libparser.so" _int))

(define setString
  (get-ffi-obj "setString" "libparser.so" (_fun _pointer -> _void)))

