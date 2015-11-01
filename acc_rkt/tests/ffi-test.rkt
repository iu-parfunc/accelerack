#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)

(require accelerack)

; One time, I had to restart DrRacket in order for it to reload new copies of .so
(printf "<-------------- Setting up the Haskell environment --------------->")
(define lib-capi (ffi-lib "../../acc_hs/libcapi"))
(define-ffi-definer define-capi lib-capi)

(define lib-ifc (ffi-lib "../../acc_hs/libhsifc"))
(define-ffi-definer define-ifc lib-ifc)

(define lib-rts (ffi-lib "../../acc_c/librts"))
(define-ffi-definer define-rts lib-rts)


(define-rts ark_init (_fun -> _int))
(define-rts ark_exit (_fun -> _int))

(ark_init)
(printf "!!! init function success !!!\n")

(define-capi entrypoint (_fun _int -> _int))

(printf "calling entrypoint: ")
(entrypoint 0)
(printf "!!! entrypoint function called successfully!!!\n")
(newline)

(printf "########## Ready to call haskell functions ###########\n")

(define-ifc modify_array (_fun  _c-array-pointer -> _void))

(define t1 (array (2 3) _int ((2 3 11) (50 2 41))))
(define t1-c (car t1))
(define t1-rkt (cadr t1))
(printf "Before modify_array Call\n")
(readData* t1-c)
;;(modify_array t1-c)
(printf "After modify_array Call\n")
(readData* t1-c)

(define t4 (array (2 2 3) (_tuple _int (_tuple _int (_tuple _int _double))) 
                    (((#(2 #(2 #(3 1.1))) #(1 #(3 #(5 2.2))) #(4 #(16 #(7 3.3)))) (#(4 #(5 #(3 4.4))) #(1 #(6 #(15 5.5))) #(14 #(26 #(7 6.6)))))
                     ((#(12 #(12 #(13 7.7))) #(11 #(13 #(15 7.7))) #(14 #(26 #(17 7.7)))) (#(14 #(25 #(13 1.1))) #(11 #(16 #(25 8.8))) #(24 #(26 #(17 2.2))))))))
(define t4-c (car t4))
(define t4-rkt (cadr t4))
;(printf "Before modify_array Call\n")
;(readData* t4-c)
;;(c-vector-type (c-array-data t4-c))
;(modify_array t4-c)
;(printf "After modify_array Call\n")
;(readData* t4-c)


(define-capi gpu (_fun _string -> _string))

;;(printf "\ncalling gpu function:\n")
;;(gpu "show (I.run $ (A.use (A.fromList (A.Z A.:.3 A.:.5) [1..] :: A.Array A.DIM2 Int)))")

(printf "########## Exiting racket safely ############\n")

