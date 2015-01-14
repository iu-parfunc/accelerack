#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)

(define the-lib (ffi-lib "/usr/lib/x86_64-linux-gnu/libcursesR"))


(define _WINDOW-pointer (_cpointer 'WINDOW))

;; Racket Docs
(define-ffi-definer define-curses the-lib)

(define-curses initscr (_fun -> _WINDOW-pointer))
(define-curses waddstr (_fun _WINDOW-pointer _string -> _int))
(define-curses wrefresh (_fun _WINDOW-pointer -> _int))
(define-curses endwin (_fun -> _int))

;;; the first call kills DrRacket
;(define win (initscr))
;(void (waddstr win "Hello"))
;(void (wrefresh win))
;(sleep 1)
;(void (endwin))

;; Try another way, from dyoo on SO
(define initscr2 (get-ffi-obj "initscr" the-lib
                              (_fun -> _WINDOW-pointer)))

;;; Also kills DrRacket
;(initscr2)