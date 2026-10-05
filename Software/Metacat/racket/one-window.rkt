#lang racket/base
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;
;; Entry point: `racket racket/one-window.rkt [SCALE]` opens Metacat in one
;; window: the menus and the control strip at the top, every graphics window
;; as a pane below (racket/gui/one-window.rkt).  Use it as racket/main.rkt:
;; type a problem such as "abc abd xyz 3852097033", press Enter, then Go.
;; racket/main.rkt still opens the original's separate windows.  racket/gui
;; is loaded only when main runs (lazy-require), so requiring this module
;; stays headless.

(require racket/lazy-require)

(lazy-require ["gui/one-window.rkt" (setup-one-window)])

(provide main)

(define (main . args)
  (let ((scale (if (null? args) 1 (or (string->number (car args)) 1))))
    (setup-one-window scale)))

(module+ main
  (apply main (vector->list (current-command-line-arguments))))
