#lang racket/base
;; Item 15: a screenshot of the whole program (control panel and every
;; window) on a virtual display, after a run driven through the control
;; panel.  Not part of the test suite (it needs Python's PIL to grab the
;; screen).  Run it as
;;
;;   env -u WAYLAND_DISPLAY GDK_BACKEND=x11 xvfb-run -a -s "-screen 0 1920x1200x24" \
;;     racket tests/gui-screenshot.rkt OUT.png PROBLEM... [--break N] [--speed S]
;;
;; e.g. OUT.png abc abd mrrjjj 1 --break 513.  With --break the run stops at
;; codelet N (the Options menu's breakpoint), otherwise at its first answer.
;;
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
(require racket/class racket/system racket/string
         (prefix-in g: racket/gui/base)
         "../racket/gui/gui.rkt"
         "../racket/engine.rkt"
         "../racket/utilities.rkt")

(define args (vector->list (current-command-line-arguments)))
(define out (car args))
(define (opt name default)
  (let ((m (member name args))) (if m (string->number (cadr m)) default)))
(define problem
  (let loop ((as (cdr args)))
    (cond ((null? as) '())
          ((string-prefix? (car as) "--") (loop (cddr as)))
          (else (cons (car as) (loop (cdr as)))))))

(setup)
(define widgets (tell *control-panel* 'get-widgets))
(define (W name) (cdr (assq name widgets)))
(define (click b) (send b command (new g:control-event% [event-type 'button])))
(define (wait-idle)
  (let loop ()
    (g:sleep/yield 0.05)
    (unless (and (not (engine-busy?)) (send (W 'go-button) is-enabled?)) (loop))))

(let ((speed (opt "--speed" 100)))
  (send (W 'speed-slider) set-value speed)
  (speed-slider-action #f speed))
(send (W 'command-line) set-value (string-join problem " "))
(send (W 'command-line) command (new g:control-event% [event-type 'text-field-enter]))
(wait-idle)
(let ((n (opt "--break" #f)))
  (when n (set-global! '*break-time* n)))
(click (W 'go-button))
(wait-idle)
(g:sleep/yield 1.5)
(printf "codelets run: ~a\n" *codelet-count*)
(let ((vp (tell *comment-window* (quote get-vp)))) (printf "info ~s scroll ~s region ~s size ~s\n" (tell *comment-window* (quote get-info)) (send vp get-scroll-position) (send vp get-scroll-region) (list (send vp get-width) (send vp get-height))))
(system (format "python3 -c 'from PIL import ImageGrab; ImageGrab.grab().save(\"~a\")'" out))
(exit 0)
