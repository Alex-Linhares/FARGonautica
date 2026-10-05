;;=============================================================================
;; Copyright (c) 1999, 2003 by James B. Marshall
;;
;; This file is part of Metacat.
;;
;; Metacat is based on Copycat, which was originally written in Common
;; Lisp by Melanie Mitchell.
;;
;; Metacat is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.
;;
;; Metacat is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;=============================================================================
;; Ported to Racket, 2026: included by racket/engine.rkt (see docs/porting-notes.md,
;; item 04).  Changes are marked port:.
;;=============================================================================

(define *codelet-count* 0)
(define *temperature* 0)

(define *workspace-window* #f)
(define *slipnet-window* #f)
(define *coderack-window* #f)
(define *themespace-window* #f)
(define *top-themes-window* #f)
(define *bottom-themes-window* #f)
(define *vertical-themes-window* #f)
(define *memory-window* #f)
(define *comment-window* #f)
(define *trace-window* #f)
(define *temperature-window* #f)
(define *EEG-window* #f)
(define *control-panel* #f)

;; Default configuration:
(define %eliza-mode% #t)
(define %justify-mode% #f)
(define %self-watching-enabled% #t)
(define %verbose% #f)
(define %workspace-graphics% #t)
(define %slipnet-graphics% #t)
(define %coderack-graphics% #t)
(define %codelet-count-graphics% #t)
(define %highlight-last-codelet% #t)
(define %nice-graphics% #t)

(define *repl-thread* #f)

;; port: (setup) and enable-resizing create and arrange the windows, which
;; the engine cannot do (engine modules never require racket/gui).  They
;; are in the GUI layer, racket/gui/setup.rktl (item 15), which sets the
;; window globals above through set-global! (racket/engine.rkt).

;;------------------------------------------------------------------
;; User-interface commands

(define eliza-mode-on
  (lambda ()
    (set! %eliza-mode% #t)
    (tell *comment-window* 'switch-modes)
    'ok))

(define eliza-mode-off
  (lambda ()
    (set! %eliza-mode% #f)
    (tell *comment-window* 'switch-modes)
    'ok))

(define slipnet-on
  (lambda ()
    (set! %slipnet-graphics% #t)
    (if* (not *display-mode?*)
      (tell *slipnet-window* 'restore-current-state))
    'ok))

(define slipnet-off
  (lambda ()
    (set! %slipnet-graphics% #f)
    (if* (not *display-mode?*)
      (tell *slipnet-window* 'blank-window))
    'ok))

(define coderack-on
  (lambda ()
    (set! %coderack-graphics% #t)
    (if* (not *display-mode?*)
      (tell *coderack-window* 'restore-current-state))
    'ok))

(define coderack-off
  (lambda ()
    (set! %coderack-graphics% #f)
    (if* (not *display-mode?*)
      (tell *coderack-window* 'blank-window "Coderack"))
    'ok))

(define codelet-counts-on
  (lambda ()
    (set! %codelet-count-graphics% #t)
    (tell *coderack-window* 'initialize)
    'ok))

(define codelet-counts-off
  (lambda ()
    (set! %codelet-count-graphics% #f)
    (tell *coderack-window* 'initialize)
    'ok))

(define clearmem
  (lambda ()
    (tell *memory* 'clear)
    'ok))

(define verbose-on
  (lambda ()
    (if* (not (tell *control-panel* 'verbose-mode?))
      (tell *control-panel* 'toggle-verbose-mode))
    'ok))

(define verbose-off
  (lambda ()
    (if* (tell *control-panel* 'verbose-mode?)
      (tell *control-panel* 'toggle-verbose-mode))
    'ok))

(define speed
  (lambda ()
    (printf "Current speed settings:~n")
    (printf "  %num-of-flashes%           ~a~%" %num-of-flashes%)
    (printf "  %flash-pause%              ~a ms~%" %flash-pause%)
    (printf "  %snag-pause%               ~a ms~%" %snag-pause%)
    (printf "  %codelet-highlight-pause%  ~a ms~%" %codelet-highlight-pause%)
    (printf "  %text-scroll-pause%        ~a ms~%" %text-scroll-pause%)))
