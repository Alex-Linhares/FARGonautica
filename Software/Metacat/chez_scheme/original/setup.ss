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

(define setup
  (lambda args
    (let ((scale (if (null? args) 1 (car args))))
      (printf "Initializing windows...")
      (set-window-size-defaults scale)
      (create-mcat-logo)
      (set! *workspace-window* (make-workspace-window))
      (set! *slipnet-window* (make-slipnet-window *13x5-layout-table*))
      (set! *coderack-window* (make-coderack-window))
      (set! *themespace-window* (make-themespace-window *themespace-window-layout*))
      (set! *top-themes-window* (tell *themespace-window* 'get-window 'top-bridge))
      (set! *bottom-themes-window* (tell *themespace-window* 'get-window 'bottom-bridge))
      (set! *vertical-themes-window* (tell *themespace-window* 'get-window 'vertical-bridge))
      (set! *memory-window* (make-memory-window))
      (set! *comment-window* (make-comment-window))
      (set! *trace-window* (make-trace-window))
      (set! *temperature-window* (make-temperature-window))
      (set! *EEG-window* (make-EEG-window))
      (set! *control-panel* (make-control-panel))
      (set! *repl-thread* (thread-self))
      (enable-resizing)
      ;; this reduces an annoying problem in SWL 0.9x in which >'s gradually fill
      ;; up the bottom line of the REPL window with each new call to (break):
      (if* (equal? swl:version "0.9x") (waiter-prompt-string (format "~%>")))
      (printf "done~%"))))

(define enable-resizing
  (lambda ()
    (tell *workspace-window* 'make-resizable 'workspace)
    (tell *slipnet-window* 'make-resizable 'slipnet)
    (tell *coderack-window* 'make-resizable 'coderack)
    (tell *temperature-window* 'make-resizable 'temperature)
    (tell *themespace-window* 'make-resizable 'theme)
    (tell *trace-window* 'make-resizable 'trace)
    (tell *memory-window* 'make-resizable 'memory)
    (tell *EEG-window* 'make-resizable 'EEG)
    (tell *comment-window* 'make-resizable 'comment)
    (start-resize-listener)))

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
