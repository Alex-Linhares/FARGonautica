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
;; Ported to Racket, 2026: setup.ss's setup and enable-resizing, included by
;; racket/gui/gui.rkt (the rest of setup.ss is in the engine,
;; racket/engine/setup.rktl).  Changes are marked port:; see
;; docs/porting-notes.md, item 15.
;;=============================================================================

(define setup
  (lambda args
    (let ((scale (if (null? args) 1 (car args))))
      (printf "Initializing windows...")
      ;; port: graphics windows are frames on the screen
      (set-window-host-maker! make-screen-host)
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
      ;; port: the windows' places (the window manager chose them), first
      ;; for an estimated control panel size
      (arrange-windows! '(400 230))
      (set! *control-panel* (make-control-panel))
      ;; port: again, now that the control panel's size is known
      (let ((cp (cdr (assq 'frame (tell *control-panel* 'get-widgets)))))
	(arrange-windows! (list (send cp get-width) (send cp get-height))))
      ;; port: the engine thread stands for the REPL thread
      (set! *repl-thread* (make-engine-thread (current-output-port)))
      (enable-resizing)
      ;; port: SWL 0.9x's waiter prompt workaround is left out; the windows'
      ;; refresh timer starts instead (racket/gui/gui.rkt)
      (start-gui-refresh!)
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
