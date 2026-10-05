#lang racket/base
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
;; Ported to Racket, 2026: the views, i.e. the windows of the graphics files,
;; as one module that includes their ported .rktl files in metacat.ss's load
;; order, on top of the engine (racket/engine.rkt), the SGL interpreter
;; (sgl.rkt), fonts.ss (fonts.rkt) and the colours (colors.rkt).  It needs
;; racket/draw but not racket/gui: windows are drawn into viewport% display
;; lists, shown by a window host (offscreen here; on screen once the control
;; panel exists).
;;
;; Loading this module is the counterpart of metacat.ss loading the graphics
;; files into the program's one top level: their definitions of names the
;; model reads (colours, fonts, *fg-color*, restore-current-state, ...) are
;; installed in the engine by engine-route.rkt's define and set!.  It changes
;; nothing in a run by itself; attach-workspace-view! turns the Workspace
;; window on, as setup.ss's (setup) and the control panel do.
;;
;; The graphics code the model calls (pexp builders: general-graphics.ss's
;; shapes, group-, bridge- and rule-graphics.ss) is in the engine.  See
;; docs/porting-notes.md, item 13.
;;=============================================================================

(require racket/class
         racket/include
         (only-in ffi/unsafe/atomic start-atomic end-atomic)
         (only-in racket/draw make-bitmap bitmap-dc%)
         "engine-route.rkt"
         "../compat.rkt"
         "../utilities.rkt"
         "../engine.rkt"
         (except-in "sgl.rkt" *platform* *tcl/tk-version-8_3?* %nice-graphics%)
         "fonts.rkt"
         (except-in "colors.rkt" =white= =black= =grey= =red= =green= =blue= =yellow=
                    =pink= =orange=))

(provide (except-out (all-defined-out) window-host%)
         attach-workspace-view! attach-views! window->bitmap save-window-png
         make-window-host set-window-host-maker! set-thread-break-handler!)

;;-----------------------------------------------------------------------------
;; port: the parts of SWL the window code uses

;; SWL's sync-display flushed Tk's drawing; the GUI hooks the same procedure
;; as the SGL interpreter (set-flush-event-queue!)
(define swl:sync-display (lambda () (*flush-event-queue*)))

;; the screen, for constants.ss's set-window-size-defaults (which reads it
;; but sizes windows by its scale argument only)
(define swl:screen-width (lambda () 1280))
(define swl:screen-height (lambda () 1024))

;; SWL thread message queues (general-graphics.ss's resize listener).  A
;; receive takes the semaphore and the message in one atomic step, so that
;; thread-msg-waiting? (a message is queued) and the semaphore agree: the
;; resize handler, in a critical section, receives a waiting message without
;; blocking.  (Taking them in two steps deadlocked the GUI thread when the
;; listener thread ran between them.)
(define thread-make-msg-queue
  (lambda (name) (vector name '() (make-semaphore 0))))
(define thread-send-msg
  (lambda (q msg)
    (start-atomic)
    (vector-set! q 1 (append (vector-ref q 1) (list msg)))
    (semaphore-post (vector-ref q 2))
    (end-atomic)))
(define thread-msg-waiting?
  (lambda (q) (pair? (vector-ref q 1))))
(define thread-receive-msg
  (lambda (q)
    (let loop ()
      (start-atomic)
      (if (semaphore-try-wait? (vector-ref q 2))
          (let ((msg (car (vector-ref q 1))))
            (vector-set! q 1 (cdr (vector-ref q 1)))
            (end-atomic)
            msg)
          (begin
            (end-atomic)
            (sync (semaphore-peek-evt (vector-ref q 2)))
            (loop))))))
(define thread-fork (lambda (thunk) (thread thunk)))
(define thread-sleep (lambda (ms) (sleep (/ ms 1000.0))))
;; SWL's critical-section: no other Racket thread runs inside it
(define-syntax-rule (critical-section e ...)
  (dynamic-wind start-atomic (lambda () e ...) end-atomic))
;; the Workspace window's mouse handler interrupts the REPL thread to (go);
;; the control panel (racket/gui/gui.rkt) installs a handler that hands the
;; thunk to its engine thread; without one (offscreen views) it raises
(define thread-break-handler
  (lambda (thread ignore k)
    (error 'thread-break "no engine thread to interrupt")))
(define (set-thread-break-handler! h) (set! thread-break-handler h))
(define thread-break
  (lambda (thread ignore k) (thread-break-handler thread ignore k)))

;; The window host: SWL's <toplevel> and its <frame> or <scrollframe>, with
;; the methods make-graphics-window sends them.  Offscreen by default (it
;; keeps the title and geometry, and has no scrollbars); the control panel
;; installs a maker of on-screen hosts with set-window-host-maker!.
(define window-host%
  (class object%
    (init-field scrolling destroy-action)
    (super-new)
    (define title "")
    (define geometry "+0+0")
    (define viewport #f)
    (define/public (show-viewport vp) (set! viewport vp))
    (define/public (get-viewport) viewport)
    (define/public (set-title! t) (set! title t))
    (define/public (get-title) title)
    (define/public (set-geometry! g) (set! geometry g))
    (define/public (get-geometry)
      (format "~ax~a~a" (get-width) (get-height)
              (let ((i (let loop ((i 0))
                         (cond ((= i (string-length geometry)) #f)
                               ((char=? (string-ref geometry i) #\+) i)
                               (else (loop (+ i 1)))))))
                (if i (substring geometry i) "+0+0"))))
    (define/public (get-width) (+ 2 (if viewport (send viewport get-width) 0)))
    (define/public (get-height) (+ 2 (if viewport (send viewport get-height) 0)))
    (define/public (set-resizable! w h) (void))
    (define/public (set-min-size! w h) (void))
    (define/public (set-aspect-ratio-bounds! a b) (void))
    (define/public (get-scrollbar orientation) #f)
    (define/public (set-vertical-view! fraction) (void))
    (define/public (raise) (void))
    (define/public (lower) (void))
    (define/public (destroy) (void))))

(define window-host-maker
  (lambda (scrolling destroy-action)
    (new window-host% (scrolling scrolling) (destroy-action destroy-action))))
(define (set-window-host-maker! maker) (set! window-host-maker maker))
(define (make-window-host scrolling destroy-action)
  (window-host-maker scrolling destroy-action))

;;-----------------------------------------------------------------------------
;; The graphics files, in metacat.ss's load order

(include "constants.rktl")             ; constants.ss (graphics part)
(include "general-graphics.rktl")      ; general-graphics.ss (windows)
(include "slipnet-graphics.rktl")      ; slipnet-graphics.ss
(include "workspace-graphics.rktl")    ; workspace-graphics.ss
(include "temperature-graphics.rktl")  ; temperature-graphics.ss
(include "coderack-graphics.rktl")     ; coderack-graphics.ss
(include "theme-graphics.rktl")        ; theme-graphics.ss (without relation-name)
(include "trace-graphics.rktl")        ; trace-graphics.ss (without group-event-pexp-text-string)
(include "memory-graphics.rktl")       ; memory-graphics.ss
(include "commentary-graphics.rktl")   ; commentary-graphics.ss
(include "eeg-graphics.rktl")          ; eeg-graphics.ss (the window)

;;-----------------------------------------------------------------------------
;; port: attaching views to a run, and pictures of them

;; The Workspace window, as (setup) makes it, with workspace graphics on.
;; gui.ss's speed settings are set as at full speed with no flashing (the
;; speed slider sets them once the control panel exists).  Call before
;; init-mcat.  Returns the window.
(define (attach-workspace-view! [width 800])
  (set! %num-of-flashes% 1)
  (set! %flash-pause% 0)
  (set! %snag-pause% 0)
  (set! %codelet-highlight-pause% 0)
  (set! %text-scroll-pause% 0)
  (set! *workspace-window* (make-workspace-window width))
  (set! %workspace-graphics% #t)
  *workspace-window*)

;; Every window, as setup.ss's (setup) makes them (without the logo and the
;; control panel, item 15), with every graphics switch on as setup.ss
;; defines them.  scale is (setup)'s, for set-window-size-defaults.  The
;; speed settings are as in attach-workspace-view!.  Call before init-mcat.
;; Returns an association list of the windows by name.
(define (attach-views! [scale 1])
  (set! %num-of-flashes% 1)
  (set! %flash-pause% 0)
  (set! %snag-pause% 0)
  (set! %codelet-highlight-pause% 0)
  (set! %text-scroll-pause% 0)
  (set-window-size-defaults scale)
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
  (set! %workspace-graphics% #t)
  (set! %slipnet-graphics% #t)
  (set! %coderack-graphics% #t)
  (set! %codelet-count-graphics% #t)
  (set! %highlight-last-codelet% #t)
  (list (cons 'workspace *workspace-window*)
        (cons 'slipnet *slipnet-window*)
        (cons 'coderack *coderack-window*)
        (cons 'top-themes *top-themes-window*)
        (cons 'bottom-themes *bottom-themes-window*)
        (cons 'vertical-themes *vertical-themes-window*)
        (cons 'memory *memory-window*)
        (cons 'commentary *comment-window*)
        (cons 'trace *trace-window*)
        (cons 'temperature *temperature-window*)
        (cons 'EEG *EEG-window*)))

;; the visible part of a graphics window, as a bitmap
(define (window->bitmap window)
  (let* ((vp (tell window 'get-vp))
         (bm (make-bitmap (send vp get-width) (send vp get-height) #f))
         (dc (new bitmap-dc% (bitmap bm))))
    (send vp render dc)
    bm))

(define (save-window-png window file)
  (send (window->bitmap window) save-file file 'png))

;; port: (set-view-global! 'name value), set! of a variable of the graphics
;; files from the control panel (racket/gui/gui.rkt, whose set! routes here
;; through engine-route.rkt), as gui.ss set!s them on the shared top level
(define (set-view-global! sym value)
  (case sym
    [(%comment-window-font%) (set! %comment-window-font% value)]
    [(*theme-edit-mode?*) (set! *theme-edit-mode?* value)]
    [else (error 'set-view-global! "not a settable view global: ~s" sym)]))
