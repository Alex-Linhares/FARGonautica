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
;; Ported to Racket, 2026: the program's user interface on racket/gui.  This
;; module includes the ports of gui.ss (gui.rktl: the control panel, its menus
;; and dialogs) and of setup.ss's setup and enable-resizing (setup.rktl), on
;; top of the views (views.rkt) and the engine.  What SWL and Tk gave the
;; original is here:
;;
;;   - screen-host%: an on-screen window host (a racket/gui frame with a
;;     canvas painting a viewport% display list), installed as views.rkt's
;;     window host maker, so every graphics window is a frame on the screen;
;;   - the engine thread, the counterpart of the original's REPL thread: the
;;     control panel hands it thunks with thread-break (init-mcat + run-mcat,
;;     go, ...), and each runs until run.ss's (reset) returns to it;
;;   - create-mcat-logo (fonts.ss) and the window layout.
;;
;; The engine never requires this module (nor racket/gui).  Watching changes
;; nothing in a run: the hosts only read display lists.  See
;; docs/porting-notes.md, item 15, and docs/divergences.md.
;;=============================================================================

(require racket/class
         racket/include
         racket/runtime-path
         (prefix-in g: racket/gui/base)
         (only-in racket/draw color%)
         "engine-route.rkt"
         "../compat.rkt"
         "../utilities.rkt"
         "../engine.rkt"
         (except-in "sgl.rkt" *platform* *tcl/tk-version-8_3?* %nice-graphics%)
         "fonts.rkt"
         (except-in "colors.rkt" =white= =black= =grey= =red= =green= =blue= =yellow=
                    =pink= =orange=)
         "views.rkt")

(provide setup enable-resizing make-control-panel create-mcat-logo
         tokenize-string char-noise? step-button-action go-button-action
         stop-button-action reset-button-action speed-slider-action
         save-commentary-action set-breakpoint-action clear-breakpoint-action
         set-step-interval-action help-action
         *mcat-logo* screen-host% engine-busy? engine-idle-evt
         set-file-dialog! arrange-windows! window-frames
         start-gui-refresh! stop-gui-refresh!
         ;; for the one-window GUI (one-window.rkt)
         make-engine-thread make-pane-host-maker set-control-panel-frame-maker!)


;;-----------------------------------------------------------------------------
;; port: SWL generics on racket/gui widgets

;; show and hide: a graphics window's host, a frame, or a widget in a panel
(define (show w)
  (cond
    ((is-a? w screen-host%) (send w show-window))
    ((is-a? w g:top-level-window<%>) (send w show #t))
    ((is-a? w g:window<%>) (send w show #t))
    (else (void))))
(define (hide w)
  (cond
    ((is-a? w screen-host%) (send w hide-window))
    ((is-a? w g:window<%>) (send w show #f))
    (else (void))))

(define (enable-widget w on?) (send w enable on?))

;; run thunk in the thread of w's eventspace and wait for it.  run.rktl's
;; break, quiet-break and go switch the control panel's mode from the engine
;; thread; a text field's set-value there raised "insert cannot be called"
;; when the GUI thread held the field's editor lock (an intermittent failure
;; in control-panel-test.rkt).  The GUI thread never waits for the engine,
;; so waiting here cannot deadlock.
(define (call-on-gui-thread w thunk)
  (let ((es (send w get-eventspace)))
    (if (eq? (current-thread) (g:eventspace-handler-thread es))
        (thunk)
        (let ((done (make-semaphore 0)) (result #f))
          (parameterize ((g:current-eventspace es))
            (g:queue-callback
              (lambda ()
                (dynamic-wind void
                              (lambda () (set! result (with-handlers ((exn:fail? values)) (thunk))))
                              (lambda () (semaphore-post done))))
              #t))
          (semaphore-wait done)
          (if (exn:fail? result) (raise result) result)))))

;; an swl-font% (fonts.rkt) as a racket/draw font for widgets
(define (widget-font f) (if f (send f get-font) g:normal-control-font))

;; swl:file-dialog: the save dialog of "Save commentary to file".  Tests
;; replace it (set-file-dialog!) to save without a modal dialog.
(define file-dialog
  (lambda (title mode dir)
    (let ((p (g:put-file title #f (and dir (directory-exists? dir) dir))))
      (and p (path->string p)))))
(define (set-file-dialog! f) (set! file-dialog f))
(define swl:file-dialog (lambda (title mode dir) (file-dialog title mode dir)))

(define *file-dialog-directory*
  (let ((home (find-system-path 'home-dir)))
    (path->string home)))

(define-runtime-path help-file "../../chez_scheme/original/help.txt")

;;-----------------------------------------------------------------------------
;; port: the on-screen window host (SWL's <toplevel> with its <frame> or
;; <scrollframe>), with the methods make-graphics-window sends (the same as
;; views.rkt's offscreen window-host%), plus show-window and hide-window for
;; the window controllers.  The canvas paints the viewport's display list;
;; a timer (start-gui-refresh!) repaints windows whose display list changed
;; and keeps the scrollbars in step with the viewport's scroll region, as
;; Tk's scrollframe did.

;; Tk's scrollbars on X11 are 15 pixels wide (-width 11 plus two 2-pixel
;; borders); create-mcat-logo measured them in the original
(define %tk-scrollbar-size% 15)

(define scrollbar-stub%
  (class object%
    (super-new)
    (define/public (get-width) %tk-scrollbar-size%)
    (define/public (get-height) %tk-scrollbar-size%)))
(define the-scrollbar (new scrollbar-stub%))

(define hosts '())

(define host-frame%
  (class g:frame%
    (init-field host)
    (super-new)
    (define/augment (can-close?) (send host close-request))))

(define view-canvas%
  (class g:canvas%
    (init-field vp host)
    (super-new)
    (define/override (on-paint) (send host paint (send this get-dc)))
    (define/override (on-size w h) (send host canvas-resized))
    (define/override (on-scroll e) (send host user-scrolled))
    (define/override (on-event e)
      (case (send e get-event-type)
        ((left-down)
         (send host press (send e get-x) (send e get-y)
               (if (send e get-shift-down) 'shift-left 'left)))
        ((right-down)
         (send host press (send e get-x) (send e get-y) 'right))
        (else (void))))))

(define screen-host%
  (class object%
    ;; port (one window): with a pane-parent (one-window.rkt), the host is a
    ;; pane of the one window: its canvas is a child of pane-parent, there
    ;; is no frame, and a window of fixed aspect ratio is letterboxed in it
    (init-field scrolling destroy-action [pane-parent #f])
    (super-new)
    (define frame (and (not pane-parent) (new host-frame% (label "") (host this))))
    (define title "")
    (define aspect #f)        ; pane: (w+2)/(h+2) of the first size, or #f
    (define offset-x 0)       ; pane: where the viewport sits in the canvas
    (define offset-y 0)
    (define canvas #f)
    (define viewport #f)
    (define visible? #f)
    (define resizable? #f)
    (define dirty? #t)
    (define sb-state #f)
    (define h-style? (and (memq scrolling '(both horizontal)) #t))
    (define v-style? (and (memq scrolling '(both vertical)) #t))
    (define/public (get-frame) frame)
    (define/public (get-canvas) canvas)
    (define/public (visible?*) visible?)
    (define/public (show-viewport vp)
      (set! viewport vp)
      (set! canvas
        (new view-canvas% (parent (or frame pane-parent)) (vp vp) (host this)
             (style (append '(no-autoclear)
                            (if h-style? '(hscroll) '())
                            (if v-style? '(vscroll) '())))))
      (when (or h-style? v-style?)
        (send canvas init-manual-scrollbars (and h-style? 1) (and v-style? 1) 1 1 0 0)
        (send canvas show-scrollbars #f #f))
      (if frame
        (begin
          (send canvas min-client-width (send vp get-width))
          (send canvas min-client-height (send vp get-height)))
        (begin
          (send canvas show #f)
          (when (memq scrolling '(both none))
            (set! aspect (/ (+ (send vp get-width) 2) (+ (send vp get-height) 2))))))
      (send vp set-changed-callback! (lambda () (set! dirty? #t)))
      (set! hosts (cons this hosts)))
    (define/public (get-viewport) viewport)
    (define/public (pane?) (not frame))
    (define/public (set-title! t) (if frame (send frame set-label t) (set! title t)))
    (define/public (get-title) (if frame (send frame get-label) title))
    (define/public (set-geometry! g)
      (let ((m (and frame (regexp-match #rx"^(?:([0-9]+)x([0-9]+))?(?:([+-][0-9]+)([+-][0-9]+))?$" g))))
        (when (and m (list-ref m 3))
          (send frame move (string->number (list-ref m 3)) (string->number (list-ref m 4))))))
    (define/public (get-geometry)
      (if frame
        (format "~ax~a+~a+~a" (get-width) (get-height) (send frame get-x) (send frame get-y))
        (format "~ax~a+0+0" (get-width) (get-height))))
    (define/public (get-width)
      (+ 2 (if canvas (let-values (((w h) (send canvas get-client-size))) w) 0)))
    (define/public (get-height)
      (+ 2 (if canvas (let-values (((w h) (send canvas get-client-size))) h) 0)))
    ;; Tk's scrollframe put its scrollbars beside the canvas from the start;
    ;; here they appear once the scroll region is known.  Show them now,
    ;; while the canvas's minimum size is still the viewport's, so that the
    ;; frame grows around them instead of the canvas shrinking (which would
    ;; resize every scrolling window at once, and the original's resize
    ;; listener keeps only the last of simultaneous resizes).
    (define/public (set-resizable! w h)
      (when (or h-style? v-style?)
        (sync-scrollbars)
        ;; the canvas's minimum size counts the scrollbars shown when it is set
        (send canvas min-client-width (send viewport get-width))
        (send canvas min-client-height (send viewport get-height)))
      (if frame
        (send frame reflow-container)
        ;; a pane's size is the one window's layout's, not the viewport's
        (begin (send canvas min-client-width 0) (send canvas min-client-height 0)))
      (set! resizable? (and w h)))
    (define/public (set-min-size! w h)
      (when frame
        (send canvas min-client-width w)
        (send canvas min-client-height h)))
    (define/public (set-aspect-ratio-bounds! a b) (void))
    (define/public (scroll-needs)
      (let* ((r (send viewport get-scroll-region))
             (rw (- (caddr r) (car r)))
             (rh (- (cadddr r) (cadr r))))
        (values rw rh
                (and h-style? (> rw (send viewport get-width)))
                (and v-style? (> rh (send viewport get-height))))))
    (define/public (get-scrollbar orientation)
      (let-values (((rw rh h? v?) (scroll-needs)))
        (and (if (eq? orientation 'horizontal) h? v?) the-scrollbar)))
    (define/public (set-vertical-view! fraction) (void))
    (define/public (raise) (when (and visible? frame) (send frame show #t)))
    (define/public (lower) (void))
    (define/public (destroy) (hide-window))
    (define/public (show-window) (set! visible? #t) (show-or-hide #t))
    (define/public (hide-window) (set! visible? #f) (show-or-hide #f))
    ;; a pane's parent lays its panes out again
    (define/private (show-or-hide on?)
      (send (or frame canvas) show on?)
      (when pane-parent (send pane-parent container-flow-modified)))
    (define/public (close-request) (and (destroy-action this) #t))
    ;; the canvas changed size: Tk's <Configure> on the viewport
    (define/public (canvas-resized)
      (when (and canvas resizable? visible?)
        (let-values (((w h) (pane-viewport-size)))
          (unless (or (and (= w (send viewport get-width)) (= h (send viewport get-height)))
                      ;; a pane waits for the resize queue to empty: the
                      ;; original's listener keeps only the last of
                      ;; simultaneous resizes, and in one window every pane
                      ;; changes size at once
                      (and (not frame) (thread-msg-waiting? *resize-message-queue*)))
            (send viewport set-size! w h)
            (send viewport configure (+ w 2) (+ h 2))))))
    ;; the viewport's size for the canvas's: all of it, or for a pane of
    ;; fixed aspect ratio the largest (w+2):(h+2) box at the pane's top centre
    (define/private (pane-viewport-size)
      (let-values (((cw ch) (send canvas get-client-size)))
        (if (and aspect (not frame))
          (let* ((W (+ cw 2)) (H (+ ch 2))
                 (fit-h (max 3 (min H (round (/ W aspect)))))
                 (fit-w (max 3 (min W (round (* fit-h aspect)))))
                 (w (- fit-w 2)) (h (- fit-h 2)))
            (set! offset-x (max 0 (quotient (- cw w) 2)))
            (set! offset-y 0)
            (values w h))
          (values cw ch))))
    ;; for tests: the viewport has the size the canvas gives it
    (define/public (settled?)
      (or (not (and canvas resizable? visible?))
          (let-values (((w h) (pane-viewport-size)))
            (and (= w (send viewport get-width)) (= h (send viewport get-height))))))
    (define/public (get-offset) (list offset-x offset-y))
    ;; on-paint: a pane paints its margins in the viewport's background
    (define/public (paint dc)
      (if (and (not frame)
               (let-values (((cw ch) (send canvas get-client-size)))
                 (or (< (send viewport get-width) cw) (< (send viewport get-height) ch))))
        (begin
          (send dc set-background (send viewport get-background-color))
          (send dc clear)
          (send dc set-clipping-rect offset-x offset-y
                (send viewport get-width) (send viewport get-height))
          (send dc set-initial-matrix (vector 1.0 0.0 0.0 1.0 offset-x offset-y))
          (send viewport render dc)
          (send dc set-initial-matrix (vector 1.0 0.0 0.0 1.0 0.0 0.0))
          (send dc set-clipping-region #f))
        (send viewport render dc)))
    (define syncing? #f)
    (define/public (user-scrolled)
      ;; GTK reports scroll events while init-manual-scrollbars changes the
      ;; range, with stale positions: only the user's scrolling counts
      (unless syncing?
       (send viewport set-scroll-position!
            (if h-style? (send canvas get-scroll-pos 'horizontal) 0)
            (if v-style? (send canvas get-scroll-pos 'vertical) 0))
       (set! sb-state #f)
       (set! dirty? #t)))
    (define/public (press x y button)
      (with-handlers ((exn:fail? (lambda (e)
                                   (eprintf "mouse handler: ~a\n" (exn-message e)))))
        (send viewport mouse-press (- x offset-x) (- y offset-y) button)))
    ;; called by the refresh timer, in the GUI thread
    (define/public (tick)
      (when viewport
        (when (or h-style? v-style?) (sync-scrollbars))
        ;; showing or hiding a scrollbar changes the client size without an
        ;; on-size (which reports the whole canvas), so look every tick
        (canvas-resized)
        (when dirty?
          (set! dirty? #f)
          (send canvas refresh))))
    (define/private (sync-scrollbars)
      (let-values (((rw rh h? v?) (scroll-needs)))
        (let* ((vw (send viewport get-width))
               (vh (send viewport get-height))
               (sp (send viewport get-scroll-position))
               (state (list rw rh vw vh sp)))
          (unless (equal? state sb-state)
            (set! sb-state state)
            (let ((hlen (max 1 (- rw vw)))
                  (vlen (max 1 (- rh vh))))
              (set! syncing? #t)
              (send canvas init-manual-scrollbars
                    (and h-style? hlen) (and v-style? vlen)
                    (max 1 vw) (max 1 vh)
                    (max 0 (min hlen (car sp))) (max 0 (min vlen (cadr sp))))
              (send canvas show-scrollbars h? v?)
              (set! syncing? #f))))))))

(define (make-screen-host scrolling destroy-action)
  (new screen-host% (scrolling scrolling) (destroy-action destroy-action)))

;; port (one window): a maker of hosts that are panes in parent
(define (make-pane-host-maker parent)
  (lambda (scrolling destroy-action)
    (new screen-host% (scrolling scrolling) (destroy-action destroy-action)
         (pane-parent parent))))

;; port (one window): the control panel's widgets go into this frame
;; instead of a frame of their own (make-control-panel, gui.rktl)
(define control-panel-frame-maker #f)
(define (set-control-panel-frame-maker! f) (set! control-panel-frame-maker f))

;; repaint changed windows 20 times a second
(define refresh-timer #f)
(define (start-gui-refresh!)
  (unless refresh-timer
    (set! refresh-timer
      (new g:timer%
           (notify-callback (lambda () (for-each (lambda (h) (send h tick)) hosts)))
           (interval 50)))))

;;-----------------------------------------------------------------------------
;; stopping it lets the eventspace finish once every window is closed
(define (stop-gui-refresh!)
  (when refresh-timer
    (send refresh-timer stop)
    (set! refresh-timer #f)))

;;-----------------------------------------------------------------------------
;; port: the engine thread, the counterpart of the original's REPL thread.
;; thread-break (SWL: interrupt the REPL thread to run a thunk) sends it the
;; thunk; it runs each one until run.ss's break or quiet-break calls
;; (reset), which returns here.  The reset handler is set, not
;; parameterized, so that (go), which re-enters a continuation captured in an
;; earlier thunk, returns here too (docs/anomalies_and_quirks.md).  An error
;; in the model is reported to the control panel, which returns to input mode.

(define engine-busy-flag #f)
(define (engine-busy?) engine-busy-flag)
(define idle-sema (make-semaphore 0))
(define (engine-idle-evt) (semaphore-peek-evt idle-sema))

(define (make-engine-thread out)
  (thread
    (lambda ()
      (current-output-port out)
      (let loop ()
        (let ((thunk (thread-receive)))
          (set! engine-busy-flag #t)
          (set! idle-sema (make-semaphore 0))
          (let/ec k
            (reset-handler (lambda () (k 'reset)))
            (call-with-continuation-prompt
              (lambda ()
                (with-handlers ((exn:fail?
                                  (lambda (e)
                                    (eprintf "Error: ~a\n" (exn-message e))
                                    (set-global! '*running?* #f)
                                    (when *control-panel*
                                      (tell *control-panel* 'engine-error (exn-message e))))))
                  (thunk)))))
          (set! engine-busy-flag #f)
          (semaphore-post idle-sema)
          (loop))))))

(set-thread-break-handler!
  (lambda (thread ignore k)
    (set! engine-busy-flag #t)
    (thread-send thread k)))

;;-----------------------------------------------------------------------------
;; port: create-mcat-logo (fonts.ss).  The logo window measured Tk's
;; scrollbars and kept a hidden canvas for measuring text; the port's fonts
;; measure on a private bitmap (fonts.rkt) and the scrollbars are Tk's size.

(define *mcat-logo* #f)

(define logo-frame%
  (class g:frame%
    (super-new)
    (define/augment (can-close?) (toplevel-destroy-action this) #f)))

(define create-mcat-logo
  (lambda ()
    (let* ((top (new logo-frame% (label "Logo") (style '(no-resize-border))))
           (frame (new g:vertical-panel% (parent top)))
           (logo (new g:canvas% (parent frame)
                      (min-width 110) (min-height 80)
                      (stretchable-width #f) (stretchable-height #f)
                      (paint-callback
                        (lambda (c dc)
                          (send dc set-background %logo-background-color%)
                          (send dc clear)
                          (send dc set-font (widget-font %logo-font%))
                          (send dc set-text-foreground =black=)
                          (let-values (((w h d a) (send dc get-text-extent "Metacat")))
                            (send dc draw-text "Metacat" (- 55 (/ w 2)) (- 50 h))))))))
      (set-scrollbar-size! %tk-scrollbar-size% %tk-scrollbar-size%)
      (set! *mcat-logo* logo)
      'done)))

;;-----------------------------------------------------------------------------
;; port: where the windows go.  The original left placement to the window
;; manager; the port tiles them on the screen in three rows: the control
;; panel with the Temperature under it, then the Workspace, Coderack and
;; Commentary; the Slipnet, the Top and Bottom Themes (stacked), the
;; Vertical Themes and the Memory; the Temporal Trace (and the EEG, hidden at
;; first) under the Slipnet.

(define (window-frames)
  (list (cons 'workspace *workspace-window*) (cons 'slipnet *slipnet-window*)
        (cons 'coderack *coderack-window*) (cons 'temperature *temperature-window*)
        (cons 'trace *trace-window*) (cons 'commentary *comment-window*)
        (cons 'memory *memory-window*) (cons 'top-themes *top-themes-window*)
        (cons 'bottom-themes *bottom-themes-window*)
        (cons 'vertical-themes *vertical-themes-window*) (cons 'EEG *EEG-window*)))

(define %window-gap% 8)
(define %title-bar% 28)

(define (arrange-windows! control-panel-size)
  (let* ((size (lambda (w) (tell w 'get-size)))
         (wd (lambda (w) (+ 2 (car (size w)))))
         (ht (lambda (w) (+ 2 (cadr (size w)) %title-bar%)))
         (place (lambda (w x y) (tell w 'set-position x y)))
         (cp-w (car control-panel-size))
         (cp-h (+ (cadr control-panel-size) %title-bar%)))
    ;; row 1
    (let* ((x1 (+ (max cp-w (wd *temperature-window*)) %window-gap%))
           (x2 (+ x1 (wd *workspace-window*) %window-gap%))
           (x3 (+ x2 (wd *coderack-window*) %window-gap%))
           (row1-h (max (+ cp-h %window-gap% (ht *temperature-window*))
                        (ht *workspace-window*) (ht *coderack-window*)
                        (ht *comment-window*)))
           (y2 (+ row1-h %window-gap%)))
      (place *temperature-window* 0 (+ cp-h %window-gap%))
      (place *workspace-window* x1 0)
      (place *coderack-window* x2 0)
      (place *comment-window* x3 0)
      ;; row 2
      (let* ((xb (+ (wd *slipnet-window*) %window-gap%))
             (xc (+ xb (max (wd *top-themes-window*) (wd *bottom-themes-window*)) %window-gap%))
             (xd (+ xc (wd *vertical-themes-window*) %window-gap%))
             (y3 (+ y2 (ht *slipnet-window*) %window-gap%)))
        (place *slipnet-window* 0 y2)
        (place *top-themes-window* xb y2)
        (place *bottom-themes-window* xb (+ y2 (ht *top-themes-window*) %window-gap%))
        (place *vertical-themes-window* xc y2)
        (place *memory-window* xd y2)
        ;; row 3
        (place *trace-window* 0 y3)
        (place *EEG-window* 0 (+ y3 (ht *trace-window*) %window-gap%))))
    'done))

;;-----------------------------------------------------------------------------

(include "gui.rktl")      ; gui.ss
(include "setup.rktl")    ; setup.ss's setup and enable-resizing
