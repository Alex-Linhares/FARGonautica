#lang racket/base
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;
;; One window for all of Metacat (loop0003, item 10): the control panel and
;; every graphics window as panes of one frame, laid out as the Python Qt
;; GUI lays them out (docs/qt-gui-plan.md 2.1-2.2):
;;
;;   menu bar; a control strip (problem, command line, speed, Step Go Stop
;;   Reset, breakpoint and self-watching messages)
;;   Temperature | Workspace | Coderack | Vertical Themes | Commentary
;;   Slipnet | Top Themes / Bottom Themes | Episodic Memory
;;   Temporal Trace / EEG (hidden at first, as today)
;;
;; Nothing here draws: the panes are gui.rkt's screen-host% in pane mode
;; (make-pane-host-maker), showing the same views, and the control panel is
;; gui.rktl's make-control-panel, built in this frame
;; (set-control-panel-frame-maker!) and then rearranged into the strip.
;; setup-one-window is setup.rktl's setup with those two changes.  The
;; panes have no splitters (racket/gui has none): they follow the window,
;; and the Windows menu hides and shows them.  A window of fixed aspect ratio
;; is letterboxed at the top centre of its pane; the others fill it.  Every
;; pane gets its size through the original's own resize protocol.  See
;; docs/divergences.md, "Racket: one window".
;;
;; The multi-window GUI (gui.rkt's setup, racket/main.rkt) is unchanged.

(require racket/class
         (prefix-in g: racket/gui/base)
         "engine-route.rkt"
         "../compat.rkt"
         "../utilities.rkt"
         "../engine.rkt"
         "views.rkt"
         "gui.rkt")

(provide setup-one-window pane-rects pane-names one-window-frame one-window-layout
         window-pane)

;;-----------------------------------------------------------------------------
;; The layout

;; the panes by row, in order
(define top-row '(temperature workspace coderack vertical-themes commentary))
(define middle-row '(slipnet top-themes bottom-themes memory))
(define bottom-row '(trace EEG))
(define pane-names (append top-row middle-row bottom-row))

;; the graphics window of a pane name
(define (pane-window name)
  (case name
    ((temperature) *temperature-window*) ((workspace) *workspace-window*)
    ((coderack) *coderack-window*) ((vertical-themes) *vertical-themes-window*)
    ((commentary) *comment-window*) ((slipnet) *slipnet-window*)
    ((top-themes) *top-themes-window*) ((bottom-themes) *bottom-themes-window*)
    ((memory) *memory-window*) ((trace) *trace-window*) ((EEG) *EEG-window*)))

;; a pane's canvas
(define (window-pane name)
  (send (tell (pane-window name) 'get-toplevel) get-canvas))

(define %gap% 4)
(define %min-text-width% 200)   ; the Commentary and the Memory

;; Where each shown pane goes in a w x h area: a hash of name ->
;; (list x y width height).  shown: the names of the shown panes.
;; Rows of 60%, 31% and 9% of the height (with the EEG shown, the bottom row
;; is 18%, shared by the Trace and the EEG); in a row, each pane of fixed
;; aspect ratio gets the width its ratio gives at the row's height, the
;; Temperature max(60, 6% of the width), and the Commentary or the Memory
;; the rest (at least 200 pixels; the others shrink if need be).  An empty
;; row gives its height to the others.
(define (pane-rects shown w h)
  (define (on? n) (and (memq n shown) #t))
  (define (row names) (filter on? names))
  (define r1 (row top-row))
  (define r2 (row middle-row))
  (define r3 (row bottom-row))
  (define rows-on (filter pair? (list r1 r2 r3)))
  (define gaps (* %gap% (max 0 (- (length rows-on) 1))))
  (define h3 (cond ((null? r3) 0)
                   ((null? (append r1 r2)) (- h gaps))
                   ((= (length r3) 2) (max 104 (round (* 0.18 h))))
                   (else (max 50 (round (* 0.09 h))))))
  (define rest (- h gaps h3))
  (define-values (h1 h2)
    (cond ((and (pair? r1) (pair? r2)) (let ((a (round (* rest 60/91)))) (values a (- rest a))))
          ((pair? r1) (values rest 0))
          ((pair? r2) (values 0 rest))
          (else (values 0 0))))
  (define y1 0)
  (define y2 (if (pair? r1) (+ h1 %gap%) 0))
  (define y3 (- h h3))
  (define rects (make-hasheq))
  ;; one row: items are (name width-or-#f), #f for the pane taking the rest
  (define (place-row! items y rh)
    (let* ((fixed (filter cadr items))
           (filler? (ormap (lambda (i) (not (cadr i))) items))
           (n (length items))
           (avail (- w (* %gap% (max 0 (- n 1)))))
           (sum (apply + (map cadr fixed)))
           (room (if filler? (- avail %min-text-width%) avail))
           (k (if (> sum room) (/ (max 0 room) sum) 1)))
      (let loop ((items items) (x 0))
        (unless (null? items)
          (let* ((i (car items))
                 (iw (if (cadr i)
                         (round (* k (cadr i)))
                         (- avail (round (* k sum))))))
            (hash-set! rects (car i) (list x y (max 1 iw) (max 1 rh)))
            (loop (cdr items) (+ x iw %gap%)))))))
  (define (width name rh)
    (case name
      ((temperature) (max 60 (round (* 0.06 w))))
      ((workspace) (round (* rh 401/301)))
      ((coderack) (round (* rh 29/75)))
      ((vertical-themes) (round (* rh 81/296)))
      ((slipnet) (round (* rh 652/311)))
      (else #f)))
  (when (pair? r1)
    (place-row! (map (lambda (n) (list n (width n h1))) r1) y1 h1))
  (when (pair? r2)
    ;; the Top and Bottom Themes are one column, stacked
    (let* ((themes (filter (lambda (n) (memq n '(top-themes bottom-themes))) r2))
           (th (if (= (length themes) 2) (quotient (- h2 %gap%) 2) h2))
           (column-w (round (* (/ (- h2 %gap%) 2) 301/71)))
           (items (append (if (on? 'slipnet) (list (list 'slipnet (width 'slipnet h2))) '())
                          (if (pair? themes) (list (list 'themes column-w)) '())
                          (if (on? 'memory) (list (list 'memory #f)) '()))))
      (place-row! items y2 h2)
      (let ((column (hash-ref rects 'themes #f)))
        (when column
          (hash-remove! rects 'themes)
          (for ((n themes) (i (in-naturals)))
            (hash-set! rects n (list (car column) (+ y2 (* i (+ th %gap%)))
                                     (caddr column) th)))))))
  (when (pair? r3)
    (let ((th (if (= (length r3) 2) (quotient (- h3 %gap%) 2) h3)))
      (for ((n r3) (i (in-naturals)))
        (hash-set! rects n (list 0 (+ y3 (* i (+ th %gap%))) w th)))))
  rects)

;; the panel of the panes: places its shown children by pane-rects
(define layout-panel%
  (class g:panel%
    (super-new)
    (define names (make-hasheq))   ; canvas -> pane name
    (define/public (name-pane! canvas name) (hash-set! names canvas name))
    (define/override (container-size info) (values 640 400))
    (define/override (place-children info w h)
      ;; info has every child, shown or not
      (let* ((children (send this get-children))
             (shown (filter (lambda (c) (send c is-shown?)) children))
             (rects (pane-rects (filter values (map (lambda (c) (hash-ref names c #f)) shown))
                                w h)))
        (for/list ((c children))
          (if (send c is-shown?)
              (hash-ref rects (hash-ref names c #f) (list 0 0 0 0))
              (list 0 0 0 0)))))))

;;-----------------------------------------------------------------------------
;; The window

(define one-window-frame #f)
(define one-window-layout #f)

;; closing the one window exits, as closing the control panel did
(define one-window-frame%
  (class g:frame%
    (super-new)
    (define/augment (can-close?) (exit 0))))

;; setup.rktl's setup for one window.  size: the frame's size, or #f for
;; the screen's (less 40 pixels for a panel)
(define setup-one-window
  (lambda ((scale 1) #:size (size #f))
    (printf "Initializing windows...")
    (let-values (((sw sh) (if size
                              (values (car size) (cadr size))
                              (let-values (((dw dh) (g:get-display-size))) (values dw (- dh 40))))))
      (set! one-window-frame
        (new one-window-frame% (label "Metacat") (width sw) (height sh)
             (border 4) (spacing 4)))
      (let ((strip (new g:horizontal-panel% (parent one-window-frame)
                        (stretchable-height #f) (spacing 16) (border 4)
                        (alignment '(left center)))))
        (set! one-window-layout (new layout-panel% (parent one-window-frame)))
        ;; port: graphics windows are panes of the one window
        (set-window-host-maker! (make-pane-host-maker one-window-layout))
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
        (for ((n pane-names)) (send one-window-layout name-pane! (window-pane n) n))
        ;; port: the control panel's widgets in this frame, then in the strip
        (set-control-panel-frame-maker! (lambda () one-window-frame))
        (set! *control-panel* (make-control-panel))
        (set-control-panel-frame-maker! #f)
        (arrange-strip! strip)
        (set! *repl-thread* (make-engine-thread (current-output-port)))
        (enable-resizing)
        (start-gui-refresh!)
        (send one-window-frame show #t)
        (printf "done~%")))))

;; the strip: the problem and the command line, the speed controls (slider
;; and buttons), then the breakpoint and self-watching messages
(define (arrange-strip! strip)
  (let* ((widgets (tell *control-panel* 'get-widgets))
         (W (lambda (name) (cdr (assq name widgets))))
         (problem (new g:vertical-panel% (parent strip) (stretchable-width #f)
                       (alignment '(left center))))
         (messages (new g:vertical-panel% (parent strip) (alignment '(left center)))))
    (send (W 'info-label) reparent problem)
    (send (W 'command-line) reparent problem)
    (send (send (W 'step-button) get-parent) reparent strip)
    (send strip change-children
          (lambda (cs) (append (remq messages cs) (list messages))))
    (send (W 'breakpoint-label) reparent messages)
    ;; reparenting shows a hidden widget
    (let* ((warning (W 'self-watching-warning-label))
           (shown? (send warning is-shown?)))
      (send warning reparent messages)
      (send warning show shown?))))
