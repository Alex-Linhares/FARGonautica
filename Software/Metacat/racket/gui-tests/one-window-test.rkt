#lang racket/base
;; loop0003 item 10: the one-window GUI (racket/gui/one-window.rkt), driven
;; through its own widgets on a virtual display.  Run it only as
;;   env -u WAYLAND_DISPLAY GDK_BACKEND=x11 xvfb-run -a -s "-screen 0 1920x1200x24" \
;;     raco test racket/gui-tests/one-window-test.rkt
;; (tests/run-tests.sh does).
;;
;; - one frame holds the control panel and every graphics window, as panes
;;   laid out by pane-rects, each pane at its own size through the
;;   original's resize protocol;
;; - Run 7 typed into the command line and run with Go, with the window
;;   resized during the run, writes exactly its golden trace (every event
;;   between the golden's start and end lines) and ends at the golden's
;;   codelet count and generator state;
;; - the Windows menu hides and shows a pane, and the others take its place;
;; - a screenshot of the screen (Python's PIL, if present) has every pane's
;;   pictures where the panes are.  It is written to
;;   $METACAT_SCREENSHOT_DIR (default: the temporary directory) as
;;   racket-one-window-run7.png.
;;
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
(require rackunit
         racket/class
         racket/file
         (only-in racket/list drop-right)
         racket/string
         racket/system
         (only-in racket/port open-output-nowhere)
         (only-in racket/draw bitmap-dc%)
         racket/runtime-path
         (prefix-in g: racket/gui/base)
         (only-in racket/draw read-bitmap color%)
         "../gui/one-window.rkt"
         "../gui/gui.rkt"
         "../engine.rkt"
         "../utilities.rkt"
         (only-in "../headless.rkt" trace-gui-runs!)
         (only-in "../compat.rkt" random-seed))

(define-runtime-path golden-dir "../../tests/golden")

(when (getenv "WAYLAND_DISPLAY")
  (error 'one-window-test "WAYLAND_DISPLAY is set: run under env -u WAYLAND_DISPLAY GDK_BACKEND=x11 xvfb-run"))

(define golden-lines (file->lines (build-path golden-dir "abc-abd-xyz_3852097033.jsonl")))
(define (golden-end lines)
  (let ((end (car (reverse lines))))
    (list (string->number (cadr (regexp-match #rx"\"t\":([0-9]+)" end)))
          (string->number (cadr (regexp-match #rx"\"rng\":([0-9]+)" end))))))

(define engine-out (open-output-string))
(parameterize ([current-output-port engine-out])
  (setup-one-window #:size '(1920 1040)))

(define widgets (tell *control-panel* 'get-widgets))
(define (W name) (cdr (assq name widgets)))
(define (click b) (send b command (new g:control-event% [event-type 'button])))
(define (enter! text)
  (send (W 'command-line) set-value text)
  (send (W 'command-line) command (new g:control-event% [event-type 'text-field-enter])))
(define (pump [secs 0.05]) (g:sleep/yield secs))
(define (input-mode?)
  (and (send (W 'go-button) is-enabled?) (not (send (W 'stop-button) is-enabled?))))
(define (wait-idle [secs 120])
  (let loop ([t 0])
    (pump 0.02)
    (cond [(and (not (engine-busy?)) (input-mode?)) #t]
          [(> t (* secs 50)) (error 'wait-idle "the engine did not stop")]
          [else (loop (add1 t))])))
(define (state) (list *codelet-count* (random-seed)))

(define (host name) (tell (window-of name) 'get-toplevel))
(define (window-of name)
  (case name
    ((temperature) *temperature-window*) ((workspace) *workspace-window*)
    ((coderack) *coderack-window*) ((vertical-themes) *vertical-themes-window*)
    ((commentary) *comment-window*) ((slipnet) *slipnet-window*)
    ((top-themes) *top-themes-window*) ((bottom-themes) *bottom-themes-window*)
    ((memory) *memory-window*) ((trace) *trace-window*) ((EEG) *EEG-window*)))
(define (shown-panes) (filter (lambda (n) (send (window-pane n) is-shown?)) pane-names))
;; every shown pane's viewport has its pane's size, and the panel has
;; redrawn at that size (the resize listener has run)
(define (wait-settled [secs 30])
  (let loop ([t 0])
    (pump 0.1)
    (cond [(and (andmap (lambda (n) (send (host n) settled?)) pane-names)
                (andmap (lambda (n)
                          (let ((vp (tell (window-of n) 'get-vp)))
                            (equal? (tell (window-of n) 'get-size)
                                    (list (send vp get-width) (send vp get-height)))))
                        (shown-panes)))
           (pump 0.6)    ; the listener's last redraw
           #t]
          [(> t (* secs 10)) (error 'wait-settled "the panes did not take their sizes")]
          [else (loop (add1 t))])))
(define (rect-of c)
  (let-values (((x y) (send c client->screen 0 0))
               ((w h) (send c get-client-size)))
    (list x y w h)))
(define (inside? a b)   ; rectangle a inside rectangle b
  (and (>= (car a) (car b)) (>= (cadr a) (cadr b))
       (<= (+ (car a) (caddr a)) (+ (car b) (caddr b)))
       (<= (+ (cadr a) (cadddr a)) (+ (cadr b) (cadddr b)))))
(define (overlap? a b)
  (and (< (car a) (+ (car b) (caddr b))) (< (car b) (+ (car a) (caddr a)))
       (< (cadr a) (+ (cadr b) (cadddr b))) (< (cadr b) (+ (cadr a) (cadddr a)))))

(module+ test
  (current-print void)
  (define run7 (golden-end golden-lines))     ; wyz at 2170

  ;; --- pane-rects, by itself
  (define all-but-eeg (remq 'EEG pane-names))
  (for ([size '((1920 940) (2560 1340) (1600 760))])
    (define w (car size))
    (define h (cadr size))
    (for ([shown (list all-but-eeg pane-names (remq 'commentary all-but-eeg)
                       '(workspace trace) '(slipnet memory EEG))])
      (define rects (pane-rects shown w h))
      (check-equal? (sort (hash-keys rects) symbol<?) (sort shown symbol<?))
      (for ([(n r) rects])
        (check-true (inside? r (list 0 0 w h)) (format "~a ~a inside ~a" n r size)))
      (for* ([a shown] [b shown] #:when (symbol<? a b))
        (check-false (overlap? (hash-ref rects a) (hash-ref rects b))
                     (format "~a and ~a overlap at ~a" a b size)))))
  (let ((rects (pane-rects all-but-eeg 1920 940)))
    (check-true (>= (caddr (hash-ref rects 'commentary)) 200))
    (check-true (>= (cadddr (hash-ref rects 'trace)) 50))
    (check-equal? (caddr (hash-ref rects 'trace)) 1920))
  (let ((rects (pane-rects pane-names 1920 940)))
    (check-true (>= (cadddr (hash-ref rects 'trace)) 50) "the EEG doubles the bottom row"))

  ;; --- one window
  (define frame one-window-frame)
  (check-equal? (send frame get-label) "Metacat")
  (check-eq? (W 'frame) frame "the control panel is the one window")
  (check-equal? (filter (lambda (f) (send f is-shown?)) (g:get-top-level-windows)) (list frame)
                "no other window is open")
  (for ([n pane-names])
    (check-true (send (host n) pane?) (format "~a is a pane" n))
    (check-false (send (host n) get-frame))
    (check-eq? (send (window-pane n) get-parent) one-window-layout))
  (check-equal? (shown-panes) all-but-eeg "the EEG starts hidden, as today")
  (check-equal? (send (W 'info-label) get-label) "Please enter a problem:")
  (check-false (send (W 'self-watching-warning-label) is-shown?) "self-watching is on")
  (check-eq? (send (W 'command-line) get-top-level-window) frame)
  (check-eq? (send (W 'go-button) get-top-level-window) frame)
  (wait-settled)
  (define (check-layout)
    (define layout-rect (rect-of one-window-layout))
    (define rects (for/hash ([n (shown-panes)]) (values n (rect-of (window-pane n)))))
    (for ([(n r) rects])
      (check-true (inside? r layout-rect) (format "~a inside the window" n))
      (check-true (and (> (caddr r) 40) (> (cadddr r) 40)) (format "~a has room: ~a" n r))
      ;; the panel took its pane's size (letterboxed or filled)
      (let* ((vp (tell (window-of n) 'get-vp))
             (off (send (host n) get-offset)))
        (check-true (<= (+ (car off) (send vp get-width)) (caddr r)) (format "~a width" n))
        (check-true (<= (send vp get-height) (cadddr r)) (format "~a height" n))
        (check-true (or (<= (- (caddr r) (send vp get-width)) 2)
                        (<= (- (cadddr r) (send vp get-height)) 2)
                        ;; a scrolling pane's scrollbar takes its part
                        (memq n '(memory commentary trace EEG)))
                    (format "~a fills its pane one way: ~a in ~a" n
                            (list (send vp get-width) (send vp get-height)) r))))
    (for* ([a (hash-keys rects)] [b (hash-keys rects)] #:when (symbol<? a b))
      (check-false (overlap? (hash-ref rects a) (hash-ref rects b)) (format "~a ~a" a b)))
    rects)
  (define rects-1080 (check-layout))
  (check-true (> (caddr (hash-ref rects-1080 'workspace)) 600) "the Workspace is large")

  ;; --- Run 7 through the widgets, traced, with the window resized mid-run
  (define trace (open-output-string))
  (trace-gui-runs! trace)
  (send (W 'speed-slider) set-value 100)
  (send (W 'speed-slider) command (new g:control-event% [event-type 'slider]))
  (enter! "abc abd xyz 3852097033")
  (wait-idle)
  (check-equal? (state) '(0 3852097033))
  (define t0 (current-inexact-milliseconds))
  (click (W 'go-button))
  (pump 1)
  (check-true (send (W 'stop-button) is-enabled?) "still running after 1 s")
  (send frame resize 1700 960)
  (pump 1)
  (send frame resize 1920 1040)
  (wait-idle)
  (define run-secs (/ (- (current-inexact-milliseconds) t0) 1000.0))
  (trace-gui-runs! #f)
  (check-equal? (state) run7 "the one window's run is the golden run")
  (define trace-lines (string-split (get-output-string trace) "\n"))
  (define expected (drop-right (cdr golden-lines) 1))   ; less the start and end lines
  (check-equal? (length trace-lines) (length expected))
  (check-equal? trace-lines expected "the trace is the golden's, event for event")
  (printf "one-window-test: Run 7 in ~a s, ~a trace lines\n" run-secs (length trace-lines))
  (check-true (regexp-match? #rx"wyz" (string-join (filter string? (tell *comment-window* 'get-lines)) " ")))
  (wait-settled)
  (check-layout)

  ;; --- a screenshot, and every pane's pictures in it
  (define python (find-executable-path "python3"))
  (define shot-dir (or (getenv "METACAT_SCREENSHOT_DIR") (path->string (find-system-path 'temp-dir))))
  (define shot (build-path shot-dir "racket-one-window-run7.png"))
  (define grabbed?
    (and python
         (parameterize ([current-error-port (open-output-nowhere)])
           (system* python "-c"
                    (string-append
                     "import os,sys\nfrom PIL import ImageGrab\n"
                     "ImageGrab.grab(xdisplay=os.environ['DISPLAY']).save(sys.argv[1])")
                    (path->string shot)))))
  (if (not grabbed?)
      (printf "one-window-test: no screenshot (python3 with PIL needed)\n")
      (let* ((bm (read-bitmap shot))
             (dc (new bitmap-dc% (bitmap bm)))
             (c (new color%)))
        (printf "one-window-test: screenshot ~a\n" shot)
        (for ([n (shown-panes)])
          (define vp (tell (window-of n) 'get-vp))
          (define bg (send vp get-background-color))
          (define r (rect-of (window-pane n)))
          (define off (send (host n) get-offset))
          (define vw (send vp get-width))
          (define vh (send vp get-height))
          ;; a 20 x 20 grid of samples over the viewport's part of the pane
          (define-values (bg-n other-n)
            (for*/fold ([b 0] [o 0]) ([i 20] [j 20])
              (send dc get-pixel (+ (car r) (car off) (quotient (* vw (+ 1 (* 2 i))) 40))
                    (+ (cadr r) (cadr off) (quotient (* vh (+ 1 (* 2 j))) 40)) c)
              (if (and (= (send c red) (send bg red)) (= (send c green) (send bg green))
                       (= (send c blue) (send bg blue)))
                  (values (add1 b) o)
                  (values b (add1 o)))))
          (check-true (> bg-n 20) (format "~a's background on the screen (~a of 400)" n bg-n))
          ;; the Bottom Themes draw only in justify runs
          (unless (eq? n 'bottom-themes)
            (check-true (> other-n 0) (format "~a has drawings on the screen" n))))))

  ;; --- the Windows menu hides a pane; the others take its place
  (define controllers (W 'window-controllers))
  (define ws-controller (car controllers))   ; Workspace
  (define coderack-x (car (rect-of (window-pane 'coderack))))
  (tell ws-controller 'toggle)
  (wait-settled)
  (check-false (send (window-pane 'workspace) is-shown?))
  (check-true (< (car (rect-of (window-pane 'coderack))) coderack-x) "the Coderack moved left")
  (tell ws-controller 'toggle)
  (wait-settled)
  (check-true (send (window-pane 'workspace) is-shown?))
  (check-equal? (car (rect-of (window-pane 'coderack))) coderack-x)
  ;; the EEG shown: the bottom row holds the Trace and the EEG
  (define eeg-controller (list-ref controllers 10))
  (tell eeg-controller 'toggle)
  (wait-settled)
  (check-true (send (window-pane 'EEG) is-shown?))
  (check-true (> (cadddr (rect-of (window-pane 'trace))) 40))
  (check-layout)
  ;; self-watching off hides the three Themes panes and shows the warning
  (define sw (W 'self-watching-mode-menu-item))
  (send sw check #f)
  (send sw command (new g:control-event% [event-type 'menu]))
  (wait-settled)
  (check-true (send (W 'self-watching-warning-label) is-shown?))
  (for ([n '(top-themes bottom-themes vertical-themes)])
    (check-false (send (window-pane n) is-shown?) (format "~a hidden" n)))
  (check-layout)
  (send sw check #t)
  (send sw command (new g:control-event% [event-type 'menu]))
  (wait-settled)
  (check-false (send (W 'self-watching-warning-label) is-shown?))
  (for ([n '(top-themes bottom-themes vertical-themes)])
    (check-true (send (window-pane n) is-shown?) (format "~a shown again" n)))
  (check-layout)
  ;; a click on a pane goes to its window (the Workspace's resumes nothing
  ;; outside a run; it must not raise)
  (send (host 'workspace) press 20 20 'left)
  (pump 0.3)

  ;; done: hide the window and stop the refresh timer, so the process ends
  (for ([w (g:get-top-level-windows)]) (send w show #f))
  (stop-gui-refresh!))
