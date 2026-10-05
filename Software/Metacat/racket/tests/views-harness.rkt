#lang racket/base
;; Items 13-14: runs of the engine with the views attached
;; (racket/gui/views.rkt), for racket/tests/views-test.rkt.  Use one fresh
;; engine (namespace) per call, as for golden-harness.rkt.
;;
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;
;;   (views-run strings seed cap keep?) -> (values trace stdout): a golden
;;     run with every window attached; stdout ends with a line
;;     "NAME view: N items" per window (its display list).
;;   (views-partial-trace) -> the trace up to the error, after views-run raised.
;;   (render-scene name) -> the scene's windows at one point of a golden run
;;     (scenes below), as an association list (window-name . bitmap).
;;   racket racket/tests/views-harness.rkt SCENE DIR renders a scene by hand.
(require racket/class
         racket/port
         "../compat.rkt"
         "../utilities.rkt"
         "../engine.rkt"
         "../headless.rkt"
         "../gui/views.rkt")

(provide views-run views-partial-trace render-scene scenes golden-runs)

;; A golden run with every window attached (attach-views!: the Workspace,
;; Slipnet, Coderack, Themespace, Memory, Commentary, Trace, Temperature and
;; EEG windows, graphics on).  stdout ends with one line per window, "NAME
;; view: N items" (the window's display list).
(define (views-run strings seed cap keep?)
  (define trace (open-output-string))
  (define stdout (open-output-string))
  (define windows #f)
  (parameterize ([current-output-port stdout])
    (run-problem strings seed cap keep? trace
                 #:views (lambda () (set! windows (attach-views!)))))
  (values (get-output-string trace)
          (apply string-append
                 (get-output-string stdout)
                 (for/list ([w windows])
                   (format "~a view: ~a items\n" (car w)
                           (length (send (tell (cdr w) 'get-vp) get-items)))))))

(define (views-partial-trace) (partial-trace))

;; name, problem, seed, codelet cap, keep-going?, what to do at the end of
;; the run, and the windows to picture:
;;   window: nothing (the windows as the run left them: at the cap, or at
;;     the first answer, drawn by draw-current-answer)
;;   snag-event: the last snag event's Workspace view, as the Temporal
;;     Trace shows it ("Event N: Snag"; the Workspace part of its display)
;;   answer-description: the first answer's description in the Episodic
;;     Memory (memory.ss's display-workspace)
;;   (click-event TYPE): a click on the last TYPE event in the Trace window
;;     (trace-graphics.ss's press handler: the event's display in every
;;     window)
;;   (click-answers I ...): clicks on the I-th icons (oldest first) of the Memory
;;     window in turn (memory-graphics.ss's press handler: the answer's
;;     description, and with two answers highlighted, their comparison)
(define all-windows
  '(workspace slipnet coderack top-themes bottom-themes vertical-themes memory
    commentary trace temperature EEG))
(define (all-windows-but . names)
  (filter (lambda (w) (not (memq w names))) all-windows))

;; windows left blank in a scene are not pictured: the bottom themes outside
;; justify runs and answer displays, the Memory before any answer, the top
;; themes of a clamp that clamped only vertical themes
(define scenes
  `((mrrjjj-513 (abc abd mrrjjj) 1 513 #f window ,(all-windows-but 'bottom-themes 'memory))
    (mrrjjj-answer (abc abd mrrjjj) 1 10000 #f window (workspace))
    (xyz-snag-event (abc abd xyz) 3852097033 800 #f snag-event (workspace))
    (xyz-answer (abc abd xyz) 3852097033 10000 #f window ,(all-windows-but 'bottom-themes))
    (xyz-answer-description (abc abd xyz) 3852097033 10000 #f answer-description (workspace))
    (xyd-justify (abc abd xyz xyd) 1760747975 10000 #f window ,all-windows)
    (xyz-clamp-click (abc abd xyz) 3852097033 10000 #f (click-event clamp)
                     (workspace slipnet coderack vertical-themes trace temperature))
    (glz-compare (abc abd glz) 1108779034 1800 #t (click-answers 1 2)
                 (workspace slipnet coderack top-themes bottom-themes vertical-themes
                  memory commentary temperature))))

;; a point inside the bounding box of thing, found by asking (find x y)
;; over a grid of the window's coordinates
(define (click-point window find thing)
  (define xmax (tell window 'get-x-max))
  (define ymax (tell window 'get-y-max))
  (or (for*/first ([i (in-range 0 2000)]
                   [j (in-range 0 200)]
                   #:when (eq? (find (* i (/ xmax 2000)) (* j (/ ymax 200))) thing))
        (list (* i (/ xmax 2000)) (* j (/ ymax 200))))
      (error 'click-point "~a is not in the window" thing)))

;; the windows of a scene, as an association list (window-name . bitmap)
(define (render-scene name)
  (define scene (or (assq name scenes) (error 'render-scene "no scene ~a" name)))
  (define windows #f)
  (parameterize ([current-output-port (open-output-nowhere)])
    (run-problem (list-ref scene 1) (list-ref scene 2) (list-ref scene 3) (list-ref scene 4)
                 #:views (lambda () (set! windows (attach-views!))))
    (define window (cdr (assq 'workspace windows)))
    (define action (list-ref scene 5))
    (cond
      [(eq? action 'window) (void)]
      [(eq? action 'snag-event)
       ;; the event's display (trace.ss) clears the window, then draws its
       ;; Workspace view; the rest of display is for the other panels
       (tell window 'clear)
       (tell (tell *trace* 'get-last-event 'snag) 'display-workspace)]
      [(eq? action 'answer-description)
       (tell (car (tell *memory* 'get-answers)) 'display-workspace)]
      [(eq? (car action) 'click-event)
       (define event (tell *trace* 'get-last-event (cadr action)))
       (define xy (click-point *trace-window*
                               (lambda (x y) (tell *trace* 'get-mouse-selected-event x y))
                               event))
       (trace-window-press-handler *trace-window* (car xy) (cadr xy))]
      [(eq? (car action) 'click-answers)
       (define answers (reverse (tell *memory* 'get-all-descriptions)))
       (for ([i (cdr action)])
         (define xy (click-point *memory-window*
                                 (lambda (x y) (tell *memory* 'get-mouse-selected-answer x y))
                                 (list-ref answers i)))
         (memory-window-press-handler *memory-window* (car xy) (cadr xy)))]))
  (for/list ([w (list-ref scene 6)])
    (cons w (window->bitmap (cdr (assq w windows))))))

;; racket racket/tests/views-harness.rkt SCENE DIR writes DIR/WINDOW-SCENE.png
(module+ main
  (define args (current-command-line-arguments))
  (define scene (string->symbol (vector-ref args 0)))
  (for ([w (render-scene scene)])
    (send (cdr w) save-file (format "~a/~a-~a.png" (vector-ref args 1) (car w) scene) 'png)))
