#lang racket/base
;; Items 13-14: the views (racket/gui/views.rkt: the windows of
;; workspace-, slipnet-, coderack-, temperature-, theme-, trace-, memory-,
;; commentary- and eeg-graphics.ss, on general-graphics.ss's windows, with
;; the engine's group-, bridge- and rule-graphics.ss) attached to runs.
;;
;; 1. Watching changes nothing: every golden run of tests/problems.txt, run
;;    with every window attached (attach-views!: all graphics on, as in the
;;    original program), gives a trace identical to tests/golden/, and every
;;    window was drawn into.  The run on which the original crashes crashes
;;    at the same point with the views attached.
;; 2. Pictures: the windows at eight points of golden runs
;;    (racket/tests/views-harness.rkt's scenes: mid-run, answers, a snag
;;    event's view, an answer description, a justify run, a click on a
;;    clamp event in the Trace window, clicks comparing two answers in the
;;    Memory window) must equal racket/tests/snapshots/WINDOW-SCENE.png
;;    pixel for pixel.  METACAT_UPDATE_SNAPSHOTS=1 rewrites them; a mismatch
;;    writes /tmp/WINDOW-SCENE-actual.png.  By hand:
;;      racket racket/tests/views-harness.rkt SCENE DIR
;; 3. views.rkt needs racket/draw but never loads racket/gui.
;;
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
(require rackunit
         compiler/cm
         racket/class
         racket/file
         racket/runtime-path
         racket/string
         (only-in racket/draw read-bitmap)
         "golden-pool.rkt")

(define-runtime-path harness "views-harness.rkt")
(define-runtime-path golden-harness "golden-harness.rkt")
(define-runtime-path problems "../../tests/problems.txt")
(define-runtime-path golden-dir "../../tests/golden")
(define-runtime-path snapshot-dir "snapshots")

(module+ test
  (define runs (load-golden-runs harness problems))
  (check-equal? (length runs) 109)
  (define results (run-golden-runs harness 'views-run runs golden-dir))
  (check-equal? (length results) (length runs))
  (define total-items (make-hasheq))
  (for ([result (sort results string<? #:key car)])
    (define name (car result))
    (cond
      [(not (caddr result)) (fail (format "~a: the run raised with the view attached: ~a"
                                          name (cadr result)))]
      [else
       (define d (first-difference (cadr result) (file->string (build-path golden-dir name))))
       (if d
           (fail (format "~a: with the view attached, traces differ at line ~a\n  golden: ~a\n  port:   ~a"
                         name (car d) (short (caddr d)) (short (cadr d))))
           (check-true #t))
       ;; every window was drawn into (the Memory and Trace windows only
       ;; once there are answers or events, the vertical themes once there
       ;; are themes, the bottom themes only in justify runs)
       (for ([window '(workspace slipnet coderack top-themes vertical-themes memory
                       commentary trace temperature EEG)])
         (define items (regexp-match (pregexp (format "\n~a view: ([0-9]+) items\n" window))
                                     (list-ref result 3)))
         (define n (and items (string->number (cadr items))))
         (check-true (and n (> n (case window
                                    [(memory trace vertical-themes) -1]
                                    [(commentary) 1]   ; the first paragraph, two lines or more
                                    [else 10]))
                          #t)
                     (format "~a: the ~a window was drawn into" name window))
         (when n
           (hash-update! total-items window (lambda (t) (+ t n)) 0)))]))
  (for ([(window n) total-items])
    (check-true (> n 1000) (format "the ~a window drew (~a items)" window n))))

;; The original's crash (abc ccbbaa ijk, seed 3: golden-test.rkt) happens at
;; the same point with the view attached.
(module+ test
  (define (crash-trace file run-name partial-name)
    (parameterize ([current-namespace (make-base-namespace)])
      ;; through the compilation manager: the harness may not be compiled yet
      (define-values (run partial)
        (parameterize ([current-load/use-compiled
                        (make-compilation-manager-load/use-compiled-handler)])
          (values (dynamic-require file run-name) (dynamic-require file partial-name))))
      (with-handlers ([exn:fail? (lambda (e) (values (exn-message e) (partial)))])
        (run '(abc ccbbaa ijk) 3 10000 #f)
        (values #f #f))))
  (define-values (plain-error plain-trace)
    (crash-trace golden-harness 'golden-run 'golden-partial-trace))
  (define-values (view-error view-trace)
    (crash-trace harness 'views-run 'views-partial-trace))
  (check-true (and view-error (regexp-match? #rx"^caddr: " view-error) #t)
              (format "with the views attached, the port crashes in caddr: ~a" view-error))
  (check-equal? view-error plain-error)
  (check-true (and view-trace (> (length (string-split view-trace "\n")) 1000) #t))
  (check-equal? view-trace plain-trace "the traces up to the crash are the same"))

;; Pictures of the windows
(module+ test
  ;; a fresh engine per scene, sharing this module's racket/draw (and so its
  ;; class system), so that the bitmaps can be read here
  (define (scene-namespace)
    (define ns (make-base-namespace))
    (namespace-attach-module (current-namespace) 'racket/draw ns)
    ns)
  (define scenes
    (parameterize ([current-namespace (scene-namespace)])
      (dynamic-require harness 'scenes)))
  (define (argb bm)
    (define w (send bm get-width))
    (define h (send bm get-height))
    (define bytes (make-bytes (* 4 w h)))
    (send bm get-argb-pixels 0 0 w h bytes)
    (values w h bytes))
  (define pictures 0)
  (for* ([scene scenes]
         [picture (parameterize ([current-namespace (scene-namespace)])
                    ((dynamic-require harness 'render-scene) (car scene)))])
    (define name (format "~a-~a" (car picture) (car scene)))
    (define bm (cdr picture))
    (define file (build-path snapshot-dir (format "~a.png" name)))
    (define-values (w h got) (argb bm))
    (set! pictures (add1 pictures))
    (when (eq? (car picture) 'workspace)
      (check-equal? (list w h) '(800 600) (format "~a: the window's size" name)))
    ;; not blank: at least two colours
    (check-true (for/or ([i (in-range 4 (bytes-length got) 4)])
                  (not (= (integer-bytes->integer got #f #f i (+ i 4))
                          (integer-bytes->integer got #f #f 0 4))))
                (format "~a: something is drawn" name))
    (cond
      [(getenv "METACAT_UPDATE_SNAPSHOTS")
       (send bm save-file file 'png)
       (printf "wrote ~a\n" file)]
      [else
       (define-values (sw sh want) (argb (read-bitmap file)))
       (define same? (and (= sw w) (= sh h) (equal? got want)))
       (unless same?
         (send bm save-file (format "/tmp/~a-actual.png" name) 'png))
       (check-true same?
                   (format "~a: the rendering differs from ~a (see /tmp/~a-actual.png)"
                           name file name))]))
  (check-equal? pictures 48 "every scene's windows were pictured"))

;; views.rkt: racket/draw, never racket/gui
(module+ test
  (define-runtime-path views "../gui/views.rkt")
  (parameterize ([current-namespace (make-base-namespace)])
    (dynamic-require views #f)
    (define declared? (lambda (m) (module-declared? m #f)))
    (check-true (declared? 'racket/draw))
    (check-false (declared? 'racket/gui/base) "views.rkt does not load racket/gui")))
