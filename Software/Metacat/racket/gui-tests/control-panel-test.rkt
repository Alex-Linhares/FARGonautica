#lang racket/base
;; Item 15: the control panel (racket/gui/gui.rkt, the port of gui.ss) driven
;; through its own widgets on a virtual display.  Run it only as
;;   env -u WAYLAND_DISPLAY GDK_BACKEND=x11 xvfb-run -a raco test racket/gui-tests
;; (tests/run-tests.sh does): with WAYLAND_DISPLAY set, GTK ignores Xvfb and
;; opens the windows on the owner's screen.
;;
;; Runs driven through the GUI must be the runs of the goldens: a full run,
;; a run in step mode, a run stopped (Stop, a breakpoint) and resumed (Go, a
;; click on the Workspace), and a Reset all end at the golden's codelet count
;; with the golden's generator state.
;;
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
(require rackunit
         racket/class
         racket/file
         racket/string
         racket/runtime-path
         (prefix-in g: racket/gui/base)
         "../gui/gui.rkt"
         "../engine.rkt"
         "../utilities.rkt"
         (only-in "../gui/fonts.rkt" *scrollbar-width* *scrollbar-height*)
         (only-in "../compat.rkt" random-seed))

(define-runtime-path golden-dir "../../tests/golden")

(when (getenv "WAYLAND_DISPLAY")
  (error 'control-panel-test "WAYLAND_DISPLAY is set: run under env -u WAYLAND_DISPLAY GDK_BACKEND=x11 xvfb-run"))

;; the golden's last line: codelet count and generator state
(define (golden-end name)
  (let* ((lines (file->lines (build-path golden-dir name)))
         (end (car (reverse lines))))
    (list (string->number (cadr (regexp-match #rx"\"t\":([0-9]+)" end)))
          (string->number (cadr (regexp-match #rx"\"rng\":([0-9]+)" end))))))

(define engine-out (open-output-string))
(parameterize ([current-output-port engine-out])
  (setup))
(define (take-output!) (bytes->string/utf-8 (get-output-bytes engine-out #t)))
(void (take-output!))   ; "Initializing windows...done"

(define widgets (tell *control-panel* 'get-widgets))
(define (W name) (cdr (assq name widgets)))
(define (click b) (send b command (new g:control-event% [event-type 'button])))
(define (enter! text)
  (send (W 'command-line) set-value text)
  (send (W 'command-line) command (new g:control-event% [event-type 'text-field-enter])))
(define (pump [secs 0.05]) (g:sleep/yield secs))
(define (input-mode?)
  (and (send (W 'go-button) is-enabled?) (not (send (W 'stop-button) is-enabled?))))
;; wait until the engine thread is idle and the panel is in input mode
(define (wait-idle [secs 120])
  (let loop ([t 0])
    (pump 0.02)
    (cond [(and (not (engine-busy?)) (input-mode?)) #t]
          [(> t (* secs 50)) (error 'wait-idle "the engine did not stop")]
          [else (loop (add1 t))])))
(define (state) (list *codelet-count* (random-seed)))
(define (clear-memory!)
  (tell *control-panel* 'clear-memory)
  (let ((dialog (tell *control-panel* 'get-clearmem-dialog)))
    (check-false (input-mode?) "the panel is disabled while the dialog is up")
    ;; the dialog's Yes button
    (let find ((ws (send dialog get-children)))
      (for-each (lambda (w)
                  (cond [(and (is-a? w g:button%) (equal? (send w get-label) "Yes")) (click w)]
                        [(is-a? w g:area-container<%>) (find (send w get-children))]))
                ws)))
  (pump)
  (check-false (tell *control-panel* 'get-clearmem-dialog))
  (check-true (input-mode?)))
;; an input dialog (breakpoint, step interval): type into its field
(define (answer-input-dialog! action value)
  (action #f)
  (pump)
  (let* ((frames (g:get-top-level-windows))
         (dialog (findf (lambda (f) (equal? (send f get-label) "Input")) frames))
         (field (let find ((ws (send dialog get-children)))
                  (for/or ([w ws])
                    (cond [(is-a? w g:text-field%) w]
                          [(is-a? w g:area-container<%>) (find (send w get-children))]
                          [else #f])))))
    (send field set-value value)
    (send field command (new g:control-event% [event-type 'text-field-enter]))
    (pump)))
(define (findf p l) (cond [(null? l) #f] [(p (car l)) (car l)] [else (findf p (cdr l))]))
(define (workspace-host) (tell *workspace-window* 'get-toplevel))

(module+ test
  (current-print void)   ; the driving expressions' values are not output
  (define ijk-1 (golden-end "abc-abd-ijk_1.jsonl"))          ; ijd at 395
  (define xyz-run7 (golden-end "abc-abd-xyz_3852097033.jsonl")) ; wyz at 2170

  ;; --- the windows, as setup made them
  (check-equal? (send (W 'frame) get-label) "Metacat Control Panel")
  (check-equal? (send (W 'info-label) get-label) "Please enter a problem:")
  (check-false (send (W 'go-button) is-enabled?) "the buttons start disabled, as in gui.ss")
  (check-false (send (W 'stop-button) is-enabled?))
  (define controllers (W 'window-controllers))
  (check-equal? (length controllers) 12)
  (check-equal? (map (lambda (c) (tell c 'visible?)) controllers)
                '(#t #t #t #t #t #t #t #t #t #t #f #f) "EEG and Logo start hidden")
  (for ([w (window-frames)])
    (define host (tell (cdr w) 'get-toplevel))
    (check-true (is-a? host screen-host%) (format "~a is on the screen" (car w)))
    (check-equal? (send host visible?*) (not (eq? (car w) 'EEG)) (format "~a shown" (car w))))
  (check-equal? (send (send (workspace-host) get-frame) get-label) "Workspace")
  (check-true (and *scrollbar-width* *scrollbar-height* #t) "create-mcat-logo set the scrollbar sizes")
  ;; the windows do not overlap the control panel
  (define cp (W 'frame))
  (for ([w (window-frames)])
    (define f (send (tell (cdr w) 'get-toplevel) get-frame))
    (check-true (or (>= (send f get-x) (+ (send cp get-x) (send cp get-width)))
                    (>= (send f get-y) (+ (send cp get-y) (send cp get-height))))
                (format "~a clear of the control panel" (car w))))

  ;; --- invalid input
  (enter! "abc 12x")
  (check-equal? (send (W 'info-label) get-label) "Invalid input!")
  (pump 1)
  (check-equal? (send (W 'info-label) get-label) "Please enter a problem:")

  ;; --- the speed slider at Fast
  (send (W 'speed-slider) set-value 100)
  (send (W 'speed-slider) command (new g:control-event% [event-type 'slider]))
  (check-equal? (list %num-of-flashes% %flash-pause% %snag-pause% %text-scroll-pause%)
                '(1 1 1 1))

  ;; --- a full run: Enter initializes the problem, Go runs it
  (enter! "abc abd ijk 1")
  (wait-idle)
  (check-equal? (send (W 'info-label) get-label) " abc -> abd; ijk -> ?       seed:  1 ")
  (check-equal? (state) '(0 1) "initialized, stopped before the first codelet")
  (check-true (send (W 'go-button) is-enabled?))
  (check-equal? (tell *control-panel* 'get-current-problem) '(abc abd ijk #f 1))
  (click (W 'go-button))
  (wait-idle)
  (check-equal? (state) ijk-1 "the GUI's run is the golden run")
  (check-equal? (take-output!) "Type (go) or click on the Workspace to continue...\nstopped\n")
  (define lines (tell *comment-window* 'get-lines))
  (check-true (for/or ([l lines]) (and (string? l) (regexp-match? #rx"ijd" l))) "the commentary")
  (check-true (> (length (send (tell *workspace-window* 'get-vp) get-items)) 50))

  ;; --- Save commentary to file
  (define tmp (make-temporary-file "commentary-~a.txt"))
  (set-file-dialog! (lambda (title mode dir) (path->string tmp)))
  (save-commentary-action #f)
  (check-equal? (file->string tmp)
                (apply string-append
                       (for/list ([l lines])
                         (if (string? l) (string-append l "\n") (make-string l #\newline)))))
  (check-true (regexp-match? #rx"ijd" (file->string tmp)))
  (delete-file tmp)

  ;; --- step mode, with the step interval set from the Options menu
  (clear-memory!)
  (enter! "abc abd ijk 1")
  (wait-idle)
  (click (W 'step-button))       ; empty command line: step mode on, go
  (wait-idle)
  (check-true *step-mode?*)
  (check-equal? (car (state)) 1 "one codelet per step at first")
  (answer-input-dialog! set-step-interval-action "50")
  (check-equal? %step-cycles% 50)
  (click (W 'step-button))
  (wait-idle)
  (check-equal? (car (state)) 50)
  (click (W 'step-button))
  (wait-idle)
  (check-equal? (car (state)) 100)
  (click (W 'go-button))         ; step mode off, run on
  (wait-idle)
  (check-false *step-mode?*)
  (check-equal? (state) ijk-1 "a stepped run is the golden run")
  (take-output!)

  ;; --- a breakpoint, then a click on the Workspace resumes
  (clear-memory!)
  (enter! "abc abd ijk 1")
  (wait-idle)
  (answer-input-dialog! set-breakpoint-action "300")
  (check-equal? *break-time* 300)
  (check-equal? (send (W 'breakpoint-label) get-label) "Breakpoint set for time step 300")
  (click (W 'go-button))
  (wait-idle)
  (check-equal? (car (state)) 300)
  (check-equal? (take-output!) "Codelets run: 300\nstopped\n")
  (clear-breakpoint-action #f)
  (check-false *break-time*)
  (check-equal? (send (W 'breakpoint-label) get-label) "")
  (send (workspace-host) press 20 20 'left)   ; workspace-window-press-handler: (go)
  (pump)
  (wait-idle)
  (check-equal? (state) ijk-1 "resumed by a click on the Workspace")
  (take-output!)

  ;; --- Stop in the middle of a run, then Go; then Reset and run again
  (clear-memory!)
  (enter! "abc abd xyz 3852097033")
  (wait-idle)
  (click (W 'go-button))
  (pump 0.5)
  (check-true (send (W 'stop-button) is-enabled?) "still running after 0.5 s")
  (check-false (input-mode?) "run mode")
  (check-equal? (send (W 'command-line) get-value) "running...")
  (check-false (send (W 'command-line) is-enabled?))
  (click (W 'stop-button))
  (wait-idle)
  (define stopped-at (car (state)))
  (check-true (< 0 stopped-at (car xyz-run7)) (format "stopped at ~a" stopped-at))
  (check-equal? (take-output!) (format "Codelets run: ~a\nstopped\n" stopped-at))
  (click (W 'go-button))
  (wait-idle)
  (check-equal? (state) xyz-run7 "stopped and resumed: the golden run")
  (take-output!)
  (clear-memory!)
  (click (W 'reset-button))      ; empty command line: the same problem again
  (wait-idle)
  (check-equal? (state) '(0 3852097033))
  (click (W 'go-button))
  (wait-idle)
  (check-equal? (state) xyz-run7 "after Reset: the golden run again")
  (take-output!)

  ;; --- the Windows menu: hide and show a window
  (define ws-controller (car controllers))
  (tell ws-controller 'toggle)
  (pump)
  (check-false (send (workspace-host) visible?*))
  (check-equal? (send (tell ws-controller 'get-menu-item) get-label) "Show Workspace")
  (tell ws-controller 'toggle)
  (pump)
  (check-true (send (workspace-host) visible?*))
  (check-equal? (send (tell ws-controller 'get-menu-item) get-label) "Hide Workspace")

  ;; --- self-watching off and on (the theme windows follow)
  (define sw (W 'self-watching-mode-menu-item))
  (send sw check #f)
  (send sw command (new g:control-event% [event-type 'menu]))
  (pump)
  (check-false %self-watching-enabled%)
  (check-true (send (W 'self-watching-warning-label) is-shown?))
  (check-false (send (tell *top-themes-window* 'get-toplevel) visible?*))
  (send sw check #t)
  (send sw command (new g:control-event% [event-type 'menu]))
  (pump)
  (check-true %self-watching-enabled%)
  (check-false (send (W 'self-watching-warning-label) is-shown?))
  (check-true (send (tell *top-themes-window* 'get-toplevel) visible?*))

  ;; --- window resizing: the Workspace follows its frame
  (define ws-frame (send (workspace-host) get-frame))
  (define before (tell *workspace-window* 'get-size))
  (send ws-frame resize 1000 760)
  (pump 1.5)
  (define after (tell *workspace-window* 'get-size))
  (check-not-equal? after before "the window took the new size")
  (check-equal? after (let ((vp (tell *workspace-window* 'get-vp)))
                        (list (send vp get-width) (send vp get-height))))
  (check-true (> (car after) (car before)))

  ;; --- a demo from the Demos menu (Run 7) initializes its problem
  (define demos (W 'demos-menu))
  (define run7-item (findf (lambda (i) (and (is-a? i g:labelled-menu-item<%>)
                                            (regexp-match? #rx"^Run 7" (send i get-label))))
                           (send demos get-items)))
  (send run7-item command (new g:control-event% [event-type 'menu]))
  (wait-idle)
  (check-equal? (send (W 'info-label) get-label) " abc -> abd; xyz -> ?       seed:  3852097033 ")
  (check-true (send run7-item is-checked?) "the demo chosen is highlighted")
  (check-equal? (state) '(0 3852097033))

  ;; --- item 16: every Demos menu item, submenus included, initializes its
  ;; demos.ss problem with its seed, in gui.ss's order, and is the only one
  ;; highlighted
  (define (demo-items menu)
    (apply append
           (for/list ([i (send menu get-items)])
             (cond [(is-a? i g:checkable-menu-item%) (list i)]
                   [(is-a? i g:menu%) (demo-items i)]
                   [else '()]))))
  (define all-demo-items (demo-items demos))
  (define demo-problems
    (list run1 run2 run3 run4 run5 run6 run7 run8
          abc-xyd abc-wyz abc-dyz rst-xyu rst-wyz rst-uyz abc-mrrkkk abc-mrrjjjj
          xqc-mrrkkk xqc-mrrjjjj eqe-baaab eqe-aaabaaa eqe-qeeeq eqe-aaabccc
          fig5.4-top fig5.4-bottom fig5.5-top fig5.5-bottom
          fig5.7 fig5.8 fig5.10 fig5.11
          misc1 misc2 misc3 misc4 misc5))
  (check-equal? (length all-demo-items) (length demo-problems))
  (for ([item all-demo-items] [problem demo-problems])
    (send item command (new g:control-event% [event-type 'menu]))
    (wait-idle)
    (define label (send (W 'info-label) get-label))
    (define seed (car (reverse problem)))
    (check-equal? (state) (list 0 seed) (send item get-label))
    (check-true (and (regexp-match? (regexp-quote (format "~a -> ~a" (car problem) (cadr problem)))
                                    label)
                     (regexp-match? (format "seed: +~a " seed) label))
                (format "~a: ~s" (send item get-label) label))
    (check-equal? (filter (lambda (i) (send i is-checked?)) all-demo-items) (list item)))

  ;; done: hide every window and stop the refresh timer, so that the
  ;; eventspace, and the process, can end
  (for ([w (g:get-top-level-windows)]) (send w show #f))
  (stop-gui-refresh!))
