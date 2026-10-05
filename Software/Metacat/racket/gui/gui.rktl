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
;; Ported to Racket, 2026: gui.ss, included by racket/gui/gui.rkt.  The
;; command-line parser, the button, menu and speed actions, the control
;; panel object (its messages and their effects) and the window controllers
;; are the original's.  SWL's widgets (<toplevel>, <label>, <entry>, <scale>,
;; <button>, <menu> ...) become racket/gui's, so widget creation and the
;; sends to widgets are rewritten; such changes are marked "port:".  Widget
;; colours and per-item menu fonts, which racket/gui does not offer, are left
;; out (docs/divergences.md).  See docs/porting-notes.md, item 15.

(define %gui-header-font% #f)
(define %gui-command-line-font% #f)
(define %gui-run-mode-font% #f)
(define %gui-speed-controls-font% #f)
(define %gui-speed-controls-italic-font% #f)
(define %gui-menubar-font% #f)
(define %gui-menu-item-font% #f)
(define %gui-warning-font% #f)
(define %gui-instructions-font% #f)
(define %gui-input-dialog-font% #f)
(define %gui-help-window-font% #f)

;; port: menu items cannot be coloured in racket/gui; a highlighted item (the
;; last demo run, the current commentary font) is a checked item instead
(define set-menu-item-color
  (lambda (item)
    (if (is-a? item g:checkable-menu-item%)
      (send item check #t))))

(define select-control-panel-fonts
  (lambda ()
    (let* ((screen-height (swl:screen-height))
	   (big
	     (cond
	       ((> screen-height 1024) 14)
	       (else 12)))
	   (medium
	     (cond
	       ((> screen-height 1024) 12)
	       (else 10)))
	   (small
	     (cond
	       ((> screen-height 1024) 10)
	       ((eq? *platform* 'macintosh) 10)
	       (else 8))))
      (set! %gui-header-font% (swl-font sans-serif big 'bold))
      (set! %gui-command-line-font% (swl-font sans-serif big 'bold))
      (set! %gui-run-mode-font% (swl-font sans-serif big 'bold 'italic))
      (set! %gui-speed-controls-font% (swl-font sans-serif small 'bold))
      (set! %gui-speed-controls-italic-font% (swl-font sans-serif small 'italic))
      (set! %gui-menubar-font% (swl-font sans-serif medium))
      (set! %gui-menu-item-font% (swl-font sans-serif medium 'bold))
      (set! %gui-warning-font% (swl-font sans-serif big 'bold))
      (set! %gui-instructions-font% (swl-font sans-serif big))
      (set! %gui-input-dialog-font% (swl-font sans-serif big 'bold))
      (set! %gui-help-window-font% (swl-font 'courier big)))))

(define %gui-slider-length% 80)
(define %gui-slider-thickness% 12)

(define %initial-speed% 50)

;;--------------------------------------------------------------------------------
;; port: pack-hspace and pack-vspace (Tk packing spacers) become panel
;; margins and spacing in make-control-panel and the dialogs

;;--------------------------------------------------------------------------------
;; command line parser for control panel

(define tokenize-string
  (lambda (input)
    (let ((chars (map char-downcase (string->list input))))
      (define consume-noise
	(lambda (buffer tokens chars)
	  (cond
	    ((null? chars) (reverse tokens))
	    ((char-noise? (1st chars))
	     (consume-noise buffer tokens (rest chars)))
	    ((char-alphabetic? (1st chars))
	     (consume-letters (cons (1st chars) buffer) tokens (rest chars)))
	    ((char-numeric? (1st chars))
	     (consume-digits (cons (1st chars) buffer) tokens (rest chars)))
	    (else 'error))))
      (define consume-letters
	(lambda (buffer tokens chars)
	  (cond
	    ((null? chars)
	     (let ((new-token (string->symbol (list->string (reverse buffer)))))
	       (reverse (cons new-token tokens))))
	    ((char-noise? (1st chars))
	     (let ((new-token (string->symbol (list->string (reverse buffer)))))
	       (consume-noise '() (cons new-token tokens) (rest chars))))
	    ((char-alphabetic? (1st chars))
	     (consume-letters (cons (1st chars) buffer) tokens (rest chars)))
	    (else 'error))))
      (define consume-digits
	(lambda (buffer tokens chars)
	  (cond
	    ((null? chars)
	     (let ((new-token (string->number (list->string (reverse buffer)))))
	       (reverse (cons new-token tokens))))
	    ((char-noise? (1st chars))
	     (let ((new-token (string->number (list->string (reverse buffer)))))
	       (consume-noise '() (cons new-token tokens) (rest chars))))
	    ((char-numeric? (1st chars))
	     (consume-digits (cons (1st chars) buffer) tokens (rest chars)))
	    (else 'error))))
      (consume-noise '() '() chars))))

(define char-noise?
  (lambda (char)
    (and (not (char-alphabetic? char))
	 (not (char-numeric? char)))))

(define step-button-action
  (lambda (ignore)
    (let ((input (tell *control-panel* 'get-command-line-string)))
      (if (= 0 (string-length input))
	(begin
	  (step-mode-on)
	  (tell *control-panel* 'resume-current-problem))
	(let ((tokens (tokenize-string input)))
	  (if (valid-token-list? tokens)
	    (tell *control-panel* 'init-new-problem tokens #t)
	    (tell *control-panel* 'display-error "Invalid input!")))))))

(define go-button-action
  (lambda (ignore)
    (let ((input (tell *control-panel* 'get-command-line-string)))
      (if (= 0 (string-length input))
	(begin
	  (step-mode-off)
	  (tell *control-panel* 'resume-current-problem))
	(let ((tokens (tokenize-string input)))
	  (if (valid-token-list? tokens)
	    (tell *control-panel* 'init-new-problem tokens #f)
	    (tell *control-panel* 'display-error "Invalid input!")))))))

(define stop-button-action
  (lambda (ignore)
    (set! *interrupt?* #t)))

(define reset-button-action
  (lambda (ignore)
    (let ((input (tell *control-panel* 'get-command-line-string)))
      (if (= 0 (string-length input))
	(tell *control-panel* 'reset-current-problem)
	(let ((tokens (tokenize-string input)))
	  (if (valid-token-list? tokens)
	    (tell *control-panel* 'init-new-problem tokens #f)
	    (tell *control-panel* 'display-error "Invalid input!")))))))

;;------------------------------------------------------------------
;; help viewer

;; port: the text widget is a racket/gui text% editor
(define read-file
  (lambda (filename text-widget)
    (send text-widget insert
      (call-with-input-file filename (lambda (port) (read-string-all port))))))

(define read-string-all
  (lambda (port)
    (let loop ((chunks '()))
      (let ((s (read-string 2048 port)))
	(if (eof-object? s)
	  (apply string-append (reverse chunks))
	  (loop (cons s chunks)))))))

(define help-action
  (let ((help-window #f))
    (lambda (item)
      (if (exists? help-window)
	(begin
	  (send help-window show #t)
	  (send help-window focus))
	(begin
	  ;; port: <toplevel> + <scrollframe> + <text>
	  (set! help-window
	    (new (class g:frame%
		   (super-new)
		   (define/augment (on-close) (set! help-window #f)))
	      (label "Help") (width 640) (height 560)))
	  (let* ((txt (new g:text%))
		 (sf (new g:editor-canvas% (parent help-window) (editor txt)
			  (style '(no-hscroll auto-vscroll)))))
	    (send txt set-max-undo-history 0)
	    (send sf set-canvas-background %gui-help-window-color%)
	    (let ((delta (new g:style-delta%)))
	      (send delta set-delta-face (send (widget-font %gui-help-window-font%) get-face))
	      (send delta set-size-mult 0)
	      (send delta set-size-add (send (widget-font %gui-help-window-font%) get-size))
	      (send (send txt get-style-list) find-or-create-style
		(send (send txt get-style-list) basic-style) delta)
	      (send txt change-style delta))
	    (send txt auto-wrap #t)
	    (send sf horizontal-inset 10)
	    (read-file help-file txt)
	    (send txt set-position 0)
	    (send txt lock #t)
	    (send help-window show #t)))))))

;;------------------------------------------------------------------------------------
;; pop-up dialogs

;; fg-color specifies the color of the dialog text.  If bg-color is #f
;; the dialog text appears on a white background surrounded by a grey
;; border, otherwise there is no border and the entire dialog
;; background is bg-color.

;; port: a dialog is a racket/gui frame (not modal: the theme edit dialog
;; stays up while theme windows are clicked).  Its destroy method runs the
;; destroy-request handler first, as SWL's did, and so does closing it from
;; the window manager.  Panel colours are left out.
(define swl-dialog%
  (class g:frame%
    (init-field destroy-action)
    (super-new)
    (define destroyed? #f)
    (define/public (destroy)
      (if* (and (not destroyed?) (destroy-action this))
	(set! destroyed? #t)
	(send this show #f)))
    (define/public (raise) (send this show #t))
    (define/public (set-focus) (send this focus))
    (define/augment (can-close?)
      (send this destroy)
      #f)))

(define (place-dialog! dialog geometry)
  (let ((m (regexp-match #rx"^[+]([-0-9]+)[+]([-0-9]+)$" geometry)))
    (if* m
      (send dialog move (string->number (cadr m)) (string->number (caddr m))))))

(define confirm-dialog
  (lambda (x y font fg-color bg-color justify message yes-label no-label
	    yes-action no-action destroy-action)
    (let* ((dialog
	     (new swl-dialog% (label "Confirm") (destroy-action destroy-action)
	       (style '(no-resize-border))
	       (border 15) (spacing 20)))
	   (message-label
	     (new g:message% (parent dialog) (label message)
	       (font (widget-font font)) (color fg-color)))
	   (button-frame
	     (new g:horizontal-panel% (parent dialog) (spacing 20)
	       (alignment '(center center)) (stretchable-height #f)))
	   (yes-button
	     (new g:button% (parent button-frame) (label yes-label)
	       (callback (lambda (b e) (yes-action b)))))
	   (no-button
	     (new g:button% (parent button-frame) (label no-label)
	       (callback (lambda (b e) (no-action b))))))
      (place-dialog! dialog (tell *control-panel* 'get-relative-position x y))
      (send dialog show #t)
      dialog)))

;; port: returns the input field, whose get-parent is the dialog (SWL's
;; <entry> in its <toplevel>)
(define input-field%
  (class g:text-field%
    (init-field dialog)
    (super-new)
    (define/override (get-parent) dialog)
    (define/public (set-focus) (send this focus))))

(define input-dialog
  (lambda (x y default message input-action destroy-action)
    (let* ((dialog
	     (new swl-dialog% (label "Input") (destroy-action destroy-action)
	       (style '(no-resize-border)) (border 15) (spacing 20)
	       (min-width 200)))
	   (message-label
	     (new g:message% (parent dialog) (label message)
	       (font (widget-font %gui-input-dialog-font%)) (color =black=)
	       (auto-resize #t)))
	   (input-field
	     (new input-field% (parent dialog) (dialog dialog) (label #f)
	       (min-width 120) (stretchable-width #f)
	       (font (widget-font %gui-command-line-font%))
	       (callback
		 (lambda (entry event)
		   (if* (eq? (send event get-event-type) 'text-field-enter)
		     (let ((input (send entry get-value)))
		       (if (string=? input "")
			 (send dialog destroy)
			 (let ((value (string->number input)))
			   (if (or (not value) (< value 1))
			     (begin
			       (send message-label set-color =red=)
			       (send message-label set-label "Invalid input!")
			       ;; port: (pause 700) in the event thread becomes a
			       ;; timer, so the window keeps repainting
			       (new g:timer% (interval 700) (just-once? #t)
				 (notify-callback
				   (lambda ()
				     (send message-label set-color =black=)
				     (send message-label set-label message)))))
			     (begin
			       (input-action value)
			       (send dialog destroy))))))))))))
      (place-dialog! dialog (tell *control-panel* 'get-relative-position x y))
      (if* (exists? default)
	(send input-field set-value default)
	(send (send input-field get-editor) set-position 0 (string-length default)))
      (send dialog show #t)
      (send input-field focus)
      input-field)))

(define set-breakpoint-action
  (let ((breakpoint-input-field #f))
    (lambda (item)
      (if (exists? breakpoint-input-field)
	(begin
	  (send (send breakpoint-input-field get-parent) raise)
	  (send breakpoint-input-field set-focus))
	(set! breakpoint-input-field
	  (input-dialog 20 80
	    (if (exists? *break-time*) (number->string *break-time*) "")
	    "Enter new breakpoint:"
	    (lambda (timestep)
	      (set! *break-time* timestep)
	      (tell *control-panel* 'display-breakpoint-message))
	    (lambda (toplevel)
	      (set! breakpoint-input-field #f)
	      #t)))))))

(define clear-breakpoint-action
  (lambda (item)
    (set! *break-time* #f)
    (tell *control-panel* 'clear-breakpoint-message)))

(define set-step-interval-action
  (let ((step-interval-input-field #f))
    (lambda (item)
      (if (exists? step-interval-input-field)
	(begin
	  (send (send step-interval-input-field get-parent) raise)
	  (send step-interval-input-field set-focus))
	(set! step-interval-input-field
	  (input-dialog 80 80 (number->string %step-cycles%)
	    "Enter new step interval:"
	    (lambda (interval)
	      (set! %step-cycles% interval))
	    (lambda (toplevel)
	      (set! step-interval-input-field #f)
	      #t)))))))

(define save-commentary-action
  (lambda (item)
    (let ((filename (swl:file-dialog "Save Commentary to File" 'save
		      *file-dialog-directory*)))   ;; port: (default-dir: ...)
      (if* (exists? filename)
	(if* (file-exists? filename)
	  (delete-file filename))
	(let ((op (open-output-file filename)))
	  (for* each line in (tell *comment-window* 'get-lines) do
	    (if (string? line)
	      (fprintf op "~a~%" line)
	      (repeat* line times (fprintf op "~%"))))
	  (close-output-port op))))))

;;--------------------------------------------------------------------------------

;; port: a <frame> with a <scale> over "Slow"/"Fast" and the title.  Returns
;; the frame; the scale is (send frame get-scale).
(define slider-frame%
  (class g:vertical-panel%
    (init-field (scale #f))
    (super-new)
    (define/public (get-scale) scale)
    (define/public (set-scale! s) (set! scale s))))

(define create-slider
  (lambda (parent text min-text max-text len init-val color slide-action)
    (let* ((slider (new slider-frame% (parent parent) (stretchable-width #f)
		     (stretchable-height #f) (alignment '(center top))))
	   (scale
	     (new g:slider% (parent slider) (label #f)
	       (min-value 0) (max-value 100) (init-value init-val)
	       (min-width len) (style '(horizontal plain))
	       (callback (lambda (s e) (slide-action s (send s get-value))))))
	   (labels (new g:horizontal-panel% (parent slider) (stretchable-height #f)))
	   (min-label
	     (new g:message% (parent labels) (label min-text)
	       (font (widget-font %gui-speed-controls-italic-font%))))
	   (main-label
	     (new g:message% (parent labels) (label text)
	       (font (widget-font %gui-speed-controls-font%))))
	   (max-label
	     (new g:message% (parent labels) (label max-text)
	       (font (widget-font %gui-speed-controls-italic-font%)))))
      (send slider set-scale! scale)
      slider)))

;;------------------------------------------------------------------------------
;; speed controls

(define %max-num-of-flashes% 5)
(define %max-flash-pause% 100)
(define %max-snag-pause% 5000)
(define %text-scroll-pause% 20)
(define %codelet-highlight-pause% 100)

(define speed-slider-action
  (lambda (scale value)
    (let ((range (lambda (low high) (max low (round (* (% (- 100 value)) high))))))
      (if (= value 100)
	(begin
	  (set! %num-of-flashes% 1)
	  (set! %flash-pause% 1)
	  (set! %snag-pause% 1)
	  (set! %text-scroll-pause% 1))
	(begin
	  (set! %num-of-flashes% (range 2 %max-num-of-flashes%))
	  (set! %flash-pause% (range 10 %max-flash-pause%))
	  (set! %snag-pause% (range 250 %max-snag-pause%))
	  (set! %text-scroll-pause% 20))))))

;;------------------------------------------------------------------------------

;; port: the control panel's toplevel; closing it exits, as its
;; destroy-request handler did
(define control-panel-frame%
  (class g:frame%
    (super-new)
    (define/augment (can-close?) (exit 0))))

(define make-control-panel
  (lambda ()
    (swl:sync-display)
    (select-control-panel-fonts)
    ;; port: racket/gui widgets are created in their parents, in display
    ;; order (Tk packed them afterwards), and menus in the menu bar
    ;; port: letrec (left to right, like letrec*), since racket/gui creates menu items in menu order, so
    ;; an item's action may name an item created after it
    (letrec ((control-panel
	     ;; port: or the one window's frame (racket/gui/one-window.rkt)
	     (if control-panel-frame-maker
	       (control-panel-frame-maker)
	       (new control-panel-frame% (label "Metacat Control Panel")
		 (style '(no-resize-border)) (border 15) (spacing 5)
		 (alignment '(center top)))))
	   (menu-bar (new g:menu-bar% (parent control-panel)))
	   (info-label
	     (new g:message% (parent control-panel)
	       (label "Please enter a problem:")
	       (font (widget-font %gui-header-font%)) (auto-resize #t)))
	   (command-line-action go-button-action)
	   (command-line
	     (new g:text-field% (parent control-panel) (label #f)
	       (min-width 360) (stretchable-width #f)
	       (font (widget-font %gui-command-line-font%))
	       (callback
		 (lambda (entry event)
		   (if* (eq? (send event get-event-type) 'text-field-enter)
		     (command-line-action entry))))))
	   (speed-controls
	     (new g:horizontal-panel% (parent control-panel) (spacing 4)
	       (stretchable-height #f) (alignment '(center top))))
	   (speed-slider
	     (create-slider speed-controls "Speed" "Slow" "Fast"
	       %gui-slider-length% %initial-speed% %gui-speed-controls-color%
	       speed-slider-action))
	   (button
	     (lambda (title action)
	       (new g:button% (parent speed-controls) (label title)
		 (font (widget-font %gui-speed-controls-font%))
		 (enabled #f)
		 (callback (lambda (b e) (action b))))))
	   (step-button (button "Step" step-button-action))
	   (go-button (button "Go" go-button-action))
	   (stop-button (button "Stop" stop-button-action))
	   (reset-button (button "Reset" reset-button-action))
	   (breakpoint-label
	     (new g:message% (parent control-panel) (label "")
	       (font (widget-font %gui-speed-controls-font%)) (color =red=)
	       (auto-resize #t)))
	   (self-watching-warning-label
	     (new g:message% (parent control-panel)
	       (label "Warning: Self-watching is disabled")
	       (font (widget-font %gui-header-font%)) (color =red=)))
	   ;; port: SWL's main menu had Help and Clear Memory as commands in
	   ;; the menu bar; racket/gui's menu bar holds only menus
	   (help-menu (new g:menu% (parent menu-bar) (label "Help")))
	   (help-item (menu-item help-menu "Help" help-action))
	   (demos-menu (new g:menu% (parent menu-bar) (label "Demos")))
	   (demos-items
	     (begin
	       (demo-menu-item demos-menu "Run 1:  abc -> abd; mrrjjj -> mrrjjjj" run1)
	       (demo-menu-item demos-menu "Run 2:  xqc -> xqd; mrrjjj -> mrrkkk" run2)
	       (demo-menu-item demos-menu "Run 3:  rst -> rsu; xyz -> uyz" run3)
	       (demo-menu-item demos-menu "Run 4:  abc -> abd; xyz -> dyz" run4)
	       (demo-menu-item demos-menu "Run 5:  xqc -> xqd; mrrjjj -> mrrjjjj" run5)
	       (demo-menu-item demos-menu "Run 6:  eqe -> qeq; abbbc -> aaabccc" run6)
	       (demo-menu-item demos-menu "Run 7:  abc -> abd; xyz -> ?" run7)
	       (demo-menu-item demos-menu "Run 8:  eqe -> qeq; abbbc -> ?" run8)
	       (menu-item-separator demos-menu)
	       (let ((m (create-submenu demos-menu "Answer comparison and reminding")))
		 (demo-menu-item m "abc / xyd" abc-xyd)
		 (demo-menu-item m "abc / wyz" abc-wyz)
		 (demo-menu-item m "abc / dyz" abc-dyz)
		 (demo-menu-item m "rst / xyu" rst-xyu)
		 (demo-menu-item m "rst / wyz" rst-wyz)
		 (demo-menu-item m "rst / uyz" rst-uyz)
		 (demo-menu-item m "abc / mrrkkk" abc-mrrkkk)
		 (demo-menu-item m "abc / mrrjjjj" abc-mrrjjjj)
		 (demo-menu-item m "xqc / mrrkkk" xqc-mrrkkk)
		 (demo-menu-item m "xqc / mrrjjjj" xqc-mrrjjjj)
		 (demo-menu-item m "eqe / baaab" eqe-baaab)
		 (demo-menu-item m "eqe / aaabaaa" eqe-aaabaaa)
		 (demo-menu-item m "eqe / qeeeq" eqe-qeeeq)
		 (demo-menu-item m "eqe / aaabccc" eqe-aaabccc))
	       (menu-item-separator demos-menu)
	       (let ((m (create-submenu demos-menu "Implausible rules")))
		 (demo-menu-item m (figure 5 4 'top) fig5.4-top)
		 (demo-menu-item m (figure 5 4 'bottom) fig5.4-bottom)
		 (demo-menu-item m (figure 5 5 'top) fig5.5-top)
		 (demo-menu-item m (figure 5 5 'bottom) fig5.5-bottom))
	       (let ((m (create-submenu demos-menu "Poor thematic characterizations")))
		 (demo-menu-item m (figure 5 7) fig5.7)
		 (demo-menu-item m (figure 5 8) fig5.8)
		 (demo-menu-item m (figure 5 10) fig5.10)
		 (demo-menu-item m (figure 5 11) fig5.11))
	       (let ((m (create-submenu demos-menu "Other sample runs")))
		 (demo-menu-item m "abc -> cba; mrrjjj -> mmmrrj" misc1)
		 (demo-menu-item m "abc -> abd; ijk -> abd" misc2)
		 (demo-menu-item m "abc -> aabbcc; kkjjii -> ?" misc3)
		 (demo-menu-item m "a -> b; z -> ?" misc4)
		 (demo-menu-item m "abc -> abd; glz -> ?" misc5))))
	   (window-controllers
	     (list
	       (window-controller "Workspace" *workspace-window* #t)
	       (window-controller "Slipnet" *slipnet-window* #t)
	       (window-controller "Coderack" *coderack-window* #t)
	       (window-controller "Temperature" *temperature-window* #t)
	       (window-controller "Temporal Trace" *trace-window* #t)
	       (window-controller "Commentary" *comment-window* #t)
	       (window-controller "Episodic Memory" *memory-window* #t)
	       (window-controller "Top Themes" *top-themes-window* #t)
	       (window-controller "Bottom Themes" *bottom-themes-window* #t)
	       (window-controller "Vertical Themes" *vertical-themes-window* #t)
	       (window-controller "EEG" *EEG-window* #f)
	       (window-controller "Logo" *mcat-logo* #f)))
	   (windows-menu (create-windows-menu menu-bar window-controllers))
	   (theme-window-controllers (sublist window-controllers 7 10))
	   (options-menu (new g:menu% (parent menu-bar) (label "Options")))
	   (options-items
	     (begin
	       (menu-item options-menu "Set breakpoint" set-breakpoint-action)
	       (menu-item options-menu "Clear breakpoint" clear-breakpoint-action)
	       (menu-item options-menu "Step mode interval" set-step-interval-action)
	       (menu-item-separator options-menu)
	       (check-menu-item options-menu "Eliza mode" %eliza-mode%
		 (lambda (item)
		   (set! %eliza-mode% (not %eliza-mode%))
		   (tell *comment-window* 'switch-modes)))
	       (check-menu-item options-menu "Slipnet graphics" %slipnet-graphics%
		 (lambda (item)
		   (set! %slipnet-graphics% (not %slipnet-graphics%))
		   (if* (not *display-mode?*)
		     (if %slipnet-graphics%
		       (tell *slipnet-window* 'restore-current-state)
		       (tell *slipnet-window* 'blank-window)))))
	       (check-menu-item options-menu "Coderack graphics" %coderack-graphics%
		 (lambda (item)
		   (set! %coderack-graphics% (not %coderack-graphics%))
		   (if* (not *display-mode?*)
		     (if %coderack-graphics%
		       (tell *coderack-window* 'restore-current-state)
		       (tell *coderack-window* 'blank-window "Coderack")))))
	       (check-menu-item options-menu "Show codelet counts" %codelet-count-graphics%
		 (lambda (item)
		   (set! %codelet-count-graphics% (not %codelet-count-graphics%))
		   (tell *coderack-window* 'initialize)))
	       (check-menu-item options-menu "Show last codelet type" %highlight-last-codelet%
		 (lambda (item)
		   (set! %highlight-last-codelet% (not %highlight-last-codelet%))
		   (if* (and %coderack-graphics% (not *display-mode?*))
		     (if %highlight-last-codelet%
		       (tell *coderack-window* 'highlight-last-codelet)
		       (tell *coderack-window* 'unhighlight-last-codelet)))))))
	   (self-watching-mode-menu-item
	     (check-menu-item options-menu "Self-watching mode" %self-watching-enabled%
	       (lambda (item)
		 (set! %self-watching-enabled% (not %self-watching-enabled%))
		 (if %self-watching-enabled%
		   (begin
		     (hide self-watching-warning-label)
		     (enable-widget clamp-themes-menu-item #t)
		     (enable-widget options:clamp-codelets-menu #t)
		     (enable-widget undo-clamp-menu-item #t)
		     (for* each controller in theme-window-controllers do
		       (tell controller 'show)))
		   (begin
		     (show self-watching-warning-label)
		     (enable-widget clamp-themes-menu-item #f)
		     (enable-widget options:clamp-codelets-menu #f)
		     (enable-widget undo-clamp-menu-item #f)
		     (tell *trace* 'undo-last-clamp)
		     (delete-themes)
		     (for* each controller in theme-window-controllers do
		       (tell controller 'hide)))))))
	   (verbose-item
	     (check-menu-item options-menu "Verbose mode" %verbose%
	       (lambda (item) (tell *control-panel* 'toggle-verbose-mode))))
	   (separator-2 (menu-item-separator options-menu))
	   (clamp-themes-menu-item
	     (menu-item options-menu "Clamp theme pattern"
	       (lambda (item) (tell *control-panel* 'theme-edit-mode-on))))
	   (options:clamp-codelets-menu
	     (let ((m (create-submenu options-menu "Clamp codelet pattern")))
	       (clamp-codelets-menu-item m "Top-down codelet pattern" 'top-down)
	       (clamp-codelets-menu-item m "Bottom-up codelet pattern" 'bottom-up)
	       (clamp-codelets-menu-item m "Rule codelet pattern" 'rule)
	       (clamp-codelets-menu-item m "Bridge codelet pattern" 'bridge)
	       (clamp-codelets-menu-item m "Group codelet pattern" 'group)
	       m))
	   (undo-clamp-menu-item
	     (menu-item options-menu "Undo last clamp"
	       (lambda (item) (tell *trace* 'undo-last-clamp))))
	   (separator-3 (menu-item-separator options-menu))
	   ;; these menus assume %comment-window-font% is sans-serif 12 (bold italic)
	   (options:comment-font-face-menu
	     (let ((m (create-submenu options-menu "Commentary font face")))
	       (comment-font-menu-item m #f "serif" serif 12)
	       (comment-font-menu-item m #f "serif italic" serif 12 'italic)
	       (comment-font-menu-item m #f "serif bold" serif 12 'bold)
	       (comment-font-menu-item m #f "serif bold italic" serif 12 'bold 'italic)
	       (menu-item-separator m)
	       (comment-font-menu-item m #f "sans-serif" sans-serif 12)
	       (comment-font-menu-item m #f "sans-serif italic" sans-serif 12 'italic)
	       (comment-font-menu-item m #f "sans-serif bold" sans-serif 12 'bold)
	       (comment-font-menu-item m #t "sans-serif bold italic" sans-serif 12 'bold 'italic)
	       (menu-item-separator m)
	       (comment-font-menu-item m #f "fancy" fancy 12)
	       (comment-font-menu-item m #f "fancy italic" fancy 12 'italic)
	       (comment-font-menu-item m #f "fancy bold" fancy 12 'bold)
	       (comment-font-menu-item m #f "fancy bold italic" fancy 12 'bold 'italic)
	       m))
	   (options:comment-font-size-menu
	     (let ((m (create-submenu options-menu "Commentary font size")))
	       (comment-font-menu-item m #f "tiny" sans-serif 8 'bold 'italic)
	       (comment-font-menu-item m #f "small" sans-serif 10 'bold 'italic)
	       (comment-font-menu-item m #t "medium" sans-serif 12 'bold 'italic)
	       (comment-font-menu-item m #f "large" sans-serif 18 'bold 'italic)
	       (comment-font-menu-item m #f "larger" sans-serif 24 'bold 'italic)
	       (comment-font-menu-item m #f "huge" sans-serif 34 'bold 'italic)
	       m))
	   (save-item
	     (menu-item options-menu "Save commentary to file" save-commentary-action))
	   (memory-menu (new g:menu% (parent menu-bar) (label "Memory")))
	   (clearmem-item (clear-memory-menu-item memory-menu "Clear Memory" %gui-menubar-font%)))
      (hide self-watching-warning-label)
      (if %self-watching-enabled%
	(begin
	  (hide self-watching-warning-label))
	(begin
	  (show self-watching-warning-label)
	  (enable-widget clamp-themes-menu-item #f)
	  (enable-widget options:clamp-codelets-menu #f)
	  (enable-widget undo-clamp-menu-item #f)))
      (set-comment-font-menu-actions
	options:comment-font-face-menu
	options:comment-font-size-menu)
      ;; port: the slider's initial value sets the speed, as Tk's scale
      ;; command did when the scale was created
      (speed-slider-action (send speed-slider get-scale) %initial-speed%)
      (send control-panel move 0 0)
      (send control-panel show #t)
      (send command-line focus)
      (let ((demos-button demos-menu)
	    (options-button options-menu)
	    (clearmem-button memory-menu)
	    (clearmem-dialog #f)
	    (theme-edit-dialog #f)
	    (edited-theme-types '())
	    (saved-theme-states '())
	    (verbose-mode? %verbose%)
	    (problem #f))
	;; port: info-label's message, kept for display-error
	(define info-title "Please enter a problem:")
	(define (set-info-title! s) (set! info-title s) (send info-label set-label s))
	;; control panel object:
	(lambda msg
	  (let ((self (1st msg)))
	    (record-case (rest msg)
	      (object-type () 'control-panel)
	      ;; port: the widgets, for tests and the window layout
	      (get-widgets ()
		`((frame . ,control-panel) (info-label . ,info-label)
		  (command-line . ,command-line) (speed-slider . ,(send speed-slider get-scale))
		  (step-button . ,step-button) (go-button . ,go-button)
		  (stop-button . ,stop-button) (reset-button . ,reset-button)
		  (breakpoint-label . ,breakpoint-label)
		  (self-watching-warning-label . ,self-watching-warning-label)
		  (demos-menu . ,demos-menu) (windows-menu . ,windows-menu)
		  (options-menu . ,options-menu) (memory-menu . ,memory-menu)
		  (self-watching-mode-menu-item . ,self-watching-mode-menu-item)
		  (window-controllers . ,window-controllers)))
	      ;; port: the info label's message, for tests
	      (get-info-title () info-title)
	      (problem-exists? () (exists? problem))
	      (get-current-problem () problem)
	      (set-position (x y)
		(send control-panel move x y))
	      (get-relative-position (x-offset y-offset)
		;; port: the toplevel's position, without parsing its geometry
		(let ((x (send control-panel get-x))
		      (y (send control-panel get-y)))
		  (format "+~a+~a" (+ x x-offset) (+ y y-offset))))
	      (get-command-line-string () (send command-line get-value))
	      (update-current-problem (tokens)
		(if* (andmap symbol? tokens)
		  (randomize))
		(set! problem
		  (cond
		    ((= (length tokens) 5) tokens)
		    ((= (length tokens) 3) `(,@tokens #f ,(random-seed)))
		    ((symbol? (4th tokens)) `(,@tokens ,(random-seed)))
		    ((number? (4th tokens))
		     `(,(1st tokens) ,(2nd tokens) ,(3rd tokens) #f ,(4th tokens)))))
		(set! %justify-mode% (exists? (4th problem)))
		(tell self 'display
		  (format " ~a -> ~a; ~a -> ~a       seed:  ~a "
		    (1st problem) (2nd problem) (3rd problem)
		    (if %justify-mode% (4th problem) '?) (5th problem)))
		(call-on-gui-thread control-panel
		  (lambda () (send command-line set-value "")))
		(unhighlight-menu-items demos-menu))
	      (init-new-problem (tokens step-mode?)
		(tell self 'update-current-problem tokens)
		(tell self 'switch-to-input-mode)
		(thread-break *repl-thread* #f
		  (lambda ()
		    (apply init-mcat problem)
		    (if* step-mode? (step-mode-on))
		    (quiet-break)
		    (run-mcat))))
	      (run-new-problem (tokens)
		(tell self 'update-current-problem tokens)
		(tell self 'switch-to-run-mode)
		(thread-break *repl-thread* #f
		  (lambda ()
		    (apply init-mcat problem)
		    (run-mcat))))
	      (resume-current-problem ()
		(if (not (exists? problem))
		  (tell self 'display-error "No current problem!")
		  (begin
		    (if* *display-mode?* (restore-current-state))
		    (thread-break *repl-thread* #f go))))
	      (reset-current-problem ()
		(if (not (exists? problem))
		  (tell self 'display-error "No current problem!")
		  (thread-break *repl-thread* #f
		    (lambda ()
		      (apply init-mcat problem)
		      (quiet-break)
		      (run-mcat)))))
	      (verbose-mode? () verbose-mode?)
	      (toggle-verbose-mode ()
		(set! verbose-mode? (not verbose-mode?))
		(set! %verbose% verbose-mode?))
	      (set-verbose-step-mode (value)
		(set! %verbose% (or value verbose-mode?))
		'done)
	      ;; port: racket/gui text fields have one font and no foreground
	      ;; colour; the run-mode look is the background and the text.
	      ;; port: run.rktl switches modes from the engine thread; the
	      ;; widgets change on the GUI thread (call-on-gui-thread, gui.rkt)
	      (switch-to-run-mode ()
		(call-on-gui-thread control-panel
		  (lambda () (tell self 'switch-to-run-mode*))))
	      (switch-to-run-mode* ()
		(send command-line set-value "running...")
		(send command-line set-field-background %gui-run-mode-foreground-color%)
		(set! command-line-action nop-event-handler)
		(enable-widget command-line #f)
		(enable-widget step-button #f)
		(enable-widget go-button #f)
		(enable-widget stop-button #t)
		(enable-widget reset-button #f)
		(enable-widget demos-button #f)
		(enable-widget options-button #f)
		(enable-widget clearmem-button #f))
	      (switch-to-input-mode ()
		(call-on-gui-thread control-panel
		  (lambda () (tell self 'switch-to-input-mode*))))
	      (switch-to-input-mode* ()
		(enable-widget command-line #t)
		(set! command-line-action go-button-action)
		(send command-line set-value "")
		(send command-line set-field-background %gui-command-line-color%)
		(enable-widget step-button #t)
		(enable-widget go-button #t)
		(enable-widget stop-button #f)
		(enable-widget reset-button #t)
		(enable-widget demos-button #t)
		(enable-widget options-button #t)
		(enable-widget clearmem-button #t))
	      (switch-to-disabled-mode ()
		(set! command-line-action nop-event-handler)
		(enable-widget command-line #f)
		(enable-widget step-button #f)
		(enable-widget go-button #f)
		(enable-widget stop-button #f)
		(enable-widget reset-button #f)
		(enable-widget demos-button #f)
		(enable-widget options-button #f)
		(enable-widget clearmem-button #f))
	      (ready-to-edit? (theme-type)
		(and (not (member? theme-type edited-theme-types))
		     (or %justify-mode%
			 (not (eq? theme-type 'bottom-bridge)))))
	      (edit-theme-type (theme-type)
		(set! edited-theme-types (cons theme-type edited-theme-types))
		(set-themes theme-type 0))
	      (raise-theme-edit-dialog ()
		(send theme-edit-dialog raise)
		(send theme-edit-dialog set-focus))
	      (get-theme-edit-dialog () theme-edit-dialog)   ;; port: for tests
	      (get-clearmem-dialog () clearmem-dialog)       ;; port: for tests
	      (theme-edit-mode-on ()
		(cond
		  ((not (exists? problem))
		   (tell self 'display-error "No current problem!"))
		  ((exists? theme-edit-dialog)
		   (tell self 'raise-theme-edit-dialog))
		  (else
		    (if* *display-mode?* (restore-current-state))
		    (tell self 'switch-to-disabled-mode)
		    (set! *theme-edit-mode?* #t)
		    (set! edited-theme-types '())
		    (set! saved-theme-states
		      (list
			(tell *themespace* 'get-partial-state 'top-bridge)
			(tell *themespace* 'get-partial-state 'vertical-bridge)
			(tell *themespace* 'get-partial-state 'bottom-bridge)))
		    (tell *themespace-window* 'set-background-colors
		      %theme-background-color:thematic-pressure-on%
		      %theme-edit-mode-color%)
		    (tell *themespace-window* 'clear)
		    (delete-themes)
		    (tell *themespace* 'thematic-pressure-off)
		    (tell *themespace-window* 'display-edit-mode-message)
		    (tell *themespace-window* 'raise-window)
		    (set! theme-edit-dialog
		      (confirm-dialog 10 20
			%gui-instructions-font% =black= =yellow= 'left
			(if (eq? *platform* 'macintosh)
			  (format "~a~%~a~%~a~%~%~a~%~a~%~a~%~a"
			    "To clamp a theme-pattern, click on one or more"
			    "theme windows, select the themes to include in"
			    "the pattern, and then click Clamp Themes."
			    "Clicking on a theme selects maximum positive"
			    "theme activation.  Shift-clicking selects maximum"
			    "negative activation.  Clicking on an already-selected"
			    "theme unselects it.")
			  (format "~a~%~a~%~a~%~%~a~%~a~%~a~%~a"
			    "To clamp a theme-pattern, click on one or more"
			    "theme windows, select the themes to include in"
			    "the pattern, and then click Clamp Themes."
			    "Left-clicking on a theme selects maximum"
			    "positive theme activation.  Right-clicking selects"
			    "maximum negative activation.  Clicking on an"
			    "already-selected theme unselects it."))
			"Clamp Themes" "Cancel"
			(lambda (button)
			  (tell self 'theme-edit-mode-off #t)
			  (send theme-edit-dialog destroy))
			(lambda (button)
			  (tell self 'theme-edit-mode-off #f)
			  (send theme-edit-dialog destroy))
			(lambda (toplevel)
			  (tell self 'theme-edit-mode-off #f)
			  (set! theme-edit-dialog #f)
			  (tell self 'switch-to-input-mode)
			  #t))))))
	      (theme-edit-mode-off (clamp-patterns?)
		(if* *theme-edit-mode?*
		  (set! *theme-edit-mode?* #f)
		  (tell *themespace-window* 'set-background-colors
		    %theme-background-color:thematic-pressure-on%
		    %theme-background-color:thematic-pressure-off%)
		  (let ((theme-types-to-clamp
			  (if clamp-patterns? edited-theme-types '())))
		    (for* each state in saved-theme-states do
		      (let ((theme-type (1st (1st state))))
			(if* (not (member? theme-type theme-types-to-clamp))
			  (tell *themespace* 'restore-state state))))
		    (if* (not (null? theme-types-to-clamp))
		      (let* ((patterns
			       (map (lambda (type)
				      (tell *themespace* 'get-nonzero-theme-pattern type))
				 theme-types-to-clamp))
			     (clamp-event
			       (make-clamp-event 'manual-clamp patterns '() 'workspace)))
			(tell *trace* 'undo-last-clamp)
			(tell *trace* 'add-event clamp-event)
			(tell clamp-event 'activate))))))
	      (clear-memory ()
		(if (exists? clearmem-dialog)
		  (begin
		    (send clearmem-dialog raise)
		    (send clearmem-dialog set-focus))
		  (begin
		    (tell self 'switch-to-disabled-mode)
		    (set! clearmem-dialog
		      (confirm-dialog 20 70
			%gui-warning-font% =red= #f 'center
			(format "Really delete all answers~%from the Episodic Memory?")
			"Yes" "Cancel"
			(lambda (button)
			  (tell *memory* 'clear)
			  (send clearmem-dialog destroy))
			(lambda (button)
			  (send clearmem-dialog destroy))
			(lambda (toplevel)
			  (set! clearmem-dialog #f)
			  (tell *control-panel* 'switch-to-input-mode)
			  #t))))))
	      (display-breakpoint-message ()
		(send breakpoint-label set-label
		  (format "Breakpoint set for time step ~a" *break-time*)))
	      (clear-breakpoint-message ()
		(send breakpoint-label set-label ""))
	      (display (message)
		(set-info-title! message))
	      (display-error (message)
		(let ((current-message info-title))
		  ;; port: the label turns red for 700 ms on a timer instead of
		  ;; a pause in the event thread (and the toplevel does not
		  ;; resize, so the border freeze is not needed)
		  (send info-label set-color =red=)
		  (send info-label set-label message)
		  (new g:timer% (interval 700) (just-once? #t)
		    (notify-callback
		      (lambda ()
			(send info-label set-color =black=)
			(send info-label set-label current-message))))
		  'done))
	      ;; port: an error in the model, which stopped the engine thread's
	      ;; thunk (in the original it went to the REPL, leaving the
	      ;; control panel in run mode)
	      (engine-error (message)
		(tell self 'switch-to-input-mode)
		(tell self 'display (format "Error: ~a" message)))
	      (hide-window (toplevel)
		(for* each controller in window-controllers do
		  (if* (eq? (tell controller 'get-toplevel) toplevel)
		    (tell controller 'hide))))
	      (raise ()
		(send control-panel show #t)
		(if* (exists? clearmem-dialog)
		  (send clearmem-dialog raise)))
	      (else (delegate msg base-object)))))))))

;;------------------------------------------------------------------------------------
;; menus

;; port: racket/gui menus and items are created in their parent menu; the
;; procedures take the parent as their first argument.  Fonts and colours of
;; menu items are left out.

(define create-submenu
  (lambda (parent text)
    (new g:menu% (parent parent) (label text))))

(define menu-item-separator
  (lambda (parent)
    (new g:separator-menu-item% (parent parent))))

(define menu-item
  (lambda (parent text action-proc . font)
    (new g:menu-item% (parent parent) (label text)
      (callback (lambda (item event) (action-proc item))))))

;; port: the on/off foreground colours become the check mark
(define check-menu-item
  (lambda (parent text selected? action-proc)
    (new g:checkable-menu-item% (parent parent) (label text) (checked selected?)
      (callback (lambda (item event) (action-proc item))))))

(define clear-memory-menu-item
  (lambda (parent text font)
    (new g:menu-item% (parent parent) (label text)
      (callback (lambda (item event) (tell *control-panel* 'clear-memory))))))

(define demo-menu-item
  (lambda (parent text problem)
    (new g:checkable-menu-item% (parent parent) (label text)
      (callback (lambda (item event)
		  (tell *control-panel* 'init-new-problem problem #f)
		  (set-menu-item-color item))))))

;; in Mac OS X, periods in menu labels don't show up for some reason
(define figure
  (lambda (m n . opt)
    (let ((separator (if (eq? *platform* 'macintosh) "-" ".")))
      (format "Figure ~a~a~a~a" m separator n
	(if (null? opt) "" (format " (~a)" (car opt)))))))

;; port: get-demos-button, get-options-button, get-clearmem-button and
;; create-options-menu picked SWL menu-bar items by position for each
;; platform; make-control-panel keeps the menus themselves

(define create-windows-menu
  (lambda (menu-bar window-controllers)
    (let* ((menu (new g:menu% (parent menu-bar) (label "Windows")))
	   (show-all
	     (lambda (item)
	       (for* each controller in window-controllers do
		 (tell controller 'show))))
	   (hide-all
	     (lambda (item)
	       (for* each controller in window-controllers do
		 (tell controller 'hide)))))
      (for* each controller in window-controllers do
	(tell controller 'make-menu-item menu))
      (menu-item-separator menu)
      (menu-item menu "Show all windows" show-all)
      (menu-item menu "Hide all windows" hide-all)
      (for* each controller in window-controllers do
	(tell controller 'initialize))
      menu)))

(define window-controller
  (lambda (text window visible?)
    (let ((toplevel
	    (if (eq? window *mcat-logo*)
	      (send (send *mcat-logo* get-parent) get-parent)
	      (tell window 'get-toplevel)))
	  (menu-item #f))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'window-controller)
	    (get-menu-item () menu-item)
	    (get-toplevel () toplevel)
	    (visible? () visible?)   ;; port: for tests
	    ;; port: the menu item is created in the Windows menu
	    (make-menu-item (menu)
	      (set! menu-item
		(new g:menu-item% (parent menu) (label text)
		  (callback (lambda (item event) (tell self 'toggle))))))
	    (initialize ()
	      (tell self 'update))
	    (toggle ()
	      (if visible?
		(tell self 'hide)
		(tell self 'show)))
	    (show ()
	      (set! visible? #t)
	      (tell self 'update)
	      (tell *control-panel* 'raise))
	    (hide ()
	      (set! visible? #f)
	      (tell self 'update))
	    (update ()
	      (if visible?
		(begin
		  (send menu-item set-label (format "Hide ~a" text))
		  (if* (not (eq? window *mcat-logo*))
		    (tell window 'restore-position))
		  (show toplevel))
		(begin
		  (send menu-item set-label (format "Show ~a" text))
		  (if* (not (eq? window *mcat-logo*))
		    (tell window 'remember-position))
		  (hide toplevel))))
	    (else (delegate msg base-object))))))))

;; port: the font of each commentary font item, which SWL kept in the item
(define comment-font-items '())
(define (item-font item) (cdr (assq item comment-font-items)))
(define (set-item-font! item font)
  (set! comment-font-items
    (cons (cons item font)
      (let loop ((ps comment-font-items))
	(cond
	  ((null? ps) '())
	  ((eq? (car (car ps)) item) (loop (cdr ps)))
	  (else (cons (car ps) (loop (cdr ps)))))))))

(define comment-font-menu-item
  (lambda (parent highlight? text face size . style)
    (let ((item (new g:checkable-menu-item% (parent parent) (label text)
		  (callback
		    (lambda (item event)
		      (let ((action (assq item comment-font-actions)))
			(if* action ((cdr action) item))))))))
      (set-item-font! item (swl-font face size style))
      (if* highlight? (set-menu-item-color item))
      item)))

;; port: a menu's items are its get-items; actions are installed by a
;; per-item callback table (racket/gui callbacks are fixed at creation)
(define comment-font-actions '())
(define (set-item-action! item action)
  (set! comment-font-actions (cons (cons item action) comment-font-actions)))

(define set-comment-font-menu-actions
  (lambda (face-menu size-menu)
    (for* each face-item in (send face-menu get-items) do
      (if* (not (is-a? face-item g:separator-menu-item%))
	(set-item-action! face-item
	  (lambda (item)
	    (let ((f (item-font item)))
	      (set! %comment-window-font%
		(make-mfont (send f get-family) (send f get-size) (send f get-style)))
	      (tell *comment-window* 'new-font %comment-window-font%)
	      (update-menu-fonts
		size-menu (send f get-family) 'same (send f get-style))
	      (unhighlight-menu-items face-menu)
	      (set-menu-item-color item))))))
    (for* each size-item in (send size-menu get-items) do
      (set-item-action! size-item
	(lambda (item)
	  (let ((f (item-font item)))
	    (set! %comment-window-font%
	      (make-mfont (send f get-family) (send f get-size) (send f get-style)))
	    (tell *comment-window* 'new-font %comment-window-font%)
	    (update-menu-fonts
	      face-menu 'same (send f get-size) 'same)
	    (unhighlight-menu-items size-menu)
	    (set-menu-item-color item)))))))

(define update-menu-fonts
  (lambda (menu new-face new-size new-style)
    (for* each item in (send menu get-items) do
      (if* (not (is-a? item g:separator-menu-item%))
	(let* ((font (item-font item))
	       (face (if (eq? new-face 'same) (send font get-family) new-face))
	       (size (if (eq? new-size 'same) (send font get-size) new-size))
	       (style (if (eq? new-style 'same) (send font get-style) new-style)))
	  (set-item-font! item (swl-font face size style)))))))

(define unhighlight-menu-items
  (lambda (menu)
    (for* each item in (send menu get-items) do
      (cond
	((is-a? item g:menu%) (unhighlight-menu-items item))
	((is-a? item g:checkable-menu-item%) (send item check #f))
	(else 'done)))))

(define clamp-codelets-menu-item
  (lambda (parent text structure-type)
    (let ((pattern
	    (case structure-type
	      (top-down %top-down-codelet-pattern%)
	      (bottom-up %bottom-up-codelet-pattern%)
	      (group (against-background %very-low-urgency% %group-codelet-pattern%))
	      (bridge (against-background %very-low-urgency% %bridge-codelet-pattern%))
	      (rule (against-background %very-low-urgency% %rule-codelet-pattern%)))))
      (menu-item parent text
	(lambda (item)
	  (if (not (tell *control-panel* 'problem-exists?))
	    (tell *control-panel* 'display-error "No current problem!")
	    (let ((clamp-event
		    (make-clamp-event 'manual-clamp (list pattern) '()
		      (if (member? structure-type '(top-down bottom-up bridge))
			'workspace
			structure-type))))
	      (if* %coderack-graphics%
		(tell *coderack-window* 'raise-window))
	      (tell *trace* 'undo-last-clamp)
	      (tell *trace* 'add-event clamp-event)
	      (tell clamp-event 'activate))))))))
