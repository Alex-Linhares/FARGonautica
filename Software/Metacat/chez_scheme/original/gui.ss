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

;; bug workaround: on mac OS X, set-background-color! actually changes
;; the text *foreground color* instead of the background color, so we
;; use =blue= instead of =white= for better visibility:
(define set-menu-item-color
  (lambda (item)
    (if (eq? *platform* 'macintosh)
	;; highlight item by setting foreground color to blue
	(send item set-background-color! =blue=)
	;; highlight item by setting background color to white
	(send item set-background-color! =white=))))

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

(define pack-hspace
  (lambda (parent width side . color)
    (let ((space (create <frame> parent with (width: width))))
      (if* (not (null? color))
	(send space set-background-color! (car color)))
      (pack space (side: side)))))

(define pack-vspace
  (lambda (parent height side . color)
    (let ((space (create <frame> parent with (height: height))))
      (if* (not (null? color))
	(send space set-background-color! (car color)))
      (pack space (side: side)))))

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

(define read-file
  (lambda (filename text-widget)
    (let ((buffer (make-string 2048)))
      (let loop ((port (open-input-file filename)))
        (let ((x (block-read port buffer 2048)))
          (unless (eof-object? x)
            (insert text-widget (if (< x 2048) (substring buffer 0 x) buffer))
            (loop port)))))))

(define help-action
  (let ((help-window #f))
    (lambda (item)
      (if (exists? help-window)
	(begin
	  (send help-window raise)
	  (send help-window set-focus))
	(begin
	  (set! help-window
	    (create <toplevel> with
	      (title: "Help")
	      (destroy-request-handler:
		(lambda (toplevel)
		  (set! help-window #f)
		  #t))))
	  (let* ((sf (create <scrollframe> help-window))
		 (txt (create <text> sf with
			(background-color: %gui-help-window-color%)
			(font: %gui-help-window-font%)
			(wrap: 'word)
			(hpad: 10))))
	    (pack sf (expand: #t) (fill: 'both))
	    (read-file "help.txt" txt)
	    (send txt set-cursor-pos! '(0 . 0))
	    (send txt set-enabled! #f)))))))

;;------------------------------------------------------------------------------------
;; pop-up dialogs

;; fg-color specifies the color of the dialog text.  If bg-color is #f
;; the dialog text appears on a white background surrounded by a grey
;; border, otherwise there is no border and the entire dialog
;; background is bg-color.

(define confirm-dialog
  (lambda (x y font fg-color bg-color justify message yes-label no-label
	    yes-action no-action destroy-action)
    (let* ((border-bg-color
	     (if (exists? bg-color) bg-color (swl-color "grey85")))
	   (dialog
	     (create <toplevel> with
	       (title: "Confirm")
	       (background-color: border-bg-color)
	       (resizable: #f #f)
	       (geometry: (tell *control-panel* 'get-relative-position x y))
	       (destroy-request-handler: destroy-action)))
	   (message-label
	     (create <label> dialog with
	       (title: message)
	       (foreground-color: fg-color)
	       (background-color: (if (exists? bg-color) bg-color =white=))
	       (justify: justify)
	       (font: font)))
	   (button-frame
	     (create <frame> dialog with
	       (background-color: border-bg-color)))
	   (yes-button
	     (create <button> button-frame with
	       (title: yes-label)
	       (action: yes-action)))
	   (no-button
	     (create <button> button-frame with
	       (title: no-label)
	       (action: no-action))))
      (pack yes-button (side: 'left))
      (pack-hspace button-frame 20 'left border-bg-color)
      (pack no-button (side: 'right))
      (pack-vspace dialog 15 'top border-bg-color)
      (pack-vspace dialog 15 'bottom border-bg-color)
      (pack-hspace dialog 15 'left border-bg-color)
      (pack-hspace dialog 15 'right border-bg-color)
      (pack message-label (side: 'top) (fill: 'both))
      (pack-vspace dialog 20 'top border-bg-color)
      (pack button-frame (side: 'top))
      dialog)))

(define input-dialog
  (lambda (x y default message input-action destroy-action)
    (let* ((dialog
	     (create <toplevel> with
	       (title: "Input")
	       (resizable: #f #f)
	       (geometry: (tell *control-panel* 'get-relative-position x y))
	       (destroy-request-handler: destroy-action)))
	   (top-border (create <frame> dialog with (width: 200) (height: 15)))
	   (message-label
	     (create <label> dialog with
	       (title: message)
	       (foreground-color: =black=)
	       (background-color: =white=)
	       (font: %gui-input-dialog-font%)))
	   (input-field
	     (create <entry> dialog with
	       (width/char: 10)
	       (font: %gui-command-line-font%)
	       (background-color: =white=)
	       (action:
		 (lambda (entry)
		   (let ((input (send entry get-string)))
		     (if (string=? input "")
		       (send dialog destroy)
		       (let ((value (string->number input)))
			 (if (or (not value) (< value 1))
			   (begin
			     (send message-label set-foreground-color! =red=)
			     (send message-label set-title! "Invalid input!")
			     (pause 700)
			     (send message-label set-foreground-color! =black=)
			     (send message-label set-title! message))
			   (begin
			     (input-action value)
			     (send dialog destroy)))))))))))
      (pack top-border (side: 'top))
      (pack-vspace dialog 15 'bottom)
      (pack-hspace dialog 15 'left)
      (pack-hspace dialog 15 'right)
      (pack message-label (side: 'top))
      (pack-vspace dialog 20 'top)
      (pack input-field (side: 'top))
      (if* (exists? default)
	(send input-field insert default)
	(send input-field select-range 0 (string-length default)))
      (send input-field set-focus)
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
		      (default-dir: *file-dialog-directory*))))
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

(define create-slider
  (lambda (parent text min-text max-text len init-val color slide-action)
    (let* ((slider (create <frame> parent with (background-color: color)))
	   (min-label
	     (create <label> slider with
	       (title: min-text)
	       (font: %gui-speed-controls-italic-font%)
	       (background-color: color)))
	   (max-label
	     (create <label> slider with
	       (title: max-text)
	       (font: %gui-speed-controls-italic-font%)
	       (background-color: color)))
	   (main-label
	     (create <label> slider with
	       (title: text)
	       (font: %gui-speed-controls-font%)
	       (background-color: color)))
	   (scale
	     (create <scale> slider with
	       (orientation: 'horizontal)
	       (value: init-val)
	       (length: len)
	       (width: %gui-slider-thickness%)
	       (show-value: #f)
	       (background-color: color)
	       (active-background-color: color)
	       (action: slide-action))))
      (pack scale (side: 'top) (fill: 'both))
      (pack min-label (side: 'left) (fill: 'both))
      (pack max-label (side: 'right) (fill: 'both))
      (pack main-label (side: 'bottom) (fill: 'both))
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

(define make-control-panel
  (lambda ()
    (swl:sync-display)
    (select-control-panel-fonts)
    (let* ((control-panel
	     (create <toplevel> with
	       (title: "Metacat Control Panel")
	       (background-color: =white=)
	       (resizable: #f #f)
	       (destroy-request-handler: (lambda (toplevel) (exit)))))
	   (top-border
	     (create <canvas> control-panel with
	       (width: 350) (height: 5) (traversal-thickness: 0)
	       (background-color: =white=)))
	   (self-watching-warning-label
	     (create <label> control-panel with
	       (title: "Warning: Self-watching is disabled")
	       (font: %gui-header-font%)
	       (foreground-color: =red=)
	       (background-color: =white=)))
	   (info-label
	     (create <label> control-panel with
	       (title: "Please enter a problem:")
	       (font: %gui-header-font%)
	       (background-color: =white=)))
	   (command-line
	     (create <entry> control-panel with
	       (width/char: 40)
	       (font: %gui-command-line-font%)
	       (background-color: %gui-command-line-color%)
	       (action: go-button-action)))
	   (speed-controls
	     (create <frame> control-panel with
	       (background-color: %gui-speed-controls-color%)))
	   (speed-slider
	     (create-slider speed-controls "Speed" "Slow" "Fast"
	       %gui-slider-length% %initial-speed% %gui-speed-controls-color%
	       speed-slider-action))
	   (step-button
	     (create <button> speed-controls with
	       (title: "Step")
	       (font: %gui-speed-controls-font%)
	       (active-background-color: =green=)
	       (traversal-thickness: 3)
	       (traversal-background-color: %gui-speed-controls-color%)
	       (enabled: #f)
	       (action: step-button-action)))
	   (go-button
	     (create <button> speed-controls with
	       (title: "Go")
	       (font: %gui-speed-controls-font%)
	       (active-background-color: =green=)
	       (traversal-thickness: 3)
	       (traversal-background-color: %gui-speed-controls-color%)
	       (enabled: #f)
	       (action: go-button-action)))
	   (stop-button
	     (create <button> speed-controls with
	       (title: "Stop")
	       (font: %gui-speed-controls-font%)
	       (active-background-color: =red=)
	       (traversal-thickness: 3)
	       (traversal-background-color: %gui-speed-controls-color%)
	       (enabled: #f)
	       (action: stop-button-action)))
	   (reset-button
	     (create <button> speed-controls with
	       (title: "Reset")
	       (font: %gui-speed-controls-font%)
	       (active-background-color: =green=)
	       (traversal-thickness: 3)
	       (traversal-background-color: %gui-speed-controls-color%)
	       (enabled: #f)
	       (action: reset-button-action)))
	   (breakpoint-label
	     (create <label> control-panel with
	       (title: "")
	       (font: %gui-speed-controls-font%)
	       (foreground-color: =red=)
	       (background-color: =white=)))
	   (demos-menu
	     (create-menu
	       (demo-menu-item "Run 1:  abc -> abd; mrrjjj -> mrrjjjj" run1)
	       (demo-menu-item "Run 2:  xqc -> xqd; mrrjjj -> mrrkkk" run2)
	       (demo-menu-item "Run 3:  rst -> rsu; xyz -> uyz" run3)
	       (demo-menu-item "Run 4:  abc -> abd; xyz -> dyz" run4)
	       (demo-menu-item "Run 5:  xqc -> xqd; mrrjjj -> mrrjjjj" run5)
	       (demo-menu-item "Run 6:  eqe -> qeq; abbbc -> aaabccc" run6)
	       (demo-menu-item "Run 7:  abc -> abd; xyz -> ?" run7)
	       (demo-menu-item "Run 8:  eqe -> qeq; abbbc -> ?" run8)
	       (menu-item-separator)
	       (create-submenu "Answer comparison and reminding"
		 (demo-menu-item "abc / xyd" abc-xyd)
		 (demo-menu-item "abc / wyz" abc-wyz)
		 (demo-menu-item "abc / dyz" abc-dyz)
		 (demo-menu-item "rst / xyu" rst-xyu)
		 (demo-menu-item "rst / wyz" rst-wyz)
		 (demo-menu-item "rst / uyz" rst-uyz)
		 (demo-menu-item "abc / mrrkkk" abc-mrrkkk)
		 (demo-menu-item "abc / mrrjjjj" abc-mrrjjjj)
		 (demo-menu-item "xqc / mrrkkk" xqc-mrrkkk)
		 (demo-menu-item "xqc / mrrjjjj" xqc-mrrjjjj)
		 (demo-menu-item "eqe / baaab" eqe-baaab)
		 (demo-menu-item "eqe / aaabaaa" eqe-aaabaaa)
		 (demo-menu-item "eqe / qeeeq" eqe-qeeeq)
		 (demo-menu-item "eqe / aaabccc" eqe-aaabccc))
	       (menu-item-separator)
	       (create-submenu "Implausible rules"
		 (demo-menu-item (figure 5 4 'top) fig5.4-top)
		 (demo-menu-item (figure 5 4 'bottom) fig5.4-bottom)
		 (demo-menu-item (figure 5 5 'top) fig5.5-top)
		 (demo-menu-item (figure 5 5 'bottom) fig5.5-bottom))
	       (create-submenu "Poor thematic characterizations"
		 (demo-menu-item (figure 5 7) fig5.7)
		 (demo-menu-item (figure 5 8) fig5.8)
		 (demo-menu-item (figure 5 10) fig5.10)
		 (demo-menu-item (figure 5 11) fig5.11))
	       (create-submenu "Other sample runs"
		 (demo-menu-item "abc -> cba; mrrjjj -> mmmrrj" misc1)
		 (demo-menu-item "abc -> abd; ijk -> abd" misc2)
		 (demo-menu-item "abc -> aabbcc; kkjjii -> ?" misc3)
		 (demo-menu-item "a -> b; z -> ?" misc4)
		 (demo-menu-item "abc -> abd; glz -> ?" misc5))))
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
	   (windows-menu (create-windows-menu window-controllers))
	   (theme-window-controllers (sublist window-controllers 7 10))
	   ;; these menus assume %comment-window-font% is sans-serif 12 (bold italic)
	   (options:comment-font-face-menu
	     (create-menu
	       (comment-font-menu-item #f "serif" serif 12)
	       (comment-font-menu-item #f "serif italic" serif 12 'italic)
	       (comment-font-menu-item #f "serif bold" serif 12 'bold)
	       (comment-font-menu-item #f "serif bold italic" serif 12 'bold 'italic)
	       (menu-item-separator)
	       (comment-font-menu-item #f "sans-serif" sans-serif 12)
	       (comment-font-menu-item #f "sans-serif italic" sans-serif 12 'italic)
	       (comment-font-menu-item #f "sans-serif bold" sans-serif 12 'bold)
	       (comment-font-menu-item #t "sans-serif bold italic" sans-serif 12 'bold 'italic)
	       (menu-item-separator)
	       (comment-font-menu-item #f "fancy" fancy 12)
	       (comment-font-menu-item #f "fancy italic" fancy 12 'italic)
	       (comment-font-menu-item #f "fancy bold" fancy 12 'bold)
	       (comment-font-menu-item #f "fancy bold italic" fancy 12 'bold 'italic)))
	   (options:comment-font-size-menu
	     (create-menu
	       (comment-font-menu-item #f "tiny" sans-serif 8 'bold 'italic)
	       (comment-font-menu-item #f "small" sans-serif 10 'bold 'italic)
	       (comment-font-menu-item #t "medium" sans-serif 12 'bold 'italic)
	       (comment-font-menu-item #f "large" sans-serif 18 'bold 'italic)
	       (comment-font-menu-item #f "larger" sans-serif 24 'bold 'italic)
	       (comment-font-menu-item #f "huge" sans-serif 34 'bold 'italic)))
	   (clamp-themes-menu-item
	     (menu-item "Clamp theme pattern"
	       (lambda (item) (tell *control-panel* 'theme-edit-mode-on))))
	   (options:clamp-codelets-menu
	     (create-submenu "Clamp codelet pattern"
	       (clamp-codelets-menu-item "Top-down codelet pattern" 'top-down)
	       (clamp-codelets-menu-item "Bottom-up codelet pattern" 'bottom-up)
	       (clamp-codelets-menu-item "Rule codelet pattern" 'rule)
	       (clamp-codelets-menu-item "Bridge codelet pattern" 'bridge)
	       (clamp-codelets-menu-item "Group codelet pattern" 'group)))
	   (undo-clamp-menu-item
	     (menu-item "Undo last clamp"
	       (lambda (item) (tell *trace* 'undo-last-clamp))))
	   (self-watching-mode-menu-item
	     (check-menu-item "Self-watching mode" %self-watching-enabled%
	       (lambda (item)
		 (set! %self-watching-enabled% (not %self-watching-enabled%))
		 (if %self-watching-enabled%
		   (begin
		     (hide self-watching-warning-label)
		     (send clamp-themes-menu-item set-enabled! #t)
		     (send options:clamp-codelets-menu set-enabled! #t)
		     (send undo-clamp-menu-item set-enabled! #t)
		     (for* each controller in theme-window-controllers do
		       (tell controller 'show)))
		   (begin
		     (show self-watching-warning-label)
		     (send clamp-themes-menu-item set-enabled! #f)
		     (send options:clamp-codelets-menu set-enabled! #f)
		     (send undo-clamp-menu-item set-enabled! #f)
		     (tell *trace* 'undo-last-clamp)
		     (delete-themes)
		     (for* each controller in theme-window-controllers do
		       (tell controller 'hide)))))))
	   (options-menu
	     (create-options-menu
	       (menu-item "Set breakpoint" set-breakpoint-action)
	       (menu-item "Clear breakpoint" clear-breakpoint-action)
	       (menu-item "Step mode interval" set-step-interval-action)
	       (menu-item-separator)
	       (check-menu-item "Eliza mode" %eliza-mode%
		 (lambda (item)
		   (set! %eliza-mode% (not %eliza-mode%))
		   (tell *comment-window* 'switch-modes)))
	       (check-menu-item "Slipnet graphics" %slipnet-graphics%
		 (lambda (item)
		   (set! %slipnet-graphics% (not %slipnet-graphics%))
		   (if* (not *display-mode?*)
		     (if %slipnet-graphics%
		       (tell *slipnet-window* 'restore-current-state)
		       (tell *slipnet-window* 'blank-window)))))
	       (check-menu-item "Coderack graphics" %coderack-graphics%
		 (lambda (item)
		   (set! %coderack-graphics% (not %coderack-graphics%))
		   (if* (not *display-mode?*)
		     (if %coderack-graphics%
		       (tell *coderack-window* 'restore-current-state)
		       (tell *coderack-window* 'blank-window "Coderack")))))
	       (check-menu-item "Show codelet counts" %codelet-count-graphics%
		 (lambda (item)
		   (set! %codelet-count-graphics% (not %codelet-count-graphics%))
		   (tell *coderack-window* 'initialize)))
	       (check-menu-item "Show last codelet type" %highlight-last-codelet%
		 (lambda (item)
		   (set! %highlight-last-codelet% (not %highlight-last-codelet%))
		   (if* (and %coderack-graphics% (not *display-mode?*))
		     (if %highlight-last-codelet%
		       (tell *coderack-window* 'highlight-last-codelet)
		       (tell *coderack-window* 'unhighlight-last-codelet)))))
	       self-watching-mode-menu-item
	       (check-menu-item "Verbose mode" %verbose%
		 (lambda (item) (tell *control-panel* 'toggle-verbose-mode)))
	       (menu-item-separator)
	       clamp-themes-menu-item
	       options:clamp-codelets-menu
	       undo-clamp-menu-item
	       (menu-item-separator)
	       (submenu-anchor "Commentary font face" options:comment-font-face-menu)
	       (submenu-anchor "Commentary font size" options:comment-font-size-menu)
	       (menu-item "Save commentary to file" save-commentary-action)))
	   (main-menu
	     (if (eq? *platform* 'macintosh)
	       (create-menu
		 (submenu-anchor "Demos" demos-menu %gui-menubar-font%)
		 (submenu-anchor "Windows" windows-menu %gui-menubar-font%)
		 (submenu-anchor "Options" options-menu %gui-menubar-font%))
	       (create-menu
		 (menu-item "Help" help-action %gui-menubar-font%)
		 (submenu-anchor "Demos" demos-menu %gui-menubar-font%)
		 (submenu-anchor "Windows" windows-menu %gui-menubar-font%)
		 (submenu-anchor "Options" options-menu %gui-menubar-font%)
		 (nop-menu-item)
		 (clear-memory-menu-item "Clear Memory" %gui-menubar-font%)))))
      (send control-panel set-menu! main-menu)
      (pack top-border (fill: 'x))
      (pack-hspace control-panel 15 'left =white=)
      (pack-hspace control-panel 15 'right =white=)
      (pack info-label)
      (pack-vspace control-panel 5 'top =white=)
      (pack command-line)
      (pack-vspace control-panel 15 'top =white=)
      (pack speed-slider (side: 'left) (anchor: 'n))
      (pack-hspace speed-controls 10 'left %gui-speed-controls-color%)
      (pack step-button (side: 'left) (anchor: 'n))
      (pack go-button (side: 'left) (anchor: 'n))
      (pack stop-button (side: 'left) (anchor: 'n))
      (pack reset-button (side: 'left) (anchor: 'n))
      (show speed-controls)
      (pack breakpoint-label)
      (pack self-watching-warning-label)
      (if %self-watching-enabled%
	(begin
	  (hide self-watching-warning-label))
	(begin
	  (send clamp-themes-menu-item set-enabled! #f)
	  (send options:clamp-codelets-menu set-enabled! #f)
	  (send undo-clamp-menu-item set-enabled! #f)))
      (set-comment-font-menu-actions
	options:comment-font-face-menu
	options:comment-font-size-menu)
      (send command-line set-focus)
      (let ((demos-button (get-demos-button main-menu))
	    (options-button (get-options-button main-menu))
	    (clearmem-button (get-clearmem-button main-menu))
	    (clearmem-dialog #f)
	    (theme-edit-dialog #f)
	    (edited-theme-types '())
	    (saved-theme-states '())
	    (verbose-mode? %verbose%)
	    (problem #f))
	;; control panel object:
	(lambda msg
	  (let ((self (1st msg)))
	    (record-case (rest msg)
	      (object-type () 'control-panel)
	      (problem-exists? () (exists? problem))
	      (get-current-problem () problem)
	      (set-position (x y)
		(send control-panel set-geometry! (format "+~a+~a" x y)))
	      (get-relative-position (x-offset y-offset)
		(let* ((geometry (send control-panel get-geometry))
		       (len (string-length geometry))
		       (ix (char-index #\+ geometry))
		       (iy (+ ix 1 (char-index #\+ (substring geometry (+ ix 1) len))))
		       (x (string->number (substring geometry ix iy)))
		       (y (string->number (substring geometry iy len))))
		  (format "+~a+~a" (+ x x-offset) (+ y y-offset))))
	      (get-command-line-string () (send command-line get-string))
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
		(send command-line delete-all)
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
	      (switch-to-run-mode ()
		(send command-line delete-all)
		(send command-line set-font! %gui-run-mode-font%)
		(send command-line set-foreground-color! =green=)
		(send command-line set-background-color! =black=)
		(send command-line set-justify! 'center)
		(send command-line insert "running...")
		(send command-line set-action! nop-event-handler)
		(send command-line set-enabled! #f)
		(send step-button set-enabled! #f)
		(send go-button set-enabled! #f)
		(send stop-button set-enabled! #t)
		(send reset-button set-enabled! #f)
		(send demos-button set-enabled! #f)
		(send options-button set-enabled! #f)
		(send clearmem-button set-enabled! #f)
		(send stop-button set-relief! 'raised))
	      (switch-to-input-mode ()
		(send command-line set-enabled! #t)
		(send command-line set-action! go-button-action)
		(send command-line delete-all)
		(send command-line set-font! %gui-command-line-font%)
		(send command-line set-foreground-color! =black=)
		(send command-line set-background-color! %gui-command-line-color%)
		(send command-line set-justify! 'left)
		(send step-button set-enabled! #t)
		(send go-button set-enabled! #t)
		(send stop-button set-enabled! #f)
		(send reset-button set-enabled! #t)
		(send demos-button set-enabled! #t)
		(send options-button set-enabled! #t)
		(send clearmem-button set-enabled! #t)
		(send stop-button set-relief! 'raised))
	      (switch-to-disabled-mode ()
		(send command-line set-action! nop-event-handler)
		(send command-line set-enabled! #f)
		(send step-button set-enabled! #f)
		(send go-button set-enabled! #f)
		(send stop-button set-enabled! #f)
		(send reset-button set-enabled! #f)
		(send demos-button set-enabled! #f)
		(send options-button set-enabled! #f)
		(send clearmem-button set-enabled! #f)
		(send stop-button set-relief! 'raised))
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
		(send breakpoint-label set-title!
		  (format "Breakpoint set for time step ~a" *break-time*)))
	      (clear-breakpoint-message ()
		(send breakpoint-label set-title! ""))
	      (display (message)
		(send info-label set-title! message))
	      (display-error (message)
		(let ((current-message (send info-label get-title))
		      (current-width (send top-border get-width)))
		  ;; temporarily freeze the top border at the current width to
		  ;; avoid window shrinkage in case title is longer than usual
		  (send top-border set-width! current-width)
		  (send info-label set-foreground-color! =red=)
		  (send info-label set-title! message)
		  (pause 700)
		  (send info-label set-foreground-color! =black=)
		  (send info-label set-title! current-message)
		  (send top-border set-width! 350)))
	      (hide-window (toplevel)
		(for* each controller in window-controllers do
		  (if* (eq? (tell controller 'get-toplevel) toplevel)
		    (tell controller 'hide))))
	      (raise ()
		(send control-panel raise)
		(if* (exists? clearmem-dialog)
		  (send clearmem-dialog raise)))
	      (else (delegate msg base-object)))))))))

;;------------------------------------------------------------------------------------
;; menus

(define create-menu
  (lambda items
    (create <menu> items)))

(define create-submenu
  (lambda (text . items)
    (submenu-anchor text (apply create-menu items))))

(define submenu-anchor
  (lambda (text submenu . font)
    (create <cascade-menu-item> with
      (title: text)
      (background-color: %gui-menu-background-color%)
      (font: (if (null? font) %gui-menu-item-font% (car font)))
      (menu: submenu))))

(define menu-item-separator
  (lambda ()
    (create <separator-menu-item> with
      (background-color: %gui-menu-background-color%))))

(define nop-menu-item
  (lambda ()
    (create <command-menu-item> with
      (title: "  ")
      (background-color: %gui-menu-background-color%)
      (enabled: #f))))

(define menu-item
  (lambda (text action-proc . font)
    (create <command-menu-item> with
      (title: text)
      (background-color: %gui-menu-background-color%)
      (font: (if (null? font) %gui-menu-item-font% (car font)))
      (action: action-proc))))

(define check-menu-item
  (lambda (text selected? action-proc)
    (create <check-menu-item> with
      (title: text)
      (foreground-color:
	(if selected?
	  %gui-menu-item-on-color%
	  %gui-menu-item-off-color%))
      (background-color: %gui-menu-background-color%)
      (select-color: %gui-checkbox-select-color%)
      (selected: selected?)
      (font: %gui-menu-item-font%)
      (action:
	(lambda (item)
	  (send item set-foreground-color!
	    (if (send item get-selected)
	      %gui-menu-item-on-color%
	      %gui-menu-item-off-color%))
	  (action-proc item))))))

(define clear-memory-menu-item
  (lambda (text font)
    (create <command-menu-item> with
      (title: text)
      (font: font)
      (background-color: %gui-menu-background-color%)
      (active-foreground-color: =red=)
      (action: (lambda (item) (tell *control-panel* 'clear-memory))))))

(define demo-menu-item
  (lambda (text problem)
    (create <command-menu-item> with
      (title: text)
      (background-color: %gui-menu-background-color%)
      (font: %gui-menu-item-font%)
      (action: (lambda (item)
		 (tell *control-panel* 'init-new-problem problem #f)
		 (set-menu-item-color item))))))

;; in Mac OS X, periods in menu labels don't show up for some reason
(define figure
  (lambda (m n . opt)
    (let ((separator (if (eq? *platform* 'macintosh) "-" ".")))
      (format "Figure ~a~a~a~a" m separator n
	(if (null? opt) "" (format " (~a)" (car opt)))))))

(define get-demos-button
  (lambda (main-menu)
    (if (eq? *platform* 'macintosh)
      (1st (send main-menu get-menu-items))
      (2nd (send main-menu get-menu-items)))))

(define get-options-button
  (lambda (main-menu)
    (if (eq? *platform* 'macintosh)
      (3rd (send main-menu get-menu-items))
      (4th (send main-menu get-menu-items)))))

(define get-clearmem-button
  (lambda (main-menu)
    (if (eq? *platform* 'macintosh)
      (1st (send (send (3rd (send main-menu get-menu-items)) get-menu) get-menu-items))
      (6th (send main-menu get-menu-items)))))

(define create-options-menu
  (lambda items
    (let ((all-options
	   (if (eq? *platform* 'macintosh)
	     (cons (clear-memory-menu-item "Clear Memory" %gui-menu-item-font%)
	       (cons (menu-item "Help" help-action %gui-menu-item-font%)
		 items))
	     items)))
      (apply create-menu all-options))))

(define create-windows-menu
  (lambda (window-controllers)
    (let* ((show-all
	     (lambda (item)
	       (for* each controller in window-controllers do
		 (tell controller 'show))))
	   (hide-all
	     (lambda (item)
	       (for* each controller in window-controllers do
		 (tell controller 'hide))))
	   (all-menu-items
	     (append
	       (tell-all window-controllers 'get-menu-item)
	       (list
		 (menu-item-separator)
		 (menu-item "Show all windows" show-all)
		 (menu-item "Hide all windows" hide-all)))))
      (for* each controller in window-controllers do
	(tell controller 'initialize))
      (create <menu> all-menu-items))))

(define window-controller
  (lambda (text window visible?)
    (let ((toplevel
	    (if (eq? window *mcat-logo*)
	      (send (send *mcat-logo* get-parent) get-parent)
	      (tell window 'get-toplevel)))
	  (menu-item
	    (create <command-menu-item> with
	      (font: %gui-menu-item-font%)
	      (background-color: %gui-menu-background-color%))))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'window-controller)
	    (get-menu-item () menu-item)
	    (get-toplevel () toplevel)
	    (initialize ()
	      (send menu-item set-action!
		(lambda (item) (tell self 'toggle)))
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
		  (send menu-item set-title! (format "Hide ~a" text))
		  (send menu-item set-foreground-color!
		    %gui-menu-item-on-color%)
		  (send menu-item set-active-foreground-color!
		    %gui-menu-item-on-color%)
		  (if* (not (eq? window *mcat-logo*))
		    (tell window 'restore-position))
		  (show toplevel))
		(begin
		  (send menu-item set-title! (format "Show ~a" text))
		  (send menu-item set-foreground-color!
		    %gui-menu-item-off-color%)
		  (send menu-item set-active-foreground-color!
		    %gui-menu-item-off-color%)
		  (if* (not (eq? window *mcat-logo*))
		    (tell window 'remember-position))
		  (hide toplevel))))
	    (else (delegate msg base-object))))))))

(define comment-font-menu-item
  (lambda (highlight? text face size . style)
    (let ((item (create <command-menu-item> with
		  (background-color: %gui-menu-background-color%)
		  (font: (swl-font face size style))
		  (title: text))))
      (if* highlight? (set-menu-item-color item))
      item)))

(define set-comment-font-menu-actions
  (lambda (face-menu size-menu)
    (for* each face-item in (send face-menu get-menu-items) do
      (if* (not (isa? face-item <separator-menu-item>))
	(send face-item set-action!
	  (lambda (item)
	    (let ((f (send item get-font)))
	      (set! %comment-window-font%
		(make-mfont (send f get-family) (send f get-size) (send f get-style)))
	      (tell *comment-window* 'new-font %comment-window-font%)
	      (update-menu-fonts
		size-menu (send f get-family) 'same (send f get-style))
	      (unhighlight-menu-items face-menu)
	      (set-menu-item-color item))))))
    (for* each size-item in (send size-menu get-menu-items) do
      (send size-item set-action!
	(lambda (item)
	  (let ((f (send item get-font)))
	    (set! %comment-window-font%
	      (make-mfont (send f get-family) (send f get-size) (send f get-style)))
	    (tell *comment-window* 'new-font %comment-window-font%)
	    (update-menu-fonts
	      face-menu 'same (send f get-size) 'same)
	    (unhighlight-menu-items size-menu)
	    (set-menu-item-color item)))))))

(define update-menu-fonts
  (lambda (menu new-face new-size new-style)
    (for* each item in (send menu get-menu-items) do
      (if* (not (isa? item <separator-menu-item>))
	(let* ((font (send item get-font))
	       (face (if (eq? new-face 'same) (send font get-family) new-face))
	       (size (if (eq? new-size 'same) (send font get-size) new-size))
	       (style (if (eq? new-style 'same) (send font get-style) new-style)))
	  (send item set-font! (swl-font face size style)))))))

(define unhighlight-menu-items
  (lambda (menu)
    (for* each item in (send menu get-menu-items) do
      (if (isa? item <cascade-menu-item>)
	(unhighlight-menu-items (send item get-menu))
	(send item set-background-color! %gui-menu-background-color%)))))

(define clamp-codelets-menu-item
  (lambda (text structure-type)
    (let ((pattern
	    (case structure-type
	      (top-down %top-down-codelet-pattern%)
	      (bottom-up %bottom-up-codelet-pattern%)
	      (group (against-background %very-low-urgency% %group-codelet-pattern%))
	      (bridge (against-background %very-low-urgency% %bridge-codelet-pattern%))
	      (rule (against-background %very-low-urgency% %rule-codelet-pattern%)))))
      (menu-item text
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
