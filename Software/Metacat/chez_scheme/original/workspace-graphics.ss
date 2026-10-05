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

(define %workspace-arrow-length% 7/200)

(define select-workspace-fonts
  (lambda (win-width win-height)
    (let ((desired-title-height (round (* 29/600 win-height)))
	  (desired-count-height (round (* 15/600 win-height)))
	  (desired-letter-height (round (* 28/600 win-height)))
	  (desired-group-lcat-height (round (* 21/600 win-height)))
	  (desired-relevant-group-height (round (* 17/600 win-height)))
	  (desired-irrelevant-group-height (round (* 13/600 win-height)))
	  (desired-bridge-label-height (round (* 13/600 win-height)))
	  (desired-relevant-cmap-height (round (* 13/600 win-height)))
	  (desired-irrelevant-cmap-height (round (* 13/600 win-height)))
	  (desired-cmap-superscript-height (round (* 11/600 win-height)))
	  (desired-rule-height (round (* 15/600 win-height))))
      (set! %workspace-title-font%
	(make-mfont sans-serif (- desired-title-height) '(bold italic)))
      (set! %codelet-count-font%
	(make-mfont sans-serif (- desired-count-height) '(italic)))
      (set! %letter-font%
	(make-mfont serif (- desired-letter-height) '(bold italic)))
      (set! %group-letter-category-font%
	(make-mfont serif (- desired-group-lcat-height) '(bold italic)))
      (set! %relevant-group-length-font%
	(make-mfont serif (- desired-relevant-group-height) '(bold italic)))
      (set! %irrelevant-group-length-font%
	(make-mfont serif (- desired-irrelevant-group-height) '(normal)))
      (set! %bridge-label-font%
	(make-mfont sans-serif (- desired-bridge-label-height) '(italic)))
      (set! %relevant-concept-mapping-font%
	(make-mfont serif (- desired-relevant-cmap-height) '(bold italic)))
      (set! %irrelevant-concept-mapping-font%
	(make-mfont serif (- desired-irrelevant-cmap-height) '(italic)))
      (set! %concept-mapping-list-superscript-font%
	(make-mfont sans-serif (- desired-cmap-superscript-height) '(normal)))
      (set! %rule-font%
	(make-mfont serif (- desired-rule-height) '(italic))))))

(define resize-workspace-fonts
  (lambda (win-width win-height)
    (tell %workspace-title-font% 'resize (round (* 29/600 win-height)))
    (tell %codelet-count-font% 'resize (round (* 15/600 win-height)))
    (tell %letter-font% 'resize (round (* 28/600 win-height)))
    (tell %group-letter-category-font% 'resize (round (* 21/600 win-height)))
    (tell %relevant-group-length-font% 'resize (round (* 17/600 win-height)))
    (tell %irrelevant-group-length-font% 'resize (round (* 13/600 win-height)))
    (tell %bridge-label-font% 'resize (round (* 13/600 win-height)))
    (tell %relevant-concept-mapping-font% 'resize (round (* 13/600 win-height)))
    (tell %irrelevant-concept-mapping-font% 'resize (round (* 13/600 win-height)))
    (tell %concept-mapping-list-superscript-font% 'resize (round (* 11/600 win-height)))
    (tell %rule-font% 'resize (round (* 15/600 win-height)))))

(define workspace-window-press-handler
  (lambda (win x y)
    (cond
      (*running?* (set! *interrupt?* #t))
      (*theme-edit-mode?* (tell *control-panel* 'raise-theme-edit-dialog))
      (*display-mode?* (restore-current-state))
      (else (thread-break *repl-thread* #f go)))))

(define restore-current-state
  (lambda ()
    (tell *trace* 'unhighlight-all-events)
    (tell *memory* 'unhighlight-all-answers)
    (tell *workspace-window* 'redraw)
    (tell *themespace* 'restore-current-state)
    (tell *slipnet-window* 'restore-current-state)
    (tell *coderack-window* 'restore-current-state)
    (tell *temperature-window* 'update-graphics *temperature*)))

(define make-workspace-window
  (lambda optional-args
    (let* ((width
	     (if (null? optional-args)
	       %default-workspace-width%
	       (1st optional-args)))
	   (window (new-workspace-window width)))
      (tell window 'initialize)
      window)))

(define new-workspace-window
  (lambda (x-pixels)
    (let* ((window-height 3/4)
	   (y-pixels (ceiling (* window-height x-pixels)))
	   (graphics-window
	     (make-unscrollable-graphics-window x-pixels y-pixels
	       %workspace-background-color%)))
      (tell graphics-window 'set-mouse-handlers workspace-window-press-handler #f)
      (tell graphics-window 'set-icon-label %workspace-icon-label%)
      (if* (exists? %workspace-icon-image%)
	(tell graphics-window 'set-icon-image %workspace-icon-image%))
      (if* (exists? %workspace-window-title%)
	(tell graphics-window 'set-window-title %workspace-window-title%))
      (select-workspace-fonts x-pixels y-pixels)
      (let* ((letter-x-width
	       (tell graphics-window 'get-character-width "x" %letter-font%))
	     (letter-height
	       (tell graphics-window 'get-character-height " " %letter-font%))
	     (pixel-value (tell graphics-window 'get-width-per-pixel))
	     (max-string-field-width 3/10)
	     (max-gap-width (* 4 letter-x-width))
	     (max-spanning-group-height (* 16/5 letter-height))
	     (max-spanning-vertical-bridge-offset 3/100)
	     (left-center 1/4)
	     (right-center 3/4)
	     (top-line (- window-height 7/25))
	     (bottom-line (- window-height 57/100))
	     (title-y
	       (- window-height (* 16/9 (tell graphics-window 'get-string-height
					  %workspace-title-font%))))
	     (title-coord `(1/2 ,title-y))
	     (codelet-count-text-x
	       (- 1/2 (* 1/2 (tell graphics-window 'get-string-width
			       "(Codelets run: 888)" %codelet-count-font%))))
	     (codelet-count-digits-x
	       (+ codelet-count-text-x (tell graphics-window 'get-string-width
					 "(Codelets run: " %codelet-count-font%)))
	     (codelet-count-y
	       (- title-y (* 3/2 (tell graphics-window 'get-string-height
				   %codelet-count-font%))))
	     (length-previously-relevant? #f)
	     (last-length-font #f)
	     (spanning-vertical-bridge-right-x #f)
	     (spanning-cm-list-offset 1/50)
	     (cm-list-x-origin #f)
	     (cm-list-y-origin (- bottom-line (* 7/10 max-spanning-group-height)))
	     (cm-list-width (* 11 (tell graphics-window 'get-character-width "m"
				    %relevant-concept-mapping-font%)))
	     (draw-workspace-arrow
	       (lambda (line)
		 (tell graphics-window 'draw
		   (centered-double-arrow
		     1/2 (+ line (* 1/3 letter-height))
		     0 %workspace-arrow-length%
		     (* 1/7 %workspace-arrow-length%)
		     (* 3/7 %workspace-arrow-length%) 60))))
	     (question-mark
	       (lambda (op mark . optional-args)
		 (let ((pexp `(let-sgl ((font ,%letter-font%)
					(text-justification center))
				(text (,right-center ,bottom-line) ,mark))))
		   (case op
		     (draw
		       (if* (not (null? optional-args))
			 (set! *fg-color* (1st optional-args)))
		       (tell graphics-window 'draw pexp)
		       (set! *fg-color* %default-fg-color%))
		     (erase
		       (tell graphics-window 'erase pexp))))
		 'done)))
	(lambda msg
	  (let ((self (1st msg)))
	    (record-case (rest msg)
	      (object-type () 'workspace-window)

	      (get-spanning-vertical-bridge-right-x () spanning-vertical-bridge-right-x)

	      (get-spanning-cm-list-offset () spanning-cm-list-offset)

	      (get-rule-coord (rule-type rule-height)
		`(,right-center ,(- (case rule-type
				      (top top-line)
				      (bottom bottom-line))
				    ;; Fudge to make the rule borders even:
				    (+ (* 1/2 max-spanning-group-height) pixel-value)
				    (* 1/2 rule-height))))

	      (get-cm-list-coord (label-num)
                `(,(+ cm-list-x-origin (* (sub1 label-num) cm-list-width))
		  ,cm-list-y-origin))

	      (update-graphics ()
		(tell graphics-window 'caching-on)
		(tell self 'repair-built-bridges)
		(tell self 'update-group-length-graphics)
		(tell self 'update-concept-mapping-graphics)
		(tell self 'update-codelet-count *codelet-count*)
		(tell graphics-window 'flush))

	      (draw-header (text)
		(tell graphics-window 'draw
		  `(let-sgl ((font ,%workspace-title-font%)
			     (text-justification center))
		     (text ,title-coord ,text))))

	      (draw-event-header (text event)
		(let ((event-number (tell event 'get-event-number)))
		  (tell graphics-window 'draw
		    `(let-sgl ((font ,%workspace-title-font%)
			       (text-justification center))
		       (text ,title-coord
			 ,(format "Event ~a:  ~a" event-number text))))))

	      ;; draw-codelet-count draws "(Codelets run: nnn)"
	      (draw-codelet-count (codelet-count)
		(tell graphics-window 'draw
		  `(let-sgl ((font ,%codelet-count-font%))
		     (text (,codelet-count-text-x ,codelet-count-y)
		       "(Codelets run:")))
		(tell self 'update-codelet-count codelet-count))

	      ;; update-codelet-count draws "nnn)"
	      (update-codelet-count (n)
		(tell graphics-window 'draw
		  `(let-sgl ((font ,%codelet-count-font%)
			     (text-mode image))
		     (text (,codelet-count-digits-x ,codelet-count-y)
		       ,(format "~a)     " n)))))

	      (draw-bridge (bridge)
		(case (tell bridge 'get-orientation)
		  (horizontal
		    (tell graphics-window 'draw (tell bridge 'get-graphics-pexp)))
		  (vertical
		    (tell graphics-window 'draw
		      `(let-sgl ((background-color ,%bridge-label-background-color%)
				 (text-mode image))
			 ,(tell bridge 'get-graphics-pexp)))
		    (if* (tell bridge 'concept-mapping-graphics-active?)
		      (if* (not (tell bridge 'group-spanning-bridge?))
			(tell self 'concept-mapping-list-superscript 'draw bridge))
		      (for* each cm in (tell bridge 'get-all-concept-mappings) do
			(tell self 'draw-concept-mapping cm)))))
		(tell bridge 'set-drawn? #t))

	      (erase-bridge (bridge)
		(case (tell bridge 'get-orientation)
		  (horizontal
		    (tell graphics-window 'erase (tell bridge 'get-graphics-pexp)))
		  (vertical
		    (tell graphics-window 'erase
		      `(let-sgl ((text-mode image))
			 ,(tell bridge 'get-graphics-pexp)))
		    (if* (tell bridge 'concept-mapping-graphics-active?)
		      (if* (not (tell bridge 'group-spanning-bridge?))
			(tell self 'concept-mapping-list-superscript 'erase bridge))
		      (for* each cm in (tell bridge 'get-all-concept-mappings) do
			(tell self 'erase-concept-mapping cm)))))
		(tell bridge 'set-drawn? #f))

	      ;; Concept-mappings
	      (draw-concept-mapping (cm)
                (let* ((pexp (tell cm 'get-graphics-pexp))
		       (currently-relevant? (tell cm 'relevant?))
		       (font (if currently-relevant?
			       %relevant-concept-mapping-font%
			       %irrelevant-concept-mapping-font%)))
		  (tell graphics-window 'draw `(let-sgl ((font ,font)) ,pexp))
		  (tell cm 'update-previously-relevant? currently-relevant?)))

	      (erase-concept-mapping (cm)
		(let* ((pexp (tell cm 'get-graphics-pexp))
		       (previously-relevant? (tell cm 'previously-relevant?))
		       (font (if previously-relevant?
			       %relevant-concept-mapping-font%
			       %irrelevant-concept-mapping-font%)))
		  (tell graphics-window 'erase `(let-sgl ((font ,font)) ,pexp))))

	      (concept-mapping-list-superscript (op bridge)
                (tell graphics-window op
		  `(let-sgl ((font ,%concept-mapping-list-superscript-font%)
			     (text-justification right)
			     (origin ,(tell bridge 'get-cm-list-coord)))
		     (text (text-relative (0 +1/3))
		       ,(format "~a" (tell bridge 'get-bridge-label-number))))))

	      (update-concept-mapping-graphics ()
		(for* each cm in (tell *workspace* 'get-all-vertical-CMs) do
		  (let ((currently-relevant? (tell cm 'relevant?))
			(previously-relevant? (tell cm 'previously-relevant?)))
		    (if* (not (eq? currently-relevant? previously-relevant?))
		      (let ((current-font (if currently-relevant?
					    %relevant-concept-mapping-font%
					    %irrelevant-concept-mapping-font%))
			    (last-font (if previously-relevant?
					 %relevant-concept-mapping-font%
					 %irrelevant-concept-mapping-font%))
			    (pexp (tell cm 'get-graphics-pexp)))
			(tell graphics-window 'erase
			  `(let-sgl ((font ,last-font)) ,pexp))
			(tell graphics-window 'draw
			  `(let-sgl ((font ,current-font)) ,pexp))
			(tell cm 'update-previously-relevant? currently-relevant?)))))
		'done)

	      ;; Letters and groups
	      (draw-all-letters ()
		(for* each letter in (tell *workspace* 'get-all-letters) do
		  (tell graphics-window 'draw (tell letter 'get-graphics-pexp)))
		'done)

	      (draw-group (group)
                (tell graphics-window 'draw (tell group 'get-graphics-pexp))
		(if* (tell group 'length-graphics-active?)
		  (tell graphics-window 'draw
		    `(let-sgl ((font ,last-length-font))
		       ,(tell group 'get-length-graphics-pexp))))
		(tell group 'set-drawn? #t))

	      (erase-group (group)
                (tell graphics-window 'erase (tell group 'get-graphics-pexp))
		(if* (tell group 'length-graphics-active?)
		  (tell graphics-window 'erase
		    `(let-sgl ((font ,last-length-font))
		       ,(tell group 'get-length-graphics-pexp))))
		(tell group 'set-drawn? #f))

	      (draw-group-length (group)
		(tell graphics-window 'draw
		  `(let-sgl ((font ,last-length-font))
		     ,(tell group 'get-length-graphics-pexp))))

	      (update-group-length-graphics ()
		(let ((length-currently-relevant? (fully-active? plato-length)))
		  (if* (not (eq? length-currently-relevant? length-previously-relevant?))
		    (let ((current-length-font (if length-currently-relevant?
						 %relevant-group-length-font%
						 %irrelevant-group-length-font%)))
		      (for* each group in (tell *workspace* 'get-groups) do
			(if* (tell group 'length-graphics-enabled?)
			  (let ((pexp (tell group 'get-length-graphics-pexp)))
			    (tell graphics-window 'erase
			      `(let-sgl ((font ,last-length-font)) ,pexp))
			    (tell graphics-window 'draw
			      `(let-sgl ((font ,current-length-font)) ,pexp)))))
		      (set! length-previously-relevant? length-currently-relevant?)
		      (set! last-length-font current-length-font))))
		'done)

	      ;; Rules
	      (draw-rule (rule color)
		(tell graphics-window 'draw
		  `(let-sgl ((foreground-color ,color))
		     ,(tell rule 'get-graphics-pexp)))
		(tell rule 'set-drawn? #t))

	      (erase-rule (rule)
                (tell graphics-window 'erase (tell rule 'get-graphics-pexp))
		(tell rule 'set-drawn? #f))

	      ;; String letters
	      (draw-string-letters (string)
		(for* each letter in (tell string 'get-letters) do
		  (tell graphics-window 'draw (tell letter 'get-graphics-pexp)))
		'done)

	      (erase-string-letters (string)
		(for* each letter in (tell string 'get-letters) do
		  (tell graphics-window 'erase (tell letter 'get-graphics-pexp)))
		'done)

	      (draw-in-color (object color)
		(set! *fg-color* color)
		(cond
		  ((group? object) (tell self 'draw-group object))
		  ((concept-mapping? object) (tell self 'draw-concept-mapping object))
		  ((vertical-bridge? object)
		   (tell graphics-window 'draw (tell object 'get-graphics-pexp))
		   (if* (not (tell object 'group-spanning-bridge?))
		     ;; graphics-pexp is (let-sgl () <zigzag-pexp> <label-pexp>).
		     ;; Draw the label in the default color on a yellow background:
		     (set! *fg-color* %default-fg-color%)
		     (tell graphics-window 'draw
		       `(let-sgl ((background-color ,%bridge-label-background-color%)
				  (text-mode image))
			  ,(4th (tell object 'get-graphics-pexp))))))
		  (else (tell graphics-window 'draw (tell object 'get-graphics-pexp))))
		(set! *fg-color* %default-fg-color%)
		'done)

	      (initialize ()
		(tell graphics-window 'caching-on)
                (tell graphics-window 'clear)
		(tell self 'draw-header "Workspace")
		(tell self 'draw-codelet-count 0)
		(set! length-previously-relevant? #f)
		(set! last-length-font %irrelevant-group-length-font%)
		(tell graphics-window 'flush)
		(set! *display-mode?* #f)
		'done)

	      (draw-problem (initial-string modified-string target-string answer-string)
		(tell self 'initialize)
		(tell graphics-window 'caching-on)
		(tell self 'init-string-graphics initial-string left-center 'top)
		(tell self 'init-string-graphics modified-string right-center 'top)
		(tell self 'init-string-graphics target-string left-center 'bottom)
		(if* %justify-mode%
		  (tell self 'init-string-graphics answer-string right-center 'bottom))
		(tell self 'draw-string-letters initial-string)
		(draw-workspace-arrow top-line)
		(tell self 'draw-string-letters modified-string)
		(tell self 'draw-string-letters target-string)
		(draw-workspace-arrow bottom-line)
		(if %justify-mode%
		  (tell self 'draw-string-letters answer-string)
		  (question-mark 'draw "?"))
		(set! cm-list-x-origin
		  (max spanning-cm-list-offset
		       (- left-center
			  (* 1/2 (tell target-string 'get-graphics-width))
			  (* 1/2 cm-list-width))))
		(let* ((spanning-vertical-bridge-left-x
			 (max (tell initial-string 'get-spanning-group-x2)
			      (tell target-string 'get-spanning-group-x2)))
		       (available-space
			 (- (- 1/2 (* 1/2 %workspace-arrow-length%))
			    spanning-vertical-bridge-left-x)))
		  (set! spanning-vertical-bridge-right-x
		    (+ spanning-vertical-bridge-left-x
		       (min (* 1/2 available-space)
			    max-spanning-vertical-bridge-offset))))
		(tell graphics-window 'flush)
		'done)

	      (init-translated-string-graphics (string placement)
		(tell self 'init-string-graphics string right-center placement))

	      (init-string-graphics (string x-center string-placement)
		(let* ((letters (tell string 'get-letters))
		       (num-of-letters (length letters))
		       (letter-widths
			 (map (lambda (letter)
				(tell graphics-window 'get-character-width
				  (tell letter 'print-name) %letter-font%))
			   letters))
		       (total-letter-width (sum letter-widths))
		       (gap-width
			 (if (= num-of-letters 1)
			   0
			   (min max-gap-width
			        (/ (- max-string-field-width total-letter-width)
				   (sub1 num-of-letters)))))
		       (string-width
			 (+ total-letter-width (* (sub1 num-of-letters) gap-width)))
		       (next-x (- x-center (* 1/2 string-width))))
		  (for* each (letter letter-width) in (letters letter-widths) do
		    (let* ((print-name (tell letter 'print-name))
			   (text-coord
			     (case string-placement
			       (top `(,next-x ,top-line))
			       (bottom `(,next-x ,bottom-line))))
			   (bbox (tell graphics-window 'get-character-bounding-box
				   print-name %letter-font% text-coord))
			   (mid-x (* 1/2 (+ (1st (1st bbox)) (1st (2nd bbox)))))
			   (top-y (+ (* -1/6 letter-height) (2nd (2nd bbox))))
			   (bot-y (+ (* 1/6 letter-height) (2nd (1st bbox)))))
		      (tell letter 'set-graphics-pexp
			`(let-sgl ((font ,%letter-font%))
			   (text ,text-coord ,print-name)))
		      (tell letter 'set-graphics-text-coord text-coord)
		      (tell letter 'set-graphics-coords (1st bbox) (2nd bbox))
		      (tell letter 'set-bridge-graphics-coords
			(coord mid-x (case string-placement
				       (top bot-y)
				       (bottom top-y)))
			(coord mid-x top-y))
		      (set! next-x (+ next-x letter-width gap-width))))
		  (let* ((x-enclosure-delta
			   (if (= num-of-letters 1)
			     total-letter-width
			     (/ gap-width (max 3 num-of-letters))))
			 (y-enclosure-delta
			   (if (= num-of-letters 1)
			     total-letter-width
			     (min (* 1/4 letter-height)
			          (/ (- max-spanning-group-height letter-height)
				     (* 2 (sub1 num-of-letters))))))
			 (spanning-group-width
			   (+ string-width
			      (* 2 (sub1 num-of-letters) x-enclosure-delta)))
			 (spanning-group-height
			   (+ letter-height
			      (* 2 (sub1 num-of-letters) y-enclosure-delta)))
			 (y-center
			   (* 1/2 (+ (tell (1st letters) 'get-graphics-y1)
				     (tell (1st letters) 'get-graphics-y2))))
			 (spanning-group-x1 (- x-center (* 1/2 spanning-group-width)))
			 (spanning-group-y1 (- y-center (* 1/2 spanning-group-height)))
			 (spanning-group-x2 (+ x-center (* 1/2 spanning-group-width)))
			 (spanning-group-y2 (+ y-center (* 1/2 spanning-group-height))))
		    (tell string 'set-graphics-info
		      x-enclosure-delta y-enclosure-delta string-width
		      spanning-group-x1 spanning-group-y1
		      spanning-group-x2 spanning-group-y2))))

	      ;; draw-current-answer and draw-current-snag draw concept-mappings
	      ;; and group-lengths in the currently appropriate font (relevant
	      ;; vs. irrelevant):

	      (draw-current-answer ()
		(let* ((answer (tell *trace* 'get-last-event 'answer))
		       (answer-string (tell answer 'get-answer-string))
		       (top-rule (tell answer 'get-rule 'top))
		       (bottom-rule (tell answer 'get-rule 'bottom)))
		  (tell graphics-window 'caching-on)
		  (tell self 'update-codelet-count *codelet-count*)
		  ;; Draw answer string if necessary:
		  (if* (not %justify-mode%)
		    (question-mark 'erase "?")
		    (tell self 'draw-string-letters answer-string)
		    (for* each g in (tell answer-string 'get-groups) do
		      (tell self 'draw-group g)))
		  ;; Highlight vertical mapping:
		  (tell self 'highlight-current-bridges %vertical-bridge-color%
		    (tell answer 'get-supporting-bridges 'vertical))
		  ;; Highlight applied vertical slippages:
		  (tell self 'highlight-current-slippages
		    (tell answer 'get-slippage-log))
		  ;; Highlight top mapping:
		  (tell self 'highlight-current-bridges %top-bridge-color%
		    (tell answer 'get-supporting-bridges 'top))
		  ;; Highlight bottom mapping:
		  (tell self 'highlight-current-bridges %bottom-bridge-color%
		    (tell answer 'get-supporting-bridges 'bottom))
		  ;; Draw rules:
		  (tell self 'draw-rule top-rule %top-rule-color%)
		  (tell self 'draw-rule bottom-rule %bottom-rule-color%)
		  (tell graphics-window 'flush)
		  )
		'done)

	      (erase-current-answer ()
		(let* ((answer (tell *trace* 'get-last-event 'answer))
		       (answer-string (tell answer 'get-answer-string)))
		  (tell graphics-window 'caching-on)
		  (tell self 'erase-rule (tell answer 'get-rule 'top))
		  (tell self 'erase-rule (tell answer 'get-rule 'bottom))
		  ;; Erase bottom mapping if running in normal mode:
		  (if* (not %justify-mode%)
		    (for* each b in (tell answer 'get-supporting-bridges 'bottom) do
		      (tell self 'erase-bridge b))
		    (for* each g in (tell answer-string 'get-groups) do
		      (tell self 'erase-group g))
		    (tell self 'erase-string-letters answer-string)
		    (question-mark 'draw "?"))
		  ;; This unhighlights everything else:
		  (tell self 'repair-all-graphics)
		  (tell graphics-window 'flush)
		  ))

	      (draw-current-snag ()
		(let* ((snag (tell *trace* 'get-last-event 'snag))
		       (rule (tell snag 'get-rule 'top))
		       (translated-rule (tell snag 'get-rule 'bottom)))
		  (tell graphics-window 'caching-on)
		  (tell self 'update-codelet-count *codelet-count*)
		  ;; Highlight vertical mapping:
		  (tell self 'highlight-current-bridges %vertical-bridge-color%
		    (tell snag 'get-supporting-bridges 'vertical))
		  ;; Highlight applied vertical slippages:
		  (tell self 'highlight-current-slippages
		    (tell snag 'get-slippage-log))
		  ;; Highlight top mapping:
		  (tell self 'highlight-current-bridges %top-bridge-color%
		    (tell snag 'get-supporting-bridges 'top))
		  ;; Highlight snag objects:
		  (for* each object in (tell snag 'get-snag-objects) do
		    (if* (not (workspace-string? object))
		      (tell self 'draw-in-color object %snag-color%)))
		  ;; Draw rules:
		  (tell self 'draw-rule rule %top-rule-color%)
		  (tell self 'draw-rule translated-rule %snag-color%)
		  (question-mark 'erase "?")
		  (question-mark 'draw "???" %snag-color%)
		  (tell graphics-window 'flush)
		  ))

	      (erase-current-snag ()
		(let* ((snag (tell *trace* 'get-last-event 'snag))
		       (rule (tell snag 'get-rule 'top))
		       (translated-rule (tell snag 'get-rule 'bottom))
		       (crossout-pexp
			 (make-snag-crossout-pexp
			   translated-rule (ceiling (/ x-pixels 160)))))
		  (tell graphics-window 'caching-on)
		  (tell self 'draw crossout-pexp)
		  (tell graphics-window 'flush)
		  (pause %snag-pause%)
		  (tell graphics-window 'caching-on)
		  (question-mark 'erase "???")
		  (question-mark 'draw "?")
		  (tell self 'erase crossout-pexp)
		  (tell self 'erase-rule translated-rule)
		  (tell self 'erase-rule rule)
		  ;; This unhighlights everything else:
		  (tell self 'repair-all-graphics)
		  (tell graphics-window 'flush)
		  ))

	      (workspace-arrow (line)
		(case line
		  (top (draw-workspace-arrow top-line))
		  (bottom (draw-workspace-arrow bottom-line))))

	      (question-mark (op mark color) (question-mark op mark color))

	      (resize (new-width new-height)
		(let ((old-width x-pixels)
		      (old-height y-pixels))
		  (set! x-pixels new-width)
		  (set! y-pixels new-height)
		  (tell self 'update-fonts)
		  (tell self 'update-rule-pexps)
		  (tell graphics-window 'retag 'all 'garbage)
		  (let ((highlighted-event (tell *trace* 'get-highlighted-event))
			(highlighted-answer (tell *memory* 'get-highlighted-description)))
		    (cond
		      ((exists? highlighted-event)
		       (tell highlighted-event 'display-workspace))
		      ((exists? highlighted-answer)
		       (tell highlighted-answer 'display-workspace))
		      (else (tell self 'refresh-graphics))))
		  (tell graphics-window 'delete 'garbage)
		  ))

	      (update-fonts ()
		(resize-workspace-fonts x-pixels y-pixels)
		(set! title-y
		  (- window-height (* 16/9 (tell graphics-window 'get-string-height
					     %workspace-title-font%))))
		(set! title-coord `(1/2 ,title-y))
		(set! codelet-count-text-x
		  (- 1/2 (* 1/2 (tell graphics-window 'get-string-width
				  "(Codelets run: 888)" %codelet-count-font%))))
		(set! codelet-count-digits-x
		  (+ codelet-count-text-x (tell graphics-window 'get-string-width
					    "(Codelets run: " %codelet-count-font%)))
		(set! codelet-count-y
		  (- title-y (* 3/2 (tell graphics-window 'get-string-height
				      %codelet-count-font%))))
		'done)

	      (update-rule-pexps ()
		(for* each rule in (tell *workspace* 'get-all-rules) do
		  (initialize-rule-graphics rule))
		(let ((other-rules
		       (tell-all (append (tell *trace* 'get-events 'answer)
					 (tell *trace* 'get-events 'snag))
			 'get-rule 'bottom)))
		  (for* each rule in other-rules do
		    (if* (tell rule 'translated?)
		      (initialize-rule-graphics rule))))
		(for* each answer in (tell *memory* 'get-answers) do
		  (update-rule-pexps! (tell answer 'get-answer-description-pexp)))
		(for* each snag in (tell *memory* 'get-snags) do
		  (update-rule-pexps! (tell snag 'get-snag-description-pexp)))
		'done)

	      (redraw ()
		(tell graphics-window 'caching-on)
		(tell graphics-window 'clear)
		(tell self 'refresh-graphics)
		(tell graphics-window 'flush)
		(set! *display-mode?* #f)
		'done)

	      (refresh-graphics ()
		(tell self 'draw-header "Workspace")
		(tell self 'draw-codelet-count *codelet-count*)
		(if* (exists? *this-run*)
		  (tell self 'repair-all-graphics)
		  (cond
		    ((tell *trace* 'current-answer?)
		     (tell self 'draw-current-answer))
		    ((tell *trace* 'immediate-snag-condition?)
		     (tell self 'draw-current-snag))
		    ((not %justify-mode%) (question-mark 'draw "?"))))
		'done)

	      (garbage-collect ()
		(tell graphics-window 'retag 'all 'garbage)
		(tell self 'refresh-graphics)
		(tell graphics-window 'delete 'garbage)
		'done)

	      ;; Repair graphics:
	      (repair-all-graphics ()
		(tell self 'draw-string-letters *initial-string*)
		(draw-workspace-arrow top-line)
		(tell self 'draw-string-letters *modified-string*)
		(tell self 'draw-string-letters *target-string*)
		(draw-workspace-arrow bottom-line)
		(if* %justify-mode%
		  (tell self 'draw-string-letters *answer-string*))
		(for* each g in (tell *workspace* 'get-all-groups) do
		  (if* (tell g 'drawn?)
		    (tell self 'draw-group g)))
		(for* each b in (append
				  (tell *workspace* 'get-all-bridges)
				  (tell *workspace* 'get-all-proposed-bridges)) do
		  (if* (tell b 'drawn?)
		    (tell self 'draw-bridge b)))
		(for* each r in (tell *workspace* 'get-clamped-rules) do
		  (tell graphics-window 'draw (tell r 'get-clamped-graphics-pexp)))
		'done)

	      (repair-built-bridges ()
		(for* each bridge in (tell *workspace* 'get-all-bridges) do
		  (tell graphics-window 'draw
		    (case (tell bridge 'get-bridge-type)
		      ((top bottom) (tell bridge 'get-graphics-pexp))
		      (vertical `(let-sgl ((background-color ,%bridge-label-background-color%)
					   (text-mode image))
				   ,(tell bridge 'get-graphics-pexp))))))
		'done)

	      ;; The following methods are used when displaying structures that
	      ;; do not necessarily exist at the current *codelet-count* time.
	      ;; The group-length and concept-mapping fonts are always the same,
	      ;; rather than being chosen dynamically as relevant vs. irrelevant.

	      (display-in-color (object color)
		(set! *fg-color* color)
		(cond
		  ((group? object) (tell self 'display-group object))
		  ((concept-mapping? object)
		   (tell self 'display-concept-mapping object))
		  ((vertical-bridge? object)
		   (tell graphics-window 'draw (tell object 'get-graphics-pexp))
		   (if* (not (tell object 'group-spanning-bridge?))
		     ;; graphics-pexp is (let-sgl () <zigzag-pexp> <label-pexp>).
		     ;; Draw the label in the default color on a yellow background:
		     (set! *fg-color* %default-fg-color%)
		     (tell graphics-window 'draw
		       `(let-sgl ((background-color ,%bridge-label-background-color%)
				  (text-mode image))
			  ,(4th (tell object 'get-graphics-pexp))))))
		  (else (tell graphics-window 'draw (tell object 'get-graphics-pexp))))
		(set! *fg-color* %default-fg-color%)
		'done)

	      (display-bridge (bridge)
		(case (tell bridge 'get-orientation)
		  (horizontal
		    (tell graphics-window 'draw (tell bridge 'get-graphics-pexp)))
		  (vertical
		    (tell graphics-window 'draw
		      `(let-sgl ((background-color ,%bridge-label-background-color%)
				 (text-mode image))
			 ,(tell bridge 'get-graphics-pexp)))
		    (if* (not (tell bridge 'group-spanning-bridge?))
		      (tell self 'concept-mapping-list-superscript 'draw bridge))
		    (for* each cm in (tell bridge 'get-all-concept-mappings) do
		      (tell self 'display-concept-mapping cm))))
		'done)

	      (display-concept-mapping (cm)
		(let* ((pexp (tell cm 'get-graphics-pexp))
		       (font %relevant-concept-mapping-font%))
		  (tell graphics-window 'draw `(let-sgl ((font ,font)) ,pexp))))

	      (display-group (group)
		(tell graphics-window 'draw (tell group 'get-graphics-pexp))
		(if* (tell group 'length-graphics-active?)
		  (tell graphics-window 'draw
		    `(let-sgl ((font ,%relevant-group-length-font%))
		       ,(tell group 'get-length-graphics-pexp))))
		'done)

	      (display-rule (rule color)
		(tell graphics-window 'draw
		  `(let-sgl ((foreground-color ,color))
		     ,(tell rule 'get-graphics-pexp))))

	      (highlight-bridges (color bridges)
		(for* each b in bridges do
		  (tell self 'display-in-color (tell b 'get-object1) color)
		  (tell self 'display-in-color b color)
		  (tell self 'display-in-color (tell b 'get-object2) color)))

	      (highlight-applied-slippages (slippage-log)
		(for* each s in (tell slippage-log 'get-applied-slippages) do
		  (let ((h (tell slippage-log 'get-slippage-to-highlight s)))
		    (tell self 'display-in-color h
		      (tell slippage-log 'get-highlight-color h s)))))

	      ;; These two methods use 'draw-in-color instead of 'display-in-color:

	      (highlight-current-bridges (color bridges)
		(for* each b in bridges do
		  (tell self 'draw-in-color (tell b 'get-object1) color)
		  (tell self 'draw-in-color b color)
		  (tell self 'draw-in-color (tell b 'get-object2) color)))

	      (highlight-current-slippages (slippage-log)
		(for* each s in (tell slippage-log 'get-applied-slippages) do
		  (let ((h (tell slippage-log 'get-slippage-to-highlight s)))
		    (tell self 'draw-in-color h
		      (tell slippage-log 'get-highlight-color h s)))))

	      (else (delegate msg graphics-window)))))))))


(define make-snag-crossout-pexp
  (lambda (rule line-width)
    (let* ((center-coord (tell rule 'get-rule-graphics-center-coord))
	   (x-length 1/10)
	   (y-length (* 3/2 (tell rule 'get-rule-graphics-height)))
	   (x1 (- (1st center-coord) (* 1/2 x-length)))
	   (y1 (- (2nd center-coord) (* 1/2 y-length)))
	   (x2 (+ (1st center-coord) (* 1/2 x-length)))
	   (y2 (+ (2nd center-coord) (* 1/2 y-length))))
      `(let-sgl ((line-width ,line-width))
	 (line (,x1 ,y1) (,x2 ,y2) (,x1 ,y2) (,x2 ,y1))))))
