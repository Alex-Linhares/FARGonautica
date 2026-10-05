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

(define %memory-answer-icon-font% #f)

(define select-memory-font
  (lambda (win-width win-height)
    (let ((desired-font-height (round (* 1/20 (min win-width win-height)))))
      (set! %memory-answer-icon-font%
	(make-mfont sans-serif (- desired-font-height) '(bold italic))))))

(define resize-memory-font
  (lambda (win-width win-height)
    (tell %memory-answer-icon-font% 'resize (round (* 1/20 (min win-width win-height))))))

(define memory-background-color
  (lambda ()
    (swl-color (string-append "grey" (number->string %memory-background-grey-level%)))))

(define memory-icon-activation-color
  (lambda (activation)
    (let ((grey-level
	    (+ %memory-background-grey-level%
	       (round (* (% activation) (100- %memory-background-grey-level%))))))
      (swl-color (string-append "grey" (number->string grey-level))))))

(define memory-window-press-handler
  (lambda (win x y)
    (if *theme-edit-mode?*
      (tell *control-panel* 'raise-theme-edit-dialog)
      (if* (not *running?*)
	(let ((selected-answer (tell *memory* 'get-mouse-selected-answer x y)))
	  (if* (exists? selected-answer)
	    (tell *trace* 'unhighlight-all-events)
	    (tell selected-answer 'toggle-highlight)
	    (if (tell selected-answer 'highlighted?)
	      (let ((previous-answer
		      (tell *memory* 'get-other-highlighted-answer selected-answer)))
		(if* (and (not (snag-description? selected-answer))
			  (exists? previous-answer))
		  (tell *comment-window* 'add-comment
		    (list "Let's see...")
		    (list "Comparing answers...")))
		(tell *memory* 'unhighlight-all-answers-except selected-answer)
		(tell selected-answer 'display)
		(if* (and (not (snag-description? selected-answer))
			  (exists? previous-answer))
		  (compare-answers selected-answer previous-answer)))
	      (restore-current-state))))))))

(define make-memory-window
  (lambda optional-args
    (let* ((width
	     (if (null? optional-args)
	       %default-memory-width%
	       (1st optional-args)))
	   (height
	     (if (< (length optional-args) 2)
	       %default-memory-height%
	       (2nd optional-args)))
	   (window (new-memory-window width height)))
      (tell window 'initialize)
      window)))

(define new-memory-window
  (lambda (x-pixels y-pixels)
    (let ((graphics-window
	    (make-vertical-scrollable-graphics-window
	      x-pixels y-pixels %virtual-memory-length% (memory-background-color))))
      (tell graphics-window 'set-icon-label %memory-window-icon-label%)
      (if* (exists? %memory-window-icon-image%)
	(tell graphics-window 'set-icon-image %memory-window-icon-image%))
      (if* (exists? %memory-window-title%)
	(tell graphics-window 'set-window-title %memory-window-title%))
      (tell graphics-window 'set-mouse-handlers memory-window-press-handler #f)
      (select-memory-font x-pixels y-pixels)
      (let* ((top-y (tell graphics-window 'get-y-max))
	     (memory-icon-spacing
	       (* 5/4 (tell graphics-window 'get-string-height %memory-answer-icon-font%)))
	     (icon-erase-border (* 1/2 memory-icon-spacing))
	     (next-y (- top-y memory-icon-spacing))
	     (center-x 1/2))
	(lambda msg
	  (let ((self (1st msg)))
	    (record-case (rest msg)
	      (object-type () 'memory-window)
	      (add-memory-icon (answer)
		(let* ((memory-icon-pexp-info
			 (get-memory-icon-pexp-info
			   graphics-window answer center-x next-y))
		       (get-normal-icon-pexp (1st memory-icon-pexp-info))
		       (highlight-icon-pexp (2nd memory-icon-pexp-info))
		       (memory-icon-width (3rd memory-icon-pexp-info))
		       (memory-icon-height (4th memory-icon-pexp-info))
		       (x1 (- center-x (* 1/2 memory-icon-width)))
		       (x2 (+ center-x (* 1/2 memory-icon-width)))
		       (y1 (- next-y memory-icon-height))
		       (y2 next-y))
		  (tell answer 'set-graphics-info
		    get-normal-icon-pexp highlight-icon-pexp)
		  (tell answer 'set-bounding-box x1 y1 x2 y2)
		  (tell graphics-window 'draw
		    (get-normal-icon-pexp (tell answer 'get-activation)))
		  (set! next-y (- next-y memory-icon-height memory-icon-spacing))
		  'done))
	      (erase-memory-icon (answer)
		(tell graphics-window 'erase
		  `(filled-rectangle
		     (,(- (tell answer 'get-bounding-box-x1) icon-erase-border)
		      ,(- (tell answer 'get-bounding-box-y1) icon-erase-border))
		     (,(+ (tell answer 'get-bounding-box-x2) icon-erase-border)
		      ,(+ (tell answer 'get-bounding-box-y2) icon-erase-border)))))
	      (initialize ()
		(tell graphics-window 'clear)
		(resize-memory-font
		  (tell graphics-window 'get-visible-w)
		  (tell graphics-window 'get-visible-h))
		(set! memory-icon-spacing
		  (* 5/4 (tell graphics-window 'get-string-height %memory-answer-icon-font%)))
		(set! next-y (- top-y memory-icon-spacing))
		'done)
	      (resize (new-width new-height)
		(resize-memory-font new-width new-height)
		(set! memory-icon-spacing
		  (* 5/4 (tell graphics-window 'get-string-height %memory-answer-icon-font%)))
		(set! icon-erase-border (* 1/2 memory-icon-spacing))
		(set! next-y (- top-y memory-icon-spacing))
		(tell graphics-window 'retag 'all 'garbage)
		(for* each descrip in (reverse (tell *memory* 'get-all-descriptions)) do
		  (let* ((memory-icon-pexp-info
			  (get-memory-icon-pexp-info
			    graphics-window descrip center-x next-y))
			 (get-normal-icon-pexp (1st memory-icon-pexp-info))
			 (highlight-icon-pexp (2nd memory-icon-pexp-info))
			 (memory-icon-width (3rd memory-icon-pexp-info))
			 (memory-icon-height (4th memory-icon-pexp-info))
			 (x1 (- center-x (* 1/2 memory-icon-width)))
			 (x2 (+ center-x (* 1/2 memory-icon-width)))
			 (y1 (- next-y memory-icon-height))
			 (y2 next-y))
		    (tell descrip 'set-graphics-info
		      get-normal-icon-pexp highlight-icon-pexp)
		    (tell descrip 'set-bounding-box x1 y1 x2 y2)
		  (tell graphics-window 'draw
		    (if (tell descrip 'highlighted?)
		      highlight-icon-pexp
		      (get-normal-icon-pexp (tell descrip 'get-activation))))
		    (set! next-y (- next-y memory-icon-height memory-icon-spacing))))
		(tell graphics-window 'delete 'garbage))
	      (redraw ()
		(tell graphics-window 'caching-on)
		(tell graphics-window 'clear)
		(for* each descrip in (tell *memory* 'get-all-descriptions) do
		  (tell graphics-window 'draw
		    (if (tell descrip 'highlighted?)
		      (tell descrip 'get-highlight-icon-pexp)
		      (tell descrip 'get-normal-icon-pexp))))
		(tell graphics-window 'flush)
		'done)
	      (else (delegate msg graphics-window)))))))))


;; get-memory-icon-pexp-info returns a list of the form:
;; (<get-normal-icon-pexp> <highlight-icon-pexp> <icon-width> <icon-height>)

(define get-memory-icon-pexp-info
  (lambda (g answer center-x next-y)
    (let* ((pixel (tell g 'get-width-per-pixel))
	   (rounded-box-sizing-factor 3/2)
	   (font %memory-answer-icon-font%)
	   (text-string (tell answer 'print-name))
  	   (text-width (tell g 'get-string-width text-string font))
	   (text-height (tell g 'get-string-height font))
	   (icon-height (* rounded-box-sizing-factor text-height))
	   (radius (* 1/2 icon-height))
	   (icon-width (+ text-width (* 2 radius)))
	   (xc center-x)
	   (yc (- next-y (* 1/2 icon-height)))
	   (outline
	     (centered-rounded-box xc yc icon-width icon-height radius))
	   (background
	     (filled-centered-rounded-box xc yc icon-width icon-height radius pixel))
	   (normal-icon-pexp
	     `(let-sgl ((foreground-color ,(memory-background-color))
			(line-width 2))
		,outline
		(let-sgl ((foreground-color ,=black=)
			  (line-width 1))
		  ,outline
		  (let-sgl ((font ,font)
			    (text-justification center)
			    (origin (,xc ,yc)))
		    (text (text-relative (0 -2/5)) ,text-string)))))
	   (get-normal-icon-pexp
	     (lambda (activation)
	       (let ((color (memory-icon-activation-color activation)))
		 `(let-sgl ((foreground-color ,color))
		    ,background
		    ,normal-icon-pexp))))
	   (highlight-icon-pexp
	     `(let-sgl ()
		(let-sgl ((foreground-color ,=black=))
		  ,background)
		(let-sgl ((foreground-color ,=yellow=) (line-width 2))
		  ,outline
		  (let-sgl ((font ,font)
			    (text-justification center)
			    (origin (,xc ,yc)))
		    (text (text-relative (0 -2/5)) ,text-string))))))
      (list get-normal-icon-pexp highlight-icon-pexp icon-width icon-height))))
