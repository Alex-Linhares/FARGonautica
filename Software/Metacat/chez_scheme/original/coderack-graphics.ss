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

(define select-coderack-fonts
  (lambda (win-width win-height)
    (let ((desired-title-height (round (* 40/1000 win-height)))
	  (desired-subtitle-height (round (* 20/1000 win-height)))
	  (desired-type-height (round (* 14/1000 win-height)))
	  (desired-count-height (round (* 19/1000 win-height)))
	  (desired-sum-height (round (* 19/1000 win-height))))
      (set! %coderack-title-font%
	(make-mfont sans-serif (- desired-title-height) '(bold italic)))
      (set! %coderack-subtitle-font%
	(make-mfont serif (- desired-subtitle-height) '(bold italic)))
      (set! %coderack-codelet-type-font%
	(make-mfont sans-serif (- desired-type-height) '(normal)))
      (set! %coderack-codelet-count-font%
	(make-mfont sans-serif (- desired-count-height) '(normal)))
      (set! %coderack-codelet-sum-font%
	(make-mfont sans-serif (- desired-sum-height) '(normal))))))

(define make-coderack-window
  (lambda optional-args
    (let* ((width
	     (if (null? optional-args)
	       %default-coderack-width%
	       (1st optional-args)))
	   (window (new-coderack-window width)))
      (tell window 'initialize)
      window)))

(define new-coderack-window
  (lambda (x-pixels)
    (let* ((window-height 26/10)
	   (coderack-top (- window-height 2/5))
	   (y-pixels (ceiling (* window-height x-pixels)))
	   (graphics-window
	     (make-unscrollable-graphics-window x-pixels y-pixels
	       %coderack-background-color%))
	   (num-codelet-types (length *codelet-types*))
	   (max-bar-right 19/20)
	   (title-coord `(1/2 ,(- window-height 4/25)))
	   (pixel-w #f)
	   (pixel-h #f)
	   (double-line-thickness #f)
	   (slot-height #f)
	   (max-codelet-count-width #f)
	   (codelet-sum-font-height #f)
	   (bar-height #f)
	   (max-label-width #f)
	   (slot-right #f)
	   (label-x #f)
	   (sum-coord #f)
	   (sum-label-coord #f)
	   (sum-y #f)
	   (bar-left #f)
	   (max-bar-width #f)
	   (slot-title-coord #f)
	   (prob-title-coord #f)
	   (prob-title #f)
	   (skeleton-graphics-pexp #f)
	   (current-title-pexp #f)
	   (current-title "Coderack")
	   (last-codelet-type #f)
	   (display-state 'normal))
      (tell graphics-window 'set-icon-label %coderack-icon-label%)
      (if* (exists? %coderack-icon-image%)
	(tell graphics-window 'set-icon-image %coderack-icon-image%))
      (if* (exists? %coderack-window-title%)
	(tell graphics-window 'set-window-title %coderack-window-title%))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'coderack-window)
	    (get-display-state () display-state)
	    (get-last-codelet-type () last-codelet-type)
	    (set-last-codelet-type (codelet-type)
	      (if* (and %coderack-graphics% (exists? last-codelet-type))
		(tell last-codelet-type 'unhighlight))
	      (set! last-codelet-type codelet-type)
	      'done)
	    (highlight-last-codelet ()
	      (if* (exists? last-codelet-type)
		(tell last-codelet-type 'highlight %last-codelet-color%))
	      'done)
	    (unhighlight-last-codelet ()
	      (if* (exists? last-codelet-type)
		(tell last-codelet-type 'unhighlight))
	      'done)
	    (initialize-parameters ()
	      (set! x-pixels (tell graphics-window 'get-visible-w))
	      (set! y-pixels (tell graphics-window 'get-visible-h))
	      (select-coderack-fonts x-pixels y-pixels)
	      (set! pixel-w (tell graphics-window 'get-width-per-pixel))
	      (set! pixel-h (tell graphics-window 'get-height-per-pixel))
	      (set! double-line-thickness (* 2 pixel-h))
	      (set! slot-height
		(/ (- coderack-top double-line-thickness) (+ 1 num-codelet-types)))
	      (set! max-codelet-count-width
		(tell graphics-window 'get-string-width "100" %coderack-codelet-count-font%))
	      (set! codelet-sum-font-height
		(tell graphics-window 'get-character-height " " %coderack-codelet-sum-font%))
	      (set! bar-height (* 3/4 slot-height))
	      (set! sum-y (- (* 1/2 slot-height) (* 3/10 codelet-sum-font-height)))
	      (set! max-label-width
		(maximum
		  (map (lambda (label)
			 (tell graphics-window 'get-string-width label
			       %coderack-codelet-type-font%))
		       (apply append (tell-all *codelet-types* 'get-graphics-labels)))))
	      (cond
		(%codelet-count-graphics%
		 (set! slot-right (+ (* 3/2 max-codelet-count-width) max-label-width))
		 (set! label-x (* 5/4 max-codelet-count-width))
		 (set! sum-coord `(,(* 2/5 slot-right) ,sum-y))
		 (set! sum-label-coord `(,(* 9/20 slot-right) ,sum-y)))
		(else
		 (set! slot-right (+ (* 1/2 max-codelet-count-width) max-label-width))
		 (set! label-x (* 1/4 max-codelet-count-width))))
	      (set! bar-left (+ slot-right pixel-w))
	      (set! max-bar-width (- max-bar-right bar-left))
	      (set! slot-title-coord `(,(* 1/2 slot-right) ,(+ coderack-top bar-height)))
	      (let ((prob-title-x (/ (+ slot-right 1) 2))
		    (prob-title-width
		      (tell graphics-window 'get-string-width
			"Selection Probability" %coderack-subtitle-font%)))
		(set! prob-title-coord
		  `(,prob-title-x ,(+ coderack-top bar-height)))
		(set! prob-title
		  (if (> (+ prob-title-x (/ prob-title-width 2)) 1)
		    "Probability"
		    "Selection Probability")))
	      (for* i from 0 to (- num-codelet-types 1) do
		(let* ((codelet-type (nth i *codelet-types*))
		       (top-y (- coderack-top (* i slot-height)))
		       (top-label-y (- top-y (* 9/20 slot-height)))
		       (bot-y (- coderack-top (* (+ i 1) slot-height)))
		       (bot-label-y (- top-y (* 17/20 slot-height)))
		       (mid-label-y (/ (+ top-label-y bot-label-y) 2))
		       (codelet-count-coord
			 (if %codelet-count-graphics%
			   `(,max-codelet-count-width ,(- top-y (* 13/20 slot-height)))
			   #f))
		       (bar-top (- top-y (* 1/2 (- slot-height bar-height))))
		       (bar-bottom (- bar-top bar-height))
		       (labels (tell codelet-type 'get-graphics-labels))
		       (label-pexp
			 (if (= (length labels) 1)
			   `(let-sgl ((font ,%coderack-codelet-type-font%))
			      (text (,label-x ,mid-label-y) ,(1st labels)))
			   `(let-sgl ((font ,%coderack-codelet-type-font%))
			      (text (,label-x ,top-label-y) ,(1st labels))
			      (text (,label-x ,bot-label-y) ,(2nd labels)))))
		       (slot-pexp
			 `(let-sgl ()
			    (line (0 ,top-y) (,slot-right ,top-y))
			    ,label-pexp))
		       (slot-background-pexp
			 (solid-box
			   0 (+ bot-y pixel-h) (- slot-right pixel-w) (- top-y pixel-h)))
		       (get-urgency-pexp
			 (lambda (urgency)
			   `(let-sgl ()
			      (let-sgl ((foreground-color ,(urgency-color urgency)))
				,slot-background-pexp)
			      ,label-pexp)))
		       (get-highlight-pexp
			 (lambda (color)
			   `(let-sgl ()
			      (let-sgl ((foreground-color ,color))
				,slot-background-pexp)
			      ,label-pexp))))
		  (tell codelet-type 'set-graphics-parameters
		    self slot-pexp get-urgency-pexp get-highlight-pexp
		    codelet-count-coord bar-left bar-bottom bar-top
		    (lambda (prob)
		      (if (zero? prob)
			  0
			  ;; Show at least a little bar for low probabilities:
			  (max pixel-w (* prob max-bar-width)))))))
	      (set! skeleton-graphics-pexp
		`(let-sgl ()
		   (let-sgl ((font ,%coderack-subtitle-font%)
			     (text-justification center))
		     (text ,slot-title-coord "Codelet Type")
		     (text ,prob-title-coord ,prob-title))
		   (line (,slot-right ,coderack-top) (,slot-right 0))
		   ,@(tell-all *codelet-types* 'get-slot-pexp)
		   (line (0 ,slot-height) (,slot-right ,slot-height))
		   (line (0 ,(+ slot-height double-line-thickness))
		     (,slot-right ,(+ slot-height double-line-thickness)))))
	      'done)
	    (garbage-collect ()
	      (tell graphics-window 'retag 'bar 'garbage)
	      (tell graphics-window 'retag 'urgency 'garbage)
	      (tell graphics-window 'retag 'count 'garbage)
	      (tell graphics-window 'retag 'sum 'garbage)
	      (tell graphics-window 'retag 'highlight 'garbage)
	      (for* each codelet-type in *codelet-types* do
		(tell codelet-type 'draw-graphics))
	      (if* %codelet-count-graphics%
		(tell self 'draw-codelet-sum
		  (tell *coderack* 'get-num-of-codelets)))
	      (if* %highlight-last-codelet%
		(tell self 'highlight-last-codelet))
	      (tell graphics-window 'delete 'garbage))
	    (clear ()
	      (tell self 'draw-title "Coderack")
	      (for* each codelet-type in *codelet-types* do
		(tell codelet-type 'reset-values-to-zero)
		(tell codelet-type 'reset-bar-width)
		(tell codelet-type 'clear-pattern-value))
	      (tell graphics-window 'retag 'urgency 'garbage)
	      (tell graphics-window 'retag 'bar 'garbage)
	      (tell graphics-window 'retag 'count 'garbage)
	      (tell graphics-window 'retag 'sum 'garbage)
	      (tell graphics-window 'retag 'total 'garbage)
	      (tell graphics-window 'retag 'highlight 'garbage)
	      (tell graphics-window 'delete 'garbage)
	      (set! last-codelet-type #f)
	      (if* (and %coderack-graphics% %codelet-count-graphics%)
		(for* each codelet-type in *codelet-types* do
		  (tell codelet-type 'draw-codelet-count))
		(tell self 'draw-codelet-sum 0)
		(tell graphics-window 'draw
		  `(let-sgl ((font ,%coderack-codelet-sum-font%))
		     (text ,sum-label-coord "Total"))
		  'total))
	      (set! display-state 'normal)
	      'done)
	    (update-graphics ()
	      (tell *coderack* 'update-all-selection-probabilities)
	      (tell graphics-window 'retag 'bar 'garbage)
	      (tell graphics-window 'retag 'count 'garbage)
	      (tell graphics-window 'retag 'sum 'garbage)
	      (tell graphics-window 'retag 'highlight 'garbage)
	      (for* each codelet-type in *codelet-types* do
		(tell codelet-type 'update-bar-graphics))
	      (if* %codelet-count-graphics%
		(tell self 'draw-codelet-sum
		  (tell *coderack* 'get-num-of-codelets)))
	      (if* %highlight-last-codelet%
		(tell self 'highlight-last-codelet))
	      (tell graphics-window 'delete 'garbage)
	      'done)
	    (draw-codelet-count (coord num color)
	      (tell graphics-window 'draw
		`(let-sgl ((font ,%coderack-codelet-count-font%)
			   (background-color ,color)
			   (text-justification right)
			   (text-mode image))
		   (text ,coord ,(format "     ~a" (if (exists? num) num ""))))
		'count))
	    (draw-codelet-sum (num)
	      (tell graphics-window 'draw
		`(let-sgl ((background-color ,%coderack-background-color%)
			   (font ,%coderack-codelet-sum-font%)
			   (text-justification right)
			   (text-mode image))
		   (text ,sum-coord ,(format "     ~a" (if (exists? num) num ""))))
		'sum))
	    (draw-title (text)
	      (tell self 'erase-title)
	      (if* (exists? text)
		(set! current-title text)
		(set! current-title-pexp
		  `(let-sgl ((font ,%coderack-title-font%)
			     (text-justification center))
		     (text ,title-coord ,text)))
		(tell graphics-window 'draw current-title-pexp 'title))
	      'done)
	    (erase-title ()
	      (if* (exists? current-title-pexp)
		(tell graphics-window 'delete 'title)
		(set! current-title #f)
		(set! current-title-pexp #f))
	      'done)
	    (draw-graphics ()
	      (tell self 'draw-title current-title)
	      (tell graphics-window 'draw skeleton-graphics-pexp 'skeleton)
	      (case display-state
		((normal)
		 (if* %coderack-graphics%
		   (for* each codelet-type in *codelet-types* do
		     (tell codelet-type 'draw-graphics))
		   (if* %codelet-count-graphics%
		     (tell self 'draw-codelet-sum
		       (tell *coderack* 'get-num-of-codelets))
		     (tell graphics-window 'draw
		       `(let-sgl ((font ,%coderack-codelet-sum-font%))
			  (text ,sum-label-coord "Total"))
		       'total))
		   (if* %highlight-last-codelet%
		     (tell self 'highlight-last-codelet))))
		((pattern)
		 (for* each codelet-type in *codelet-types* do
		   (tell codelet-type 'draw-pattern-value))))
	      'done)
	    (initialize ()
	      (tell self 'initialize-parameters)
	      (tell graphics-window 'clear)
	      (tell self 'draw-graphics))
	    (resize (new-width new-height)
	      (set! x-pixels new-width)
	      (set! y-pixels new-height)
	      (tell self 'initialize-parameters)
	      (tell graphics-window 'retag 'all 'garbage)
	      (tell self 'draw-graphics)
	      (tell graphics-window 'delete 'garbage))
	    ;; title = #f just erases the current title (if any):
	    (blank-window (title)
	      (tell self 'draw-title title)
	      (for* each codelet-type in *codelet-types* do
		(tell codelet-type 'clear-pattern-value))
	      (tell graphics-window 'retag 'urgency 'garbage)
	      (tell graphics-window 'retag 'count 'garbage)
	      (tell graphics-window 'retag 'sum 'garbage)
	      (tell graphics-window 'retag 'total 'garbage)
	      (tell graphics-window 'retag 'bar 'garbage)
	      (tell graphics-window 'retag 'highlight 'garbage)
	      (tell graphics-window 'delete 'garbage)
	      (set! display-state 'blank)
	      'done)
	    (display-patterns (codelet-patterns title)
	      (tell self 'blank-window title)
	      (for* each pattern in codelet-patterns do
		(for* each entry in (rest pattern) do
		  (tell (1st entry) 'display-urgency (2nd entry))))
	      (set! display-state 'pattern)
	      'done)
	    (restore-current-state ()
	      (tell *coderack* 'update-all-selection-probabilities)
	      (tell self 'blank-window "Coderack")
	      (if* %coderack-graphics%
		(for* each codelet-type in *codelet-types* do
		  (tell codelet-type 'draw-graphics))
		(if* %codelet-count-graphics%
		  (tell self 'draw-codelet-sum
		    (tell *coderack* 'get-num-of-codelets))
		  (tell graphics-window 'draw
		    `(let-sgl ((font ,%coderack-codelet-sum-font%))
		       (text ,sum-label-coord "Total"))
		    'total))
		(if* %highlight-last-codelet%
		  (tell self 'highlight-last-codelet)))
	      (set! display-state 'normal)
	      'done)
	    (else (delegate msg graphics-window))))))))
