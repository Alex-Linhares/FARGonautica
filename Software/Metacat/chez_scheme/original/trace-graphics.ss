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

(define %answer-event-icon-font% #f)
(define %clamp-event-icon-font% #f)
(define %concept-activation-event-icon-font% #f)
(define %concept-mapping-event-icon-font% #f)
(define %group-event-icon-font% #f)
(define %rule-event-icon-font% #f)
(define %snag-event-icon-font% #f)

(define select-trace-fonts
  (lambda (win-width win-height)
    (let ((desired-answer-height (round (* 17/60 win-height)))
	  (desired-clamp-height (round (* 17/60 win-height)))
	  (desired-concept-height (round (* 15/60 win-height)))
	  (desired-cmap-height (round (* 15/60 win-height)))
	  (desired-group-height (round (* 17/60 win-height)))
	  (desired-rule-height (round (* 17/60 win-height)))
	  (desired-snag-height (round (* 11/60 win-height))))
      (set! %answer-event-icon-font%
	(make-mfont sans-serif (- desired-answer-height) '(bold italic)))
      (set! %clamp-event-icon-font%
	(make-mfont fancy (- desired-clamp-height) '(bold italic)))
      (set! %concept-activation-event-icon-font%
	(make-mfont fancy (- desired-concept-height) '(bold italic)))
      (set! %concept-mapping-event-icon-font%
	(make-mfont fancy (- desired-cmap-height) '(bold italic)))
      (set! %group-event-icon-font%
	(make-mfont serif (- desired-group-height) '(bold italic)))
      (set! %rule-event-icon-font%
	(make-mfont serif (- desired-rule-height) '(bold italic)))
      (set! %snag-event-icon-font%
	(make-mfont sans-serif (- desired-snag-height) '(bold))))))

(define resize-trace-fonts
  (lambda (win-width win-height)
    (tell %answer-event-icon-font% 'resize (round (* 17/60 win-height)))
    (tell %clamp-event-icon-font% 'resize (round (* 17/60 win-height)))
    (tell %concept-activation-event-icon-font% 'resize (round (* 15/60 win-height)))
    (tell %concept-mapping-event-icon-font% 'resize (round (* 15/60 win-height)))
    (tell %group-event-icon-font% 'resize (round (* 17/60 win-height)))
    (tell %rule-event-icon-font% 'resize (round (* 17/60 win-height)))
    (tell %snag-event-icon-font% 'resize (round (* 11/60 win-height)))))
      
(define %group-event-icon-arrowhead-length% 2/25)
(define %minimum-event-width% 1)
(define %minimum-event-height% 13/20)

(define trace-window-press-handler
  (lambda (win x y)
    (if *theme-edit-mode?*
      (tell *control-panel* 'raise-theme-edit-dialog)
      (if* (not *running?*)
	(let ((selected-event (tell *trace* 'get-mouse-selected-event x y)))
	  (if* (exists? selected-event)
	    (tell *memory* 'unhighlight-all-answers)
	    (tell selected-event 'toggle-highlight)
	    (if (tell selected-event 'highlighted?)
	      (begin
		(tell *trace* 'unhighlight-all-events-except selected-event)
		(tell selected-event 'display))
	      (restore-current-state))))))))

(define make-trace-window
  (lambda optional-args
    (let* ((width
	     (if (null? optional-args)
	       %default-trace-width%
	       (1st optional-args)))
	   (height
	     (if (< (length optional-args) 2)
	       %default-trace-height%
	       (2nd optional-args)))
	   (window (new-trace-window width height)))
      (tell window 'initialize)
      window)))

(define new-trace-window
  (lambda (x-pixels y-pixels)
    (let ((graphics-window
	    (make-horizontal-scrollable-graphics-window
	      x-pixels y-pixels %virtual-trace-length%
	      %trace-background-color%)))
      (tell graphics-window 'set-icon-label %trace-icon-label%)
      (if* (exists? %trace-icon-image%)
	(tell graphics-window 'set-icon-image %trace-icon-image%))
      (if* (exists? %trace-window-title%)
	(tell graphics-window 'set-window-title %trace-window-title%))
      (tell graphics-window 'set-mouse-handlers trace-window-press-handler #f)
      (select-trace-fonts x-pixels y-pixels)
      (let* ((event-spacing 1/5)
	     (next-x event-spacing)
	     (center-y 1/2))
	(lambda msg
	  (let ((self (1st msg)))
	    (record-case (rest msg)
	      (object-type () 'trace-window)
	      (add-event (event)
		(let* ((get-event-pexp-info
			 (case (tell event 'get-type)
			   (answer answer-event-pexp-info)
			   (clamp clamp-event-pexp-info)
			   (concept-activation concept-activation-event-pexp-info)
			   (group group-event-pexp-info)
			   (rule rule-event-pexp-info)
			   (concept-mapping concept-mapping-event-pexp-info)
			   (snag snag-event-pexp-info)))
		       (event-pexp-info
			 (get-event-pexp-info graphics-window event next-x center-y))
		       (normal-pexp (1st event-pexp-info))
		       (highlight-pexp (2nd event-pexp-info))
		       (event-width (3rd event-pexp-info))
		       (event-height (4th event-pexp-info))
		       (x1 next-x)
		       (x2 (+ next-x event-width))
		       (y1 (- center-y (* 1/2 event-height)))
		       (y2 (+ center-y (* 1/2 event-height))))
		  (tell event 'set-graphics-pexps normal-pexp highlight-pexp)
		  (tell event 'set-bounding-box x1 y1 x2 y2)
		  (tell graphics-window 'draw normal-pexp)
		  (set! next-x (+ next-x event-width event-spacing))
		  'done))
	      (initialize ()
		(tell graphics-window 'clear)
		(set! next-x event-spacing)
		(resize-trace-fonts
		  (tell graphics-window 'get-visible-w)
		  (tell graphics-window 'get-visible-h))
		'done)
	      (resize (new-width new-height)
		(resize-trace-fonts new-width new-height)
		(tell graphics-window 'retag 'all 'garbage)
		(for* each event in (tell *trace* 'get-all-events) do
		  (tell graphics-window 'draw
		    (if (tell event 'highlighted?)
		      (tell event 'get-highlight-graphics-pexp)
		      (tell event 'get-normal-graphics-pexp))))
		(tell graphics-window 'delete 'garbage))
	      (redraw ()
		(tell graphics-window 'caching-on)
		(tell graphics-window 'clear)
		(for* each event in (tell *trace* 'get-all-events) do
		  (tell graphics-window 'draw
		    (if (tell event 'highlighted?)
		      (tell event 'get-highlight-graphics-pexp)
		      (tell event 'get-normal-graphics-pexp))))
		(tell graphics-window 'flush)
		'done)
	      (else (delegate msg graphics-window)))))))))

;; *-event-pexp-info procedures return a list of the form:
;; (<normal-pexp> <highlight-pexp> <event-width> <event-height>)

(define answer-event-pexp-info
  (lambda (g event left-x center-y)
    (let* ((ovalness 3/4)
	   (ovaloid-sizing-factor 3/2)
	   (font %answer-event-icon-font%)
	   (answer-text-string
	     (format "Answer ~a"
	       (tell (tell event 'get-answer-string) 'print-name)))
	   (text-width (tell g 'get-string-width answer-text-string font))
	   (text-height (tell g 'get-string-height font))
	   (width (max %minimum-event-width% (* ovaloid-sizing-factor text-width)))
	   (height (max %minimum-event-height% (* ovaloid-sizing-factor text-height)))
	   (xc (+ left-x (* 1/2 width)))
	   (yc center-y)
	   (normal-pexp
	     `(let-sgl ()
		(let-sgl ((foreground-color ,=white=))
		  ,(filled-centered-ovaloid xc yc width height ovalness))
		(let-sgl ((foreground-color ,=black=))
		  ,(centered-ovaloid xc yc width height ovalness)
		  (let-sgl ((font ,font)
			    (text-justification center)
			    (origin (,xc ,yc)))
		    (text (text-relative (0 -7/20)) ,answer-text-string)))))
	   (highlight-pexp
	     `(let-sgl ()
		(let-sgl ((foreground-color ,%answer-event-icon-highlight-color%))
		  ,(filled-centered-ovaloid xc yc width height ovalness))
		(let-sgl ((foreground-color ,=black=))
		  ,(centered-ovaloid xc yc width height ovalness)
		  (let-sgl ((font ,font)
			    (text-justification center)
			    (origin (,xc ,yc)))
		    (text (text-relative (0 -7/20)) ,answer-text-string))))))
      (list normal-pexp highlight-pexp width height))))


(define clamp-event-pexp-info
  (lambda (g event left-x center-y)
    (let* ((corner-radius 1/5)
	   (rounded-box-sizing-factor 3/2)
	   (text "Clamp")
	   (font %clamp-event-icon-font%)
	   (pixel (tell g 'get-height-per-pixel))
	   (text-width (tell g 'get-string-width text font))
	   (text-height (tell g 'get-string-height font))
	   (width
	     (max %minimum-event-width% (* rounded-box-sizing-factor text-width)))
	   (height
	     (max %minimum-event-height% (* rounded-box-sizing-factor text-height)))
	   (xc (+ left-x (* 1/2 width)))
	   (yc center-y)
	   (normal-pexp
	     `(let-sgl ()
		(let-sgl ((foreground-color ,=white=))
		  ,(filled-centered-rounded-box xc yc width height corner-radius pixel))
		(let-sgl ((foreground-color ,=black=))
		  ,(centered-rounded-box xc yc width height corner-radius)
		  (let-sgl ((font ,font)
			    (text-justification center)
			    (origin (,xc ,yc)))
		    (text (text-relative (0 -7/20)) ,text)))))
	   (highlight-pexp
	     `(let-sgl ()
		(let-sgl ((foreground-color ,%clamp-event-icon-highlight-color%))
		  ,(filled-centered-rounded-box xc yc width height corner-radius pixel))
		(let-sgl ((foreground-color ,=black=))
		  ,(centered-rounded-box xc yc width height corner-radius)
		  (let-sgl ((font ,font)
			    (text-justification center)
			    (origin (,xc ,yc)))
		    (text (text-relative (0 -7/20)) ,text))))))
      (list normal-pexp highlight-pexp width height))))


(define concept-activation-event-pexp-info
  (lambda (g event left-x center-y)
    (let* ((ovalness 1)
	   (text (tell (tell event 'get-slipnode) 'get-short-name))
	   (font %concept-activation-event-icon-font%)
	   (text-width (tell g 'get-string-width text font))
	   (text-height (tell g 'get-string-height font))
	   (width (+ text-width (* 3/2 text-height)))
	   (height (* 2 text-height))
	   (xc (+ left-x (* 1/2 width)))
	   (yc center-y)
	   (normal-pexp
	     `(let-sgl ()
		(let-sgl ((foreground-color ,=white=))
		  ,(filled-centered-ovaloid xc yc width height ovalness))
		(let-sgl ((foreground-color ,=black=))
		  ,(centered-ovaloid xc yc width height ovalness)
		  (let-sgl ((font ,font)
			    (text-justification center)
			    (origin (,xc ,yc)))
		    (text (text-relative (0 -7/20)) ,text)))))
	   (highlight-pexp
	     `(let-sgl ()
		(let-sgl ((foreground-color
			    ,%concept-activation-event-icon-highlight-color%))
		  ,(filled-centered-ovaloid xc yc width height ovalness))
		(let-sgl ((foreground-color ,=black=))
		  ,(centered-ovaloid xc yc width height ovalness)
		  (let-sgl ((font ,font)
			    (text-justification center)
			    (origin (,xc ,yc)))
		    (text (text-relative (0 -7/20)) ,text))))))
      (list normal-pexp highlight-pexp width height))))


(define group-event-pexp-info
  (lambda (g event left-x center-y)
    (let* ((group (tell event 'get-group))
	   (direction (tell group 'get-direction))
	   (text (group-event-pexp-text-string group))
	   (font %group-event-icon-font%)
	   (text-width (tell g 'get-string-width text font))
	   (text-height (tell g 'get-string-height font))
	   (width (+ text-width text-height))
	   (height (* 3/2 text-height))
	   (x1 left-x)
	   (x2 (+ left-x width))
	   (y1 (- center-y (* 1/2 height)))
	   (y2 (+ center-y (* 1/2 height)))
	   (xc (+ left-x (* 1/2 width)))
	   (yc center-y)
	   (normal-pexp
	     `(let-sgl ()
		(let-sgl ((foreground-color ,=white=))
		  ,(solid-box x1 y1 x2 y2))
		(let-sgl ((foreground-color ,=black=))
		  ,(outline-box x1 y1 x2 y2)
		  ,@(group-event-pexp-arrowhead direction xc y2)
		  (let-sgl ((font ,font)
			    (text-justification center)
			    (origin (,xc ,yc)))
		    (text (text-relative (0 -7/20)) ,text)))))
	   (highlight-pexp
	     `(let-sgl ()
		(let-sgl ((foreground-color ,%group-event-icon-highlight-color%))
		  ,(solid-box x1 y1 x2 y2))
		(let-sgl ((foreground-color ,=black=))
		  ,(outline-box x1 y1 x2 y2)
		  ,@(group-event-pexp-arrowhead direction xc y2)
		  (let-sgl ((font ,font)
			    (text-justification center)
			    (origin (,xc ,yc)))
		    (text (text-relative (0 -7/20)) ,text))))))
      (list normal-pexp highlight-pexp width height))))


(define group-event-pexp-text-string
  (lambda (group)
    (let* ((bond-facet (tell group 'get-bond-facet))
	   (constituent-objects (tell group 'get-constituent-objects))
	   (descriptors (tell-all constituent-objects 'get-descriptor-for bond-facet))
	   (descriptor-strings
	     (map (lambda (object descriptor)
		    (cond
		      ((platonic-number? descriptor)
		       (format "~a" (platonic-number->number descriptor)))
		      ((letter? object) (tell descriptor 'get-lowercase-name))
		      ((group? object) (tell descriptor 'get-uppercase-name))))
	       constituent-objects
	       descriptors)))
      (apply string-append
	(cons (1st descriptor-strings)
	  (adjacency-map
	    (lambda (x y) (format "-~a" y))
	    descriptor-strings))))))


(define group-event-pexp-arrowhead
  (lambda (direction xc y2)
    (if (exists? direction)
      (let ((orientation-angle (if (eq? direction plato-right) 0 180)))
	(list (arrowhead xc y2 orientation-angle
		%group-event-icon-arrowhead-length%
		%group-arrowhead-angle%)))
      '())))


(define rule-event-pexp-info
  (lambda (g event left-x center-y)
    (let* ((text
	     (case (tell event 'get-rule-type)
	       (top "Top Rule")
	       (bottom "Bottom Rule")))
	   (font %rule-event-icon-font%)
	   (text-width (tell g 'get-string-width text font))
	   (text-height (tell g 'get-string-height font))
	   (width (+ text-width (* 3/2 text-height)))
	   (height (* 3/2 text-height))
	   (border (* 1/10 height))
	   (xc (+ left-x (* 1/2 width)))
	   (yc center-y)
	   (x1 left-x)
	   (y1 (- center-y (* 1/2 height)))
	   (x2 (+ left-x width))
	   (y2 (+ center-y (* 1/2 height)))
	   (lower-left `(,x1 ,y1))
	   (upper-left `(,x1 ,y2))
	   (upper-right `(,x2 ,y2))
	   (border-lower-left `(,(+ x1 border) ,(+ y1 border)))
	   (border-upper-right `(,(- x2 border) ,(- y2 border)))
	   (normal-pexp
	     `(let-sgl ()
		(let-sgl ((foreground-color ,%trace-background-color%)
			  (line-width 2))
		  (polyline ,lower-left ,upper-left ,upper-right))
		(let-sgl ((foreground-color ,=white=))
		  (filled-rectangle ,lower-left ,upper-right))
		(let-sgl ((foreground-color ,=black=))
		  (rectangle ,lower-left ,upper-right)
		  (rectangle ,border-lower-left ,border-upper-right)
		  (let-sgl ((font ,%rule-event-icon-font%)
			    (text-justification center)
			    (origin (,xc ,yc)))
		    (text (text-relative (0 -7/20)) ,text)))))
	   (highlight-pexp
	     `(let-sgl ()
		(let-sgl ((foreground-color ,=white=))
		  (filled-rectangle ,lower-left ,upper-right))
		(let-sgl ((foreground-color
			    ,(case (tell event 'get-rule-type)
			       (top %top-rule-event-icon-highlight-color%)
			       (bottom %bottom-rule-event-icon-highlight-color%))))
		  (rectangle ,lower-left ,upper-right)
		  (rectangle ,border-lower-left ,border-upper-right)
		  (let-sgl ((line-width 2))
		    (polyline ,lower-left ,upper-left ,upper-right))
		  (let-sgl ((font ,%rule-event-icon-font%)
			    (text-justification center)
			    (origin (,xc ,yc)))
		    (text (text-relative (0 -7/20)) ,text))))))
      (list normal-pexp highlight-pexp width height))))


(define concept-mapping-event-pexp-info
  (lambda (g event left-x center-y)
    (let* ((text (tell (tell event 'get-concept-mapping) 'print-name))
	   (font %concept-mapping-event-icon-font%)
	   (text-width (tell g 'get-string-width text font))
	   (text-height (tell g 'get-string-height font))
	   (width (+ text-width text-height))
	   (height (* 3/2 text-height))
	   (x1 left-x)
	   (x2 (+ left-x width))
	   (y1 (- center-y (* 1/2 height)))
	   (y2 (+ center-y (* 1/2 height)))
	   (xc (+ left-x (* 1/2 width)))
	   (yc center-y)
	   (normal-pexp
	     `(let-sgl ()
		(let-sgl ((foreground-color ,=white=))
		  ,(solid-box x1 y1 x2 y2))
		(let-sgl ((foreground-color ,=black=))
		  ,(outline-box x1 y1 x2 y2)
		  (let-sgl ((font ,font)
			    (text-justification center)
			    (origin (,xc ,yc)))
		    (text (text-relative (0 -7/20)) ,text)))))
	   (highlight-pexp
	     `(let-sgl ()
		(let-sgl ((foreground-color
			    ,%concept-mapping-event-icon-highlight-color%))
		  ,(solid-box x1 y1 x2 y2))
		(let-sgl ((foreground-color ,=black=))
		  ,(outline-box x1 y1 x2 y2)
		  (let-sgl ((font ,font)
			    (text-justification center)
			    (origin (,xc ,yc)))
		    (text (text-relative (0 -7/20)) ,text))))))
      (list normal-pexp highlight-pexp width height))))


(define snag-event-pexp-info
  (lambda (g event left-x center-y)
    (let* ((text "SNAG")
	   (font %snag-event-icon-font%)
	   (width 7/10)
	   (height 7/10)
	   (border (* 13/100 width))
	   (xc (+ left-x (* 1/2 width)))
	   (yc center-y)
	   (normal-pexp
	     `(let-sgl ((background-color ,=white=))
		,(centered-octagon xc yc width)
		,(centered-octagon xc yc (- width border))
		(let-sgl ((font ,font)
			  (text-justification center)
			  (origin (,xc ,yc)))
		  (text (text-relative (0 -7/20)) ,text))))
	   (highlight-pexp
	    `(let-sgl ((foreground-color ,=white=)
		       (background-color ,%snag-event-icon-highlight-color%))
	       ,(centered-octagon xc yc width)
	       (let-sgl ((line-width 2))
		 ,(centered-octagon xc yc (- width border)))
	       (let-sgl ((font ,font)
			 (text-justification center)
			 (origin (,xc ,yc)))
		 (text (text-relative (0 -7/20)) ,text)))))
      (list normal-pexp highlight-pexp width height))))
