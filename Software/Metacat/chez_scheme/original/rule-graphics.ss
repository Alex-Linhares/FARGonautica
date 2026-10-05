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

(define initialize-rule-graphics
  (lambda (rule)
    (let* ((clauses (tell rule 'get-english-transcription))
	   (num-of-clauses (length clauses))
	   (clause-widths
	     (map (lambda (clause)
		    (tell *workspace-window* 'get-string-width clause %rule-font%))
	       clauses))
	   (max-clause-width (maximum clause-widths))
	   (text-height (tell *workspace-window* 'get-character-height " " %rule-font%))
	   (x-extra (tell *workspace-window* 'get-string-width "xx" %rule-font%))
	   (y-extra (* 3/5 text-height))
	   (border-width (* 3/20 text-height))
	   (height-minus-border (+ (* num-of-clauses text-height) y-extra))
	   (total-height (+ height-minus-border (* 2 border-width)))
	   (center-coord (tell *workspace-window* 'get-rule-coord
			   (tell rule 'get-rule-type)
			   total-height))
	   (x1-border (- (1st center-coord) (* 1/2 max-clause-width) x-extra))
	   (x2-border (+ (1st center-coord) (* 1/2 max-clause-width) x-extra))
	   (y1-border (- (2nd center-coord) (* 1/2 height-minus-border)))
	   (y2-border (+ (2nd center-coord) (* 1/2 height-minus-border)))
	   (shading (tell *workspace-window* 'get-width-per-pixel))
	   (x1 (- x1-border border-width shading))
	   (y1 (- y1-border border-width))
	   (x2 (+ x2-border border-width))
	   (y2 (+ y2-border border-width shading))
	   (text-x0 (- (1st center-coord) (* 1/2 max-clause-width)))
	   (text-y0 (+ (- (2nd center-coord) (* 1/2 height-minus-border)) y-extra))
	   (clause-pexps
	     (map (lambda (i clause)
		    `(text (,text-x0 ,(+ text-y0 (* i text-height))) ,clause))
	       (descending-index-list num-of-clauses)
	       clauses))
	   (rule-pexp
	     `(let-sgl ((font ,%rule-font%))
		(let-sgl ((foreground-color ,=white=))
		   (filled-rectangle (,x1 ,y1) (,x2 ,y2)))
		,@clause-pexps
		(let-sgl ((line-width 2))
		  (polyline (,x1 ,y1) (,x1 ,y2) (,x2 ,y2)))
		(polyline (,x2 ,y2) (,x2 ,y1) (,x1 ,y1))
		,(outline-box x1-border y1-border x2-border y2-border)))
	   (tagged-rule-pexp
	     `(rule ,(tell rule 'get-rule-type) ,clauses ,rule-pexp))
	   (clamped-rule-pexp
	     `(let-sgl ((font ,%rule-font%))
		,@clause-pexps
		,(dashed-box x1 y1 x2 y2 %dash-density% %dash-length%))))
      (tell rule 'set-graphics-pexp tagged-rule-pexp)
      (tell rule 'set-auxiliary-rule-graphics-info
	clamped-rule-pexp center-coord (- y2 y1)))))

(define update-rule-pexps!
  (lambda (pexp)
    (record-case pexp
      (rule (rule-type clauses rule-pexp)
	(set-car! (cdddr pexp) (make-new-rule-pexp rule-type clauses)))
      (let-sgl (bindings . pexps)
	(for* each p in pexps do
	  (update-rule-pexps! p)))
      (erase (c pexp) (update-rule-pexps! pexp))
      (else 'done))))

(define make-new-rule-pexp
  (lambda (rule-type clauses)
    (let* ((num-of-clauses (length clauses))
	   (clause-widths
	     (map (lambda (clause)
		    (tell *workspace-window* 'get-string-width clause %rule-font%))
	       clauses))
	   (max-clause-width (maximum clause-widths))
	   (text-height (tell *workspace-window* 'get-character-height " " %rule-font%))
	   (x-extra (tell *workspace-window* 'get-string-width "xx" %rule-font%))
	   (y-extra (* 3/5 text-height))
	   (border-width (* 3/20 text-height))
	   (height-minus-border (+ (* num-of-clauses text-height) y-extra))
	   (total-height (+ height-minus-border (* 2 border-width)))
	   (center-coord (tell *workspace-window* 'get-rule-coord rule-type total-height))
	   (x1-border (- (1st center-coord) (* 1/2 max-clause-width) x-extra))
	   (x2-border (+ (1st center-coord) (* 1/2 max-clause-width) x-extra))
	   (y1-border (- (2nd center-coord) (* 1/2 height-minus-border)))
	   (y2-border (+ (2nd center-coord) (* 1/2 height-minus-border)))
	   (shading (tell *workspace-window* 'get-width-per-pixel))
	   (x1 (- x1-border border-width shading))
	   (y1 (- y1-border border-width))
	   (x2 (+ x2-border border-width))
	   (y2 (+ y2-border border-width shading))
	   (text-x0 (- (1st center-coord) (* 1/2 max-clause-width)))
	   (text-y0 (+ (- (2nd center-coord) (* 1/2 height-minus-border)) y-extra))
	   (clause-pexps
	     (map (lambda (i clause)
		    `(text (,text-x0 ,(+ text-y0 (* i text-height))) ,clause))
	       (descending-index-list num-of-clauses)
	       clauses)))
      `(let-sgl ((font ,%rule-font%))
	 (let-sgl ((foreground-color ,=white=))
	   (filled-rectangle (,x1 ,y1) (,x2 ,y2)))
	 ,@clause-pexps
	 (let-sgl ((line-width 2))
	   (polyline (,x1 ,y1) (,x1 ,y2) (,x2 ,y2)))
	 (polyline (,x2 ,y2) (,x2 ,y1) (,x1 ,y1))
	 ,(outline-box x1-border y1-border x2-border y2-border)))))
