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
;; Ported to Racket, 2026: included by racket/gui/views.rkt.  Verbatim but
;; for the changes marked "port:" (docs/porting-notes.md, item 14).

;; port: the EEG object (make-EEG, *EEG*), %EEG-table% and %EEG-buffer-size%
;; are in the engine (racket/engine/eeg-graphics.rktl): run.ss and
;; workspace.ss use them whether or not the window exists.

(define %max-EEG-window-cycles% 400)
(define %EEG-title-font% #f)

(define select-EEG-font
  (lambda (win-width win-height)
    (let ((desired-font-height (round (* 18/120 win-height))))
      (set! %EEG-title-font%
	(make-mfont serif (- desired-font-height) '(italic))))))

(define make-EEG-window
  (lambda optional-args
    (let* ((width
	     (if (null? optional-args)
	       %EEG-window-width%
	       (1st optional-args)))
	   (height
	     (if (< (length optional-args) 2)
	       %EEG-window-height%
	       (2nd optional-args)))
	   (window (new-EEG-window width height)))
      (tell window 'initialize)
      window)))

(define new-EEG-window
  (lambda (x-pixels y-pixels)
    (select-EEG-font x-pixels y-pixels)
    (let* ((plot? 5th)
	   (graphics-window
	     (make-horizontal-scrollable-graphics-window
	       x-pixels y-pixels %virtual-EEG-length% %EEG-background-color%))
	   (visible-x-max (tell graphics-window 'get-visible-x-max))
	   (cycle-width (/ visible-x-max %max-EEG-window-cycles%))
	   (title-height
	     (tell graphics-window 'get-string-height %EEG-title-font%))
	   (title-x (* 1/2 visible-x-max))
	   (title-y (- 1 title-height))
	   (max-height title-y)
	   (entries-to-plot #f)
	   (colors #f)
	   (previous-points #f)
	   (num-cycles #f)
	   (title #f)
	   (pexps '()))
      (tell graphics-window 'set-icon-label %EEG-icon-label%)
      (if* (exists? %EEG-icon-image%)
	(tell graphics-window 'set-icon-image %EEG-icon-image%))
      (if* (exists? %EEG-window-title%)
	(tell graphics-window 'set-window-title %EEG-window-title%))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'EEG-window)
	    (plot-current-values ()
	      (let* ((x (* num-cycles cycle-width))
		     (current-points
		       (map (lambda (i)
			      (let ((value (tell *EEG* 'get-current-value i)))
				`(,x ,(* (% value) max-height))))
			 entries-to-plot)))
		(tell graphics-window 'caching-on)
		(for* each (p0 p1 c) in (previous-points current-points colors) do
		  (let ((pexp `(let-sgl ((foreground-color ,c)) (line ,p0 ,p1))))
		    (tell graphics-window 'draw pexp 'curve)
		    (set! pexps (cons pexp pexps))))
		(tell graphics-window 'flush)
		(set! previous-points current-points)
		(set! num-cycles (add1 num-cycles)))
	      'done)
	    (initialize ()
	      (set! title
		(format " ~a "
		  (punctuate
		    (filter-map plot?
		      (lambda (entry) (format "~a (~a)" (2nd entry) (3rd entry)))
		      %EEG-table%))))
	      (tell graphics-window 'clear)
	      (tell graphics-window 'draw
		`(let-sgl ((foreground-color ,%EEG-title-color%)
			   (font ,%EEG-title-font%)
			   (text-justification center))
		   (text (,title-x ,title-y) ,title))
		'title)
	      (set! entries-to-plot (filter-map plot? 1st %EEG-table%))
	      (set! colors (filter-map plot? 3rd %EEG-table%))
	      (set! previous-points
		(filter-map plot?
		  (lambda (entry) `(0 ,(* (% (4th entry)) max-height)))
		  %EEG-table%))
	      (set! num-cycles 0)
	      (set! pexps '())
	      'done)
	    (resize (new-width new-height)
	      (let ((old-width x-pixels)
		    (old-height y-pixels))
		(set! x-pixels new-width)
		(set! y-pixels new-height)
		(select-EEG-font x-pixels y-pixels)
		(set! title-height
		  (tell graphics-window 'get-string-height %EEG-title-font%))
		(set! title-x
		  (max (* 1/2 (tell graphics-window 'get-visible-x-max))
		       (* 1/2 (tell graphics-window 'get-string-width title
				    %EEG-title-font%))))
		(set! title-y (- 1 title-height))
		(tell graphics-window 'retag 'all 'garbage)
		(tell graphics-window 'draw
		  `(let-sgl ((foreground-color ,%EEG-title-color%)
			     (font ,%EEG-title-font%)
			     (text-justification center))
		     (text (,title-x ,title-y) ,title))
		  'title)
		(tell graphics-window 'draw `(let-sgl () ,@pexps))
		(tell graphics-window 'delete 'garbage)))
	    (else (delegate msg graphics-window))))))))

