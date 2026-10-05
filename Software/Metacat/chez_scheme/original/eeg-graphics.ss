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

;; The %EEG-table% specifies a set of values to track, which of these values
;; to plot in the EEG window, and how to label the information.  Each table
;; entry defines a particular value that the EEG object will compute and
;; record at regular intervals during a run.  If desired, these values may be
;; computed from other values recorded by the EEG (using the EEG's
;; 'get-current-value or 'get-average-value methods).
;;
;; Each entry in the table is of the form:
;;
;;  (index-number label-string color-name initial-value plot? thunk)
;;
;; Values are referred to by their index number.  All values listed in the
;; table will be recorded, but only those with plot? set to #t will be
;; plotted.  All plotted values must be in the range [0..100]
;;
;; For example, the table below specifies three values to track: the current
;; Workspace activity, the average of the last 10 values recorded for table
;; entry #0 (Workspace activity), and the current temperature.  The average
;; Workspace activity will be plotted in yellow and the temperature will be
;; plotted in red.  Current Workspace activity is recorded so that the average
;; Workspace activity can be computed, but is not plotted itself.  In general,
;; plotting average values instead of instantaneous values usually results in
;; a smoother curve that is less sensitive to momentary fluctuations in value.

(define %EEG-table%
  `(
    (0 "Workspace Activity" "white" 100 #f
      ;; current Workspace activity value
      ,(lambda () (tell *workspace* 'get-activity)))
    (1 "Average Workspace Activity" "yellow" 100 #t
      ;; average of last 10 Workspace activity values
      ,(lambda () (tell *EEG* 'get-average-value 0 10)))
    (2 "Temperature" "red" 100 #t
      ;; current temperature
      ,(lambda () *temperature*))
))

;;---------------------------------------------------------------------------

(define %EEG-buffer-size% 40)
(define %max-EEG-window-cycles% 400)
(define %EEG-title-font% #f)

(define select-EEG-font
  (lambda (win-width win-height)
    (let ((desired-font-height (round (* 18/120 win-height))))
      (set! %EEG-title-font%
	(make-mfont serif (- desired-font-height) '(italic))))))

(define make-EEG
  (lambda ()
    (let* ((circular-array #f)
	   (initial-values #f)
	   (value-procs #f)
	   (get-values #f)
	   (next #f))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'EEG)
	    (print ()
	      (for* i from 0 to (sub1 (length %EEG-table%)) do
		(printf "[~a]:  " i)
		(for* j from 0 to (sub1 %EEG-buffer-size%) do
		  (printf "~a "
		    (round-to-100ths (list-ref (vector-ref circular-array j) i))))
		(newline))
	      (printf "next = ~a~n" next))
	    (get-current-value (value-index)
	      (list-ref (tell self 'get-current-values) value-index))
	    (get-current-values ()
	      (vector-ref circular-array (modulo (sub1 next) %EEG-buffer-size%)))
	    (record-current-values ()
	      (vector-set! circular-array next (get-values))
	      (set! next (modulo (add1 next) %EEG-buffer-size%))
	      'done)
	    (get-average-value (value-index . args)
	      (average
		(if (null? args)
		  (tell self 'get-previous-values value-index)
		  (tell self 'get-previous-values value-index (1st args)))))
	    (get-max-variation (value-index . args)
	      (let ((previous-values
		      (if (null? args)
			(tell self 'get-previous-values value-index)
			(tell self 'get-previous-values value-index (1st args)))))
		(abs (- (maximum previous-values) (minimum previous-values)))))
	    (get-previous-values (value-index . args)
	      (let ((spread-size
		      (if (null? args)
			%EEG-buffer-size%
			(min (1st args) %EEG-buffer-size%)))
		    (previous-values '()))
		(for* i from 1 to spread-size do
		  (set! previous-values
		    (cons (list-ref (vector-ref circular-array
				      (modulo (- next i) %EEG-buffer-size%))
			    value-index)
		      previous-values)))
		previous-values))
	    (initialize ()
	      (set! initial-values (map 4th %EEG-table%))
	      (set! value-procs (map 6th %EEG-table%))
	      (set! get-values (lambda () (map (lambda (f) (f)) value-procs)))
	      (set! circular-array (make-vector %EEG-buffer-size% initial-values))
	      (set! next 0)
	      'done)
	    (else (delegate msg base-object))))))))

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

(define *EEG* (make-EEG))
