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
;; Ported to Racket, 2026: the model part of eeg-graphics.ss, verbatim: the
;; EEG object, which the Workspace initializes and run.ss's update-everything
;; feeds, and the table of values it records.  Part of the engine
;; (racket/engine.rkt); the EEG window is in racket/gui/eeg-graphics.rktl
;; (docs/porting-notes.md, item 14).

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

(define *EEG* (make-EEG))
