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

(define %temperature-value-font% #f)
(define %temperature-title-font% #f)

;; On Windows, the smallest possible x-dimension for an SWL window is about
;; 100 pixels, due to the presence of the window manager buttons on the title
;; bar, so for temperature windows smaller than this, a "fudge factor" is
;; required in order to keep the thermometer centered in the window.
(define %smallest-window-width% 100)

(define select-temperature-fonts
  (lambda (win-width win-height)
    (let ((desired-title-height (round (* 6/100 win-height)))
	  (desired-value-height (round (* 6/100 win-height))))
      (set! %temperature-title-font%
	(make-mfont sans-serif (- desired-title-height) '(normal)))
      (set! %temperature-value-font%
	(make-mfont serif (- desired-value-height) '(italic))))))

(define make-temperature-window
  (lambda optional-args
    (let* ((width
	     (if (null? optional-args)
		 %default-temperature-width%
		 (1st optional-args)))
	   (window (new-temperature-window width)))
      (tell window 'initialize)
      window)))

(define new-temperature-window
  (lambda (x-pixels)
    (let* ((y-pixels (ceiling (* 5/2 x-pixels)))
	   (graphics-window
	     (make-unscrollable-graphics-window
	       x-pixels y-pixels %temperature-background-color%))
	   (x #f)
	   (y #f)
	   (bulb-center #f)
	   (bulb-diameter #f)
	   (left #f)
	   (right #f)
	   (text-x #f)
	   (zero-level #f)
	   (scale-length #f)
	   (title-y #f)
	   (pixel #f)
	   (bar-left #f)
	   (bar-right #f)
	   (title #f)
	   (current-value 0))
      (tell graphics-window 'set-icon-label title)
      (if* (exists? %temperature-icon-image%)
	(tell graphics-window 'set-icon-image %temperature-icon-image%))
      (if* (exists? %temperature-window-title%)
	(tell graphics-window 'set-window-title %temperature-window-title%))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'temperature-window)
	    (initialize-parameters ()
	      (set! x-pixels (tell graphics-window 'get-visible-w))
	      (set! y-pixels (tell graphics-window 'get-visible-h))
	      (set! x (if (and (eq? *platform* 'windows)
			       (< x-pixels %smallest-window-width%))
			  (+ 1/2 (/ (- %smallest-window-width% x-pixels) (* 2 x-pixels)))
			  1/2))
	      (set! y 3/10)
	      (set! bulb-center `(,x ,y))
	      (set! bulb-diameter 3/10)
	      (set! left (- x 1/20))
	      (set! right (+ x 1/20))
	      (set! text-x (+ x 3/25))
	      (set! zero-level 1/2)
	      (set! scale-length 29/20)
	      (set! title-y (+ y 19/10))
	      (set! pixel (tell graphics-window 'get-width-per-pixel))
	      (set! bar-left (+ left (* 2 pixel)))
	      (set! bar-right (- right (* 2 pixel)))
	      (select-temperature-fonts x-pixels y-pixels)
	      (if (< x-pixels (tell %temperature-title-font% 'get-pixel-width "Temperature"))
		  (set! title "Temp.")
		  (set! title "Temperature"))
	      'done)
	    (draw-graphics ()
	      (tell graphics-window 'draw
		`(let-sgl ((font ,%temperature-title-font%)
			   (text-justification center))
		   (text (,x ,title-y) ,title))
		'title)
	      (draw-thermometer graphics-window bulb-center bulb-diameter)
	      (let ((new-level (+ zero-level (* (% current-value) scale-length))))
		(tell graphics-window 'draw
		  (mercury-pexp bar-left zero-level bar-right new-level
		    %thermometer-mercury-color%)
		  'bar)
		(tell graphics-window 'draw
		  `(let-sgl ((font ,%temperature-value-font%)
			     (origin (,text-x ,new-level)))
		     (text (text-relative (0 -2/5)) ,(format "~a" current-value)))
		  'value)))
	    (update-graphics (new-value)
	      (if* (not (= new-value current-value))
		(tell graphics-window 'retag 'bar 'garbage)
		(let ((new-level (+ zero-level (* (% new-value) scale-length))))
		  (tell graphics-window 'draw
		    (mercury-pexp bar-left zero-level bar-right new-level
		      %thermometer-mercury-color%)
		    'bar)
		  (tell graphics-window 'delete 'garbage)
		  (tell graphics-window 'delete 'value)
		  (tell graphics-window 'draw
		    `(let-sgl ((font ,%temperature-value-font%)
			       (origin (,text-x ,new-level)))
		       (text (text-relative (0 -2/5)) ,(format "~a" new-value)))
		    'value))
		(set! current-value new-value))
	      'done)
	    (resize (new-width new-height)
	      (tell self 'initialize-parameters)
	      (tell graphics-window 'retag 'all 'garbage)
	      (tell self 'draw-graphics)
	      (tell graphics-window 'delete 'garbage))
	    (initialize ()
	      (tell self 'initialize-parameters)
	      (set! current-value 0)
	      (tell graphics-window 'clear)
	      (tell self 'draw-graphics))
	    (else (delegate msg graphics-window))))))))

(define mercury-pexp
  (lambda (left from-level right to-level color)
    (if (= left right)
      `(let-sgl ((foreground-color ,color))
	 (line (,left ,from-level) (,left ,to-level)))
      `(let-sgl ((foreground-color ,color))
	 (filled-rectangle (,left ,from-level) (,right ,to-level))))))

(define draw-thermometer
  (lambda (g bulb-center bulb-diameter)
    (let* ((x (1st bulb-center))
	   (y (2nd bulb-center))
	   (width (* 1/3 bulb-diameter))
	   (bottom y)
	   (top (+ y (* 17 width)))
	   (left (- x (* 1/2 width)))
	   (right (+ x (* 1/2 width)))
	   (pixel (tell g 'get-width-per-pixel))
	   (bar-left (+ left (* 2 pixel)))
	   (bar-right (- right (* 2 pixel)))
	   (zero-level (+ y (* 2 width)))
	   (max-level (- top (* 1/2 width)))
	   (gradation (* 1/10 (- max-level zero-level)))
	   (mark-left (- x (* 3/2 width)))
	   (bigmark-left (- x (* 2 width)))
	   (mark-right (- x width)))
      (tell g 'draw
	`(let-sgl ()
	   (let-sgl ((foreground-color ,=grey=))
	     (line (,left ,bottom) (,left ,top))
	     (line (,right ,bottom) (,right ,top))
	     (let-sgl ((foreground-color ,=white=))
	       (filled-arc
		(,x ,top) (,(- width (* 2 pixel)) ,(- width (* 2 pixel))) 0 180))
	     (arc (,x ,top) (,width ,width) 0 180))
	   (let-sgl ((foreground-color ,%thermometer-mercury-color%))
	     ,(disk bulb-center bulb-diameter))
	   ,(mercury-pexp bar-left bottom bar-right zero-level
	      %thermometer-mercury-color%)
	   ,(mercury-pexp bar-left zero-level bar-right top =white=)
	   (let-sgl ((foreground-color ,=white=)
		     (background-color ,%thermometer-mercury-color%))
	     (ring ,bulb-center ,(* 11/16 bulb-diameter)
	       ,(* 3/8 bulb-diameter) 110 63)
	     ;; this gets rid of any residual white pixels at the bulb center
	     (let-sgl ((foreground-color ,%thermometer-mercury-color%))
	       ,(disk bulb-center (* 1/8 bulb-diameter))))))
      (for* each n in (ascending-index-list 11) do
	(let ((level (+ zero-level (* n gradation))))
	  (tell g 'draw
	    (if (zero? (modulo n 5))
	      `(line (,bigmark-left ,level) (,mark-right ,level))
	      `(line (,mark-left ,level) (,mark-right ,level)))))))))
