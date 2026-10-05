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

(define %big-group-arrowhead-length% 1/100)
(define %small-group-arrowhead-length% 3/500)
(define %group-arrowhead-angle% 60)
(define %group-grope-zigzag-length% 1/50)
(define %group-grope-num-of-flashes% 2)


(define group-dashed-line-density
  (lambda (letter-span)
    (max 1/4 (- 49/100 (* letter-span 2/50)))))


(define group-graphics
  (lambda (op group)
    (let ((proposal-level (tell group 'get-proposal-level)))
      (tell *workspace-window* 'caching-on)
      (case op
	(flash
	  (if (tell group 'drawn?)
	    (tell *workspace-window* 'flash (tell group 'get-graphics-pexp))
	    (let ((drawn-group (tell group 'get-drawn-coincident-group)))
	      (if* (and (exists? drawn-group)
		        (= proposal-level (tell drawn-group 'get-proposal-level)))
		(tell *workspace-window* 'flash (tell drawn-group 'get-graphics-pexp))))))
	(set-pexp-and-draw
	  (tell group 'set-graphics-pexp (make-group-pexp group proposal-level))
	  (let ((drawn-group (tell group 'get-drawn-coincident-group)))
	    (if* (not (exists? drawn-group))
	      (tell *workspace-window* 'draw-group group))))
	(erase
	  (if* (tell group 'drawn?)
	    (tell *workspace-window* 'erase-group group)
	    ;; Repair damage to any overlapping groups:
	    (for* each g in (tell group 'get-drawn-overlapping-groups) do
	      (tell *workspace-window* 'draw-group g))
	    (let ((pending-group (tell group 'get-highest-level-coincident-group)))
	      (if* (exists? pending-group)
		(tell *workspace-window* 'draw-group pending-group)))))
	(update-level
	  (let ((new-pexp (make-group-pexp group proposal-level)))
	    (if (tell group 'drawn?)
	      (begin
		(tell *workspace-window* 'erase (tell group 'get-graphics-pexp))
		(tell group 'set-graphics-pexp new-pexp)
		(tell *workspace-window* 'draw-group group))
	      (begin
		(tell group 'set-graphics-pexp new-pexp)
		(let ((drawn-group (tell group 'get-drawn-coincident-group)))
		  (cond
		    ((not (exists? drawn-group))
		     (tell *workspace-window* 'draw-group group))
		    ((> proposal-level (tell drawn-group 'get-proposal-level))
		     (tell *workspace-window* 'erase-group drawn-group)
		     (tell *workspace-window* 'draw-group group)))))))))
      (tell *workspace-window* 'flush)
      'done)))


(define make-group-pexp
  (lambda (group proposal-level)
    (let* ((x1 (tell group 'get-graphics-x1))
	   (y1 (tell group 'get-graphics-y1))
	   (x2 (tell group 'get-graphics-x2))
	   (y2 (tell group 'get-graphics-y2))
	   (x-mid (/ (+ x1 x2) 2))
	   (letter-span (tell group 'get-letter-span))
	   (rectangle-pexp
	    (cond
	     ((= proposal-level %proposed%)
	      (dotted-box x1 y1 x2 y2 %dot-interval%))
	     ((= proposal-level %evaluated%)
	      (dashed-box
		x1 y1 x2 y2 (group-dashed-line-density letter-span) %dash-length%))
	     ((= proposal-level %built%) (outline-box x1 y1 x2 y2))))
	   (direction (tell group 'get-direction)))
      (if (exists? direction)
	  `(let-sgl ()
	     ,rectangle-pexp
	     ,(arrowhead
		x-mid y2 (if (eq? direction plato-right) 0 180)
		(if (or (< proposal-level %built%)
		        (= letter-span 1))
		  %small-group-arrowhead-length%
		  %big-group-arrowhead-length%)
		%group-arrowhead-angle%))
	  (let ((letcat-pexp (tell group 'get-letcat-graphics-pexp)))
	    (if (and (exists? letcat-pexp)
		     (= proposal-level %built%))
	      `(let-sgl ,(2nd letcat-pexp) ,rectangle-pexp ,(3rd letcat-pexp))
	      rectangle-pexp))))))


(define draw-group-grope
  (lambda (group)
    (let* ((x1 (tell group 'get-graphics-x1))
	   (y1 (tell group 'get-graphics-y1))
	   (x2 (tell group 'get-graphics-x2))
	   (y2 (tell group 'get-graphics-y2))
	   (grope-pexp (make-group-grope-pexp x1 y1 x2 y2)))
      (tell *workspace-window* 'flash grope-pexp))))


(define make-group-grope-pexp
  (lambda (x1 y1 x2 y2)
    (let ((mid-left (+ x1 (* 1/4 (- x2 x1))))
	  (mid-right (- x2 (* 1/4 (- x2 x1)))))
      `(let-sgl ()
	 (polyline
	   ,@(zigzag-line-points
	       (make-rectangular mid-left y1)
	       (make-rectangular x1 y1)
	       %group-grope-zigzag-length% -)
	   ,@(zigzag-line-points
	       (make-rectangular x1 y1)
	       (make-rectangular x1 y2)
	       %group-grope-zigzag-length% +)
	   ,@(zigzag-line-points
	       (make-rectangular x1 y2)
	       (make-rectangular mid-left y2)
	       %group-grope-zigzag-length% -))
	 (polyline
	   ,@(zigzag-line-points
	       (make-rectangular mid-right y1)
	       (make-rectangular x2 y1)
	       %group-grope-zigzag-length% +)
	   ,@(zigzag-line-points
	       (make-rectangular x2 y1)
	       (make-rectangular x2 y2)
	       %group-grope-zigzag-length% -)
	   ,@(zigzag-line-points
	       (make-rectangular x2 y2)
	       (make-rectangular mid-right y2)
	       %group-grope-zigzag-length% +))))))
