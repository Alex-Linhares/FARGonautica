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

(define %arc-height-factor% 15/100)
(define %bridge-dash-density% 35/100)
(define %bridge-zigzag-length% 1/100)


(define bridge-graphics
  (lambda (op bridge)
    (let ((proposal-level (tell bridge 'get-proposal-level)))
      (tell *workspace-window* 'caching-on)
      (case op
	(flash
	  (if (tell bridge 'drawn?)
	    (tell *workspace-window* 'flash (tell bridge 'get-graphics-pexp))
	    (let ((drawn-bridge (tell bridge 'get-drawn-coincident-bridge)))
	      (if* (and (exists? drawn-bridge)
		        (= proposal-level (tell drawn-bridge 'get-proposal-level)))
		(tell *workspace-window* 'flash
		  (tell drawn-bridge 'get-graphics-pexp))))))
	(set-pexp-and-draw
	  (tell bridge 'set-graphics-pexp (make-bridge-pexp bridge proposal-level))
	  (let ((drawn-bridge (tell bridge 'get-drawn-coincident-bridge)))
	    (if* (not (exists? drawn-bridge))
	      (tell *workspace-window* 'draw-bridge bridge))))
	(erase
	  (if* (tell bridge 'drawn?)
	    (tell *workspace-window* 'erase-bridge bridge)
	    ;; Repair damage to bridge's objects:
	    (let ((object1 (if (and (< proposal-level %built%)
				    (tell bridge 'flipped-group1?))
			     (tell bridge 'get-original-group1)
			     (tell bridge 'get-object1)))
		  (object2 (if (and (< proposal-level %built%)
				    (tell bridge 'flipped-group2?))
			     (tell bridge 'get-original-group2)
			     (tell bridge 'get-object2))))
	      (tell *workspace-window* 'draw (tell object1 'get-graphics-pexp))
	      (if (letter? object2)
		(tell *workspace-window* 'draw (tell object2 'get-graphics-pexp))
		(tell *workspace-window* 'draw-group object2)))
	    (let ((pending-bridge (tell bridge 'get-highest-level-coincident-bridge)))
	      (if* (exists? pending-bridge)
		(tell *workspace-window* 'draw-bridge pending-bridge)))))
	(update-level
	  (let ((new-pexp (make-bridge-pexp bridge proposal-level)))
	    (if (tell bridge 'drawn?)
	      (begin
		(tell *workspace-window* 'erase (tell bridge 'get-graphics-pexp))
		(tell bridge 'set-graphics-pexp new-pexp)
		(tell *workspace-window* 'draw-bridge bridge))
	      (begin
		(tell bridge 'set-graphics-pexp new-pexp)
		(let ((drawn-bridge (tell bridge 'get-drawn-coincident-bridge)))
		  (cond
		    ((not (exists? drawn-bridge))
		     (tell *workspace-window* 'draw-bridge bridge))
		    ((> proposal-level (tell drawn-bridge 'get-proposal-level))
		     (tell *workspace-window* 'erase-bridge drawn-bridge)
		     (tell *workspace-window* 'draw-bridge bridge)))))))))
      (tell *workspace-window* 'flush)
      'done)))


(define make-bridge-pexp
  (lambda (bridge proposal-level)
    (case (tell bridge 'get-orientation)
      (horizontal (make-horizontal-bridge-pexp bridge proposal-level))
      (vertical (make-vertical-bridge-pexp bridge proposal-level)))))


(define make-horizontal-bridge-pexp
  (lambda (bridge proposal-level)
    (let* ((left (tell bridge 'get-from-graphics-coord))
	   (right (tell bridge 'get-to-graphics-coord))
	   (x-left (x-coord left))
	   (y-left (y-coord left))
	   (x-right (x-coord right))
	   (y-right (y-coord right))
	   (height (* %arc-height-factor% (- x-right x-left))))
      (cond
	((tell bridge 'group-spanning-bridge?)
	 (make-spanning-horizontal-bridge-pexp left right height proposal-level))
	((and (letter? (tell bridge 'get-object1))
	      (letter? (tell bridge 'get-object2)))
	 (elliptical-horizontal-bridge-arc
	   x-left y-left x-right y-right (* 2/3 height) proposal-level))
	(else (elliptical-horizontal-bridge-arc
		x-left y-left x-right y-right height proposal-level))))))


(define elliptical-horizontal-bridge-arc
  (lambda (x-left y-left x-right y-right arc-height proposal-level)
    (cond
     ((= proposal-level %proposed%)
      (dotted-elliptical-arc
       x-left y-left x-right y-right arc-height %dot-interval%))
     ((= proposal-level %evaluated%)
      (dashed-elliptical-arc x-left y-left x-right y-right arc-height))
     (else (elliptical-arc x-left y-left x-right y-right arc-height)))))


;; not used
(define circular-horizontal-bridge-arc
  (lambda (x-left y-left x-right y-right arc-height proposal-level)
    (cond
     ((= proposal-level %proposed%)
      (dotted-circular-arc
       x-left y-left x-right y-right arc-height %dot-interval%))
     ((= proposal-level %evaluated%)
      (dashed-circular-arc x-left y-left x-right y-right arc-height))
     (else (circular-arc x-left y-left x-right y-right arc-height)))))


(define make-vertical-bridge-pexp
  (lambda (bridge proposal-level)
    (let* ((top (tell bridge 'get-from-graphics-coord))
	   (bot (tell bridge 'get-to-graphics-coord))
	   (x-top (x-coord top))
	   (y-top (y-coord top))
	   (x-bot (x-coord bot))
	   (y-bot (y-coord bot)))
      (cond
       ((tell bridge 'group-spanning-bridge?)
	(make-spanning-vertical-bridge-pexp top bot proposal-level))
       ((= proposal-level %proposed%)
	(dotted-line x-top y-top x-bot y-bot %dot-interval%))
       ((= proposal-level %evaluated%)
	(dashed-line x-top y-top x-bot y-bot %bridge-dash-density% %dash-length%))
       (else
	(let* ((label-coord (- top (/ (- top bot) 3)))
	       (x (x-coord label-coord))
	       (y (y-coord label-coord))
	       (label-num (tell bridge 'get-bridge-label-number)))
	  `(let-sgl ()
	     ,(centered-zigzag-line
		top bot %bridge-zigzag-length% (if (< x-top x-bot) - +))
	     (let-sgl ((font ,%bridge-label-font%)
		       (text-justification center))
	       (text (,x ,y) ,(format "~a" label-num))))))))))


(define make-spanning-horizontal-bridge-pexp
  (lambda (left right height proposal-level)
    (let* ((y-left (y-coord left))
	   (y-right (y-coord right))
	   (delta (abs (- y-left y-right)))
	   (left-offset
	     (make-rectangular 0 (+ height (if (< y-left y-right) delta 0))))
	   (right-offset
	     (make-rectangular 0 (+ height (if (< y-right y-left) delta 0))))
	   (left-corner (+ left left-offset))
	   (right-corner (+ right right-offset)))
      (cond
	((= proposal-level %proposed%)
	 `(polypoints
	    ,@(dotted-line-points
		(x-coord left) (y-coord left)
		(x-coord left-corner) (y-coord left-corner) %dot-interval%)
	    ,@(dotted-line-points
		(x-coord left-corner) (y-coord left-corner)
		(x-coord right-corner) (y-coord right-corner) %dot-interval%)
	    ,@(dotted-line-points
		(x-coord right-corner) (y-coord right-corner)
		(x-coord right) (y-coord right) %dot-interval%)))
	((= proposal-level %evaluated%)
	 `(line
	    ,@(dashed-line-points
		(x-coord left) (y-coord left)
		(x-coord left-corner) (y-coord left-corner)
		%bridge-dash-density% %dash-length%)
	    ,@(dashed-line-points
		(x-coord left-corner) (y-coord left-corner)
		(x-coord right-corner) (y-coord right-corner)
		%bridge-dash-density% %dash-length%)
	    ,@(dashed-line-points
		(x-coord right-corner) (y-coord right-corner)
		(x-coord right) (y-coord right)
		%bridge-dash-density% %dash-length%)))
	(else
	  `(polyline
	     ,@(zigzag-line-points left left-corner %bridge-zigzag-length% +)
	     ,@(zigzag-line-points left-corner right-corner %bridge-zigzag-length% +)
	     ,@(zigzag-line-points right-corner right %bridge-zigzag-length% +)))))))


(define make-spanning-vertical-bridge-pexp
  (lambda (top bot proposal-level)
    (let* ((x-right (tell *workspace-window* 'get-spanning-vertical-bridge-right-x))
	   (top-right (make-rectangular x-right (y-coord top)))
	   (bot-right (make-rectangular x-right (y-coord bot)))
	   (x-top (x-coord top))
	   (y-top (y-coord top))
	   (x-bot (x-coord bot))
	   (y-bot (y-coord bot)))
      (cond
       ((= proposal-level %proposed%)
	`(polypoints
	   ,@(dotted-line-points x-top y-top x-right y-top %dot-interval%)
	   ,@(dotted-line-points x-right y-top x-right y-bot %dot-interval%)
	   ,@(dotted-line-points x-right y-bot x-bot y-bot %dot-interval%)))
       ((= proposal-level %evaluated%)
	`(line
	   ,@(dashed-line-points
	       x-top y-top x-right y-top %bridge-dash-density% %dash-length%)
	   ,@(dashed-line-points
	       x-right y-top x-right y-bot %bridge-dash-density% %dash-length%)
	   ,@(dashed-line-points
	       x-right y-bot x-bot y-bot %bridge-dash-density% %dash-length%)))
       (else
	 `(polyline
	    ,@(zigzag-line-points top top-right %bridge-zigzag-length% +)
	    ,@(zigzag-line-points top-right bot-right %bridge-zigzag-length% -)
	    ,@(zigzag-line-points bot-right bot %bridge-zigzag-length% +)))))))


(define draw-bridge-grope
  (lambda (bridge-orientation object1 object2)
    (case bridge-orientation
      (horizontal (draw-horizontal-bridge-grope object1 object2))
      (vertical (draw-vertical-bridge-grope object1 object2)))))


(define draw-horizontal-bridge-grope
  (lambda (object1 object2)
    (if (both-spanning-groups? object1 object2)
      (tell *workspace-window* 'flash
	(make-spanning-horizontal-bridge-grope-pexp
	  (tell object1 'get-group-spanning-bridge-graphics-coord 'horizontal)
	  (tell object2 'get-group-spanning-bridge-graphics-coord 'horizontal)))
      (tell *workspace-window* 'flash
	(make-horizontal-bridge-grope-pexp
	  (tell object1 'get-bridge-graphics-coord 'horizontal)
	  (tell object2 'get-bridge-graphics-coord 'horizontal))))))


(define draw-vertical-bridge-grope
  (lambda (object1 object2)
    (if (both-spanning-groups? object1 object2)
      (tell *workspace-window* 'flash
	(make-spanning-vertical-bridge-grope-pexp
	  (tell object1 'get-group-spanning-bridge-graphics-coord 'vertical)
	  (tell object2 'get-group-spanning-bridge-graphics-coord 'vertical)))
      (tell *workspace-window* 'flash
	(make-vertical-bridge-grope-pexp
	  (tell object1 'get-bridge-graphics-coord 'vertical)
	  (tell object2 'get-bridge-graphics-coord 'vertical))))))


(define make-horizontal-bridge-grope-pexp
  (lambda (left right)
    (let* ((x-left (x-coord left))
	   (y-left (y-coord left))
	   (x-right (x-coord right))
	   (y-right (y-coord right))
	   (offset (/ (- right left) 10))
	   (voffset (make-rectangular 0 (magnitude offset))))
      `(let-sgl ()
	 (polyline
	   ,@(zigzag-line-points left (+ left voffset)
	       %bridge-zigzag-length% +)
	   ,@(zigzag-line-points (+ left voffset) (+ left voffset offset)
	       %bridge-zigzag-length% +))
	 (polyline
	   ,@(zigzag-line-points right (+ right voffset)
	       %bridge-zigzag-length% -)
	   ,@(zigzag-line-points (+ right voffset) (+ right voffset (- offset))
	       %bridge-zigzag-length% -))))))


(define make-spanning-horizontal-bridge-grope-pexp
  make-horizontal-bridge-grope-pexp)


(define make-vertical-bridge-grope-pexp
  (lambda (top bot)
    (let* ((offset (/ (- top bot) 4))
	   (top-prime (- top offset))
	   (bot-prime (+ bot offset))
	   (sign (if (< (x-coord top) (x-coord bot)) + -)))
      `(let-sgl ()
	 ,(centered-zigzag-line top top-prime %bridge-zigzag-length% sign)
	 ,(centered-zigzag-line bot-prime bot %bridge-zigzag-length% sign)))))


(define make-spanning-vertical-bridge-grope-pexp
  (let ((offset (make-rectangular 0 (* 1/2 %bridge-zigzag-length%))))
    (lambda (top bot)
      (let* ((top (+ top offset))
	     (bot (- bot offset))
	     (right (tell *workspace-window* 'get-spanning-vertical-bridge-right-x))
	     (top-right (make-rectangular right (y-coord top)))
	     (bot-right (make-rectangular right (y-coord bot))))
	`(polyline
	  ,@(zigzag-line-points top top-right %bridge-zigzag-length% -)
	  ,@(zigzag-line-points top-right bot-right %bridge-zigzag-length% -)
	  ,@(zigzag-line-points bot-right bot %bridge-zigzag-length% -))))))


(define new-bridge-label-number
  (lambda (bridge)
    (let* ((bridge-type (tell bridge 'get-bridge-type))
	   (other-bridge-numbers
	     (tell-all (remq bridge (tell *workspace* 'get-bridges bridge-type))
	       'get-bridge-label-number))
	   (n 1))
      (repeat* until (not (member? n other-bridge-numbers))
	(set! n (add1 n)))
      n)))
