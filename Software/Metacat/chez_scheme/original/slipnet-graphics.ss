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

(define select-slipnet-fonts
  (lambda (win-width win-height)
    (let ((desired-title-height (round (* 80/1000 win-height)))
	  (desired-label-height (round (* 35/1000 win-height))))
      (set! %slipnet-title-font%
	(make-mfont sans-serif (- desired-title-height) '(bold italic)))
      (set! %slipnode-label-font%
	(make-mfont fancy (- desired-label-height) '(italic))))))

(define make-slipnet-window
  (lambda (slipnet-layout-table . optional-args)
    (let* ((width
	     (if (null? optional-args)
	       %default-13x5-slipnet-width%
	       (1st optional-args)))
	   (window (new-slipnet-window slipnet-layout-table width)))
      (tell window 'initialize)
      window)))

(define new-slipnet-window
  (lambda (slipnet-layout-table x-pixels)
    (let* ((x-dim (row-dimension slipnet-layout-table))
	   (y-dim (column-dimension slipnet-layout-table))
	   (x-deltas (+ (* 3 x-dim) 1))
	   (y-deltas (+ (* 3 y-dim) 4))
	   (y-pixels (ceiling (* y-deltas (/ x-pixels x-deltas))))
	   (delta (/ 1 x-deltas))
	   (max-activation-diameter (* 2 delta))
	   (title-coord `(1/2 ,(* (- y-deltas 2) delta)))
	   (graphics-window
	     (make-unscrollable-graphics-window x-pixels y-pixels
	       %slipnet-background-color%))
	   (current-title-pexp #f)
	   (slipnode-labels-pexp '()))
      (tell graphics-window 'set-icon-label %slipnet-icon-label%)
      (if* (exists? %slipnet-icon-image%)
	(tell graphics-window 'set-icon-image %slipnet-icon-image%))
      (if* (exists? %slipnet-window-title%)
	(tell graphics-window 'set-window-title %slipnet-window-title%))
      (select-slipnet-fonts x-pixels y-pixels)
      (for-each-table-element* (slipnet-layout-table i j) do
        (let ((slipnode (table-ref slipnet-layout-table i j)))
	  (if* (exists? slipnode)
	       (let* ((x (+ (* (* 3 delta) i) (* 2 delta)))
		      (y (+ (* (* 3 delta) j) (* 3 delta)))
		      (center `(,x ,y))
		      (label-coord `(,x ,(- y (* 13/8 delta)))))
		 (tell slipnode 'set-graphics-coord center)
		 (tell slipnode 'set-graphics-label-coord label-coord)
		 (set! slipnode-labels-pexp
		   (cons `(text ,label-coord ,(tell slipnode 'get-short-name))
		     slipnode-labels-pexp))))))
;;      ;; check here to make sure labels don't overlap; if they do,
;;      ;; select a smaller size font for %slipnode-label-font%
;;      (check-for-label-overlap slipnet-layout-table graphics-window)
      (set! slipnode-labels-pexp
	`(let-sgl ((text-justification center))
	   ,@slipnode-labels-pexp))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'slipnet-window)
	    (garbage-collect () (tell self 'update-graphics))
            (update-graphics ()
	      (tell graphics-window 'retag 'activation 'garbage)
	      (for* each node in *slipnet-nodes* do
		(tell node 'draw-activation-graphics self))
	      (tell graphics-window 'delete 'garbage))
	    (erase-all-activations ()
	      (tell graphics-window 'delete 'activation))
            (draw-activation (center activation frozen?)
	      (tell graphics-window 'draw
		`(let-sgl ((foreground-color
			     ,(if frozen?
				%frozen-slipnode-activation-color%
				%slipnode-activation-color%)))
		   ,(disk center (* (% activation) max-activation-diameter)))
		'activation))
	    (draw-title (text)
	      (tell self 'erase-title)
	      (if* (exists? text)
		(set! current-title-pexp
		  `(let-sgl ((text-justification center)) (text ,title-coord ,text)))
		(tell graphics-window 'draw
		  `(let-sgl ((font ,%slipnet-title-font%)) ,current-title-pexp)
		  'title))
	      'done)
	    (erase-title ()
	      (if* (exists? current-title-pexp)
		(tell graphics-window 'delete 'title)
		(set! current-title-pexp #f))
	      'done)
	    (resize (new-width new-height)
	      (let ((old-width x-pixels)
		    (old-height y-pixels))
		(set! x-pixels new-width)
		(set! y-pixels new-height)
		(select-slipnet-fonts x-pixels y-pixels)
		(tell graphics-window 'retag 'title 'garbage)
		(tell graphics-window 'retag 'label 'garbage)
		(tell graphics-window 'draw
		  `(let-sgl ((font ,%slipnet-title-font%)) ,current-title-pexp)
		  'title)
		(tell graphics-window 'draw
		  `(let-sgl ((font ,%slipnode-label-font%)) ,slipnode-labels-pexp)
		  'label)
		(tell graphics-window 'delete 'garbage)
		(tell graphics-window 'rescale 'activation
		  (/ new-width old-width) (/ new-height old-height))))
	    (initialize ()
	      (tell graphics-window 'clear)
	      (tell self 'draw-title "Slipnet Activation")
	      (tell graphics-window 'draw
		`(let-sgl ((font ,%slipnode-label-font%))
		   ,slipnode-labels-pexp)
		'label))
	    (clear () (tell self 'blank-window))
	    (blank-window ()
	      (tell self 'draw-title "Slipnet Activation")
	      (tell graphics-window 'delete 'activation))
	    ;; title = #f just erases the current title (if any)
	    (display-patterns (concept-patterns color title)
	      (tell self 'draw-title title)
	      (tell graphics-window 'delete 'activation)
	      (for* each pattern in concept-patterns do
		(for* each entry in (rest pattern) do
		  (tell self 'display-activation (1st entry) (2nd entry) color))))
	    (display-activation (node value color)
	      (let ((center (tell node 'get-graphics-coord)))
		(tell graphics-window 'draw
		  `(let-sgl ()
		     (let-sgl ((foreground-color ,color))
		       ,(disk center (* (% value) max-activation-diameter)))
		     ,(circle center max-activation-diameter))
		  'activation)))
	    (restore-current-state ()
	      (tell self 'draw-title "Slipnet Activation")
	      (tell graphics-window 'delete 'activation)
	      (if* %slipnet-graphics%
		(for* each node in *slipnet-nodes* do
		  (tell node 'draw-activation-graphics self))))
	    (else (delegate msg graphics-window))))))))


(define *13x5-layout-table*
  (slipnet-layout-table*
    (plato-opposite plato-string-position-category plato-leftmost plato-middle
      plato-rightmost plato-whole plato-single plato-object-category plato-letter
      plato-group plato-alphabetic-position-category plato-alphabetic-first
      plato-alphabetic-last)
    (plato-identity plato-direction-category plato-left plato-right
      plato-bond-category plato-predecessor plato-successor plato-sameness
      plato-group-category plato-predgrp plato-succgrp plato-samegrp
      plato-letter-category)
    (plato-a plato-b plato-c plato-d plato-e plato-f plato-g
      plato-h plato-i plato-j plato-k plato-l plato-m)
    (plato-n plato-o plato-p plato-q plato-r plato-s plato-t
      plato-u plato-v plato-w plato-x plato-y plato-z)
    (#f #f #f plato-length plato-one plato-two plato-three
      plato-four plato-five plato-bond-facet #f #f #f)))


;;-------------------------------------------------------------------------
;;
;;(define check-for-label-overlap
;;  (lambda (layout-table graphics-window)
;;    (for-each-table-element* (layout-table i j) do
;;      (let ((slipnode (table-ref layout-table i j)))
;;	(if* (and (exists? slipnode)
;;		  (< i (- (row-dimension layout-table) 1)))
;;	  (let ((neighbor (table-ref layout-table (+ i 1) j)))
;;	    (if* (exists? neighbor)
;;	      (let* ((label1 (tell slipnode 'get-short-name))
;;		     (label2 (tell neighbor 'get-short-name))
;;		     (coord1 (tell slipnode 'get-graphics-label-coord))
;;		     (coord2 (tell neighbor 'get-graphics-label-coord))
;;		     (distance (- (car coord2) (car coord1)))
;;		     (width1 (tell graphics-window 'get-string-width label1
;;				   %slipnode-label-font%))
;;		     (width2 (tell graphics-window 'get-string-width label2
;;				   %slipnode-label-font%)))
;;		(if* (>= (/ (+ width1 width2) 2) distance)
;;		  (printf "Labels ~a and ~a overlap~n" label1 label2)
;;		  (printf "switching from ~s~n"
;;		    (tell %slipnode-label-font% 'get-actual-values))
;;		  (let ((new-font
;;			 (make-mfont
;;			   (tell %slipnode-label-font% 'get-face)
;;			   (- (tell %slipnode-label-font% 'get-size) 1)
;;			   (tell %slipnode-label-font% 'get-style))))
;;		    (set! %slipnode-label-font% new-font)
;;		    (printf "            to ~s~n"
;;		      (tell %slipnode-label-font% 'get-actual-values))))))))))))
