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


;; <layout> = (<top-window-layout> <bottom-window-layout> <vertical-window-layout>)
;; <window-layout> =  (<theme-type> <rows> <cols> <panel-orientation>)
;; where <rows> or <cols> = * means "unbounded"

(define *themespace-window-layout*
  '((top-bridge 2 * horizontal)
    (bottom-bridge 2 * horizontal)
    (vertical-bridge * 2 vertical)))

(define *theme-edit-mode?* #f)

;;------------------------------------------------------------------------------------
;; mouse handlers

(define theme-window-left-press-handler
  (lambda (theme-type)
    (lambda (win x y)
      (if* *theme-edit-mode?*
	(if (tell *control-panel* 'ready-to-edit? theme-type)
	  (tell *control-panel* 'edit-theme-type theme-type)
	  (let ((theme (select-meth (tell *themespace* 'get-themes theme-type) 'clicked? x y)))
	    (if* (exists? theme)
	      (let ((dimension (tell theme 'get-dimension))
		    (relation (tell theme 'get-relation))
		    (activation (tell theme 'get-activation)))
		(set-themes theme-type dimension 0)
		(if* (not (= activation 100))
		  (set-themes theme-type dimension relation 100))))))))))

(define theme-window-right-press-handler
  (lambda (theme-type)
    (lambda (win x y)
      (if* *theme-edit-mode?*
	(if (tell *control-panel* 'ready-to-edit? theme-type)
	  (tell *control-panel* 'edit-theme-type theme-type)
	  (let ((theme (select-meth (tell *themespace* 'get-themes theme-type) 'clicked? x y)))
	    (if* (exists? theme)
	      (let ((dimension (tell theme 'get-dimension))
		    (relation (tell theme 'get-relation))
		    (activation (tell theme 'get-activation)))
		(set-themes theme-type dimension 0)
		(if* (not (= activation -100))
		  (set-themes theme-type dimension relation -100))))))))))

(define set-theme-window-mouse-handlers
  (lambda (window theme-type)
    (tell window 'set-mouse-handlers
      (theme-window-left-press-handler theme-type)
      (theme-window-right-press-handler theme-type))))

;;------------------------------------------------------------------------------------

;; (make-themespace-window <layout>
;;    [<top-window-pixel-size> <bottom-window-pixel-size> <vertical-window-pixel-size>])
;; Examples:
;; (make-themespace-window *themespace-window-layout*)
;; (make-themespace-window *themespace-window-layout* '(785 130) '(785 130) '(150 595))

(define make-themespace-window
  (lambda args
    (let ((window (apply new-themespace-window args)))
      (tell window 'initialize)
      window)))

(define new-themespace-window
  (lambda (layout . optional-args)
    (let* ((top-window
	     (if (null? optional-args)
	       (make-bridge-themes-window (1st layout))
	       (make-bridge-themes-window (1st layout) (1st optional-args))))
	   (bottom-window
	     (if (null? optional-args)
	       (make-bridge-themes-window (2nd layout))
	       (make-bridge-themes-window (2nd layout) (2nd optional-args))))
	   (vertical-window
	     (if (null? optional-args)
	       (make-bridge-themes-window (3rd layout))
	       (make-bridge-themes-window (3rd layout) (3rd optional-args))))
	   (all-windows (list top-window bottom-window vertical-window)))
      (set-theme-window-mouse-handlers top-window 'top-bridge)
      (set-theme-window-mouse-handlers bottom-window 'bottom-bridge)
      (set-theme-window-mouse-handlers vertical-window 'vertical-bridge)
      (tell top-window 'set-icon-label %top-bridge-themes-icon-label%)
      (if* (exists? %top-bridge-themes-icon-image%)
	(tell top-window 'set-icon-image %top-bridge-themes-icon-image%))
      (if* (exists? %top-bridge-themes-window-title%)
	(tell top-window 'set-window-title %top-bridge-themes-window-title%))
      (tell bottom-window 'set-icon-label %bottom-bridge-themes-icon-label%)
      (if* (exists? %bottom-bridge-themes-icon-image%)
	(tell bottom-window 'set-icon-image %bottom-bridge-themes-icon-image%))
      (if* (exists? %bottom-bridge-themes-window-title%)
	(tell bottom-window 'set-window-title %bottom-bridge-themes-window-title%))
      (tell vertical-window 'set-icon-label %vertical-bridge-themes-icon-label%)
      (if* (exists? %vertical-bridge-themes-icon-image%)
	(tell vertical-window 'set-icon-image %vertical-bridge-themes-icon-image%))
      (if* (exists? %vertical-bridge-themes-window-title%)
	(tell vertical-window 'set-window-title %vertical-bridge-themes-window-title%))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'themespace-window)
	    (get-window (theme-type)
	      (case theme-type
		(top-bridge top-window)
		(bottom-bridge bottom-window)
		(vertical-bridge vertical-window)))
	    (set-theme-graphics-parameters (theme)
	      (let ((window (tell self 'get-window (tell theme 'get-theme-type))))
		(tell window 'set-theme-graphics-parameters theme)))
	    (set-theme-graphics-parameters-and-draw (theme)
	      (let ((window (tell self 'get-window (tell theme 'get-theme-type))))
		(tell window 'set-theme-graphics-parameters-and-draw theme)))
	    (erase-theme (theme)
	      (let ((window (tell self 'get-window (tell theme 'get-theme-type))))
		(tell window 'erase-theme theme)))
	    (erase-all-themes theme-types
	      (tell self 'apply-method 'erase-all-themes theme-types))
	    (update-graphics theme-types
	      (tell self 'apply-method 'update-graphics theme-types))
	    (initialize theme-types
	      (tell self 'apply-method 'initialize theme-types))
	    (redraw theme-types
	      (tell self 'apply-method 'redraw theme-types))
	    (garbage-collect ()
	      (for* each window in all-windows do
		(tell window 'garbage-collect))
	      'done)
	    (update-thematic-pressure theme-types
	      (tell self 'apply-method 'update-thematic-pressure theme-types))
	    (apply-method (method-name theme-types)
	      (if (null? theme-types)
		(for* each window in all-windows do
		  (tell window method-name))
		(for* each theme-type in theme-types do
		  (tell (tell self 'get-window theme-type) method-name)))
	      'done)
	    (raise-window ()
	      (tell top-window 'raise-window)
	      (tell vertical-window 'raise-window)
	      (if* %justify-mode%
		(tell bottom-window 'raise-window)))
	    (else (delegate-to-all msg top-window vertical-window bottom-window))))))))

(define *panel-order*
  (list
    plato-letter-category plato-length plato-string-position-category
    plato-group-category plato-direction-category plato-alphabetic-position-category
    plato-object-category plato-bond-facet plato-bond-category))

(define *panel-theme-order*
  (list plato-identity plato-successor plato-predecessor plato-opposite #f))

(define select-theme-dimension-normal-font
  (lambda (orientation win-width win-height)
    (case orientation
      (horizontal
	(let ((desired-font-height (round (* 15/140 win-height))))
	  (make-mfont serif (- desired-font-height) '(italic))))
      (vertical
        (let ((desired-font-height (round (* 23/1000 win-height))))
	  (make-mfont serif (- desired-font-height) '(italic)))))))

(define select-theme-dimension-highlight-font
  (lambda (orientation win-width win-height)
    (case orientation
      (horizontal
	(let ((desired-font-height (round (* 15/140 win-height))))
	  (make-mfont serif (- desired-font-height) '(bold italic))))
      (vertical
        (let ((desired-font-height (round (* 23/1000 win-height))))
	  (make-mfont serif (- desired-font-height) '(bold italic)))))))

(define select-theme-relation-normal-font
  (lambda (orientation win-width win-height)
    (case orientation
      (horizontal
	(let ((desired-font-height (round (* 15/140 win-height))))
	  (make-mfont serif (- desired-font-height) '(italic))))
      (vertical
        (let ((desired-font-height (round (* 23/1000 win-height))))
	  (make-mfont serif (- desired-font-height) '(italic)))))))

(define select-theme-relation-highlight-font
  (lambda (orientation win-width win-height)
    (case orientation
      (horizontal
	(let ((desired-font-height (round (* 15/140 win-height))))
	  (make-mfont serif (- desired-font-height) '(bold italic))))
      (vertical
        (let ((desired-font-height (round (* 23/1000 win-height))))
	  (make-mfont serif (- desired-font-height) '(bold italic)))))))

(define resize-theme-fonts
  (lambda (dim-normal-font dim-highlight-font rel-normal-font rel-highlight-font
	    orientation win-width win-height)
    (case orientation
      (horizontal
	(tell dim-normal-font 'resize (round (* 15/140 win-height)))
	(tell dim-highlight-font 'resize (round (* 15/140 win-height)))
	(tell rel-normal-font 'resize (round (* 15/140 win-height)))
	(tell rel-highlight-font 'resize (round (* 15/140 win-height))))
      (vertical
	(tell dim-normal-font 'resize (round (* 23/1000 win-height)))
	(tell dim-highlight-font 'resize (round (* 23/1000 win-height)))
	(tell rel-normal-font 'resize (round (* 23/1000 win-height)))
	(tell rel-highlight-font 'resize (round (* 23/1000 win-height)))))))

(define select-bridge-themes-window-pixel-size
  (lambda (layout)
    (case (1st layout)
      (top-bridge %top-theme-window-size%)
      (bottom-bridge %bottom-theme-window-size%)
      (vertical-bridge %vertical-theme-window-size%)
      (fake %top-theme-window-size%))))

(define make-bridge-themes-window
  (lambda (layout . optional-args)
    (let* ((pixels
	     (if (null? optional-args)
	       (select-bridge-themes-window-pixel-size layout)
	       (1st optional-args)))
	   (x-pixels (1st pixels))
	   (y-pixels (2nd pixels))
	   (theme-type (1st layout))
	   (bg-color:pressure-on %theme-background-color:thematic-pressure-on%)
	   (bg-color:pressure-off %theme-background-color:thematic-pressure-off%)
	   (graphics-window
	     (make-unscrollable-graphics-window x-pixels y-pixels
	       %theme-background-color:thematic-pressure-off%))
	   (dimensions
	     (sort-wrt-order
	       (remq plato-bond-category (tell *themespace* 'get-dimensions))
	       *panel-order*))
	   (panel-relations
	     (map (lambda (dimension)
		    (sort-wrt-order
		      (tell *themespace* 'get-relations theme-type dimension)
		      *panel-theme-order*))
	       dimensions))
	   (num-panels (length dimensions))
	   (max-panel-relations (apply max (map length panel-relations)))
	   (row-major? (number? (3rd layout)))
	   (rows (if row-major? (ceiling (/ num-panels (3rd layout))) (2nd layout)))
	   (cols (if row-major? (3rd layout) (ceiling (/ num-panels (2nd layout)))))
	   (panel-orientation (4th layout))
	   (panel-x-length (/ 1 cols))
	   (panel-y-length (/ y-pixels (* x-pixels rows)))
	   (panel-x-pixels (/ x-pixels cols))
	   (panel-y-pixels (/ y-pixels rows))
	   (dim-normal-font
	     (select-theme-dimension-normal-font panel-orientation x-pixels y-pixels))
	   (dim-highlight-font
	     (select-theme-dimension-highlight-font panel-orientation x-pixels y-pixels))
	   (rel-normal-font
	     (select-theme-relation-normal-font panel-orientation x-pixels y-pixels))
	   (rel-highlight-font
	     (select-theme-relation-highlight-font panel-orientation x-pixels y-pixels))
	   (dim-name-height
	     (tell graphics-window 'get-character-height " " dim-highlight-font))
	   (dim-name-offset
	     (tell graphics-window 'get-text-offset dim-highlight-font))
	   (rel-name-height
	     (tell graphics-window 'get-character-height " " rel-highlight-font))
	   (rel-name-offset
	     (tell graphics-window 'get-text-offset rel-highlight-font))
	   (get-rel-name-width
	     (lambda (relation)
	       (tell graphics-window 'get-string-width
		 (relation-name relation) rel-highlight-font)))
	   (max-rel-name-width
	     (apply max (map get-rel-name-width (apply append panel-relations))))
	   (top-margin
	     (case panel-orientation
	       (horizontal (* 1/4 rel-name-height))
	       (vertical 0)))
	   (bottom-margin (* 1/4 dim-name-height))
	   (get-dim-name
	     (case panel-orientation
	       (horizontal dimension-name)
	       (vertical abbreviated-dimension-name)))
	   (dim-name-coord
	     `(,(* 1/2 panel-x-length) ,(+ bottom-margin dim-name-offset)))
	   (max-activation-diameter
	     (case panel-orientation
	       (horizontal
		 (min
		   (/ panel-x-length max-panel-relations)
		   (- panel-y-length dim-name-height rel-name-height
		      top-margin bottom-margin)))
	       (vertical
		 (- (min
		      (- panel-x-length max-rel-name-width)
		      (/ (- panel-y-length dim-name-height top-margin bottom-margin)
			 max-panel-relations))
		    ;; Fudge to avoid corrupting panel outline:
		    (* 3 (tell graphics-window 'get-width-per-pixel))))))
	   (full-activation-diameter (* 9/10 max-activation-diameter))
	   (compute-lower-left
	     (if row-major?
	       (lambda (n)
		 `(,(* panel-x-length (remainder n cols))
		   ,(* panel-y-length (- rows (quotient n cols) 1))))
	       (lambda (n)
		 `(,(* panel-x-length (quotient n rows))
		   ,(* panel-y-length (- rows (remainder n rows) 1))))))
	   (compute-panel-info
	     (case panel-orientation
	       (horizontal
		 (compute-horizontal-panel-info
		   panel-x-length panel-y-length full-activation-diameter
		   top-margin bottom-margin dim-name-height rel-name-height
		   rel-name-offset))
	       (vertical
		 (compute-vertical-panel-info
		   panel-x-length panel-y-length full-activation-diameter
		   top-margin bottom-margin dim-name-height rel-name-height
		   rel-name-offset get-rel-name-width))))
	   (panels
	     (map (lambda (i dimension relations)
		    (let* ((lower-left (compute-lower-left i))
			   (panel-info (compute-panel-info relations lower-left))
			   (upper-right
			     (map + lower-left `(,panel-x-length ,panel-y-length)))
			   (lower-right `(,(1st upper-right) ,(2nd lower-left)))
			   (upper-left `(,(1st lower-left) ,(2nd upper-right)))
			   (dim-name (get-dim-name dimension))
			   (coord (map + lower-left dim-name-coord)))
		      (make-panel
			graphics-window dimension theme-type panel-info
			dim-name coord lower-left upper-right lower-right upper-left
			full-activation-diameter max-activation-diameter
			dim-normal-font dim-highlight-font
			rel-normal-font rel-highlight-font)))
	       (ascending-index-list num-panels) dimensions panel-relations))
	   (thematic-pressure? #f))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'bridge-themes-window)
	    (get-all-panels () panels)
	    (show-fonts ()
	      (printf "---------------------------------------~n")
	      (printf "~a themespace window~n" theme-type)
	      (printf "Dimension-normal font:~n")
	      (tell dim-normal-font 'print)
	      (printf "Dimension-highlight font:~n")
	      (tell dim-highlight-font 'print)
	      (printf "Relation-normal font:~n")
	      (tell rel-normal-font 'print)
	      (printf "Relation-highlight font:~n")
	      (tell rel-highlight-font 'print)
	      'done)
	    (display-edit-mode-message ()
	      (if* (tell *control-panel* 'ready-to-edit? theme-type)
		(let ((center (tell graphics-window 'get-center-coord)))
		  (tell graphics-window 'draw
		    `(let-sgl ((font ,dim-normal-font)
			       (text-justification center))
		       (text ,center "Click to select themes"))))))
	    (get-background-colors ()
	      (list bg-color:pressure-on bg-color:pressure-off))
	    (set-background-colors (color1 color2)
	      (set! bg-color:pressure-on color1)
	      (set! bg-color:pressure-off color2)
	      (for* each panel in panels do
		(tell panel 'set-background-colors color1 color2))
	      (tell graphics-window 'set-background-color color2))
	    ;; Some themes in themespace may not appear in the
	    ;; themespace-window, such as bond-category themes.
	    ;; Such themes do not have associated panels:
	    (set-theme-graphics-parameters (theme)
	      (let ((panel (select-meth panels 'dimension?
			     (tell theme 'get-dimension))))
		(if* (exists? panel)
		  (tell panel 'set-theme-graphics-parameters theme)
		  (tell panel 'add-relation (tell theme 'get-relation))))
	      'done)
	    (set-theme-graphics-parameters-and-draw (theme)
	      (let ((panel (select-meth panels 'dimension?
			     (tell theme 'get-dimension))))
		(if* (exists? panel)
		  (tell panel 'set-theme-graphics-parameters theme)
		  (tell panel 'add-relation (tell theme 'get-relation))
		  (tell graphics-window 'caching-on)
		  (tell panel 'draw-panel)
		  (tell graphics-window 'flush)))
	      'done)
	    (update-thematic-pressure ()
	      (if* (not (eq? thematic-pressure?
			     (tell *themespace* 'thematic-pressure? theme-type)))
		(set! thematic-pressure? (not thematic-pressure?))
		(let ((highlight-color
			(if thematic-pressure?
			  %panel-highlight-color:thematic-pressure-on%
			  %panel-highlight-color:thematic-pressure-off%))
		      (background-color
			(if thematic-pressure?
			  bg-color:pressure-on
			  bg-color:pressure-off)))
		  (tell graphics-window 'set-background-color background-color)
		  (for* each panel in panels do
		    (tell panel 'set-highlight-color highlight-color))
		  (tell self 'redraw)))
	      'done)
	    (erase-theme (theme)
	      (let ((panel (tell theme 'get-graphics-panel)))
		(if* (exists? panel)
		  (tell panel 'remove-relation (tell theme 'get-relation))
		  (tell graphics-window 'caching-on)
		  (tell panel 'draw-panel)
		  (tell graphics-window 'flush)))
	      'done)
	    (erase-all-themes ()
	      (for* each panel in panels do
		(tell panel 'delete-all-relations))
	      (tell self 'redraw)
	      'done)
	    (update-graphics ()
	      (tell graphics-window 'caching-on)
	      (for* each panel in panels do
		(tell panel 'update-graphics))
	      (tell graphics-window 'flush))
	    (initialize ()
	      (tell graphics-window 'caching-on)
	      (tell graphics-window 'clear)
	      (for* each panel in panels do
		(tell panel 'initialize))
	      (tell self 'update-thematic-pressure)
	      (tell graphics-window 'flush))
	    (garbage-collect ()
	      (tell graphics-window 'retag 'all 'garbage)
	      (for* each panel in panels do
		(tell panel 'redraw-panel))
	      (tell graphics-window 'unhide 'highlight)
	      (tell graphics-window 'delete 'garbage))
	    (resize (new-width new-height)
	      (set! x-pixels new-width)
	      (set! y-pixels new-height)
	      (resize-theme-fonts
		dim-normal-font dim-highlight-font rel-normal-font rel-highlight-font
		panel-orientation x-pixels y-pixels)
	      (tell graphics-window 'retag 'all 'garbage)
	      (for* each panel in panels do
		(tell panel 'redraw-panel))
	      (tell graphics-window 'unhide 'highlight)
	      (tell graphics-window 'delete 'garbage)
	      (if* *theme-edit-mode?*
		(tell self 'display-edit-mode-message))
	      'done)
	    (redraw ()
	      (tell graphics-window 'caching-on)
	      ;; Ensure that edges around highlighted panels get
	      ;; reset to the window's original background color:
	      (tell graphics-window 'clear)
	      (for* each panel in panels do
		(tell panel 'draw-panel))
	      (tell graphics-window 'flush))
	    (else (delegate msg graphics-window))))))))


;; panel-info ::= (<relation-info> ...)
;; <relation-info> ::= (<relation> <center-coord> <name-coord> <name>)
;; Example:  (<plato-successor> (0.25 0.5) (0.3 0.7) "succ")

(define make-panel
  (lambda (graphics-window dimension theme-type panel-info dim-name
	    dim-name-coord lower-left upper-right lower-right upper-left
	    full-activation-diameter max-activation-diameter
	    dim-normal-font dim-highlight-font rel-normal-font rel-highlight-font)
    (let* ((delta (* 2 (tell graphics-window 'get-width-per-pixel)))
	   (ll `(,(+ (1st lower-left) delta) ,(+ (2nd lower-left) delta)))
	   (ul `(,(+ (1st upper-left) delta) ,(- (2nd upper-left) delta)))
	   (ur `(,(- (1st upper-right) delta) ,(- (2nd upper-right) delta)))
	   (lr `(,(- (1st lower-right) delta) ,(+ (2nd lower-right) delta)))
	   (outline-pexp `(polyline ,ll ,lr ,ur ,ul ,ll))
	   (relations '())
	   (relation-names-pexps '())
	   (normal-panel-pexp '())
	   (highlighted-panel-pexp '())
	   (theme-cluster #f)
	   (highlight-color %panel-highlight-color:thematic-pressure-off%)
	   (highlighted-theme #f)
	   (bg-color:pressure-on %theme-background-color:thematic-pressure-on%)
	   (bg-color:pressure-off %theme-background-color:thematic-pressure-off%))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'theme-panel)
	    (get-relations () relations)
	    (get-relation-names-pexp () relation-names-pexp)
	    (get-normal-panel-pexp () normal-panel-pexp)
	    (get-highlighted-panel-pexp () highlighted-panel-pexp)
	    (get-graphics-window () graphics-window)
	    (get-highlighted-theme () highlighted-theme)
	    (get-cluster () theme-cluster)
	    (get-activation-diameter () full-activation-diameter)
	    (set-background-colors (color1 color2)
	      (set! bg-color:pressure-on color1)
	      (set! bg-color:pressure-off color2)
	      'done)
	    (set-highlight-color (color)
	      (set! highlight-color color)
	      'done)
	    (dimension? (dim) (eq? dim dimension))
	    (add-relation (relation)
	      (set! relations (cons relation relations))
	      (tell self 'update-relation-names))
	    (remove-relation (relation)
	      (set! relations (remq relation relations))
	      (tell self 'update-relation-names))
	    (delete-all-relations ()
	      (set! relations '())
	      (tell self 'update-relation-names))
	    (update-relation-names ()
	      (if (null? relations)
		(begin
		  (set! relation-names-pexps '())
		  (set! normal-panel-pexp '())
		  (set! highlighted-panel-pexp '()))
		(begin
		  (set! relation-names-pexps
		    (map-compress
		      (lambda (r)
			(let ((info (assq r panel-info)))
			  (if (exists? info)
			    `(text ,(3rd info) ,(4th info))
			    #f)))
		      relations))
		  (set! normal-panel-pexp
		    `(let-sgl ((text-justification center))
		       ,outline-pexp
		       (let-sgl ((font ,rel-normal-font))
			 ,@relation-names-pexps)
		       (let-sgl ((font ,dim-normal-font))
			 (text ,dim-name-coord ,dim-name))))
		  (set! highlighted-panel-pexp
		    `(let-sgl ((text-justification center))
		       ,outline-pexp
		       (let-sgl ((font ,rel-normal-font))
			 ,@relation-names-pexps)
		       (let-sgl ((font ,dim-highlight-font))
			 (text ,dim-name-coord ,dim-name))))))
	      'done)
	    (set-theme-graphics-parameters (theme)
	      (let* ((relation (tell theme 'get-relation))
		     (info (assq relation panel-info))
		     (center-coord (2nd info))
		     (text-coord (3rd info))
		     (normal-pexp
		       `(let-sgl ((font ,rel-normal-font)
				  (text-justification center))
			  (text ,text-coord ,(4th info))))
		     (highlight-pexp
		       `(let-sgl ((font ,rel-highlight-font)
				  (text-justification center))
			  (text ,text-coord ,(4th info)))))
		(tell theme 'set-graphics-parameters
		  self center-coord text-coord normal-pexp highlight-pexp)))
	    (draw-panel ()
	      (let ((dominant-theme (tell theme-cluster 'get-dominant-theme)))
		(if (exists? dominant-theme)
		  (begin
		    (tell graphics-window 'erase-on-background highlight-color
		      `(filled-rectangle ,ll ,ur))
		    (tell graphics-window 'draw highlighted-panel-pexp))
		  (begin
		    (tell graphics-window 'erase
		      `(filled-rectangle ,lower-left ,upper-right))
		    (tell graphics-window 'draw normal-panel-pexp)))
		(for* each theme in (tell theme-cluster 'get-themes) do
		  (if* (eq? theme dominant-theme)
		    (tell graphics-window 'erase-on-background highlight-color
		      (tell theme 'get-normal-pexp))
		    (tell graphics-window 'draw (tell theme 'get-highlight-pexp)))
		  (tell theme 'draw-activation-graphics))
		(set! highlighted-theme dominant-theme))
	      'done)
	    ;; this method is used only for garbage collection
	    (redraw-panel ()
	      (if* (not (null? (tell theme-cluster 'get-themes)))
		(let ((dominant-theme (tell theme-cluster 'get-dominant-theme))
		      (vp (tell graphics-window 'get-vp)))
		  (if* (exists? dominant-theme)
		    (send vp draw-hidden-filled-rectangle highlight-color
		      (car ll) (cadr ll) (car ur) (cadr ur) 'highlight))
		  (tell graphics-window 'draw outline-pexp)
		  (tell graphics-window 'draw
		    `(let-sgl ((text-justification center)
			       (font ,(if (exists? dominant-theme)
					dim-highlight-font
					dim-normal-font)))
		       (text ,dim-name-coord ,dim-name)))
		  (for* each theme in (tell theme-cluster 'get-themes) do
		    (if (eq? theme dominant-theme)
		      (tell graphics-window 'draw (tell theme 'get-highlight-pexp))
		      (tell graphics-window 'draw (tell theme 'get-normal-pexp)))
		    (tell theme 'draw-activation-graphics))))
	      'done)
	    (update-graphics ()
	      (let ((themes (tell theme-cluster 'get-themes))
		    (dominant-theme (tell theme-cluster 'get-dominant-theme)))
		(cond
		  ((eq? dominant-theme highlighted-theme)
		   (for* each theme in themes do
		     (tell theme 'update-activation-graphics)))
		  ((and (exists? highlighted-theme) (exists? dominant-theme))
		   (tell graphics-window 'erase-on-background highlight-color
		     (tell highlighted-theme 'get-highlight-pexp))
		   (tell graphics-window 'draw
		     (tell highlighted-theme 'get-normal-pexp))
		   (tell graphics-window 'erase-on-background highlight-color
		     (tell dominant-theme 'get-normal-pexp))
		   (tell graphics-window 'draw
		     (tell dominant-theme 'get-highlight-pexp))
		   (for* each theme in themes do
		     (tell theme 'update-activation-graphics))
		   (set! highlighted-theme dominant-theme))
		  (else (tell self 'draw-panel))))
	      'done)
	    (draw-absolute-activation (coord activation)
	      (set! *fg-color*
		(if (negative? activation)
		  %negative-theme-activation-color%
		  %positive-theme-activation-color%))
	      (tell graphics-window 'draw
		(disk coord (* (% (abs activation)) full-activation-diameter)))
	      (set! *fg-color* %default-fg-color%)
	      'done)
	    (decrease-absolute-activation (coord activation)
	      (let ((diameter (* (% (abs activation)) full-activation-diameter))
		    ;; not sure if this is really necessary, but just to be safe...
		    (activation-color
		      (if (negative? activation)
			%negative-theme-activation-color%
			%positive-theme-activation-color%)))
		(if (exists? highlighted-theme)
		  (tell graphics-window 'draw
		    `(let-sgl ((foreground-color ,highlight-color)
			       (background-color ,activation-color))
		       (ring ,coord ,max-activation-diameter ,diameter)))
		  (tell graphics-window 'draw
		    `(let-sgl ((foreground-color
				;; again, just to be safe...
				 ,(if (tell *themespace* 'thematic-pressure? theme-type)
				    bg-color:pressure-on
				    bg-color:pressure-off))
			       (background-color ,activation-color))
		       (ring ,coord ,max-activation-diameter ,diameter))))))
	    (erase-activation (coord)
	      (if (exists? highlighted-theme)
		(tell graphics-window 'erase-on-background highlight-color
		  (disk coord max-activation-diameter))
		(tell graphics-window 'erase (disk coord max-activation-diameter))))
	    (initialize ()
	      (set! theme-cluster
		(tell *themespace* 'get-cluster theme-type dimension))
	      (set! relations
		(tell-all (tell theme-cluster 'get-themes) 'get-relation))
	      (tell self 'update-relation-names)
	      (tell self 'draw-panel))
	    (else (delegate msg base-object))))))))


(define compute-horizontal-panel-info
  (lambda (x-length y-length full-activation-diameter top-margin bottom-margin
	    dim-name-height rel-name-height rel-name-offset)
    (lambda (relations lower-left)
      (let* ((num-relations (length relations))
	     (required-x-space (* num-relations full-activation-diameter))
	     (x-spacing (/ (- x-length required-x-space) (add1 num-relations)))
	     (required-y-space
	       (+ rel-name-height full-activation-diameter dim-name-height
		  top-margin bottom-margin))
	     (y-spacing
	       (min (* 1/4 full-activation-diameter)
		    (* 1/3 (- y-length required-y-space))))
	     (y-margin (+ top-margin (* 1/2 (- y-length required-y-space y-spacing))))
	     (names (map relation-name relations))
	     (name-y (+ (- y-length y-margin rel-name-height) rel-name-offset))
	     (center-y (- y-length y-margin rel-name-height y-spacing
			  (* 1/2 full-activation-diameter))))
	(map (lambda (i relation name)
	       (let* ((x (+ (* (add1 i) x-spacing)
			    (* (+ i 1/2) full-activation-diameter)))
		      (center-coord (map + lower-left `(,x ,center-y)))
		      (name-coord (map + lower-left `(,x ,name-y))))
		 `(,relation ,center-coord ,name-coord ,name)))
	  (ascending-index-list num-relations) relations names)))))


(define compute-vertical-panel-info
  (lambda (x-length y-length full-activation-diameter top-margin bottom-margin
	    dim-name-height rel-name-height rel-name-offset get-rel-name-width)
    (lambda (relations lower-left)
      (let* ((num-relations (length relations))
	     (max-name-width (apply max (map get-rel-name-width relations)))
	     (required-x-space (+ max-name-width full-activation-diameter))
	     (x-spacing
	       (min (* 1/2 full-activation-diameter)
		    (* 1/3 (- x-length required-x-space))))
	     (left-margin (* 1/2 (- x-length required-x-space x-spacing)))
	     (required-y-space
	       (+ (* num-relations full-activation-diameter)
		  dim-name-height bottom-margin))
	     (y-spacing (/ (- y-length required-y-space) (add1 num-relations)))
	     (names (map relation-name relations))
	     (name-x (+ left-margin (* 1/2 max-name-width)))
	     (center-x
	       (+ left-margin max-name-width x-spacing
		  (* 1/2 full-activation-diameter))))
	(map (lambda (i relation name)
	       (let* ((y (- y-length (* (add1 i) y-spacing)
			    (* (+ i 1/2) full-activation-diameter)))
		      (center-coord (map + lower-left `(,center-x ,y)))
		      (name-y (+ y (* -1/2 rel-name-height) rel-name-offset))
		      (name-coord (map + lower-left `(,name-x ,name-y))))
		 `(,relation ,center-coord ,name-coord ,name)))
	  (ascending-index-list num-relations) relations names)))))


(define relation-name
  (lambda (relation)
    (cond
      ((eq? relation #f) "diff")
      ((eq? relation plato-identity) "iden")
      ((eq? relation plato-opposite) "opp")
      ((eq? relation plato-successor) "succ")
      ((eq? relation plato-predecessor) "pred")
      (else #f))))


(define dimension-name
  (lambda (dimension)
    (cond
      ((eq? dimension plato-letter-category) "Letter Category")
      ((eq? dimension plato-alphabetic-position-category) "Alphabetic Position")
      ((eq? dimension plato-direction-category) "Direction")
      ((eq? dimension plato-object-category) "Object Type")
      ((eq? dimension plato-string-position-category) "String Position")
      ((eq? dimension plato-group-category) "Group Type")
      ((eq? dimension plato-bond-category) "Bond Type")
      ((eq? dimension plato-bond-facet) "Bond Facet")
      (else (tell dimension 'get-short-name)))))


(define abbreviated-dimension-name
  (lambda (dimension)
    (cond
      ((eq? dimension plato-letter-category) "Letter Ctgy.")
      ((eq? dimension plato-alphabetic-position-category) "Alpha. Pos.")
      ((eq? dimension plato-string-position-category) "String Pos.")
      (else (dimension-name dimension)))))

