;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;  DESCRIPTION  : sgl-d.ss - A symbolic graphics language client for
;;                            SchemeXM. Implements the SGL sublanguage
;;                            for drawing.
;;
;;
;;
;;  PREPARED BY  : John B. Zuckerman
;;                 Motorola, Inc.
;;                 8201 E. McDowell
;;                 Scottsdale, Arizona 85257
;;                 (602) 441-7437
;;
;;  ORIGINAL     : Feb 12, 1990
;;  UPDATE       : April 16, 1991 - continuing development.
;;
;;                 May 21, 1991 - added support for erasure.
;;
;;               : June 24, 1991 - fixed garbage bug.
;;
;;               : June 26, 1991 - fixed draw-prior-to-expose problems. 
;;                 The window manager may delay creation of drawing window;
;;                 therefore, must wait for first expose event.
;;
;;               : July 11, 1991 - plugged all known memory leaks.  Some minor
;;                 leakage of unkown origin still occurs at the server.
;;
;;               : July 20, 1991 - corrected bugs that affected color screens.
;;
;;               : August 6, 1991 - corrected bug that affected redrawing
;;                 of simple (non let-sgl containing) pictures.
;;
;;               : August 7, 1991 - corrected bugs that affect drawing and
;;                 erasing of arcs.
;;
;;               : December 5, 1991 - corrected bugs in bounding-box
;;                 and backing-store computations for text and vertical text.
;;
;;               : January 2, 1992 - updated for SchemeXM 0.89.
;;
;;               : March 2, 1992 - updated for SchemeXM 0.94.
;;
;;               : prefixed some private toplevel procedures to reduce name
;;                 clashes.
;;                 
;;               : March 20, 1992 - updated for SchemeXM 0.94.
;;
;;               : April 18, 1992 - added conditional creation of
;;                 graphics contexts to sgl-make-env.  This eliminates
;;                 most of the gc creation/deletion done by the plot
;;                 program, for example, resulting in significant speedup.
;;
;;               : added foreground-color, background-color, and line-style
;;                 environment variables.
;;
;;               : April 27, 1992 - efficiency hack - inexact->exact to
;;                 be avoided at all costs.  Now using xf-flonum->fixnum.
;;
;;               : May 6, 1992 - added support for specifing origin of
;;                 the clipping window.  Previously origin was fixed at (0,0).
;;
;;               : June 2, 1992 - added virtual viewport mode, which provides
;;                 for scrollable windows.
;;
;;               : June 3, 1992 - added polypoint primitive.
;;
;;               : June 10, 1992 - made sgl-coalesce-points more efficient.
;;
;;               : added sgl-pick code.
;;
;;               : June 13, 1992 - performance improvements for polyline and
;;                 polypoint.
;;
;;               : June 16, 1992 - corrected arithmetic bug in coalesce-points
;;                 involving computation with exact zero.
;;
;;               : July 03, 1992 - added picture selection to sgl-pick
;;                 procedure. list of selected pictures is passed to the
;;                 press-action.
;;
;;               : July 6, 1992 - corrected sgl-delete-cb to behave better
;;                 if invoked multiple times by impatient user.
;;
;;               : July 8, 1992 - added polymarker picture primitive.
;;
;;               : July 12, 1992 - corrected problem in sgl-handle-mode: the
;;                 computed backing store pixmap dimensions were not being
;;                 clipped against viewport maximum dimensions, leading to
;;                 creation of excessively large pixmaps.
;;
;;               : Aug 20, 1992 - added gc and font caches to enviroment
;;                 handler.  This should be a big performance boost.
;;                 Also changed pixmap maintenance strategy: we now
;;                 draw to both window and pixmap; this is generally
;;                 faster than copying one to the other, as it permits
;;                 use of many draw! statements to be reasonably
;;                 efficient.
;;
;;                 Corrected bug that reversed picture ordering after
;;                 erasure.
;;
;;               : Aug 21, 1992 - corrected bug that affected off-screen
;;                 drawing with backing store (see bugs below).
;;
;;               : added text-mode environment variable.  arguments: normal,
;;                 (default) and image (forces use of x-draw-image-string).
;;                 
;;               : Aug 22, 1992 - corrected bug that affected arc drawing
;;                 for other than default clipping window (improper use
;;                 of viewing transform on arc extents).
;;
;;               : Sept 9, 1992 - added rectangle, filled-rectangle
;;                 picture primitives.
;;
;;               : Sept 16, 1992 - modified sgl-pick to support popup menus.
;;
;;               : Sept 18, 1992 - added 'omit-from-database to list of
;;                 draw! option-specifiers.
;;
;;               : added color cacheing.
;;
;;               : Sept 26, 1992 - modified bounding-box procedure
;;                 to return a bounding box represented as a list of two lists:
;;                 the lower-left and upper-right points in world coordinates.
;;
;;               : Sept 29, 1992 - corrected bug in use of x-draw-rectangle.
;;               : merged in the rband and popup functionality.
;;
;;               : Jan 03, 1993 - brought up-to-date for MCG release.
;;
;;               : April 7, 1993 - fixed bug in rubber-band that
;;                 caused problem on color displays.
;;
;;               : April 12, 1993 - fixed April 7th bug fix so that rubber band
;;                 now works correctly for both Mono and Color screens (April 7
;;                 fix broke rubber band for Mono screens---ugh!)
;;
;;               : removed bogus attempt to apply optimize-level 3 within
;;                 sgl-coalesce-points.
;;
;;               : added code to sgl-coalesce-points in order to enforce
;;                 16-bit (short) integer range for screen coordinate values.
;;
;;               : April 14, 1993 - uses new SchemeXM viewing transform
;;                 primitives. 
;;
;;               : added binding for create-graphics-viewport, the documented
;;                 name for the create-virtual-viewport procedure.
;;
;;               : April 16, 1993 - corrected create-graphics-viewport
;;                 parameter forms to conform to the documentation.
;;
;;               : April 17, 1993 - adding overlay constructs.
;;
;;               : July 5, 1993 - Overlays have been optimized as described in
;;                 the RCS rev 1.3 comments.  An intermediate pixmap is used
;;                 to build up the overlay image before it is displayed.
;;
;;               : Nov 5, 1993 - cleaned up internal documentation.
;;
;;                 Changed overlay specification from ov:<sym> ... to
;;                 (overlays <sym> ...), which is more conventional, 
;;                 eliminates symbol bashing.
;;
;;                 draw! syntax is now: (draw! <g> <pe> <options> ...) where
;;                 <options> is one or more of <mode> | (overlays ov1 ...)
;;
;;               : Nov 14, 1993 - fixed problem related to Nov 5 change.
;;                 Mapping options to overlay names sometimes failed.
;;
;;               : Dec 1, 1993 - fixed font specifications (brought up to
;;                 X11R4 standards).
;;
;;               : Dec 2, 1993 - removed unneeded viewing transform syntax.
;;
;;               : restructured code to hide private definitions. Added
;;                 syntax for structures (based on Dybvig's define-structure), 
;;                 which appends sgl- prefix to all top-level names.
;;
;;               : Dec 8, 1993 - corrected bug affecting resizing on color
;;                 displays (backing store pixmap was not recomputed).
;;
;;               : Dec 10, 1993 - overlays are now unmapped when created.
;;                 This permits greater flexibility.  
;;
;;               : Dec 13, 1993 - Improved error reporting.  Set graphics-
;;                 exposures slot to #f in all GC values records, which
;;                 eliminates NoExpose events, improves performance.
;;
;;               : Jan 18, 1995 - added graphics-function let declaration
;;                 for Jim Marshall.
;;
;;               : Jan 21, 1995 - added dash-offset and dashes for Gary McGraw.
;;
;;               : March 5, 1995 - added clear-overlay! for Gary McGraw.
;;
;;               : Oct 30, 1996 - changed text justifications top, bottom
;;                 to up, down, for conformance with documentation, and
;;                 corrected the vertical text placement computation.
;;
;;                 Added sgl-set-title! for Jim Marshall (sets the
;;                 title of the parent of the drawing canvas).  And
;;                 sgl-set-icon-pixmap! (sets the icon pixmap to the
;;                 specified pixmap file).
;;
;;               : Oct 31, 1996 - added sgl-set-icon-name! 
;;
;;
;;  BUGS
;;
;;  minor memory leaks at the server still apparent.
;;
;;  Aug 21, 1992
;; 
;;  backing store pixmaps are allocated for partially or wholly off-screen
;;  pictures.  This causes creation of unnecessary and/or excessively
;;  large backing store pixmaps.  Dimension of a backing store should be
;;  the intersection of the viewport and the picture's bounding-box.
;;
;;
;;  Copyright (c) Motorola, Inc., 1991-1993.  All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Things to do:-
;;
;;
;; need to fix bounding box computations - it's a mess.
;;
;;
;; sgl-handle-modes and sgl-handle-options should coordinate to
;; return unrecognized modes/options, for error reporting.  
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Notes:-
;;
;; must not draw to window until first expose event has been received,
;; else things are discarded (see XMapWindow).
;;
;; All top-level names are prefixed by "sgl-".
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;


(printf "SchemeSGL-D 10/30/96~%")

(eval-when (compile) (optimize-level 2))


(define (sgl-error . args)
  (display "SchemeSGL-D Error: ")
  (let ((args (if (symbol? (car args))
		  (begin
		    (display (car args))
		    (display ": ")
		    (cdr args))
		  args)))
    (for-each display args)
    (display ".")
    (newline)
    (break)
    ))


(define (sgl-warning . args)
  (display "SchemeSGL-D Warning: ")
  (let ((args (if (symbol? (car args))
		  (begin
		    (display (car args))
		    (display ": ")
		    (cdr args))
		  args)))
    (for-each display args)
    (display ".")
    (newline)
    ))


(define (sgl-reduce1 pred ls)
  (define (reduce1 ls)
    (if (null? ls)
	'()
	(let ((val (pred (car ls))))
	  (if val
	      (cons val (reduce1 (cdr ls)))
	      (reduce1 (cdr ls))))))
  (reduce1 ls))

(define sgl-set-title!
  (lambda (g title)
    (xt-set-values  
      (let f ((w (getprop g 'widget)))
	(if (xt-is-application-shell w)
	  w
	  (f (xt-parent w))))
      `(("XmNtitle" ,title)))))

(define sgl-set-icon-name!
  (lambda (g name)
    (xt-set-values  
      (let f ((w (getprop g 'widget)))
	(if (xt-is-application-shell w)
	  w
	  (f (xt-parent w))))
      `(("XmNiconName" ,name)))))

(define sgl-set-icon-pixmap!
  (lambda (g filename)
    (define pm
      (xm-get-pixmap 
        (xt-screen *top-level-widget*)
	filename
        (x-black-pixel (xt-display *top-level-widget*) 0)
        (x-white-pixel (xt-display *top-level-widget*) 0)))
    (xt-set-values  
      (let f ((w (getprop g 'widget)))
        (if (xt-is-application-shell w)
          w
          (f (xt-parent w))))
      `(("XmNiconPixmap" ,pm)))))

;;
;; this syntax exploits the new viewing transform primitives in SchemeXM
;; 1.1.0b if available, but also supports backward compatibility with
;; pre-1.1.0b versions of SchemeXM.
;;

(extend-syntax (sgl-view-x)
  [(sgl-view-x <x> <y> <z>)
   (top-level-bound? 'xf-viewing-transform-x)
   (xf-viewing-transform-x (exact->inexact <x>) <y> <z>)]
  [(sgl-view-x <x> <y> <z>)
   (with ((*val* (gensym)))
     (let ((*val* (xf-flonum->fixnum (* (+ <x> <y>) <z>))))
       (if (> *val* 30000)
	   30000 ;;; some servers have magic numbers beyond this!
	   (if (< *val* -30000)
	       -30000 ;;; some servers have magic numbers beyond this!
	       *val*))))]
  )

(extend-syntax (sgl-view-y)
  [(sgl-view-y <x> <y> <z>)
   (top-level-bound? 'xf-viewing-transform-y)
   (xf-viewing-transform-y (exact->inexact <x>) <y> <z>)]
  [(sgl-view-y <x> <y> <z>)
   (with ((*val* (gensym)))
     (let ((*val* (xf-flonum->fixnum (* (- <y> <x>) <z>))))
       (if (> *val* 30000)
	   30000 ;;; some servers have magic numbers beyond this!
	   (if (< *val* -30000)
	       -30000 ;;; some servers have magic numbers beyond this!
	       *val*))))]
  )

;;
;; sgl-define-structure (given below) is derived from the following source:
;;
;;    The Scheme Programming Language
;;    -------------------------------
;;    By R. Kent Dybvig
;;    Copyright (c) 1987 Prentice-Hall, Inc.
;;                                      
;;
(extend-syntax (sgl-define-structure)
  [(sgl-define-structure (name id1 ...))
   (andmap symbol? '(name id1 ...))
   (with ([constructor
	   (string->symbol (format "sgl-make-~a" 'name))]
	  [predicate
	   (string->symbol (format "sgl-~a?" 'name))]
	  [(access ...)
	   (map (lambda (x)
		  (string->symbol
		   (format "sgl-~a-~a" 'name x)))
		'(id1 ...))]
	  [(assign ...)
	   (map (lambda (x)
		  (string->symbol
		   (format "sgl-set-~a-~a!" 'name x)))
		'(id1 ...))]
	  [count (length '(name id1 ...))])
     (with ([(index ...)
	     (let f ([i 1])
	       (if (= i 'count)
		   '()
		   (cons i (f (+ i 1)))))])
       (begin
	 (define predicate
	   (lambda (obj)
	     (and (vector? obj)
		  (= (vector-length obj) count)
		  (eq? (vector-ref obj 0) 'name))))
	 (define access
	   (lambda (obj)
	     (vector-ref obj index)))
	 ...
	 (define assign
	   (lambda (obj newval)
	     (vector-set! obj index newval)))
	 ...
	 (define constructor
	   (lambda (id1 ...)
	     (vector 'name id1 ...))))))])

;;----------------------------------------------------------------------------
;;
;; Data Types
;;
(sgl-define-structure (picture pe options modes id overlay-names))
(sgl-define-structure (mode id backing-store origin dimension))
(sgl-define-structure (point x y))
(sgl-define-structure (bounding-box min max))
(sgl-define-structure (binding variable value))
(sgl-define-structure (overlay name bitmap gc mapped? color-name pixel))
(sgl-define-structure (cache value))
;;
;;----------------------------------------------------------------------------
;;
;; Application Hooks
;;
(define sgl-toplevel #f)
(define sgl-toplevel-args '())
(define sgl-virtual-viewport-args '())
(define sgl-canvas-args '())
;;
;;----------------------------------------------------------------------------
;;
;; Overlay User Interface Procedures
;;
;;
;; (overlay-names <g>) ==> returns list of overlay names
;;
;; (map-overlay! <g> <ov>)  => maps overlay. <ov> is the 
;;    name of an overlay to be mapped.
;;
;; (unmap-overlay! <g> <ov>) => unmaps overlay.  <ov> is the
;;    name of an overlay to be unmapped.
;;
;; (map-all-overlays! <g>)  => maps all overlays.
;;
;; (unmap-all-overlays! <g>) => unmaps all overlays.
;;
;; (set-overlay-color! <g> <ov> <color>) => assigns color <color>
;;    to overlay named <ov>.
;;
;; (reorder-overlays! <g> (<ov> ...)) - sets overlay order according to
;;    list of names <ov> ... .
;;    Overlays are stored top-to-bottom order.
;;
;; (overlay-mapped? <g> <ov>) - returns #t if overlay named <ov> is mapped.
;;
;;----------------------------------------------------------------------------

(define (overlay-names graph)
  (map (lambda (ov) (sgl-overlay-name ov)) (getprop graph 'overlays)))

(define (map-overlay! graph name-to-map)
  (let f ((ovs (getprop graph 'overlays)))
    (if (null? ovs)
	(sgl-error 'map-overlay!
		  "overlay " name-to-map " not found")
	(let ((ov (car ovs)))
	  (if (eq? (sgl-overlay-name ov)
		   name-to-map)
	      (if (sgl-overlay-mapped? ov)
		  (sgl-warning 'map-overlay!
			      "overlay " name-to-map " already mapped")
		  (sgl-set-overlay-mapped?! ov #t))
	      (f (cdr ovs))))))
  (sgl-update-backing-store graph)
  (sgl-expose-cb (getprop graph 'widget) graph #f))


(define (clear-overlay! graph name-to-clear)
  (let f ((ovs (getprop graph 'overlays)))
    (if (null? ovs)
	(sgl-error 'clear-overlay!
		  "overlay " name-to-clear " not found")
	(let ((ov (car ovs)))
	  (if (eq? (sgl-overlay-name ov) name-to-clear)
	    (begin ;;; clear the bitmap
	      (let ((dpy (getprop graph 'dpy))
		    (vp-w (getprop graph 'vp-w))
		    (vp-h (getprop graph 'vp-h))
		    (bitmap-clear-gc (getprop graph 'bitmap-clear-gc))
		    )
		(let ((bit (sgl-overlay-bitmap ov)))
		  (x-fill-rectangle dpy bit bitmap-clear-gc 0 0 vp-w vp-h)
		  )))
	    (f (cdr ovs))))))
  ;;
  ;; remove overlay from pictures
  ;;
  (putprop graph 'pictures
    (sgl-reduce1 (lambda (x) x)
      (map
	(lambda (pic)
	  (let ((pic-overlay-names (sgl-picture-overlay-names pic)))
	    (if (null? pic-overlay-names)
	      pic
	      (let ((new-names (remq name-to-clear pic-overlay-names)))
		(sgl-set-picture-overlay-names! pic new-names)
		(and (pair? new-names) pic)
		))))
	(getprop graph 'pictures))))
  (sgl-update-backing-store graph)
  (sgl-expose-cb (getprop graph 'widget) graph #f))


(define (map-all-overlays! graph)
  (for-each (lambda (ov)
	      (sgl-set-overlay-mapped?! ov #t))
	    (getprop graph 'overlays))
  (sgl-update-backing-store graph)
  (sgl-expose-cb (getprop graph 'widget) graph #f))


(define (unmap-all-overlays! graph)
  (for-each (lambda (ov)
	      (sgl-set-overlay-mapped?! ov #f))
	    (getprop graph 'overlays))
  (sgl-update-backing-store graph)
  (sgl-expose-cb (getprop graph 'widget) graph #f))


(define (unmap-overlay! graph name-to-map)
  (let f ((ovs (getprop graph 'overlays)))
    (if (null? ovs)
	(sgl-error 'unmap-overlay!
		  "overlay " name-to-map " not found")
	(let ((ov (car ovs)))
	  (if (eq? (sgl-overlay-name ov)
		   name-to-map)
	      (if (not (sgl-overlay-mapped? ov))
		  (sgl-warning 'unmap-overlay!
			      "overlay " name-to-map " not mapped")
		  (sgl-set-overlay-mapped?! ov #f))
	      (f (cdr ovs))))))
  (sgl-update-backing-store graph)
  (sgl-expose-cb (getprop graph 'widget) graph #f))


(define (overlay-mapped? graph name)
  (let f ((ovs (getprop graph 'overlays)))
    (if (null? ovs)
	(sgl-error 'map-overlay! "overlay " name " not found")
	(let ((ov (car ovs)))
	  (if (eq? (sgl-overlay-name ov) name)
	      (sgl-overlay-mapped? ov)
	      (f (cdr ovs)))))))


(define (set-overlay-color! graph ov-name c-name)
  (let* ((dpy (getprop graph 'dpy))
	 (x-color
	  (let ((cache-hit (assoc c-name (sgl-cache-value sgl-color-cache))))
	    (if cache-hit
		(cdr cache-hit)
		(let* ((rtn (x-alloc-named-color dpy
						 (x-default-colormap dpy 0)
						 c-name))
		       (rtn 
			(if (eqv? (car rtn) 0)
			    (begin 
			      (sgl-warning 'set-overlay-color!
					  "requested color not available: "
					  c-name)
			      (x-alloc-named-color dpy
						   (x-default-colormap dpy 0)
						   "black"))
			    rtn)))
		  (set! sgl-color-cache (sgl-make-cache
					 (cons (cons c-name (cadr rtn))
					       (sgl-cache-value
						sgl-color-cache))))
		  (cadr rtn)))))
	 (ov (let f ((ovs (getprop graph 'overlays)))
	       (if (null? ovs)
		   (sgl-error 'set-overlay-color! "overlay " ov-name
			     " not found")
		   (if (equal? (sgl-overlay-name (car ovs)) ov-name)
		       (car ovs)
		       (f (cdr ovs))))))
	 (ov-gc (sgl-overlay-gc ov))
	 )
    (x-set-foreground dpy ov-gc (=> 'get-pixel x-color))
    (sgl-set-overlay-pixel! ov (=> 'get-pixel x-color))
    (sgl-set-overlay-color-name! ov c-name)
    (sgl-update-backing-store graph)
    (sgl-expose-cb (getprop graph 'widget) graph #f)
    ))


(define (reorder-overlays! graph ov-names)
  (let* ((dpy (getprop graph 'dpy))
	 (old-ovs (getprop graph 'overlays))
	 (old-ovnames (map (lambda (ov) (sgl-overlay-name ov)) old-ovs))
	 (bad-ovnames (sgl-reduce1 (lambda (ov-name)
				     (if (not (memq ov-name old-ovnames))
					 ov-name
					 #f))
				   ov-names))
	 )
    (if (not (null? bad-ovnames))
	(sgl-error 'reorder-overlays!
		  bad-ovnames
		  " are not valid overlay names"))
    (let* ((ovs-in
	    (sgl-reduce1 (lambda (ov)
			   (if (memq (sgl-overlay-name ov) ov-names)
			       ov
			       #f))
			 old-ovs))
	   (ovs-out
	    (sgl-reduce1 (lambda (ov)
			   (if (not (memq (sgl-overlay-name ov) ov-names))
			       ov
			       #f))
			 old-ovs))
	   (keys-plus-ovs-in (map (lambda (ov) (cons (sgl-overlay-name ov) ov))
				  ovs-in))
	   (reordered-ovs-in
	    (map cdr
		 (sort (lambda (x y)
			 (> (length (memq (car x) ov-names))
			    (length (memq (car y) ov-names))))
		       keys-plus-ovs-in)))
	   )
      (putprop graph 'overlays (append reordered-ovs-in ovs-out))
      )
    (sgl-update-backing-store graph)
    (sgl-expose-cb (getprop graph 'widget) graph #f)
    ))

;;
;;----------------------------------------------------------------------------
;;
;; Window Creation User Interface Procedures
;;

(define create-graphics-window
  (case-lambda
   [()
    (sgl-create 300 300 300 300 0 0 1 1 #f)]
   [(vp-w vp-h)
    (sgl-create vp-w vp-h vp-w vp-h 0 0 1 1 #f)]
   [(vp-w vp-h x-max y-max)
    (sgl-create vp-w vp-h vp-w vp-h 0 0 x-max y-max #f)]
   [(vp-w vp-h x-min y-min x-max y-max)
    (sgl-create vp-w vp-h vp-w vp-h x-min y-min x-max y-max #f)]
   ))


(define create-graphics-viewport
  (case-lambda
   [()
    (sgl-create 300 300 300 300 0 0 1 1 #t)]
   [(vp-w vp-h)
    (sgl-create vp-w vp-h 300 300 0 0 1 1 #t)]
   [(vp-w vp-h x-max y-max)
    (sgl-create vp-w vp-h 300 300 0 0 x-max y-max #t)]
   [(vp-w vp-h x-min y-min x-max y-max)
    (sgl-create vp-w vp-h 300 300 x-min y-min x-max y-max #t)]
   [(vp-w vp-h win-w win-h x-min y-min x-max y-max)
    (sgl-create vp-w vp-h win-w win-h x-min y-min x-max y-max #t)]
   ))

(define create-virtual-viewport create-graphics-viewport) ;;; alternate name


(define (sgl-create vp-w vp-h win-w win-h x-min y-min x-max y-max
		    virtual-viewport)
  ;;
  ;; decl's are two-element lists (i.e., the external rep for a binding).
  ;;
  (define make-decl list)
  (define decl.variable car)
  (define decl.value cadr)

  (let* ((graph (gensym))
	 (toplevel
	  (if sgl-toplevel
	      sgl-toplevel
	      (xt-create-managed-widget
	       ""
	       xx-application-shell
	       *top-level-widget*
	       (let ((args `(("XmNwidth" ,win-w)
			     ("XmNheight" ,win-h)
			     ("XmNdeleteResponse" "XmDO_NOTHING")
			     )))
		 (append sgl-toplevel-args args))
	       )))
	 (parent
	  (if (not virtual-viewport)
	      toplevel
	      (let ((sw 
		     (xm-create-scrolled-window
		      toplevel
		      ""
		      (append sgl-virtual-viewport-args
			      `(("XmNscrollingPolicy" "XmAUTOMATIC")
				("XmNwidth" ,win-w)
				("XmNheight" ,win-h)
				)))))
		(xt-manage-child sw)
		sw)))
	 (canvas (xt-create-managed-widget "canvas"
					   xx-xm-drawing-area
					   parent
					   (append sgl-canvas-args
						   `(("XmNwidth" ,vp-w)
						     ("XmNheight" ,vp-h)
						     ))))
	 (dpy (xt-display canvas))
	 (window (xt-window canvas))
	 (screen (xt-screen canvas))
	 ;;
	 ;; base layer pixmap - where base layer drawing goes
	 ;;
	 (base-pm (x-create-pixmap dpy
				   (x-default-root-window dpy)
				   vp-w
				   vp-h
				   (x-default-depth-of-screen screen)))
	 ;;
	 ;; backing store pixmap - used by expose callback to refresh window
	 ;;
	 (back-pm (x-create-pixmap dpy
				   (x-default-root-window dpy)
				   vp-w
				   vp-h
				   (x-default-depth-of-screen screen)))
	 (tiny-bitmap (x-create-pixmap dpy
				       (x-default-root-window dpy)
				       1 ;;; width
				       1 ;;; height
				       1 ;;; depth
				       ))
	 (work-bitmap (x-create-pixmap dpy
				       (x-default-root-window dpy)
				       vp-w
				       vp-h
				       1 ;;; depth
				       ))
	 (toplevel-bitmap-gcv (let ((v (=> 'new xx-x-g-c-values)))
				(=> 'set-foreground! v 1) ;;; set
				(=> 'set-background! v 0) ;;; clear
				(=> 'set-graphics-exposures! v #f)
				v))
	 (toplevel-bitmap-gc (x-create-g-c dpy
					   tiny-bitmap
					   '("GCForeground" "GCBackground")
					   toplevel-bitmap-gcv))
	 (bitmap-clear-gcv (let ((v (=> 'new xx-x-g-c-values)))
			     (=> 'set-foreground! v 0) ;;; clear
			     (=> 'set-background! v 1) ;;; set
			     (=> 'set-graphics-exposures! v #f)
			     v))
	 (bitmap-clear-gc (x-create-g-c dpy
					tiny-bitmap
					'("GCForeground" "GCBackground")
					bitmap-clear-gcv))
	 (bitmap-gxor-gcv (let ((v (=> 'new xx-x-g-c-values)))
			    (=> 'set-function! v "GXor")
			    (=> 'set-graphics-exposures! v #f)
			    v))
	 (bitmap-gxor-gc (x-create-g-c dpy
				       tiny-bitmap
				       '("GCFunction")
				       bitmap-gxor-gcv))
	 (overlay-erase-gcv (let ((v (=> 'new xx-x-g-c-values)))
			      (=> 'set-foreground! v
				  ;;
				  ;; pixel value of all ones
				  ;;
				  (1- (expt 2 (x-default-depth-of-screen
					       screen))))
			      (=> 'set-background! v 0)
			      (=> 'set-function! v "GXandInverted")
			      (=> 'set-graphics-exposures! v #f)
			      v))
	 (overlay-erase-gc (x-create-g-c dpy
					 window
					 '("GCForeground"
					   "GCBackground"
					   "GCFunction")
					 overlay-erase-gcv))
	 (toplevel-gcv
	  (let ((gcv (=> 'new xx-x-g-c-values)))
	    (=> 'set-foreground! gcv (x-black-pixel-of-screen screen))
	    (=> 'set-background! gcv (x-white-pixel-of-screen screen))
	    (=> 'set-graphics-exposures! gcv #f)
	    gcv))
	 (toplevel-gc (x-create-g-c dpy
				    window
				    (=> 'compute-xx-x-g-c-values-mask
					toplevel-gcv)
				    toplevel-gcv))
	 )
    ;;
    ;; believe XmNspacing is the right resource to use for max sizing of
    ;; scrollable viewport.
    ;;
    (if (and (not sgl-toplevel) virtual-viewport)
	(let ((spacing (car (xt-get-values parent '("XmNspacing")))))
	  (xt-set-values toplevel
			 `(("XmNmaxHeight" ,(inexact->exact
					     (+ vp-h spacing)))
			   ("XmNmaxWidth" ,(inexact->exact
					    (+ vp-w spacing)))))))
    
    (x-set-foreground dpy toplevel-gc (x-white-pixel-of-screen screen))
    (x-fill-rectangle dpy base-pm toplevel-gc 0 0 vp-w vp-h)
    (x-fill-rectangle dpy back-pm toplevel-gc 0 0 vp-w vp-h)
    
    (x-set-window-background dpy window (x-white-pixel-of-screen screen))
    
    (putprop graph 'overlays '())
    (putprop graph 'toplevel-bitmap-gc toplevel-bitmap-gc)
    (putprop graph 'bitmap-clear-gc bitmap-clear-gc)
    (putprop graph 'bitmap-gxor-gc bitmap-gxor-gc)
    (putprop graph 'toplevel-gc toplevel-gc)
    (putprop graph 'overlay-erase-gc overlay-erase-gc)
    (putprop graph 'widget canvas)
    (putprop graph 'dpy dpy)
    (putprop graph 'window window)
    (putprop graph 'base-pm base-pm)
    (putprop graph 'back-pm back-pm)
    (putprop graph 'tiny-bitmap tiny-bitmap)
    (putprop graph 'work-bitmap work-bitmap)
    (putprop graph 'screen screen)
    (putprop graph 'virtual-viewport virtual-viewport)
    (putprop graph 'toplevel-env
	     (sgl-make-env graph
			   (list (make-decl 'font "9x15")
				 (make-decl 'text-justification 'left)
				 (make-decl 'text-mode 'normal)
				 )
			   (list (sgl-make-binding 'origin
						   (sgl-make-point 0 0))
				 (sgl-make-binding 'line-width 0)
				 (sgl-make-binding 'gcv toplevel-gcv)
				 (sgl-make-binding 'env 'base)
				 ) ;;; seed the current environment
			   window ;;; reference drawable
			   #t ;;; color?
			   sgl-base-gc-cache
			   #f
			   'sgl-create))
    (putprop graph 'toplevel-overlay-env
	     (sgl-make-env graph
			   (list (make-decl 'font "9x15")
				 (make-decl 'text-justification 'left)
				 (make-decl 'text-mode 'normal)
				 )
			   (list (sgl-make-binding 'origin
						   (sgl-make-point 0 0))
				 (sgl-make-binding 'line-width 0)
				 (sgl-make-binding 'gcv toplevel-bitmap-gcv)
				 (sgl-make-binding 'env 'overlay)
				 ) ;;; seed the current environment
			   tiny-bitmap ;;; reference drawable
			   #f ;;; color?
			   sgl-bitmap-gc-cache
			   #f
			   'sgl-create))
    (putprop graph 'pictures '())
    (putprop graph 'vp-w vp-w)
    (putprop graph 'vp-h vp-h)
    (putprop graph 'x-min (exact->inexact x-min)) ;;; clip window
    (putprop graph 'y-min (exact->inexact y-min))
    (putprop graph 'x-max (exact->inexact x-max))
    (putprop graph 'y-max (exact->inexact y-max))
    (putprop graph 'x-mpy (exact->inexact (/ vp-w (- x-max x-min))))
    (putprop graph 'y-mpy (exact->inexact (/ vp-h (- y-max y-min))))
    
    (xt-add-callback canvas "XmNresizeCallback" sgl-resize-cb graph)
    (xt-add-callback canvas "XmNexposeCallback" sgl-expose-cb graph)
    
    (if (=> 'is-a? (=> 'class toplevel) 'xx-vendor-shell)
	(let ((property (x-intern-atom dpy "WM_PROTOCOLS" #f))
	      (protocol (x-intern-atom dpy "WM_DELETE_WINDOW" #f)))
	  (xm-add-protocol-callback toplevel
				    property
				    protocol
				    sgl-delete-cb
				    graph)))
    graph
    ))

;;----------------------------------------------------------------------------
;;
;; Graphics User Interface Procedures
;;
(define draw!
  (let ((proc
	 (lambda (graph pe options)
	   (call/cc
	    (lambda (escape)
	      (putprop graph 'overlays (sgl-augment-overlays graph options))
	      (let* ((picture-id (gensym))
		     (modes (sgl-handle-modes graph pe options escape))
		     (dpy (getprop graph 'dpy))
		     (win (getprop graph 'window))
		     (screen (getprop graph 'screen))
		     (vp-h (getprop graph 'vp-h))
		     (vp-w (getprop graph 'vp-w))
		     (toplevel-gc (getprop graph 'toplevel-gc))
		     (option-ov-names
		      (sgl-options->overlay-names options))
		     (ovs-to-draw (sgl-reduce1 (lambda (ov)
						 (if (member
						      (sgl-overlay-name ov)
						      option-ov-names)
						     ov
						     #f))
					       (getprop graph 'overlays)))
		     )
		;;
		;; must not draw in window until first expose event!
		;;
		(let f ()
		  (if (not (getprop graph 'exposed))
		      (begin
			(*flush-event-queue*)
			(f))))
		
		(sgl-eval-pe 'draw! graph pe ovs-to-draw escape 'draw!)

		(sgl-update-backing-store graph)
		
		(if (and (pair? ovs-to-draw)
			 (andmap sgl-overlay-mapped? ovs-to-draw))
		    (sgl-expose-cb (getprop graph 'widget) graph #f))

		(if (not (memq 'omit-from-database options))
		    (putprop graph 'pictures
			     (cons (sgl-make-picture pe
						     options
						     modes
						     picture-id
						     option-ov-names)
				   (getprop graph 'pictures))))
		picture-id))))))
    (case-lambda [(graph pe) (proc graph pe (list 'normal))]
		 [(graph pe . options) (proc graph pe options)]
		 )))

(define add-picture draw!) ;;; alias


(define erase!
  (let ((proc
	 (lambda (graph pid)
	   (let* ((pic (let f ((pics (getprop graph 'pictures)))
			 (if (null? pics)
			     (sgl-error 'erase! "invalid picture id"))
			 (if (eq? pid (sgl-picture-id (car pics)))
			     (car pics)
			     (f (cdr pics)))))
		  (modes (sgl-picture-modes pic))
		  )
	     (for-each
	      (lambda (mode)
		(case (sgl-mode-id mode)
		  [WITH-BACKING-STORE
		   (let* ((dim (sgl-mode-dimension mode))
			  (dx (sgl-point-x dim))
			  (dy (sgl-point-y dim))
			  (dpy (getprop graph 'dpy))
			  (window (getprop graph 'window))
			  (base-pm (getprop graph 'base-pm))
			  (back-pm (getprop graph 'back-pm))
			  (env (getprop graph 'toplevel-env))
			  (org (sgl-mode-origin mode))
			  (ox (sgl-point-x org))
			  (oy (sgl-point-y org))
			  (gc (sgl-get-binding-value 'gc env))
			  (backing (sgl-mode-backing-store mode))
			  )
		     (x-copy-area dpy backing window gc 0 0 dx dy ox oy)
		     (x-copy-area dpy backing base-pm gc 0 0 dx dy ox oy)
		     (x-copy-area dpy backing back-pm gc 0 0 dx dy ox oy)
		     
		     (x-free-pixmap dpy backing) ;;; free backing store
		     ;;
		     ;; remove pic from pictures
		     ;; pictures must remain in order!!
		     ;;
		     (putprop graph 'pictures
			      (let f ((newpics '())
				      (pics (getprop graph 'pictures)))
				(if (null? pics)
				    (reverse newpics) ;;; preserve order!
				    (if (eq? (car pics) pic)
					(f newpics (cdr pics))
					(f (cons (car pics) newpics)
					   (cdr pics))))))
		     'void
		     )
		   ]
		  [NORMAL
		   (sgl-error 'erase! "can't erase normal-mode picture")
		   ]
		  [NULL
		   #f
		   ]
		  [else
		   (sgl-error 'erase! "invalid picture mode")
		   ]
		  ))
	      modes)))))
    (case-lambda [(graph pid)
		  (proc graph pid)
		  ]
		 )))


(define bounding-box
  (let ((proc
	 (lambda (graph pe)
	   (call/cc
	    (lambda (escape)
	      (let* ((bb 
		      (sgl-eval-pe 'bounding-box
				   graph
				   pe
				   '() ;;; overlays irrelevant
				   escape
				   'bounding-box))
		     (bbmin (sgl-bounding-box-min bb))
		     (bbmax (sgl-bounding-box-max bb)))
		(list (list (sgl-point-x bbmin) (sgl-point-y bbmin))
		      (list (sgl-point-x bbmax) (sgl-point-y bbmax)))
		))))))
    (case-lambda [(graph pe)
		  (proc graph pe)
		  ]
		 )))


;;
;;============================================================================
;;============================================================================
;;
;; caches
;;
(if (not (top-level-bound? 'sgl-base-gc-cache))
    (define sgl-base-gc-cache (sgl-make-cache '() ))) ;;;  (gcv . gc) pairs

(if (not (top-level-bound? 'sgl-bitmap-gc-cache))
    (define sgl-bitmap-gc-cache (sgl-make-cache '() ))) ;;; (gcv . gc) pairs

(if (not (top-level-bound? 'sgl-color-cache))
    (define sgl-color-cache (sgl-make-cache '() ))) ;;; (name . pixel) pairs

(if (not (top-level-bound? 'sgl-font-cache))
    (define sgl-font-cache (sgl-make-cache '() ))) ;;; (name . fs) pairs

;;
;;----------------------------------------------------------------------------
;;
(define (sgl-resize-cb widget graph call-data)
  (let* ((args (xt-get-values widget (list "XmNwidth" "XmNheight")))
	 (new-w (car args))
	 (new-h (cadr args))
	 (dpy (xt-display widget))
	 (window (getprop graph 'window))
	 (back-pm (getprop graph 'back-pm))
	 (work-bitmap (getprop graph 'work-bitmap))
	 (base-pm (getprop graph 'base-pm))
	 (virtual-viewport (getprop graph 'virtual-viewport))
	 (screen (xt-screen widget))
	 (toplevel-gc (getprop graph 'toplevel-gc))
	 (bitmap-clear-gc (getprop graph 'bitmap-clear-gc))
	 (tiny-bitmap (getprop graph 'tiny-bitmap))
	 (pictures (reverse (getprop graph 'pictures)))
	 (x-min (getprop graph 'x-min))
	 (x-max (getprop graph 'x-max))
	 (y-min (getprop graph 'y-min))
	 (y-max (getprop graph 'y-max))
	 )
    (x-fill-rectangle dpy window toplevel-gc 0 0 new-w new-h)
    ;;
    ;; make new backing store pixmap
    ;;
    (if back-pm (x-free-pixmap dpy back-pm))
    (set! back-pm (x-create-pixmap dpy
				   (x-default-root-window dpy)
				   new-w
				   new-h
				   (x-default-depth-of-screen screen)))
    (x-fill-rectangle dpy back-pm toplevel-gc 0 0 new-w new-h)
    (putprop graph 'back-pm back-pm)

    ;;
    ;; make new working bitmap
    ;;
    (if work-bitmap (x-free-pixmap dpy work-bitmap))
    (set! work-bitmap (x-create-pixmap dpy
				   (x-default-root-window dpy)
				   new-w
				   new-h
				   1))
    (putprop graph 'work-bitmap work-bitmap)

    ;;
    ;; make new base layer pixmap
    ;;
    (if base-pm (x-free-pixmap dpy base-pm))
    (set! base-pm (x-create-pixmap dpy
				   (x-default-root-window dpy)
				   new-w
				   new-h
				   (x-default-depth-of-screen screen)))
    (x-fill-rectangle dpy base-pm toplevel-gc 0 0 new-w new-h)
    (putprop graph 'base-pm base-pm)

    ;;
    ;; make new overlay bitmaps
    ;;
    (for-each (lambda (ov)
		(if (sgl-overlay-bitmap ov)
		    (x-free-pixmap dpy (sgl-overlay-bitmap ov)))
		(let ((bit (x-create-pixmap dpy
					    tiny-bitmap
					    new-w
					    new-h
					    1)))
		  (x-fill-rectangle dpy bit bitmap-clear-gc 0 0 new-w new-h)
		  (sgl-set-overlay-bitmap! ov bit))
		)
	      (getprop graph 'overlays))
		
    (putprop graph 'vp-w new-w)
    (putprop graph 'vp-h new-h)
    (putprop graph 'x-mpy (exact->inexact (/ new-w (- x-max x-min))))
    (putprop graph 'y-mpy (exact->inexact (/ new-h (- y-max y-min))))
    ;;
    ;; crunch through all the pictures
    ;;
    (for-each
     (lambda (pic)
       (let* ((pe (sgl-picture-pe pic))
	      (modes (sgl-picture-modes pic))
	      (options (sgl-picture-options pic))
	      (pic-overlay-names (sgl-picture-overlay-names pic))
	      (graph-ovs (getprop graph 'overlays))
	      (pic-ovs (sgl-reduce1 (lambda (ov)
				      (if (memq (sgl-overlay-name ov)
						pic-overlay-names)
					  ov
					  #f))
				    graph-ovs))
	      )
	 ;;
	 ;; crunch modes
	 ;;
	 (for-each (lambda (mode)
		     (let ((pix (sgl-mode-backing-store mode)))
		       (if pix (x-free-pixmap dpy pix))))
		   modes)
	 
	 (sgl-set-picture-modes! pic (sgl-handle-modes graph pe options #f))
	 (sgl-eval-pe 'draw! graph pe pic-ovs #f 'sgl-resize-cb)
	 ))
     pictures)
    
    (sgl-update-backing-store graph)

    (sgl-expose-cb (getprop graph 'widget) graph #f)
    ))
;;
;;----------------------------------------------------------------------------
;;
;; update window from backing-store
;;
(define (sgl-expose-cb widget graph call-data)
  (let* ((args (xt-get-values widget (list "XmNwidth" "XmNheight")))
	 (width (car args))
	 (height (cadr args))
	 (dpy (xt-display widget))
	 (win (xt-window widget))
	 (toplevel-gc (getprop graph 'toplevel-gc))
	 (back-pm (getprop graph 'back-pm))
	 )
    (x-copy-area dpy back-pm win toplevel-gc 0 0 width height 0 0)
    (putprop graph 'exposed #t)
    )
  )
;;
;;----------------------------------------------------------------------------
;;
(define (sgl-delete-cb widget graph call-data)
  (if (getprop graph 'toplevel-env)
      (let* ((dpy (xt-display widget))
	     (base-pm (getprop graph 'base-pm))
	     (back-pm (getprop graph 'back-pm))
	     )
	(if base-pm (x-free-pixmap dpy base-pm))
	(if back-pm (x-free-pixmap dpy back-pm))
	(for-each (lambda (pic)
		    (let ((modes (sgl-picture-modes pic)))
		      (for-each
		       (lambda (mode)
			 (let ((pix (sgl-mode-backing-store mode)))
			   (if pix (x-free-pixmap dpy pix))
			   ))
		       modes)))
		  (getprop graph 'pictures))

	(for-each (lambda (ov)
		    (x-free-pixmap dpy (sgl-overlay-bitmap ov))
		    (x-free-g-c dpy (sgl-overlay-gc ov))
		    )
		  (getprop graph 'overlays))

	(putprop graph 'exposed #f)
	(for-each (lambda (prop) (remprop graph prop)) (property-list graph))
	(xt-destroy-widget widget)
	))
  )
;;
;;----------------------------------------------------------------------------
;;
;; update backing-store pixmap(s)
;;
(define (sgl-update-backing-store graph)
  (let* ((overlay-erase-gc (getprop graph 'overlay-erase-gc))
	 (toplevel-gc (getprop graph 'toplevel-gc))
	 (tbgc (getprop graph 'toplevel-bitmap-gc))
	 (bitmap-gxor-gc (getprop graph 'bitmap-gxor-gc))
	 (dpy (xt-display (getprop graph 'widget)))
	 (win (xt-window (getprop graph 'widget)))
	 (back-pm (getprop graph 'back-pm))
	 (base-pm (getprop graph 'base-pm))
	 (work-bitmap (getprop graph 'work-bitmap))
	 (vp-w (getprop graph 'vp-w))
	 (vp-h (getprop graph 'vp-h))
	 (ovs (getprop graph 'overlays))
	 (mapped-ov-groups (sgl-group-mapped-overlays-by-pixel ovs))
	 )
    (x-copy-area dpy base-pm back-pm toplevel-gc 0 0 vp-w vp-h 0 0)
    (for-each
     (lambda (mapped-ov-group)
       (let* ((first-ov (car mapped-ov-group))
	      (ov-gc (sgl-overlay-gc first-ov)))
	 ;;
	 ;; init work-bitmap with contents of first overlay
	 ;;
	 (x-copy-area dpy
		      (sgl-overlay-bitmap first-ov)
		      work-bitmap
		      tbgc
		      0 0
		      vp-w vp-h
		      0 0)
	 ;;
	 ;; OR in the remaining bitmaps
	 ;;
	 (for-each (lambda (mapped-ov)
		     (x-copy-area dpy
				  (sgl-overlay-bitmap mapped-ov)
				  work-bitmap
				  bitmap-gxor-gc
				  0 0
				  vp-w vp-h
				  0 0))
		   (cdr mapped-ov-group))
	 ;;
	 ;; erase portion of backing-store pm overlayed by the ov-group
	 ;;
	 (x-copy-plane dpy work-bitmap back-pm overlay-erase-gc
		       0 0 vp-w vp-h 0 0 1)
	 ;;
	 ;; add overlay group to backing-store pm
	 ;;
	 (x-copy-plane dpy work-bitmap back-pm ov-gc 0 0 vp-w vp-h 0 0 1)
	 ))
     mapped-ov-groups)
    ))

;;
;; an overlay group is a collection of mapped overlays related
;; by pixel value (color). Overlays are returned in drawable order
;; (left-to-right = bottom-to-top)
;;

(define (sgl-group-mapped-overlays-by-pixel ovs)
  (define (group-ovs ovs groups like-ovs group-pixel)
    (if (null? ovs)
	(if (pair? like-ovs)
	    (cons like-ovs groups)
	    groups)
	(let* ((ov (car ovs))
	       (ov-pixel (sgl-overlay-pixel ov)))
	  (if (=> 'equal? group-pixel ov-pixel)
	      (group-ovs (cdr ovs) groups (cons ov like-ovs) group-pixel)
	      (group-ovs (cdr ovs)
			 (cons like-ovs groups)
			 (list ov) ov-pixel)))))
  (let ((mapped-ovs (sgl-reduce1 (lambda (ov)
			       (if (sgl-overlay-mapped? ov) ov #f))
			     ovs)))
    (if (pair? mapped-ovs)
	(group-ovs mapped-ovs '() '() (sgl-overlay-pixel (car mapped-ovs)))
	'())))
;;----------------------------------------------------------------------------
;;
;; sgl-handle-modes
;;
;; processes draw! mode options - returns a list of mode objects
;;
(define (sgl-handle-modes graph pe options escape)
  (define (handle-mode option)
    ;;
    ;; option symbol
    ;;
    (case option
      [WITH-BACKING-STORE
       (let ((bb (sgl-eval-pe 'bounding-box
			      graph
			      pe
			      '() ;;; overlays irrelevant
			      escape
			      'sgl-handle-modes)))
	 (if (eq? bb #f);; null picture?
	     (sgl-make-mode 'NULL #f #f #f)
	     (let* ((dpy (getprop graph 'dpy))
		    (screen (getprop graph 'screen))
		    (src (getprop graph 'base-pm))
		    (back-pm (getprop graph 'back-pm))
		    (env (getprop graph 'toplevel-env))
		    (vp-h (getprop graph 'vp-h))
		    (vp-w (getprop graph 'vp-w))
		    (bb-min (sgl-bounding-box-min bb))
		    (bb-max (sgl-bounding-box-max bb))
		    (toplevel-gc (getprop graph 'toplevel-gc))
		    (x-min (getprop graph 'x-min))
		    (y-max (getprop graph 'y-max))
		    (x-mpy (getprop graph 'x-mpy))
		    (y-mpy (getprop graph 'y-mpy))
		    (x-add (- 0 x-min))
		    (y-add (- y-max 0))
		    (vt-bb-min.x
		     (1- (sgl-view-x (sgl-point-x bb-min) x-add x-mpy)))
		    (vt-bb-min.y
		     (1+ (sgl-view-y (sgl-point-y bb-min) y-add y-mpy)))
		    (vt-bb-max.x
		     (1+ (sgl-view-x (sgl-point-x bb-max) x-add x-mpy)))
		    (vt-bb-max.y
		     (1- (sgl-view-y (sgl-point-y bb-max) y-add y-mpy)))
		    (dim.x (- vt-bb-max.x vt-bb-min.x))
		    (dim.y (- vt-bb-min.y vt-bb-max.y))
		    (backing-store
		     (x-create-pixmap dpy
				      src
				      dim.x
				      dim.y
				      (x-default-depth-of-screen screen)))
		    )
	       
	       (x-fill-rectangle dpy backing-store toplevel-gc 0 0
				 dim.x dim.y)
	       
	       (x-copy-area dpy src backing-store toplevel-gc
			    vt-bb-min.x vt-bb-max.y dim.x dim.y 0 0)
	       (sgl-make-mode option
			      backing-store
			      (sgl-make-point vt-bb-min.x vt-bb-max.y)
			      (sgl-make-point dim.x dim.y))
	       )))
       ]
      [NORMAL
       (sgl-make-mode option #f #f #f)
       ]
      [(OMIT-FROM-DATABASE NULL)
       (sgl-make-mode option #f #f #f)
       ]
      [else 'unrecognized-mode]
      ))
  (sgl-reduce1 (lambda (m)
		 (if (not (eq? m 'unrecognized-mode))
		     m
		     #f))
	       (map handle-mode options))
  )

;;
;; extracts overlay names from options list, returning
;; a (possibly empty) list of the names.
;;

(define (sgl-options->overlay-names options)
  (if (pair? options)
      (let ((opt (car options)))
	(if (pair? opt)
	    (let ((ovs (if (eq? (car opt) 'overlays) (cdr opt) #f)))
	      (if ovs
		  ovs
		  (sgl-options->overlay-names (cdr options))))
	    (sgl-options->overlay-names (cdr options))))
      '()))

;;
;; crunches option list, creates new overlays, returns all overlays
;;
(define (sgl-augment-overlays graph options)
  (let* ((ov-names (sgl-options->overlay-names options))
	 (old-overlays (getprop graph 'overlays))
	 (old-names (map (lambda (l) (sgl-overlay-name l)) old-overlays))
	 (new-names (sgl-reduce1 (lambda (name)
				   (if (not (memq name old-names))
				       name
				       #f))
				 ov-names))
	 (dpy (getprop graph 'dpy))
	 (screen (getprop graph 'screen))
	 (base-pm (getprop graph 'base-pm))
	 (toplevel-gc (getprop graph 'toplevel-gc))
	 (vp-h (getprop graph 'vp-h))
	 (vp-w (getprop graph 'vp-w))
	 (tiny-bitmap (getprop graph 'tiny-bitmap))
	 (bitmap-clear-gc (getprop graph 'bitmap-clear-gc))
	 (initial-ov-pixel (x-black-pixel-of-screen screen))
	 (new-ovs
	  (map (lambda (overlay-name)
		 (sgl-make-overlay
		  overlay-name
		  (let ((bit (x-create-pixmap dpy tiny-bitmap vp-w vp-h 1)))
		    (x-fill-rectangle dpy bit bitmap-clear-gc 0 0 vp-w vp-h)
		    bit)
		  ;;
		  ;; an overlay GC has the depth of the window!
		  ;;
		  (let* ((overlay-gcv
			  (let ((v (=> 'new xx-x-g-c-values)))
			    (=> 'set-foreground! v initial-ov-pixel)
			    (=> 'set-background! v 0) ;;; transparent color
			    (=> 'set-function! v "GXor")
			    (=> 'set-graphics-exposures! v #f)
			    v))
			 (overlay-gc (x-create-g-c dpy
						   base-pm
						   '("GCForeground"
						     "GCBackground"
						     "GCFunction")
						   overlay-gcv)))
		    overlay-gc)
		  #f ;;; initially unmapped
		  "black"
		  initial-ov-pixel)
		 )
	       new-names))
	 )
    (append new-ovs old-overlays)))
;;
;;----------------------------------------------------------------------------
;;
;; sgl-eval-pe - picture expression interpreter
;;

;;
;; Drawing may be either to overlays, or to the base layer (but not both
;; simultaneously).
;;
;; overlay drawing is constrained: foreground-color and background-color
;; are prohibited.
;;
;;
;; if overlays is null list, drawing is into base, else drawing is
;; into (one or more) overlays.
;;
(define (sgl-eval-pe operation graph pe overlays escape for-whom)
  ;;
  ;; "data" are two-element lists (i.e., the external rep for a point).
  ;;
  (define make-datum list)
  (define d.x car)
  (define d.y cadr)
  
  (let* ((draw-to-base? (null? overlays))
	 (dpy (getprop graph 'dpy))
	 (win (if draw-to-base? (getprop graph 'window) #f))
	 (base-pm (if draw-to-base? (getprop graph 'base-pm) #f))
	 (vp-w (getprop graph 'vp-w))
	 (vp-h (getprop graph 'vp-h))
	 (x-min (getprop graph 'x-min))
	 (y-max (getprop graph 'y-max))
	 (x-mpy (getprop graph 'x-mpy))
	 (y-mpy (getprop graph 'y-mpy))
	 (env (if draw-to-base?
		  (getprop graph 'toplevel-env)
		  (getprop graph 'toplevel-overlay-env)
		  ))
	 ;;
	 ;; bitmaps to be drawn into
	 ;;
	 (bitmaps (map sgl-overlay-bitmap overlays))
	 
	 (ref-drawable (if draw-to-base?
			   (getprop graph 'window)
			   (getprop graph 'tiny-bitmap)
			   ))
	 (color? draw-to-base?) ;;; color enabled?
	 (gc-cache (if draw-to-base? sgl-base-gc-cache sgl-bitmap-gc-cache))
	 )
    (define (line-draw data env)
      (let* ((gc (sgl-get-binding-value 'gc env))
	     (origin (sgl-get-binding-value 'origin env))
	     (x-add (- (sgl-point-x origin) x-min))
	     (y-add (- y-max (sgl-point-y origin)))
	     )
	(let f ((data data))
	  (if (null? data)
	      #f
	      (let ((p1.x (sgl-view-x (d.x (car data)) x-add x-mpy))
		    (p1.y (sgl-view-y (d.y (car data)) y-add y-mpy))
		    (p2.x (sgl-view-x (d.x (cadr data)) x-add x-mpy))
		    (p2.y (sgl-view-y (d.y (cadr data)) y-add y-mpy))
		    )
		(if draw-to-base?
		    (begin
		      (x-draw-line dpy win gc p1.x p1.y p2.x p2.y)
		      (x-draw-line dpy base-pm gc p1.x p1.y p2.x p2.y))
		    (for-each (lambda (bit)
				(x-draw-line dpy bit gc p1.x p1.y p2.x p2.y))
			      bitmaps))
		(f (cddr data))))))
      )
    (define (rectangle-draw type data env)
      (let* ((gc (sgl-get-binding-value 'gc env))
	     (origin (sgl-get-binding-value 'origin env))
	     (x-add (- (sgl-point-x origin) x-min))
	     (y-add (- y-max (sgl-point-y origin)))
	     )
	(let f ((data data))
	  (if (null? data)
	      #f
	      (let* ((p1.x (sgl-view-x (d.x (car data)) x-add x-mpy))
		     (p1.y (sgl-view-y (d.y (car data)) y-add y-mpy))
		     (p2.x (sgl-view-x (d.x (cadr data)) x-add x-mpy))
		     (p2.y (sgl-view-y (d.y (cadr data)) y-add y-mpy))
		     (proc (case type
			     [RECTANGLE x-draw-rectangle]
			     [FILLED-RECTANGLE x-fill-rectangle]))
		     (h (- p1.y p2.y))
		     (h (if (eq? type 'FILLED-RECTANGLE) (1+ h) h))
		     (w (- p2.x p1.x))
		     (w (if (eq? type 'FILLED-RECTANGLE) (1+ w) w))
		     )
		(if (and (positive? w) (positive? h))
		    (begin
		      (if draw-to-base?
			  (begin
			    (proc dpy win gc p1.x p2.y (1- w) (1- h))
			    (proc dpy base-pm gc p1.x p2.y (1- w) (1- h)))
			  (for-each
			   (lambda (bit)
			     (proc dpy bit gc p1.x p2.y (1- w) (1- h)))
			   bitmaps))
		      ))
		(f (cddr data)))))))
    
    (define (polyline-draw data env)
      (let* ((gc (sgl-get-binding-value 'gc env))
	     (origin (sgl-get-binding-value 'origin env))
	     (pts (sgl-coalesce-points data graph origin))
	     )
	(if draw-to-base?
	    (begin
	      (x-draw-lines dpy win gc pts "CoordModeOrigin")
	      (x-draw-lines dpy base-pm gc pts "CoordModeOrigin"))
	    (for-each (lambda (bit)
			(x-draw-lines dpy bit gc pts "CoordModeOrigin"))
		      bitmaps))))
    
    (define (polypoint-draw data env)
      (let* ((gc (sgl-get-binding-value 'gc env))
	     (origin (sgl-get-binding-value 'origin env))
	     (x-add (- (sgl-point-x origin) x-min))
	     (y-add (- y-max (sgl-point-y origin)))
	     )
	(let ((pts (map (lambda (d)
			  (list (sgl-view-x (d.x d) x-add x-mpy)
				(sgl-view-y (d.y d) y-add y-mpy)))
			data)))
	  (if draw-to-base?
	      (begin
		(x-draw-points dpy win gc pts "CoordModeOrigin")
		(x-draw-points dpy base-pm gc pts "CoordModeOrigin"))
	      (for-each (lambda (bit)
			  (x-draw-points dpy bit gc pts "CoordModeOrigin"))
			bitmaps)
	      ))))
    
    (define (polymarker data env)
      ;;
      ;; "segments" are two element lists, where each element is a datum.
      ;;
      (define s.1 car)
      (define s.2 cadr)

      (let* ((marker (car data))
	     (data-pts (cdr data))
	     (gc (sgl-get-binding-value 'gc env))
	     (origin (sgl-get-binding-value 'origin env))
	     (x-add (- (sgl-point-x origin) x-min))
	     (y-add (- y-max (sgl-point-y origin)))
	     )
	(for-each
	 (lambda (datum)
	   (let* ((dx (d.x datum))
		  (dy (d.y datum))
		  (pts (map (lambda (seg)
			      (let ((s1 (s.1 seg))
				    (s2 (s.2 seg)))
				(list
				 (sgl-view-x (+ dx (d.x s1)) x-add x-mpy)
				 (sgl-view-y (+ dy (d.y s1)) y-add y-mpy)
				 (sgl-view-x (+ dx (d.x s2)) x-add x-mpy)
				 (sgl-view-y (+ dy (d.y s2)) y-add y-mpy)
				 )))
			    marker))
		  )
	     (if draw-to-base?
		 (begin
		   (x-draw-segments dpy win gc pts)
		   (x-draw-segments dpy base-pm gc pts))
		 (for-each (lambda (bit)
			     (x-draw-segments dpy bit gc pts))
			   bitmaps))
	     ))
	 data-pts)))
    ;;
    ;; needs work
    ;;
    (define (polymarker-bb data env)
      (sgl-compute-bounding-box vp-w vp-h env (cdr data)))
    
    (define (arc-draw type data env)
      (let* ((gc (sgl-get-binding-value 'gc env))
	     (new-orig (car data))
	     (extent (cadr data))
	     (angle1 (caddr data))
	     (angle2 (cadddr data))
	     (x (- (d.x new-orig) (/ (d.x extent) 2)))
	     (y (+ (d.y new-orig) (/ (d.y extent) 2)))
	     (origin (sgl-get-binding-value 'origin env))
	     (x-add (- (sgl-point-x origin) x-min))
	     (y-add (- y-max (sgl-point-y origin)))
	     )
	(let* ((pxe (* (d.x extent) x-mpy))
	       (pye (* (d.y extent) y-mpy))
	       (pxp (sgl-view-x x x-add x-mpy))
	       (pyp (sgl-view-y y y-add y-mpy))
	       (a1 (* angle1 64))
	       (a2 (* angle2 64))
	       (proc (case type
		       [ARC x-draw-arc]
		       [FILLED-ARC x-fill-arc]))
	       )
	  (if draw-to-base?
	      (begin
		(proc dpy win gc pxp pyp pxe pye a1 a2)
		(proc dpy base-pm gc pxp pyp pxe pye a1 a2))
	      (for-each (lambda (bit)
			  (proc dpy bit gc pxp pyp pxe pye a1 a2))
			bitmaps))
	  )))
    
    (define (arc-bb data env)
      (let ((new-orig (car data))
	    (extent (cadr data))
	    )
	(sgl-compute-bounding-box vp-w
				  vp-h
				  env
				  (list (make-datum (- (d.x new-orig)
						       (d.x extent))
						    (- (d.y new-orig)
						       (d.y extent)))
					(make-datum (+ (d.x new-orig)
						       (d.x extent))
						    (+ (d.y new-orig)
						       (d.y extent)))))))
    
    (define (text operation data env)
      (let ((len (length data)))
	(if (and (not (eqv? len 1))
		 (not (eqv? len 2)))
	    (sgl-error for-whom
		      "sgl-eval-pe: text: syntax error in expression: "
		      data))
	(let* ((just-type (sgl-get-binding-value 'text-justification env))
	       (text-mode (sgl-get-binding-value 'text-mode env))
	       (text-prim (case text-mode
			    [normal x-draw-string]
			    [image x-draw-image-string]
			    ))
	       (text (if (eqv? len 1) (car data) (cadr data)))
	       (fs (sgl-get-binding-value 'font-struct env))
	       (just-ofs (case just-type
			   [center
			    (/ (exact->inexact (/ (x-text-width fs text) vp-w))
			       2)]
			   [right
			    (exact->inexact (/ (x-text-width fs text) vp-w))]
			   [else 0]))
	       (env (if (eqv? len 1)
			env
			(sgl-make-env graph
				      `((origin ,(car data))) ;;; new binding
				      env
				      ref-drawable
				      color?
				      gc-cache
				      (lambda x
					(sgl-error
					 for-whom
					 "sgl-eval-pe: text: env error"))
				      for-whom)))
	       )
	  (case operation
	    [draw!
	     (let* ((gc (sgl-get-binding-value 'gc env))
		    (origin (sgl-get-binding-value 'origin env))
		    (x-add (- (- (sgl-point-x origin) just-ofs) x-min))
		    (y-add (- y-max (sgl-point-y origin)))
		    (px (sgl-view-x 0 x-add x-mpy))
		    (py (sgl-view-y 0 y-add y-mpy))
		    )
	       (if draw-to-base?
		   (begin
		     (text-prim dpy win gc px py text)
		     (text-prim dpy base-pm gc px py text))
		   (for-each (lambda (bit)
			       (text-prim dpy bit gc px py text))
			     bitmaps))
	       )
	     ]
	    [bounding-box
	     (let* ((mba (/ (=> 'get-max-bounds-ascent fs) vp-h))
		    (mbd (/ (=> 'get-max-bounds-descent fs) vp-h))
		    (w (x-text-width fs text))
		    (x-ext (exact->inexact (/ w vp-w)))
		    )
	       (sgl-compute-bounding-box vp-w
					 vp-h
					 env
					 (list (make-datum (- 0 just-ofs)
							   (- mbd))
					       (make-datum (- x-ext just-ofs)
							   mba)))
	       )
	     ]
	    ))))
    
    (define (text-vertical operation data env)
      (let ((len (length data)))
	(if (and (not (= len 1))
		 (not (= len 2)))
	    (sgl-error
	     for-whom
	     "sgl-eval-pe: text-vertical: syntax error in expression: "
	     data))
	(let* ((text (if (= len 1) (car data) (cadr data)))
	       (fs (sgl-get-binding-value 'font-struct env))
	       (text-mode (sgl-get-binding-value 'text-mode env))
	       (text-prim (case text-mode
			    [normal x-draw-string]
			    [image x-draw-image-string]
			    ))
	       (pch1-x (x-text-width fs "M"))
	       (pch1-y (=> 'get-font-height fs))
	       (ch1-y (exact->inexact (/ pch1-y vp-h)))
	       (x-ext (exact->inexact (/ pch1-x vp-w)))
	       (y-ext (* ch1-y (string-length text)))
	       (y-descent
		 (exact->inexact (/ (=> 'get-max-bounds-descent fs) vp-h)))
	       (just-ofs (+ y-descent (- ch1-y)
			   (case
			     (sgl-get-binding-value 'text-justification env)
			     [center (/ y-ext 2)]
			     [up y-ext]
			     [else 0])))
	       (env (if (= len 1)
			env
			(sgl-make-env graph
				      `((origin ,(car data))) ;;; new binding
				      env
				      ref-drawable
				      color?
				      gc-cache
				      (lambda x
					(sgl-error
					 for-whom
					 "sgl-eval-pe: text: escape error"))
				      for-whom)))
	       )
	  (case operation
	    [draw!
	     (let* ((gc (sgl-get-binding-value 'gc env))
		    (origin (sgl-get-binding-value 'origin env))
		    (x-add (- (sgl-point-x origin) x-min))
		    (y-add (- y-max (+ (sgl-point-y origin) just-ofs)))
		    (px (sgl-view-x 0 x-add x-mpy))
		    (py (sgl-view-y 0 y-add y-mpy))
		    )
	       (letrec
		   ((draw-chars-vert
		     (lambda (chars x y)
		       (if (null? chars)
			   '()
			   (let* ((str (make-string 1 (car chars)))
				  (x-adj (round (- x (/ (x-text-width fs str)
							2)))))
			     (if draw-to-base?
				 (begin
				   (text-prim dpy win gc x-adj y str)
				   (text-prim dpy base-pm gc x-adj y str))
				 (for-each (lambda (bit)
					     (text-prim dpy bit gc
					       x-adj y str))
					   bitmaps))
			     (draw-chars-vert (cdr chars) x (+ y pch1-y)))))))
		 (draw-chars-vert (string->list text) px py)))
	     ]
	    [bounding-box
	     (sgl-compute-bounding-box vp-w
				       vp-h
				       env
				       (list (make-datum -.02 ;;; fudge
							 (+ (- y-ext)
							    just-ofs
							    ch1-y))
					     (make-datum x-ext (+ just-ofs
								  ch1-y))))]
	    ))))

    (define (let-sgl data env)
      (let* ((new-env (sgl-make-env graph
				    (car data)
				    env
				    ref-drawable
				    color?
				    gc-cache
				    escape
				    for-whom))
	     (rtn-val
	      (case operation
		[draw!
		 (for-each (lambda (expr) (eval-pe 'draw! expr new-env))
			   (cdr data))
		 ]
		[bounding-box
		 (let ((boxes (map (lambda (pe)
				     (eval-pe 'bounding-box pe new-env))
				   (cdr data))))
		   (let crunch-boxes
		       ([min-x (sgl-point-x
				(sgl-bounding-box-min (car boxes)))]
			[min-y (sgl-point-y
				(sgl-bounding-box-min (car boxes)))]
			[max-x (sgl-point-x
				(sgl-bounding-box-max (car boxes)))]
			[max-y (sgl-point-y
				(sgl-bounding-box-max (car boxes)))]
			[boxes boxes]
			)
		     (if (null? boxes)
			 (sgl-make-bounding-box (sgl-make-point min-x min-y)
						(sgl-make-point max-x max-y))
			 (let* ((box (car boxes))
				(b-min (sgl-bounding-box-min box))
				(b-max (sgl-bounding-box-max box)))
			   (crunch-boxes (min min-x (sgl-point-x b-min))
					 (min min-y (sgl-point-y b-min))
					 (max max-x (sgl-point-x b-max))
					 (max max-y (sgl-point-y b-max))
					 (cdr boxes))))))
		 ]
		)))
	;;	  (if (eq? (sgl-binding-variable (car new-env)) 'gc)
	;;	      (x-free-g-c dpy (sgl-get-binding-value 'gc new-env)))
	rtn-val))
    
    (define (eval-pe operation pe env)
      (if (not (null? pe))
	  (let ((type (car pe))
		(data (cdr pe)))
	    (case type
	      [LINE
	       (case operation
		 [draw! (line-draw data env)]
		 [bounding-box (sgl-compute-bounding-box vp-w vp-h env data)])]
	      [(RECTANGLE FILLED-RECTANGLE)
	       (case operation
		 [draw! (rectangle-draw type data env)]
		 [bounding-box (sgl-compute-bounding-box vp-w vp-h env data)])]
	      [POLYLINE
	       (case operation
		 [draw! (polyline-draw data env)]
		 [bounding-box (sgl-compute-bounding-box vp-w vp-h env data)])]
	      [POLYPOINT
	       (case operation
		 [draw! (polypoint-draw data env)]
		 [bounding-box (sgl-compute-bounding-box vp-w vp-h env data)])]
	      [POLYMARKER
	       (case operation
		 [draw! (polymarker-draw data env)]
		 [bounding-box (polymarker-bb data env)])]
	      [(ARC FILLED-ARC)
	       (case operation
		 [draw! (arc-draw type data env)]
		 [bounding-box (arc-bb data env)])]
	      [TEXT (text operation data env)]
	      [(TEXT-VERTICAL TEXT-V) (text-vertical operation data env)]
	      [LET-SGL (let-sgl data env)]
	      [else (sgl-error for-whom
			       "sgl-eval-pe: unknown operation: "
			       type)
		    (escape (void))]
	      ))))
    (eval-pe operation pe env)
    ))

;;----------------------------------------------------------------------------


(define (sgl-get-binding-value name bindings)
  (let ((binding (sgl-lookup-binding name bindings)))
    (if (sgl-binding? binding)
	(sgl-binding-value binding)
	(sgl-error 'sgl-get-binding-value "binding for " name " not found"))))

(define (sgl-lookup-binding name bindings)
  (cond [(null? bindings) '()]
	[(eq? name (sgl-binding-variable (car bindings))) (car bindings)]
	[else (sgl-lookup-binding name (cdr bindings))]))

;;
;; sgl-make-env
;;
;; takes a graph, environment declarations, old environment, and
;; error escape procedure.
;;
;; returns a new environment. The new environment possibly contains new
;; bindings for gc and gcv variables.
;;
;; modifies the inputted gc-cache as a side effect
;;

(define (sgl-make-env graph
		      decls
		      old-env
		      ref-drawable
		      color?
		      gc-cache
		      escape
		      for-whom)
  ;;
  ;; decl's are two-element lists (i.e., the external rep for a binding).
  ;;
  (define decl.variable car)
  (define decl.value cadr)
  ;;
  ;; "data" are two-element lists (i.e., the external rep for a point).
  ;;
  (define d.x car)
  (define d.y cadr)
  
  (define (make-env gcv decls old-env)
    (if (null? decls)
	(if gcv
	    (let ((gc (let f ((cache-hit (sgl-cache-value gc-cache)))
			(if (null? cache-hit)
			    ;;
			    ;; cache miss - create gc and insert
			    ;;
			    (let ((gc (x-create-g-c
				       (getprop graph 'dpy)
				       ref-drawable
				       (=> 'compute-xx-x-g-c-values-mask gcv)
				       gcv)))
			      (sgl-set-cache-value!
			       gc-cache
			       (cons (cons gcv gc)
				     (sgl-cache-value gc-cache)))
			      gc)
			    (if (=> 'equal? gcv (caar cache-hit))
				;;
				;; cache hit - return gc
				;;
				(cdar cache-hit)
				(f (cdr cache-hit)))))))
	      (cons (sgl-make-binding 'gc gc)
		    (cons (sgl-make-binding 'gcv gcv)
			  old-env)))
	    old-env)
	(let* ((decl (car decls))
	       (name (decl.variable decl))
	       (value (decl.value decl))
	       (gcv (if (and (not gcv)
			     ;;
			     ;; is new binding a gc element?
			     ;;
			     (memq name '(foreground-color
					  background-color
					  line-width
					  line-style
					  fill-style
					  drawing-function
					  dash-offset
					  dashes
					  font)))
			(=> 'new-copy xx-x-g-c-values
			    (sgl-get-binding-value 'gcv old-env))
			gcv))
	       )
	  (case name
	    [(FOREGROUND-COLOR BACKGROUND-COLOR)
	     (if color?
		 (let* ((dpy (getprop graph 'dpy))
			(color (let ((cache-hit
				      (assoc
				       value
				       (sgl-cache-value sgl-color-cache))))
				 (if cache-hit
				     (cdr cache-hit)
				     (let* ((rtn (x-alloc-named-color
						  dpy
						  (x-default-colormap dpy 0)
						  value))
					    (rtn
					     (if (eq? (car rtn) 0)
						 (begin 
						   (sgl-warning
						    for-whom
						    "requested color "
						    "not available: "
						    value)
						   (x-alloc-named-color
						    dpy
						    (x-default-colormap dpy 0)
						    "black"))
						 rtn)))
				       (sgl-set-cache-value!
					sgl-color-cache
					(cons (cons value (cadr rtn))
					      (sgl-cache-value
					       sgl-color-cache)))
				       (cadr rtn)))))
			)
		   (if (eq? name 'foreground-color)
		       (=> 'set-foreground! gcv (=> 'get-pixel color))
		       (=> 'set-background! gcv (=> 'get-pixel color)))
		   (make-env gcv
			     (cdr decls)
			     (cons (sgl-make-binding name value)
				   old-env)))
		 (sgl-error for-whom "cannot bind color for overlay - "
			   "use set-overlay-color!"))
	     ]
	  
	    [DRAWING-FUNCTION
	      (if (not (=> 'type-ok? xx-function value))
		(sgl-error for-whom "invalid drawing function: " value))
	      (=> 'set-function! gcv value)
	      (make-env gcv
		(cdr decls)
		(cons (sgl-make-binding 'drawing-function value) old-env))]
	    
	    [DASH-OFFSET
	      (if (not (=> 'type-ok? xx-int value))
		(sgl-error for-whom "invalid dash offset: " value))
	      (=> 'set-dash-offset! gcv value)
	      (make-env gcv
		(cdr decls)
		(cons (sgl-make-binding 'dash-offset value) old-env))]

    	    [DASHES
	      (if (not (=> 'type-ok? xx-char value))
		(sgl-error for-whom "invalid dashes: " value))
	      (=> 'set-dashes! gcv value)
	      (make-env gcv
		(cdr decls)
		(cons (sgl-make-binding 'dashes value) old-env))]

	    [LINE-WIDTH
	     (=> 'set-line-width! gcv value)
	     (make-env gcv
		       (cdr decls)
		       (cons (sgl-make-binding 'line-width value) old-env))]
	  
	    [LINE-STYLE
	     (=> 'set-line-style! gcv value)
	     (make-env gcv
		       (cdr decls)
		       (cons (sgl-make-binding 'line-style value) old-env))]
	  
	    [FILL-STYLE
	     (=> 'set-fill-style! gcv value)
	     (make-env gcv
		       (cdr decls)
		       (cons (sgl-make-binding 'fill-style value) old-env))]
	  
	    [ORIGIN
	     (let ((origin (sgl-get-binding-value 'origin old-env)))
	       (if (eq? (car value) 'text-relative)
		   (let* ((factor (cadr value))
			  (fs (sgl-get-binding-value 'font-struct old-env))
			  (fh (/ (* (=> 'get-max-bounds-ascent fs)
				    (d.y factor))
				 (getprop graph 'vp-h)))
			  (fw (/ (* (x-text-width fs "M")
				    (d.x factor))
				 (getprop graph 'vp-w)))
			  )
		     (make-env gcv
			       (cdr decls)
			       (cons (sgl-make-binding 'origin
						       (sgl-make-point
							(+ fw
							   (sgl-point-x
							    origin))
							(+ fh (sgl-point-y
							       origin))))
				     old-env)))
		   (make-env gcv
			     (cdr decls)
			     (cons (sgl-make-binding 'origin
						     (sgl-make-point
						      (+ (d.x value)
							 (sgl-point-x origin))
						      (+ (d.y value)
							 (sgl-point-y
							  origin))))
				   old-env))))]
	    [FONT
	     (if (and (list? value)
		      (eq? (car value) 'variable))
		 ;;
		 ;; variable font
		 ;;
		 (let* ((factor (cadr value))
			(h (/ (getprop graph 'vp-h) 15))
			(w (/ (getprop graph 'vp-w) 35))
			(fs
			 (cdar
			  (sort (lambda (a b)
				  (< (car a) (car b)))
				(map (lambda (fs)
				       (let* ((fh (/ (=> 'get-font-height fs)
						     factor))
					      (fw (/ (x-text-width fs "M")
						     factor))
					      (h1 (if (> fh h) (* 10 (- fh h))
						      (- h fh)))
					      (w1 (if (> fw w) (* 10 (- fw w))
						      (- w fw))))
					 (cons (max h1 w1) fs)))
				     sgl-variable-font-structs))))
			)
		   (=> 'set-font! gcv (=> 'get-fid fs))
		   (make-env gcv
			     (cdr decls)
			     (append (list (sgl-make-binding 'font "")
					   (sgl-make-binding 'font-struct fs))
				     old-env)))
		 ;;
		 ;; fixed font
		 ;;
		 (let* ((name value)
			(fs
			 (let ((cache-hit
				(assoc name (sgl-cache-value sgl-font-cache))))
			   (if cache-hit
			       (cdr cache-hit) ;;; get font struct
			       (let* ((dpy (getprop graph 'dpy))
				      (fs (x-load-query-font dpy name))
				      )
				 (if (eq? fs 'void) ;;; doesn't exist?
				     (sgl-error for-whom
					       "requested font not found: "
					       name))
				 (sgl-set-cache-value!
				  sgl-font-cache
				  (cons (cons name fs)
					(sgl-cache-value sgl-font-cache)))
				 fs)))))
		   (=> 'set-font! gcv (=> 'get-fid fs))
		   (make-env gcv
			     (cdr decls)
			     (append (list (sgl-make-binding 'font name)
					   (sgl-make-binding 'font-struct fs))
				     old-env))))]

	    [TEXT-JUSTIFICATION
	     (let ((style value))
	       (if (not (memq style '(center left right up down)))
		   (sgl-error for-whom
			     "sgl-make-env: unknown text-justification: "
			     style))
	       (make-env gcv
			 (cdr decls)
			 (cons (sgl-make-binding 'text-justification style)
			       old-env)))]

	    [TEXT-MODE
	     (let ((mode value))
	       (if (not (memq mode '(image normal)))
		   (sgl-error for-whom "sgl-make-env: unknown text-mode: "
			     mode))
	       (make-env gcv
			 (cdr decls)
			 (cons (sgl-make-binding 'text-mode mode)
			       old-env)))]

	    [else (sgl-error for-whom "sgl-make-env: unknown name: " name)
		  (escape (void))]
	    ))))
  (make-env #f decls old-env)
  )

;;----------------------------------------------------------------------------
;;
;; transforms and such.
;;

(define (sgl-inverse-viewing-transform-xy x y g origin)
  (sgl-make-point (+ (- (/ x (getprop g 'x-mpy))
			(sgl-point-x origin))
		     (getprop g 'x-min))
		  (+ (- (/ (- (getprop g 'vp-h) y)
			   (getprop g 'y-mpy))
			(sgl-point-y origin))
		     (getprop g 'y-min))
		  ))
;;
;; coalesce-points - attempts to reduce the data by coalescing
;; points.  For runs of points that do not vary in either x or y,
;; only the end-points of the run are kept.
;;
;; returns list of xx-x-point values ready for x-draw-lines.
;;
(define (sgl-coalesce-points pts graph origin)
  (if (null? pts)
      '()
      (let* ((x-min (getprop graph 'x-min))
	     (y-max (getprop graph 'y-max))
	     (x-mpy (getprop graph 'x-mpy))
	     (y-mpy (getprop graph 'y-mpy))
	     (x-add (- (sgl-point-x origin) x-min))
	     (y-add (- y-max (sgl-point-y origin))))
	(define coalesce-points
	  (lambda (pts prev-x prev-y prev-dx prev-dy)
	    (if (null? pts)
		(list (list prev-x prev-y))
		;;
		;; perform optimized viewing transforms
		;;
		(let* ((pt (car pts))
		       (x (sgl-view-x (car pt) x-add x-mpy))
		       (y (sgl-view-y (cadr pt) y-add y-mpy)))
		  (cond [(fx= x prev-x)
			 (if (fx= y prev-y)
			     (coalesce-points (cdr pts) x y prev-dx prev-dy)
			     (let ((dy (if (fx> y prev-y) '+ '-)))
			       (if (eq? dy prev-dy)
				   (coalesce-points (cdr pts) x y 0 dy)
				   (cons (list prev-x prev-y)
					 (coalesce-points (cdr pts) x y 0 dy)))))]
			[(fx= y prev-y)
			 (if (fx= x prev-x)
			     (coalesce-points (cdr pts) x y prev-dx prev-dy)
			     (let ((dx (if (fx> x prev-x) '+ '-)))
			       (if (eq? dx prev-dx)
				   (coalesce-points (cdr pts) x y dx 0)
				   (cons (list prev-x prev-y)
					 (coalesce-points (cdr pts) x y dx 0)))))]
			[else (cons (list prev-x prev-y)
				    (coalesce-points (cdr pts) x y 0 0))])))))
	(let* ((pt (car pts))
	       (x (sgl-view-x (car pt) x-add x-mpy))
	       (y (sgl-view-y (cadr pt) y-add y-mpy)))
	  (coalesce-points (cdr pts) x y 0 0)
	  ))))

(define (sgl-compute-bounding-box g-width g-height env data)
  ;;
  ;; "data" are two-element lists (i.e., the external rep for a point).
  ;;
  (define make-datum list)
  (define d.x car)
  (define d.y cadr)
  
  (if (null? data)
      #f;; can't compute
      (let* ([d (car data)]
	     [w (d.x d)]
	     [h (d.y d)]
	     [origin (sgl-get-binding-value 'origin env)]
	     [line-width (sgl-get-binding-value 'line-width env)]
	     [lw (if (= line-width 0) 1 line-width)]
	     [o.x (sgl-point-x origin)]
	     [o.y (sgl-point-y origin)]
	     [fudge lw] ;;; simplistic fudge factor
	     [lw-w (/ fudge g-width)] ;;; cvrt fudge to world
	     [lw-h (/ fudge g-height)] ;;; cvrt fudge to world
	     )
	(let f ([w1 w] [h1 h] [w2 w] [h2 h] [data (cdr data)])
	  (if (null? data)
	      (sgl-make-bounding-box (sgl-make-point (+ o.x w1 (- lw-w))
					     (+ o.y h1 (- lw-h)))
				 (sgl-make-point (+ o.x w2 lw-w)
					     (+ o.y h2 lw-h)))
	      (let* ((d (car data)))
		(f (min w1 (d.x d))
		   (min h1 (d.y d))
		   (max w2 (d.x d))
		   (max h2 (d.y d))
		   (cdr data))))))))



(define sgl-variable-font-names
  (x-list-fonts (xt-display *top-level-widget*) "*times-medium-r-*" 9999))

(if (null? sgl-variable-font-names)
    (set! sgl-variable-font-names
	  (x-list-fonts (xt-display *top-level-widget*) "*times-*-r-*" 9999)))

(if (null? sgl-variable-font-names)
    (set! sgl-variable-font-names
	  (x-list-fonts (xt-display *top-level-widget*) "*-r-*" 9999)))

(if (null? sgl-variable-font-names)
    (set! sgl-variable-font-names
	  (x-list-fonts (xt-display *top-level-widget*) "*" 9999)))

(define sgl-variable-font-structs
  (map (lambda (fn)
	 (x-load-query-font (xt-display *top-level-widget*) fn))
       sgl-variable-font-names))

;;----------------------------------------------------------------------------
;;
;; sgl-pick code
;;
(define (sgl-press-event-handler wgt client-data event)
  (let* ((g (car client-data))
	 (press-action (caddr client-data))
	 (x (=> 'get-x event))
	 (y (=> 'get-y event))
	 (pt (sgl-inverse-viewing-transform-xy x y g (sgl-make-point 0 0)))
	 (pics (sgl-point-containing-picture-ids g pt))
	 )
    (press-action event g pics (sgl-point-x pt) (sgl-point-y pt))
    ))


(define (sgl-release-event-handler wgt client-data event)
  (let* ((g (car client-data))
	 (release-action (caddr client-data))
	 (x (=> 'get-x event))
	 (y (=> 'get-y event))
	 (pt (sgl-inverse-viewing-transform-xy x y g (sgl-make-point 0 0)))
	 )
    (release-action event g (sgl-point-x pt) (sgl-point-y pt))
    ))


(define (sgl-pick g press-action release-action)
  (let ((wgt (getprop g 'widget))
	(pick-id (gensym)))
    (xt-add-event-handler wgt
			  '("ButtonPressMask")
			  #f
			  sgl-press-event-handler
			  (list g pick-id press-action))
    (xt-add-event-handler wgt
			  '("ButtonReleaseMask")
			  #f
			  sgl-release-event-handler
			  (list g pick-id release-action))
    (putprop pick-id 'graph g)
    (putprop pick-id 'press-action press-action)
    (putprop pick-id 'release-action release-action)
    pick-id))


(define (sgl-remove-pick pick-id)
  (let* ((g (getprop pick-id 'graph))
	 (wgt (getprop g 'widget))
	 (pa (getprop pick-id 'press-action))
	 (ra (getprop pick-id 'release-action))
	 )
    (xt-remove-event-handler wgt
			     '("ButtonPressMask")
			     #f
			     sgl-press-event-handler
			     (list g pick-id pa))
    (xt-remove-event-handler wgt
			     '("ButtonReleaseMask")
			     #f
			     sgl-release-event-handler
			     (list g pick-id ra))
    ))

;;
;; this is a naive algorithm - it uses bounding boxes to
;; approximate picture perimeters.
;;
(define (sgl-point-containing-picture-ids graph pt)
  (sgl-reduce1 (lambda (picture)
	     (let* ((pe (sgl-picture-pe picture))
		    (bb (sgl-eval-pe 'bounding-box
				     graph
				     pe
				     '()
				     (lambda (v) #f)
				     'sgl-point-containing-picture-ids))
		    (bb-min (sgl-bounding-box-min bb))
		    (bb-max (sgl-bounding-box-max bb))
		    )
	       (if (and (>= (sgl-point-x pt) (sgl-point-x bb-min))
			(>= (sgl-point-y pt) (sgl-point-y bb-min))
			(<= (sgl-point-x pt) (sgl-point-x bb-max))
			(<= (sgl-point-y pt) (sgl-point-y bb-max)))
		   (sgl-picture-id picture)
		   #f)))
	   (getprop graph 'pictures)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;  MODULE       : rband.ss - adds "rubber-band" input to SGL-D.
;;
;;
;;  NEW SYNTAX   : (sgl-rubber-band <g> <callback-procedure>)
;;
;;                 where: <g> must be a previously created SGL-D graph
;;                 identifier.
;;
;;                 <callback-procedure> must be a procedure of five
;;                 arguments, which will be called when the user
;;                 depresses and releases the left mouse button.  The
;;                 five arguments are:
;;
;;                     1: the graph id registered with sgl-rubber-band
;;                     2: the mouse-press world x coordinate
;;                     3: the mouse-press world y coordinate
;;                     4: the mouse-release world x coordinate
;;                     4: the mouse-release world y coordinate
;;
;;  EXAMPLE      : First, load the shell, sgl-d, and rband code.
;;
;;                    (load "../shell/shell.ss")
;;                    (load "sgl-d.ss")
;;                    (load "rband.ss")
;;
;;                 Next, create an SGL-D graph, saving the graph's
;;                 identifier in a global variable.
;;
;;                    (define g (create-graphics-window))
;;
;;                 Finally, add the rubber-band action to the graph.
;;
;;                    (define handler
;;                        (lambda (g p1x p1y p2x p2y)
;;                            (printf "point 1: ~s ~s " p1x p1y)
;;                            (printf "point 2: ~s ~s~%" p2x p2y)))
;;
;;                    (sgl-add-rubber-band g handler)
;;
;;                 Depress the left mouse button within the graph
;;                 and drag the arrow to another point.  Release
;;                 the button.
;;
;;                 To disable the rubber band, do the following
;;
;;                   (sgl-remove-rubber-band g handler)
;;
;;                 ... where handler is the same handler registered with
;;                 sgl-add-rubber-band.
;;                 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
(define (sgl-add-rubber-band g release-action)
  (let ((wgt (getprop g 'widget)))
    (xt-add-event-handler wgt
			  '("ButtonPressMask")
			  #f
			  sgl-press-rband-handler
			  (list g))
    (xt-add-event-handler wgt
			  '("ButtonReleaseMask")
			  #f
			  sgl-release-rband-handler
			  (list g release-action))
    (set! sgl-press-x -1)  ;;; indicates not handling a press
    (set! sgl-cur-x -1)  ;;; indicates not drawn rband yet
  ))


(define (sgl-remove-rubber-band g release-action)
  (let ((wgt (getprop g 'widget)))
    (xt-remove-event-handler wgt
			  '("ButtonPressMask")
			  #f
			  sgl-press-rband-handler
			  (list g))
    (xt-remove-event-handler wgt
			  '("ButtonReleaseMask")
			  #f
			  sgl-release-rband-handler
			  (list g release-action))
  ))

(define (sgl-press-rband-handler wgt client-data event)
  (let* ((g (car client-data))
	 (x (=> 'get-x event))
	 (y (=> 'get-y event))
	 (app (xt-widget-to-application-context wgt))
	 )
    (if (= sgl-press-x -1) ;;; not already handling a press
	(begin
	  (set! sgl-press-x x)
	  (set! sgl-press-y y)
	  (let* ((dpy (getprop g 'dpy))
		 (win (getprop g 'window))
		 (tgc (getprop g 'toplevel-gc))
		 (gc (x-create-g-c dpy win '() (=> 'new xx-x-g-c-values)))
		 )
	    (x-copy-g-c dpy
			tgc
			(=> 'new$ xx-x-g-c-values-mask -1) ;;; all values
			gc)
	    ;;
	    ;; the following is a total kludge and does not result in
	    ;; proper choice of color for rubber band for color screens.
	    ;;
	    (x-set-function dpy gc "GXxor")
	    (x-set-foreground
	     dpy
	     gc
	     (1- (expt 2 (x-default-depth-of-screen (xt-screen wgt)))))
	    (putprop g 'rband-gc gc)
	    )
	  (set! work-proc-id (xt-app-add-work-proc app sgl-work-proc (list g)))
	  ))))


(define (sgl-release-rband-handler wgt client-data event)
  (let* ((g (car client-data))
	 (wgt (getprop g 'widget))
	 (dpy (getprop g 'dpy))
	 (win (xt-window wgt))
	 (gc  (getprop g 'rband-gc))
	 (release-action (cadr client-data))
	 (pt0 (sgl-make-point 0 0))
	 (pt (sgl-inverse-viewing-transform-xy sgl-cur-x sgl-cur-y g pt0))
	 (ppt (sgl-inverse-viewing-transform-xy sgl-press-x sgl-press-y g pt0))
	 )
    (if (not (= sgl-cur-x -1)) ;;; if rband rdrawn
	(sgl-draw-rect dpy win gc sgl-press-x sgl-press-y sgl-cur-x sgl-cur-y))
    (if (not (= sgl-press-x -1))  ;;; if press being handled
	(begin 
	  (release-action g
			  (sgl-point-x ppt) (sgl-point-y ppt)
			  (sgl-point-x pt) (sgl-point-y pt))
	  (xt-remove-work-proc work-proc-id)
	  (set! sgl-press-x -1)
	  (set! sgl-cur-x -1)
	  (putprop g 'rband-gc #f)
	  (x-free-g-c dpy gc)
	  ))
    ))


(define (sgl-work-proc client-data)
  (let* ((g (car client-data))
	 (dpy (getprop g 'dpy))
	 (win (getprop g 'window))
	 (wgt (getprop g 'widget))
	 (wgt-dim (xt-get-values wgt '("XmNwidth" "XmNheight")))
	 (gc  (getprop g 'rband-gc))
	 (rtn (x-query-pointer dpy (xt-window wgt)))
	 (win-x (cadddr (cddr rtn)))
	 (win-y (cadddr (cdddr rtn)))
	 (wgt-w (car wgt-dim))
	 (wgt-h (cadr wgt-dim))
	 (win-x (cond [(< win-x 0) 0]
		      [(> win-x wgt-w) wgt-w]
		      [else win-x]))
	 (win-y (cond [(< win-y 0) 0]
		      [(> win-y wgt-h) wgt-h]
		      [else win-y]))
	 )
    (if (not (and (eqv? win-x sgl-cur-x)
		  (eqv? win-y sgl-cur-y)))
	(begin
	  (if (not (= sgl-cur-x -1))
	      (sgl-draw-rect dpy win gc
			     sgl-press-x sgl-press-y
			     sgl-cur-x sgl-cur-y))
	  (set! sgl-cur-x win-x)
	  (set! sgl-cur-y win-y)
	  (sgl-draw-rect dpy win gc
			 sgl-press-x sgl-press-y
			 win-x win-y))))
  #f)


(define (sgl-draw-rect dpy win gc x1 y1 x2 y2)
  (let* ((min-x (min x1 x2))
	 (min-y (min y1 y2))
	 (max-x (max x1 x2))
	 (max-y (max y1 y2))
	 (width (- max-x min-x))
	 (height (- max-y min-y)))
    (if (and (positive? width)
	     (positive? height))
	(x-draw-rectangle dpy win gc min-x min-y (1- width) (1- height))))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;  MODULE       : popup.ss - adds popup menu input to SGL-D.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(define (sgl-create-popup-menu g handler strs pics)
  (let ((wgt (getprop g 'widget))
	(pb-list '())
	)
    (define popup-menu
      (xm-create-popup-menu wgt "popup-menu" '()))

    (define (unmap-callback widget cd ca)
      (xt-destroy-widget (xt-parent widget))
      )

    (xt-add-callback popup-menu "XmNunmapCallback" unmap-callback #f)

    (for-each
     (lambda (str)
       (let ((pb 
	      (xm-create-push-button
	       popup-menu
	       ""
	       (list `("XmNlabelString" ,str)
		     `("XmNbackground"
		       ,(x-white-pixel-of-screen (xt-screen wgt)))
		     ))))
	 (xt-add-callback pb
			  "XmNactivateCallback"
			  (lambda (w cli cal)
			    (handler g str pics)
			    (for-each xx-remove-all-scheme-callbacks
				      pb-list))
			  #f)
	 (xt-manage-child pb)
	 (set! pb-list (cons pb pb-list))
	 ))
     strs)
    popup-menu))

;;
;; pics must be a list of pics for which the menu is to be enabled,
;; or the symbol all, for all pics in the graph.
;;

(define (sgl-popup g popup-handler strs pics)
  (sgl-pick g
	    (lambda (event g picked-pics px py)
	      (if (and (pair? picked-pics)
		       (= (=> 'get-button event) 3)
		       (or (eq? pics 'all)
			   (ormap (lambda (picked-pic)
				    (memq picked-pic pics))
				  picked-pics)))
		  (let ((pm (sgl-create-popup-menu g
						   popup-handler
						   strs
						   picked-pics)))
		    (xm-menu-position pm event)
		    (xt-manage-child pm)))
	      #f)
	    (lambda (event g px py)
	      #f)
	    ))

;;
;; must first call create-popup-menu once, to define menu.
;;
;; Then, within the sgl-pick press handler,
;; must call (xm-menu-position <popup-menu> <event>)
;; and (xt-manage-child <popup-menu>)
;;
;; control is passed to callback handler, with selected string
;; as the client data.
;;


;;
;; test case
;;


;;(define g (create-graphics-window))
;;
;;(define p1 (draw! g '(rectangle (.1 .1) (.3 .3))))
;;(define p2 (draw! g '(rectangle (.2 .2) (.4 .4))))
;;
;;
;;(define (popup-handler g str pics)
;;  (printf "the string is: ~s - " str)
;;  (printf "the pic(s) are: ~s~%" pics)
;;  )
;;
;;
;;(sgl-pick g
;;	  (lambda (event g pics px py)
;;	    (if (and (= (=> 'get-button event) 3)
;;		     (pair? pics))
;;		(let ((pm (sgl-create-popup-menu g
;;						 popup-handler
;;						 (list "string1"
;;						       "this is two")
;;						 pics)))
;;		  (xm-menu-position pm event)
;;		  (xt-manage-child pm)))
;;	    #f)
;;	  (lambda (event g px py)
;;	    #f))
