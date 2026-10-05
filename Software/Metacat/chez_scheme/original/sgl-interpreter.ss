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

;; Metacat's graphics were originally implemented using a proprietary
;; windowing and graphics system for Scheme called SchemeXM/SGL, developed by
;; John B. Zuckerman at Motorola.  SGL was a symbolic graphics language built
;; on top of SchemeXM, which was in turn built on top of Chez Scheme and X.
;;
;; In order to port Metacat to SWL without having to completely rewrite all of
;; the graphics code, I implemented an SGL interpreter in SWL.  This file
;; contains the bulk of the interpreter.  Extra language features or minor
;; variations on SGL's features were introduced when needed, so the language
;; implemented here is not identical to SGL, although it is very similar.
;;
;; An informal summary of the implemented language forms is given below.
;;
;; <sgl-expression> =
;;   (rectangle (x1 y1) (x2 y2))
;;   (filled-rectangle (x1 y1) (x2 y2))
;;   (arc (xcenter ycenter) (xdiam ydiam) startdegs sweepdegs)
;;   (filled-arc (xcenter ycenter) (xdiam ydiam) startdegs sweepdegs)
;;   (line (x1 y1) (x2 y2) ...)
;;   (polyline (x1 y1) (x2 y2) (x3 y3) ...)
;;   (polypoints (x1 y1) (x2 y2) (x3 y3) ...)
;;   (dashed-polypoints (x1 y1) (x2 y2) (x3 y3) ...)
;;   (text "string")
;;   (text (x y) "string")
;;   (text (text-relative (+x +y)) "string")
;;   (let-sgl ()
;;     <sgl-expression>
;;     ...)
;;   (let-sgl ((origin (x y))
;;             (line-width num)
;;             (foreground-color "red")
;;             (background-color "blue")
;;             (line-style {dashed | dotted | solid})
;;             (font f)
;;             (text-justification {center | left | right})
;;             (text-mode {image | normal})
;;     <sgl-expression>
;;     ...)
;;   (ring (x y) outerdiam innerdiam)
;;   (ring (x y) outerdiam innerdiam startdegs sweepdegs)
;;   (polygon (x1 y1) (x2 y2) ...)          ;; all points distinct
;;   (filled-polygon (x1 y1) (x2 y2) ...)   ;; all points distinct
;;   (erase <color> <sgl-expression>)
;;   (clear)
;;   (clear <color>)
;;   (rule <rule-type> <clauses> <sgl-expression>)


;;------------------------------------------------------------------------------
;; The following workaround is necessary because swl0.9u comes bundled with
;; Tcl/Tk version 8.0, which doesn't support the -dash or -state options at all.

(define tcl-eval
  (if *tcl/tk-version-8_3?*
    swl:tcl-eval
    (lambda args
      (apply swl:tcl-eval (remove-unsupported-tcl-args args)))))

(define remove-unsupported-tcl-args
  (lambda (args)
    (cond
      ((null? args) args)
      ((eq? (car args) '-dash) (cddr args))
      ((eq? (car args) '-state) (cddr args))
      (else (cons (car args) (remove-unsupported-tcl-args (cdr args)))))))

;;------------------------------------------------------------------------------
;; SGL interpreter

;; these two functions work around a bug in SWL 0.9u
(define my-screen->canvas-x
  (lambda (win x)
    (flonum->fixnum (string->number (swl:tcl-eval win 'canvasx x)))))

(define my-screen->canvas-y
  (lambda (win y)
    (flonum->fixnum (string->number (swl:tcl-eval win 'canvasy y)))))

(define nop-event-handler
  (lambda ignore (void)))

(define default-press-handler
  (lambda (win x y)
    (printf "mouse pressed at (~a ~a)~%" (exact->inexact x) (exact->inexact y))))

(define-class (<viewport> parent px py xp yp) (<canvas> parent)
  (ivars (resize-handler nop-event-handler)
	 (left-press-handler nop-event-handler)
	 (right-press-handler nop-event-handler)
	 (pixel->x px)
	 (pixel->y py)
	 (x->pixel xp)
	 (y->pixel yp))
  (inherited)
  (inheritable)
  (private)
  (protected)
  (public
    (set-resize-handler! (resize)
      (set! resize-handler resize))
    (configure (w h)
      (resize-handler self w h))
    (set-mouse-handlers! (left-press right-press)
      (if* (exists? left-press) (set! left-press-handler left-press))
      (if* (exists? right-press) (set! right-press-handler right-press)))
    (mouse-press (i j mods)
      (event-case ((modifier= mods))
	(([right-button] [shift left-button])
	 (let ((x (pixel->x (+ i (my-screen->canvas-x self 1))))
	       (y (pixel->y (+ j (my-screen->canvas-y self 1)))))
	   (right-press-handler self x y)))
	(([left-button])
	 (let ((x (pixel->x (+ i (my-screen->canvas-x self 1))))
	       (y (pixel->y (+ j (my-screen->canvas-y self 1)))))
	   (left-press-handler self x y)))
	(else (send-base self mouse-press i j mods))))
    (draw-open-rectangle (fg lw ls ox oy x1 y1 x2 y2 tag)
      (unless (or (= x1 x2) (= y1 y2))
	(tcl-eval self 'create 'rectangle
	  (x->pixel x1 ox) (y->pixel y1 oy) (x->pixel x2 ox) (y->pixel y2 oy)
	  '-outline fg '-width lw '-dash ls '-tags tag)))
    (draw-filled-rectangle (fg ox oy x1 y1 x2 y2 tag)
      (unless (or (= x1 x2) (= y1 y2))
	(tcl-eval self 'create 'rectangle
	  (x->pixel x1 ox) (y->pixel y1 oy) (x->pixel x2 ox) (y->pixel y2 oy)
	  '-outline fg '-fill fg '-tags tag)))
    ;; This is used only by the theme-panel 'redraw-panel method:
    (draw-hidden-filled-rectangle (fg x1 y1 x2 y2 tag)
      (tcl-eval self 'create 'rectangle
	(x->pixel x1 0) (y->pixel y1 0) (x->pixel x2 0) (y->pixel y2 0)
	'-outline fg '-fill fg '-tags tag '-state 'hidden))
    (draw-line-segments (fg lw ls ox oy points tag)
      (let loop ((points points))
	(unless (null? points)
	  (let ((x1 (caar points))
		(y1 (cadar points))
		(x2 (caadr points))
		(y2 (cadadr points)))
	    (tcl-eval self 'create 'line
	      (x->pixel x1 ox) (y->pixel y1 oy) (x->pixel x2 ox) (y->pixel y2 oy)
	      '-fill fg '-width lw '-dash ls '-tags tag)
	    (loop (cddr points))))))
    (draw-polyline (fg lw ls ox oy points tag)
      (apply tcl-eval
	`(,self create line ,@(generate-polyline-coords points ox oy x->pixel y->pixel)
	   -fill ,fg -width ,lw -dash ,ls -tags ,tag)))
    (draw-open-oval (fg lw ls ox oy x1 y1 x2 y2 tag)
      (unless (or (= x1 x2) (= y1 y2))
	(tcl-eval self 'create 'oval
	  (x->pixel x1 ox) (y->pixel y1 oy) (x->pixel x2 ox) (y->pixel y2 oy)
	  '-outline fg '-width lw '-dash ls '-tags tag)))
    (draw-filled-oval (fg ox oy x1 y1 x2 y2 tag)
      (unless (or (= x1 x2) (= y1 y2))
	(tcl-eval self 'create 'oval
	  (x->pixel x1 ox) (y->pixel y1 oy) (x->pixel x2 ox) (y->pixel y2 oy)
	  '-outline fg '-fill fg '-tags tag)))
    (draw-open-arc (fg lw ls ox oy x1 y1 x2 y2 start sweep tag)
      (unless (or (= x1 x2) (= y1 y2))
	(tcl-eval self 'create 'arc
	  (x->pixel x1 ox) (y->pixel y1 oy) (x->pixel x2 ox) (y->pixel y2 oy)
	  '-style 'arc '-outline fg '-width lw '-dash ls
	  '-start start '-extent sweep '-tags tag)))
    (draw-filled-arc (fg ox oy x1 y1 x2 y2 start sweep tag)
      (unless (or (= x1 x2) (= y1 y2))
	(tcl-eval self 'create 'arc
	  (x->pixel x1 ox) (y->pixel y1 oy) (x->pixel x2 ox) (y->pixel y2 oy)
	  '-style 'pieslice '-outline fg '-fill fg '-start start '-extent sweep '-tags tag)))
    (draw-ring (fg bg ox oy x1a y1a x2a y2a x1b y1b x2b y2b tag)
      (unless (or (= x1a x2a) (= y1a y2a))
	(tcl-eval self 'create 'oval
	  (x->pixel x1a ox) (y->pixel y1a oy) (x->pixel x2a ox) (y->pixel y2a oy)
	  '-outline fg '-fill fg '-tags tag))
      (unless (or (= x1b x2b) (= y1b y2b))
	(tcl-eval self 'create 'oval
	  (x->pixel x1b ox) (y->pixel y1b oy) (x->pixel x2b ox) (y->pixel y2b oy)
	  '-outline bg '-fill bg '-tags tag)))
    (draw-arc-ring (fg bg ox oy x1a y1a x2a y2a x1b y1b x2b y2b start sweep tag)
      (unless (or (= x1a x2a) (= y1a y2a))
	(tcl-eval self 'create 'arc
	  (x->pixel x1a ox) (y->pixel y1a oy) (x->pixel x2a ox) (y->pixel y2a oy)
	  '-style 'pieslice '-outline bg '-fill fg '-start start '-extent sweep '-tags tag))
      (unless (or (= x1b x2b) (= y1b y2b))
	(tcl-eval self 'create 'arc
	  (x->pixel x1b ox) (y->pixel y1b oy) (x->pixel x2b ox) (y->pixel y2b oy)
	  '-style 'pieslice '-outline bg '-fill bg '-start start '-extent sweep '-tags tag)))
    (draw-open-polygon (fg bg lw ls ox oy points tag)
      (apply tcl-eval
	`(,self create polygon ,@(generate-polyline-coords points ox oy x->pixel y->pixel)
	   -outline ,fg -fill ,bg -width ,lw -dash ,ls -tags ,tag)))
    (draw-filled-polygon (fg ox oy points tag)
      (apply tcl-eval
	`(,self create polygon ,@(generate-polyline-coords points ox oy x->pixel y->pixel)
	   -outline ,fg -fill ,fg -tags ,tag)))
    (draw-polypoints (fg ox oy points dashed? tag)
      (let loop ((points points))
	(unless (null? points)
	  (let ((x (x->pixel (caar points) ox))
		(y (y->pixel (cadar points) oy)))
	    (if dashed?
	      (tcl-eval self 'create 'line x y (+ x 4) y '-fill fg '-tags tag)
	      (tcl-eval self 'create 'line x y (+ x 1) y '-fill fg '-tags tag))
	    (loop (cdr points))))))
    (draw-text (fg bg text ox oy relx rely justify font mode tag)
      (let* ((size (tell font 'get-pixel-size text))
	     (width (car size))
	     (height (cadr size))
	     (baseline (caddr size))
	     (M-width (car (tell font 'get-pixel-size "M")))
	     (text-relative-x-offset (* relx M-width))
	     (text-relative-y-offset (* -1 rely (- height baseline)))
	     (justification-offset
	       (case justify
		 ((center) 0)
		 ((left) (* 1/2 width))
		 ((right) (* -1/2 width))))
	     (center (+ (x->pixel 0 ox) text-relative-x-offset justification-offset))
	     (left (- center (* 1/2 width)))
	     (right (+ center (* 1/2 width)))
	     (lower (+ (y->pixel 0 oy) baseline text-relative-y-offset))
	     (upper (- lower height)))
	(when (eq? mode 'image)
	  (tcl-eval self 'create 'rectangle
	    (+ left 1) (+ upper 1) (- right 1) (- lower 1)
	    '-outline bg '-fill bg '-tags tag))
	(tcl-eval self 'create 'text center lower
	  '-text text '-anchor 's '-font (tell font 'get-swl-font) '-fill fg '-tags tag)))
    (move (dx dy tag)
      (let ((xshift (- (x->pixel dx 0) (x->pixel 0 0)))
	    (yshift (- (y->pixel dy 0) (y->pixel 0 0))))
	(tcl-eval self 'move tag xshift yshift)))
    (move-pixels (dx dy tag)
      (tcl-eval self 'move tag dx dy))
    (raise (tag)
      (tcl-eval self 'raise tag 'all))
    (unhide (tag)
      (tcl-eval self 'itemconfigure tag '-state 'normal))
    (retag (old new)
      (tcl-eval self 'itemconfigure old '-tags new))
    (rescale (tag xfactor yfactor)
      (tcl-eval self 'scale tag 0 0 xfactor yfactor))
    (delete (tag)
      (tcl-eval self 'delete tag))))

(define generate-polyline-coords
  (lambda (points ox oy x->pixel y->pixel)
    (if (null? points)
      '()
      (cons (x->pixel (caar points) ox)
	(cons (y->pixel (cadar points) oy)
	  (generate-polyline-coords (cdr points) ox oy x->pixel y->pixel))))))

;;-------------------------------------------------------------------------------

(define draw!
  (lambda (vp pexp . tag)
    (let ((bg (send vp get-background-color)))
      (draw-exp vp pexp (extend init-env 'background-color bg) 0 0 bg
	(if (null? tag) 'all (car tag))))))

(define erase!
  (lambda (vp pexp)
    (draw! vp `(erase ,(send vp get-background-color) ,pexp))))

(define draw-exps
  (lambda (vp pexps env ox oy erase-color tag)
    (unless (null? pexps)
      (draw-exp vp (car pexps) env ox oy erase-color tag)
      (draw-exps vp (cdr pexps) env ox oy erase-color tag))))

(define draw-exp
  (lambda (vp pexp env ox oy erase-color tag)
    (if (not (null? pexp))
	(record-case pexp
	  (let-sgl (bindings . pexps)
	    (let* ((origin-binding (assq 'origin bindings))
		   (ox (if origin-binding (+ ox (caadr origin-binding)) ox))
		   (oy (if origin-binding (+ oy (cadadr origin-binding)) oy)))
	      (draw-exps vp pexps (extend* env bindings) ox oy erase-color tag)))
	  (rectangle (p1 p2)
	    (let ((x1 (car p1))
		  (y1 (cadr p1))
		  (x2 (car p2))
		  (y2 (cadr p2))
		  (fg (if (eq? tag 'eraser) erase-color (lookup env 'foreground-color)))
		  (lw (lookup env 'line-width))
		  (ls (lookup env 'line-style)))
	      (send vp draw-open-rectangle fg lw ls ox oy x1 y1 x2 y2 tag)))
	  (filled-rectangle (p1 p2)
	    (let ((x1 (car p1))
		  (y1 (cadr p1))
		  (x2 (car p2))
		  (y2 (cadr p2))
		  (fg (if (eq? tag 'eraser) erase-color (lookup env 'foreground-color))))
	      (send vp draw-filled-rectangle fg ox oy x1 y1 x2 y2 tag)))
	  (line points
	    (let ((fg (if (eq? tag 'eraser) erase-color (lookup env 'foreground-color)))
		  (lw (lookup env 'line-width))
		  (ls (lookup env 'line-style)))
	      (send vp draw-line-segments fg lw ls ox oy points tag)))
	  (polyline points
	    (let ((fg (if (eq? tag 'eraser) erase-color (lookup env 'foreground-color)))
		  (lw (lookup env 'line-width))
		  (ls (lookup env 'line-style)))
	      (send vp draw-polyline fg lw ls ox oy points tag)))
	  (polygon points
	    (let ((fg (if (eq? tag 'eraser) erase-color (lookup env 'foreground-color)))
		  (bg (if (eq? tag 'eraser) erase-color (lookup env 'background-color)))
		  (lw (lookup env 'line-width))
		  (ls (lookup env 'line-style)))
	      (send vp draw-open-polygon fg bg lw ls ox oy points tag)))
	  (filled-polygon points
	    (let ((fg (if (eq? tag 'eraser) erase-color (lookup env 'foreground-color))))
	      (send vp draw-filled-polygon fg ox oy points tag)))
	  (arc (center size start sweep)
	    (let* ((width (car size))
		   (height (cadr size))
		   (fg (if (eq? tag 'eraser) erase-color (lookup env 'foreground-color)))
		   (lw (lookup env 'line-width))
		   (ls (lookup env 'line-style))
		   (x1 (- (car center) (/ width 2)))
		   (y1 (+ (cadr center) (/ height 2)))
		   (x2 (+ (car center) (/ width 2)))
		   (y2 (- (cadr center) (/ height 2))))
	      (if (>= sweep 360)
		(send vp draw-open-oval fg lw ls ox oy x1 y1 x2 y2 tag)
		(send vp draw-open-arc fg lw ls ox oy x1 y1 x2 y2 start sweep tag))))
	  (filled-arc (center size start sweep)
	    (let* ((width (car size))
		   (height (cadr size))
		   (fg (if (eq? tag 'eraser) erase-color (lookup env 'foreground-color)))
		   (x1 (- (car center) (/ width 2)))
		   (y1 (+ (cadr center) (/ height 2)))
		   (x2 (+ (car center) (/ width 2)))
		   (y2 (- (cadr center) (/ height 2))))
	      (if (>= sweep 360)
		(send vp draw-filled-oval fg ox oy x1 y1 x2 y2 tag)
		(send vp draw-filled-arc fg ox oy x1 y1 x2 y2 start sweep tag))))
	  (ring (center outer-diam inner-diam . args)
	    (let ((fg (if (eq? tag 'eraser) erase-color (lookup env 'foreground-color)))
		  (bg (if (eq? tag 'eraser) erase-color (lookup env 'background-color)))
		  (x1-outer (- (car center) (/ outer-diam 2)))
		  (y1-outer (+ (cadr center) (/ outer-diam 2)))
		  (x2-outer (+ (car center) (/ outer-diam 2)))
		  (y2-outer (- (cadr center) (/ outer-diam 2)))
		  (x1-inner (- (car center) (/ inner-diam 2)))
		  (y1-inner (+ (cadr center) (/ inner-diam 2)))
		  (x2-inner (+ (car center) (/ inner-diam 2)))
		  (y2-inner (- (cadr center) (/ inner-diam 2))))
	      (if (null? args)
		(send vp draw-ring fg bg ox oy x1-outer y1-outer x2-outer y2-outer
		  x1-inner y1-inner x2-inner y2-inner tag)
		(send vp draw-arc-ring fg bg ox oy x1-outer y1-outer x2-outer y2-outer
		  x1-inner y1-inner x2-inner y2-inner (car args) (cadr args) tag))))
	  (polypoints points
	    (let ((fg (if (eq? tag 'eraser) erase-color (lookup env 'foreground-color))))
	      (send vp draw-polypoints fg ox oy points #f tag)))
	  (dashed-polypoints points
	    (let ((fg (if (eq? tag 'eraser) erase-color (lookup env 'foreground-color))))
	      (send vp draw-polypoints fg ox oy points #t tag)))
	  (text args
	    (let ((fg (if (eq? tag 'eraser) erase-color (lookup env 'foreground-color)))
		  (bg (if (eq? tag 'eraser) erase-color (lookup env 'background-color)))
		  (justify (lookup env 'text-justification))
		  (mode (lookup env 'text-mode))
		  (font (lookup env 'font)))
	      (cond
		((string? (car args))
		 (send vp draw-text fg bg (car args) ox oy 0 0 justify font mode tag))
		((eq? (caar args) 'text-relative)
		 (let* ((relative-offsets (cadar args))
			(relx (car relative-offsets))
			(rely (cadr relative-offsets)))
		   (send vp draw-text fg bg (cadr args) ox oy relx rely justify font mode tag)))
		(else
		  (let ((ox (+ (caar args) ox))
			(oy (+ (cadar args) oy)))
		    (send vp draw-text fg bg (cadr args) ox oy 0 0 justify font mode tag))))))
	  (erase (c pexp)
	    (draw-exp vp pexp env ox oy (if (string? c) (swl-color c) c) 'eraser))
	  (clear color
	    (send vp delete 'all)
	    (if* (not (null? color))
	      (send vp set-background-color! (car color))))
	  ;; The following is a total hack.  When the workspace window is resized, we
	  ;; need to recompute all rule pexps to reflect the new rule widths.  Many
	  ;; of these pexps are embedded within larger pexps for answer and snag
	  ;; descriptions.  Rule tags make it possible to replace the rule
	  ;; subexpressions within these larger pexps.  The rule type and clause
	  ;; information is needed to compute the new pexps.
	  (rule (rule-type clauses pexp)
	    (draw-exp vp pexp env ox oy erase-color tag))
	  (else (error 'draw-exp "invalid picture expression:~n~a" pexp))))
    'ok))

(define graphics-dash-pattern
  (lambda ()
    (if (and (eq? *platform* 'windows)
	     *tcl/tk-version-8_3?*
	     %nice-graphics%)
      ". "
      "- ")))

(define lookup
  (lambda (env symbol)
    (let ((value (env symbol)))
      (case symbol
	((line-style)
	 (case value
	   ((dotted) ". ")
	   ((dashed) (graphics-dash-pattern))
	   (else "")))
	((foreground-color background-color erase-color)
	 (if (string? value)
	   (swl-color value)
	   value))
	(else value)))))

(define extend
  (lambda (env sym val)
    (if (eq? sym 'origin)
      env
      (lambda (symbol)
	(if (eq? symbol sym)
	  val
	  (env symbol))))))

(define extend*
  (lambda (env bindings)
    (if (null? bindings)
      env
      (extend*
	(extend env (caar bindings) (cadar bindings))
	(cdr bindings)))))

(define empty-env
  (lambda (symbol) #f))

(define init-env
  (extend* empty-env
    `((foreground-color ,=black=)
      (font ,(swl-font sans-serif 10))
      (text-justification left)
      (text-mode normal)
      (line-width 1)
      (line-style solid))))

(define *flush-event-queue* swl:sync-display)
