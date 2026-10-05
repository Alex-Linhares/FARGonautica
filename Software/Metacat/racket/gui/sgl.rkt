#lang racket/base
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
;;=============================================================================
;; Ported to Racket, 2026: the SGL interpreter on racket/draw (no racket/gui).
;;
;; The interpreter proper (draw!, erase!, draw-exps, draw-exp, lookup,
;; extend, extend*, empty-env, init-env, graphics-dash-pattern,
;; generate-polyline-coords) is the original's, line for line; racket/class's
;; `send' has SWL's syntax.  SWL's <viewport> (a Tk canvas) becomes
;; viewport% below: it keeps the original's draw-... methods, which made Tk
;; canvas items, and records the same items (kind, coordinates, options,
;; tags) in a display list that `render' paints on any racket/draw dc<%>,
;; emulating Tk: items in creation order, tags for move/raise/delete/...,
;; Tk's dash patterns, text anchored at its bottom centre.  The Tcl/Tk
;; workarounds (tcl-eval, remove-unsupported-tcl-args, my-screen->canvas-x/y)
;; have no counterpart; mouse coordinates come with the scroll offset added.
;; See docs/porting-notes.md, item 12.
;;=============================================================================

(require racket/class
         (only-in racket/math pi)
         (only-in racket/draw make-pen make-brush color% dc-path%)
         "../compat.rkt"
         "../utilities.rkt"
         "colors.rkt"
         "fonts.rkt")

(provide viewport% render-items
         item? item-kind item-coords item-tags item-options item-state
         draw! erase! draw-exps draw-exp lookup extend extend* empty-env init-env
         graphics-dash-pattern generate-polyline-coords tk-dash-pattern
         nop-event-handler default-press-handler *flush-event-queue*
         set-flush-event-queue!
         *platform* *tcl/tk-version-8_3?* %nice-graphics%)

;; port: metacat.ss and setup.ss define these for the whole program; the
;; interpreter only reads them in graphics-dash-pattern
(define *platform* 'linux)          ; as engine/general-graphics.rktl
(define *tcl/tk-version-8_3?* #t)
(define %nice-graphics% #t)

;;------------------------------------------------------------------------------
;; SGL interpreter

(define nop-event-handler
  (lambda ignore (void)))

(define default-press-handler
  (lambda (win x y)
    (printf "mouse pressed at (~a ~a)~%" (exact->inexact x) (exact->inexact y))))

;; port: a Tk canvas item
(struct item ([tags #:mutable] kind [coords #:mutable] options [state #:mutable]))

(define item-option
  (lambda (it key)
    (let ((p (assq key (item-options it)))) (and p (cdr p)))))

(define tag->tags
  (lambda (tag) (if (list? tag) tag (list tag))))

(define viewport%
  (class object%
    (init-field pixel->x pixel->y x->pixel y->pixel
                [width 100] [height 100] [background-color =white=])
    (super-new)
    (define resize-handler nop-event-handler)
    (define left-press-handler nop-event-handler)
    (define right-press-handler nop-event-handler)
    ;; port: the display list, newest item first (get-items gives it oldest
    ;; first), the scroll position (the canvas coordinates of the visible
    ;; window's top left corner) and the scroll region (the canvas size)
    (define items '())
    (define scroll-region (list 0 0 width height))
    (define scroll-x 0)
    (define scroll-y 0)
    (define changed-callback void)

    (define/private (add-item! tag kind coords . options)
      (set! items (cons (item (tag->tags tag) kind coords options 'normal) items))
      (changed-callback))

    (define/private (matches? it tag)
      (or (eq? tag 'all) (and (member tag (item-tags it)) #t)))

    (define/private (for-tag tag f)
      (for-each (lambda (it) (when (matches? it tag) (f it))) items)
      (changed-callback))

    ;; port: SWL canvas methods the graphics code uses
    (define/public (get-background-color) background-color)
    (define/public (set-background-color! c)
      (set! background-color (if (string? c) (swl-color c) c))
      (changed-callback))
    (define/public (get-width) width)
    (define/public (get-height) height)
    (define/public (set-size! w h) (set! width w) (set! height h) (changed-callback))
    (define/public (set-scroll-position! x y) (set! scroll-x x) (set! scroll-y y))
    (define/public (get-scroll-position) (list scroll-x scroll-y))
    (define/public (set-scroll-region! x1 y1 x2 y2)
      (set! scroll-region (list x1 y1 x2 y2)))
    (define/public (get-scroll-region) scroll-region)
    (define/public (get-items) (reverse items))
    (define/public (set-changed-callback! f) (set! changed-callback f))
    (define/public (render dc) (render-items dc (reverse items) background-color width height
                                             scroll-x scroll-y))

    (define/public (set-resize-handler! resize)
      (set! resize-handler resize))
    (define/public (configure w h)
      (resize-handler this w h))
    (define/public (set-mouse-handlers! left-press right-press)
      (if* (exists? left-press) (set! left-press-handler left-press))
      (if* (exists? right-press) (set! right-press-handler right-press)))
    ;; port: button is 'left, 'right or 'shift-left; i j are window pixels
    (define/public (mouse-press i j button)
      (let ((x (pixel->x (+ i scroll-x)))
            (y (pixel->y (+ j scroll-y))))
        (case button
          ((right shift-left) (right-press-handler this x y))
          ((left) (left-press-handler this x y))
          (else (void)))))
    (define/public (draw-open-rectangle fg lw ls ox oy x1 y1 x2 y2 tag)
      (unless (or (= x1 x2) (= y1 y2))
	(add-item! tag 'rectangle
	  (list (x->pixel x1 ox) (y->pixel y1 oy) (x->pixel x2 ox) (y->pixel y2 oy))
	  (cons 'outline fg) (cons 'width lw) (cons 'dash ls))))
    (define/public (draw-filled-rectangle fg ox oy x1 y1 x2 y2 tag)
      (unless (or (= x1 x2) (= y1 y2))
	(add-item! tag 'rectangle
	  (list (x->pixel x1 ox) (y->pixel y1 oy) (x->pixel x2 ox) (y->pixel y2 oy))
	  (cons 'outline fg) (cons 'fill fg))))
    ;; This is used only by the theme-panel 'redraw-panel method:
    (define/public (draw-hidden-filled-rectangle fg x1 y1 x2 y2 tag)
      (add-item! tag 'rectangle
	(list (x->pixel x1 0) (y->pixel y1 0) (x->pixel x2 0) (y->pixel y2 0))
	(cons 'outline fg) (cons 'fill fg))
      (set-item-state! (car items) 'hidden))
    (define/public (draw-line-segments fg lw ls ox oy points tag)
      (let loop ((points points))
	(unless (null? points)
	  (let ((x1 (caar points))
		(y1 (cadar points))
		(x2 (caadr points))
		(y2 (cadadr points)))
	    (add-item! tag 'line
	      (list (x->pixel x1 ox) (y->pixel y1 oy) (x->pixel x2 ox) (y->pixel y2 oy))
	      (cons 'fill fg) (cons 'width lw) (cons 'dash ls))
	    (loop (cddr points))))))
    (define/public (draw-polyline fg lw ls ox oy points tag)
      (add-item! tag 'line (generate-polyline-coords points ox oy x->pixel y->pixel)
	(cons 'fill fg) (cons 'width lw) (cons 'dash ls)))
    (define/public (draw-open-oval fg lw ls ox oy x1 y1 x2 y2 tag)
      (unless (or (= x1 x2) (= y1 y2))
	(add-item! tag 'oval
	  (list (x->pixel x1 ox) (y->pixel y1 oy) (x->pixel x2 ox) (y->pixel y2 oy))
	  (cons 'outline fg) (cons 'width lw) (cons 'dash ls))))
    (define/public (draw-filled-oval fg ox oy x1 y1 x2 y2 tag)
      (unless (or (= x1 x2) (= y1 y2))
	(add-item! tag 'oval
	  (list (x->pixel x1 ox) (y->pixel y1 oy) (x->pixel x2 ox) (y->pixel y2 oy))
	  (cons 'outline fg) (cons 'fill fg))))
    (define/public (draw-open-arc fg lw ls ox oy x1 y1 x2 y2 start sweep tag)
      (unless (or (= x1 x2) (= y1 y2))
	(add-item! tag 'arc
	  (list (x->pixel x1 ox) (y->pixel y1 oy) (x->pixel x2 ox) (y->pixel y2 oy))
	  (cons 'style 'arc) (cons 'outline fg) (cons 'width lw) (cons 'dash ls)
	  (cons 'start start) (cons 'extent sweep))))
    (define/public (draw-filled-arc fg ox oy x1 y1 x2 y2 start sweep tag)
      (unless (or (= x1 x2) (= y1 y2))
	(add-item! tag 'arc
	  (list (x->pixel x1 ox) (y->pixel y1 oy) (x->pixel x2 ox) (y->pixel y2 oy))
	  (cons 'style 'pieslice) (cons 'outline fg) (cons 'fill fg)
	  (cons 'start start) (cons 'extent sweep))))
    (define/public (draw-ring fg bg ox oy x1a y1a x2a y2a x1b y1b x2b y2b tag)
      (unless (or (= x1a x2a) (= y1a y2a))
	(add-item! tag 'oval
	  (list (x->pixel x1a ox) (y->pixel y1a oy) (x->pixel x2a ox) (y->pixel y2a oy))
	  (cons 'outline fg) (cons 'fill fg)))
      (unless (or (= x1b x2b) (= y1b y2b))
	(add-item! tag 'oval
	  (list (x->pixel x1b ox) (y->pixel y1b oy) (x->pixel x2b ox) (y->pixel y2b oy))
	  (cons 'outline bg) (cons 'fill bg))))
    (define/public (draw-arc-ring fg bg ox oy x1a y1a x2a y2a x1b y1b x2b y2b start sweep tag)
      (unless (or (= x1a x2a) (= y1a y2a))
	(add-item! tag 'arc
	  (list (x->pixel x1a ox) (y->pixel y1a oy) (x->pixel x2a ox) (y->pixel y2a oy))
	  (cons 'style 'pieslice) (cons 'outline bg) (cons 'fill fg)
	  (cons 'start start) (cons 'extent sweep)))
      (unless (or (= x1b x2b) (= y1b y2b))
	(add-item! tag 'arc
	  (list (x->pixel x1b ox) (y->pixel y1b oy) (x->pixel x2b ox) (y->pixel y2b oy))
	  (cons 'style 'pieslice) (cons 'outline bg) (cons 'fill bg)
	  (cons 'start start) (cons 'extent sweep))))
    (define/public (draw-open-polygon fg bg lw ls ox oy points tag)
      (add-item! tag 'polygon (generate-polyline-coords points ox oy x->pixel y->pixel)
	(cons 'outline fg) (cons 'fill bg) (cons 'width lw) (cons 'dash ls)))
    (define/public (draw-filled-polygon fg ox oy points tag)
      (add-item! tag 'polygon (generate-polyline-coords points ox oy x->pixel y->pixel)
	(cons 'outline fg) (cons 'fill fg)))
    (define/public (draw-polypoints fg ox oy points dashed? tag)
      (let loop ((points points))
	(unless (null? points)
	  (let ((x (x->pixel (caar points) ox))
		(y (y->pixel (cadar points) oy)))
	    (if dashed?
	      (add-item! tag 'line (list x y (+ x 4) y) (cons 'fill fg))
	      (add-item! tag 'line (list x y (+ x 1) y) (cons 'fill fg)))
	    (loop (cdr points))))))
    (define/public (draw-text fg bg text ox oy relx rely justify font mode tag)
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
	  (add-item! tag 'rectangle
	    (list (+ left 1) (+ upper 1) (- right 1) (- lower 1))
	    (cons 'outline bg) (cons 'fill bg)))
	(add-item! tag 'text (list center lower)
	  (cons 'text text) (cons 'anchor 's) (cons 'font (tell font 'get-swl-font))
	  (cons 'fill fg))))
    (define/public (move dx dy tag)
      (let ((xshift (- (x->pixel dx 0) (x->pixel 0 0)))
	    (yshift (- (y->pixel dy 0) (y->pixel 0 0))))
	(move-pixels xshift yshift tag)))
    (define/public (move-pixels dx dy tag)
      (for-tag tag
        (lambda (it)
          (set-item-coords! it
            (let loop ((cs (item-coords it)))
              (if (null? cs) '()
                  (cons (+ (car cs) dx) (cons (+ (cadr cs) dy) (loop (cddr cs))))))))))
    ;; Tk: raise the items with the tag above all others, keeping their order
    ;; (items is newest first, so the raised ones go in front)
    (define/public (raise tag)
      (let-values (((raised others)
                    (let loop ((l items) (r '()) (o '()))
                      (cond ((null? l) (values (reverse r) (reverse o)))
                            ((matches? (car l) tag) (loop (cdr l) (cons (car l) r) o))
                            (else (loop (cdr l) r (cons (car l) o)))))))
        (set! items (append raised others))
        (changed-callback)))
    (define/public (unhide tag)
      (for-tag tag (lambda (it) (set-item-state! it 'normal))))
    (define/public (retag old new)
      (for-tag old (lambda (it) (set-item-tags! it (tag->tags new)))))
    (define/public (rescale tag xfactor yfactor)
      (for-tag tag
        (lambda (it)
          (set-item-coords! it
            (let loop ((cs (item-coords it)))
              (if (null? cs) '()
                  (cons (* (car cs) xfactor)
                        (cons (* (cadr cs) yfactor) (loop (cddr cs))))))))))
    (define/public (delete tag)
      (set! items (filter (lambda (it) (not (matches? it tag))) items))
      (changed-callback))))

(define generate-polyline-coords
  (lambda (points ox oy x->pixel y->pixel)
    (if (null? points)
      '()
      (cons (x->pixel (caar points) ox)
	(cons (y->pixel (cadar points) oy)
	  (generate-polyline-coords (cdr points) ox oy x->pixel y->pixel))))))

;;-------------------------------------------------------------------------------
;; port: painting the display list, as Tk 8.5 on X11 would

;; Tk's dash strings (tkCanvUtil.c, DashConvert): each of _ - , . is a dash
;; of 8, 6, 4, 2 times the line width followed by a gap of 4 times the width;
;; a space lengthens the preceding gap by width+1.  "" is a solid line.
(define tk-dash-pattern
  (lambda (string lw)
    (let ((w (max 1 (inexact->exact (floor (+ lw 1/2))))))
      (let loop ((cs (string->list string)) (acc '()))
        (cond
          ((null? cs) (reverse acc))
          ((char=? (car cs) #\space)
           (loop (cdr cs) (if (null? acc) acc (cons (+ (car acc) w 1) (cdr acc)))))
          (else
            (let ((size (case (car cs) ((#\_) 8) ((#\-) 6) ((#\,) 4) ((#\.) 2) (else 0))))
              (loop (cdr cs) (cons (* 4 w) (cons (* size w) acc))))))))))

(define ->real (lambda (x) (exact->inexact x)))

(define pairs
  (lambda (coords)
    (if (null? coords) '()
        (cons (cons (->real (car coords)) (->real (cadr coords))) (pairs (cddr coords))))))

;; points on the arc of the ellipse in box (x1 y1 x2 y2), Tk angles
;; (degrees, counter-clockwise from 3 o'clock, y up)
(define arc-points
  (lambda (x1 y1 x2 y2 start extent)
    (let* ((cx (/ (+ x1 x2) 2.0)) (cy (/ (+ y1 y2) 2.0))
           (rx (/ (abs (- x2 x1)) 2.0)) (ry (/ (abs (- y2 y1)) 2.0))
           (n (max 8 (inexact->exact (ceiling (/ (* (abs extent) (max rx ry)) 60.0))))))
      (let loop ((i 0) (acc '()))
        (if (> i n)
            (reverse acc)
            (let ((a (* (/ pi 180) (+ start (* extent (/ i n))))))
              (loop (+ i 1) (cons (cons (+ cx (* rx (cos a))) (- cy (* ry (sin a)))) acc))))))))

;; split an open path into the runs of a Tk dash pattern
(define dash-runs
  (lambda (points pattern)
    (let loop ((ps points) (pat pattern) (left (car pattern)) (on? #t)
               (run (list (car points))) (runs '()))
      (if (null? (cdr ps))
          (reverse (if on? (cons (reverse run) runs) runs))
          (let* ((p (car ps)) (q (cadr ps))
                 (dx (- (car q) (car p))) (dy (- (cdr q) (cdr p)))
                 (len (sqrt (+ (* dx dx) (* dy dy)))))
            (if (<= len left)
                (loop (cdr ps) pat (- left len) on? (cons q run) runs)
                (let* ((t (/ left len))
                       (m (cons (+ (car p) (* t dx)) (+ (cdr p) (* t dy))))
                       (next (if (null? (cdr pat)) pattern (cdr pat))))
                  (loop (cons m (cdr ps)) next (car next) (not on?) (list m)
                        (if on? (cons (reverse (cons m run)) runs) runs)))))))))

;; X11 draws a butt-capped line of width 1 from p to q over the pixels from
;; p up to, not including, q (Tk's polypoints are lines one pixel long);
;; racket/draw's unsmoothed 1-pixel lines include q, so a thin run stops one
;; pixel step short of its last point (wider lines already end at q)
(define draw-run
  (lambda (dc thin? run)
    (let* ((run (if thin? (shorten-end run) run)))
      (cond
        ((null? run) (void))
        ((null? (cdr run))
         (send dc draw-line (caar run) (cdar run) (caar run) (cdar run)))
        (else (send dc draw-lines run))))))

(define shorten-end
  (lambda (run)
    (let loop ((rev (reverse run)) (remaining 1))
      (if (null? (cdr rev))
          '()                                   ;; under one pixel step long
          (let* ((q (car rev)) (p (cadr rev))
                 (dx (- (car q) (car p))) (dy (- (cdr q) (cdr p)))
                 (steps (max (abs dx) (abs dy))))
            (if (>= steps remaining)
                (let ((f (/ remaining steps)))
                  (reverse (cons (cons (- (car q) (* f dx)) (- (cdr q) (* f dy)))
                                 (cdr rev))))
                (loop (cdr rev) (- remaining steps))))))))

(define stroke
  (lambda (dc points color lw dash closed?)
    (let ((points (if closed? (append points (list (car points))) points))
          (pattern (if (and dash (not (equal? dash ""))) (tk-dash-pattern dash lw) '()))
          (thin? (<= lw 1)))
      (send dc set-pen (make-pen #:color color #:width (max 1 lw) #:cap 'butt
                                 #:join (if closed? 'miter 'round)))
      (for-each (lambda (run) (draw-run dc thin? run))
                (if (null? pattern) (list points) (dash-runs points pattern))))))

(define solid?
  (lambda (dash) (or (not dash) (equal? dash ""))))

;; fill with color; the 1-pixel pen paints the edge in outline (default: color)
(define fill-shape
  (lambda (dc color thunk [outline color])
    (send dc set-pen (make-pen #:color (or outline color) #:width 1))
    (send dc set-brush (make-brush #:color color))
    (thunk)
    (send dc set-brush (make-brush #:style 'transparent))))

(define render-items
  (lambda (dc items bg width height scroll-x scroll-y)
    (send dc set-smoothing 'unsmoothed)
    (send dc set-origin (- scroll-x) (- scroll-y))
    (send dc set-background bg)
    (send dc set-brush (make-brush #:color bg))
    (send dc set-pen (make-pen #:style 'transparent))
    (send dc draw-rectangle scroll-x scroll-y width height)
    (send dc set-brush (make-brush #:style 'transparent))
    (for-each (lambda (it) (unless (eq? (item-state it) 'hidden) (render-item dc it)))
              items)
    (send dc set-origin 0 0)))

(define render-item
  (lambda (dc it)
    (let* ((cs (map ->real (item-coords it)))
           (opt (lambda (k) (item-option it k)))
           (lw (or (opt 'width) 1))
           (dash (opt 'dash))
           (box (lambda ()
                  (let ((x1 (min (car cs) (caddr cs))) (x2 (max (car cs) (caddr cs)))
                        (y1 (min (cadr cs) (cadddr cs))) (y2 (max (cadr cs) (cadddr cs))))
                    (values x1 y1 x2 y2)))))
      (case (item-kind it)
        ((rectangle)
         (let-values (((x1 y1 x2 y2) (box)))
           (when (opt 'fill)
             (fill-shape dc (opt 'fill)
               (lambda () (send dc draw-rectangle x1 y1 (- x2 x1) (- y2 y1)))))
           (when (opt 'outline)
             (stroke dc (list (cons x1 y1) (cons x2 y1) (cons x2 y2) (cons x1 y2))
                     (opt 'outline) lw dash #t))))
        ((oval)
         (let-values (((x1 y1 x2 y2) (box)))
           (cond
             ((opt 'fill)
              (fill-shape dc (opt 'fill)
                (lambda () (send dc draw-ellipse x1 y1 (- x2 x1) (- y2 y1)))
                (opt 'outline)))
             ((solid? dash)
              (send dc set-pen (make-pen #:color (opt 'outline) #:width (max 1 lw)))
              (send dc draw-ellipse x1 y1 (- x2 x1) (- y2 y1)))
             (else
               (stroke dc (arc-points x1 y1 x2 y2 0 360) (opt 'outline) lw dash #f)))))
        ((arc)
         (let-values (((x1 y1 x2 y2) (box)))
           (let ((points (arc-points x1 y1 x2 y2 (opt 'start) (opt 'extent))))
             (if (eq? (opt 'style) 'pieslice)
                 (let ((center (cons (/ (+ x1 x2) 2.0) (/ (+ y1 y2) 2.0))))
                   (fill-shape dc (opt 'fill)
                     (lambda ()
                       (send dc set-pen (make-pen #:style 'transparent))
                       (send dc draw-polygon (map (lambda (p) (cons (car p) (cdr p)))
                                                  (cons center points)))))
                   (stroke dc (cons center points) (opt 'outline) 1 #f #t))
                 (if (solid? dash)
                     (let ((start (* (/ pi 180) (opt 'start)))
                           (end (* (/ pi 180) (+ (opt 'start) (opt 'extent)))))
                       (send dc set-pen (make-pen #:color (opt 'outline) #:width (max 1 lw)
                                                  #:cap 'butt))
                       (send dc draw-arc x1 y1 (- x2 x1) (- y2 y1)
                             (min start end) (max start end)))
                     (stroke dc points (opt 'outline) lw dash #f))))))
        ((line)
         (stroke dc (pairs cs) (opt 'fill) lw dash #f))
        ((polygon)
         (let ((points (pairs cs)))
           (when (opt 'fill)
             (fill-shape dc (opt 'fill) (lambda () (send dc draw-polygon points))))
           (stroke dc points (opt 'outline) lw dash #t)))
        ((text)
         (let* ((font (send (opt 'font) get-font))
                (text (opt 'text)))
           (let-values (((w h d a) (send dc get-text-extent text font #t)))
             (send dc set-font font)
             (send dc set-text-foreground (opt 'fill))
             ;; anchor s: (x y) is the bottom centre of the text's box
             (send dc draw-text text
                   (round (- (car cs) (/ (ceiling w) 2)))
                   (round (- (cadr cs) (ceiling h)))
                   #t))))
        (else (error 'render-item "unknown item kind ~s" (item-kind it)))))))

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

;; port: was swl:sync-display; the GUI sets it to flush its canvases
(define *flush-event-queue* void)
(define set-flush-event-queue!
  (lambda (f) (set! *flush-event-queue* f)))
