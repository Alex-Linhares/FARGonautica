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
;; Ported to Racket, 2026: the window part of general-graphics.ss, included
;; by racket/gui/views.rkt.  The pexp builders and text helpers of the file
;; are in the engine (racket/engine/general-graphics.rktl).  SWL's toplevel,
;; frame and scrollframe become a window host (views.rkt: offscreen by
;; default, a racket/gui frame once the control panel exists) and SWL's
;; <viewport> becomes racket/gui/sgl.rkt's viewport%.  Changes are marked
;; "port:"; see docs/porting-notes.md, item 13.

(define %default-fg-color% =black=)
(define %default-bg-color% =white=)
(define %default-scrollable-text-window-font% (swl-font serif 24))

(define *fg-color* %default-fg-color%)

(define toplevel-destroy-action
  (lambda (toplevel)
    (tell *control-panel* 'hide-window toplevel)
    #f))

(define %resize-listener-pause% 250)
(define *resize-message-queue* (thread-make-msg-queue 'resizeq))

(define start-resize-listener
  (lambda ()
    (thread-fork
      (lambda ()
	(let loop ()
	  (let ((resize (thread-receive-msg *resize-message-queue*)))
	    (resize)
	    (thread-sleep %resize-listener-pause%)
	    (loop)))))
    'done))

(define make-scrollable-graphics-window
  (lambda (visible-w visible-h . color)
    (let ((bg-color (if (null? color) %default-bg-color% (car color)))
	  (ymax (/ visible-h visible-w)))
      (make-graphics-window
	visible-w visible-h visible-w visible-h 0 0 1 ymax bg-color 'both))))

(define make-unscrollable-graphics-window
  (lambda (visible-w visible-h . color)
    (let ((bg-color (if (null? color) %default-bg-color% (car color)))
	  (ymax (/ visible-h visible-w)))
      (make-graphics-window
	visible-w visible-h visible-w visible-h 0 0 1 ymax bg-color 'none))))

(define make-horizontal-scrollable-graphics-window
  (lambda (visible-w visible-h canvas-w . color)
    (let ((bg-color (if (null? color) %default-bg-color% (car color)))
	  (xmax (/ canvas-w visible-h)))
      (make-graphics-window
	visible-w visible-h canvas-w visible-h 0 0 xmax 1 bg-color 'horizontal))))

(define make-vertical-scrollable-graphics-window
  (lambda (visible-w visible-h canvas-h . color)
    (let ((bg-color (if (null? color) %default-bg-color% (car color)))
	   (ymax (/ canvas-h visible-w)))
      (make-graphics-window
	visible-w visible-h visible-w canvas-h 0 0 1 ymax bg-color 'vertical))))

;; scrolling = {both | horizontal | vertical | none}
;; determines which dimensions of a window will get rescaled when the window is
;; resized.  scrolling = {both | none} maintains a fixed aspect ratio.

(define make-graphics-window
  (lambda (visible-w visible-h canvas-w canvas-h xmin ymin xmax ymax bg-color scrolling)
    (let* ((canvas-aspect-ratio (/ canvas-w canvas-h))
	   (width-per-pixel (/ (- xmax xmin) canvas-w))
	   (height-per-pixel (/ (- ymax ymin) canvas-h))
	   (pixel->x (lambda (i) (+ xmin (* width-per-pixel i))))
	   (pixel->y (lambda (j) (- ymax (* height-per-pixel j))))
	   (x->pixel
	     (lambda (x offset)
	       (inexact->exact (floor (/ (- (+ x offset) xmin) width-per-pixel)))))
	   (y->pixel
	     (lambda (y offset)
	       (inexact->exact (floor (/ (- ymax (+ y offset)) height-per-pixel)))))
	   ;; port: the toplevel and its (scroll)frame are one window host
	   (top (make-window-host scrolling toplevel-destroy-action))
	   (frame top)
	   (vp (new viewport% (pixel->x pixel->x) (pixel->y pixel->y)
		 (x->pixel x->pixel) (y->pixel y->pixel)
		 (width visible-w) (height visible-h) (background-color bg-color)))
	   (resizable? #f)
	   (position #f)
	   (cache-mode? #f)
	   (cached-pexps '())
	   (aspect-ratio-workaround? #f))  ;; disabled 5/2015

;;	    ;; set-aspect-ratio-bounds! does not seem to work under Windows
;;	    ;; or Mac OS X, so we need a workaround to compensate for this
;;	    (and (or (eq? *platform* 'windows) (eq? *platform* 'macintosh))
;;		 (or (eq? scrolling 'both) (eq? scrolling 'none)))))

      ;; port: Tk packing becomes the host showing the viewport
      (send vp set-scroll-region! 0 0 canvas-w canvas-h)
      (send top show-viewport vp)
      (send top raise)
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'graphics-window)
	    (get-vp () vp)
	    (get-toplevel () top)
	    (get-size () (list visible-w visible-h))
	    (get-visible-w () visible-w)
	    (get-visible-h () visible-h)
	    (set-position (x y)
	      (send top set-geometry! (format "+~a+~a" x y)))
	    (remember-position ()
	      (let ((g (send top get-geometry)))
		(set! position (substring g (char-index #\+ g) (string-length g))))
	      'done)
	    (restore-position ()
	      (if* (exists? position)
		(send top set-geometry! position))
	      'done)
	    (repair-aspect-ratio ()
		(let* ((w (send top get-width))
		       (h (send top get-height))
		       (ratio (/ (- w 2) (- h 2))))
		  (cond
		    ((> ratio canvas-aspect-ratio)
		     (send top set-geometry!
		       (format "~ax~a" (round (* h canvas-aspect-ratio)) h)))
		    ((< ratio canvas-aspect-ratio)
		     (send top set-geometry!
		       (format "~ax~a" w (round (/ w canvas-aspect-ratio))))))))
	    (make-resizable winname
	      (if* (not (and (exists? *scrollbar-width*)
			     (exists? *scrollbar-height*)))
		(error #f "need to run (create-mcat-logo) first"))
	      (if* (not resizable?)
		(send top set-resizable! #t #t)
		(send top set-min-size! 10 10)
		(if* (or (eq? scrolling 'both) (eq? scrolling 'none))
		  ;; For some reason, after creating an SWL window of size W x H
		  ;; with (create <viewport> ...), the window thinks its width
		  ;; and height are W+2 and H+2.  We need to use these values when
		  ;; setting the aspect ratio bounds, otherwise the window may
		  ;; resize itself slightly when made resizable.
		  (let ((ratio (/ (+ visible-w 2) (+ visible-h 2))))
		    (send top set-aspect-ratio-bounds! ratio ratio)))
		(send vp set-resize-handler!
		  (lambda (win w h)
		    (cond
		      ((and (> w 2) (> h 2))
		       ;; Recalculate and set visible-w, visible-h, canvas-w, canvas-h,
		       ;; width-per-pixel, and height-per-pixel values.  pixel->x, x->pixel,
		       ;; etc. procedures in vp will automatically see the new values.
		       (set! visible-w (- w 2))
		       (set! visible-h (- h 2))

		       ;; Windows/Macintosh problem workaround
		       (if* aspect-ratio-workaround?
			 (let ((ratio (/ visible-w visible-h)))
			   (if (> ratio canvas-aspect-ratio)
			     (set! visible-w (round (* visible-h canvas-aspect-ratio)))
			     (set! visible-h (round (/ visible-w canvas-aspect-ratio))))))

		       (case scrolling
			 ((none)
			  (set! canvas-w visible-w)
			  (set! canvas-h visible-h)
			  (set! width-per-pixel (/ (- xmax xmin) canvas-w))
			  (set! height-per-pixel (/ (- ymax ymin) canvas-h))
			  (send vp set-scroll-region! 0 0 canvas-w canvas-h))
			 ((horizontal)
			  (set! canvas-w (round (* visible-h canvas-aspect-ratio)))
			  (set! canvas-h visible-h)
			  (set! width-per-pixel (/ (- xmax xmin) canvas-w))
			  (set! height-per-pixel (/ (- ymax ymin) canvas-h))
			  (cond
			    ((and (> canvas-w visible-w)
				  (not (tell self 'scrollbar-present? 'horizontal))
				  (<= (round (* (- visible-h *scrollbar-height*)
						canvas-aspect-ratio))
				      visible-w))
			     (send vp set-scroll-region! 0 0 visible-w canvas-h))
			    ((and (<= canvas-w visible-w)
				  (tell self 'scrollbar-present? 'horizontal)
				  (> (round (* (+ visible-h *scrollbar-height*)
					       canvas-aspect-ratio))
				     visible-w))
			     (send vp set-scroll-region! 0 0 visible-w canvas-h))
			    (else
			      (send vp set-scroll-region! 0 0 canvas-w canvas-h))))
			 ((vertical)
			  (set! canvas-w visible-w)
			  (set! canvas-h (round (/ visible-w canvas-aspect-ratio)))
			  (set! width-per-pixel (/ (- xmax xmin) canvas-w))
			  (set! height-per-pixel (/ (- ymax ymin) canvas-h))
			  (cond
			    ((and (> canvas-h visible-h)
				  (not (tell self 'scrollbar-present? 'vertical))
				  (<= (round (/ (- visible-w *scrollbar-width*)
						canvas-aspect-ratio))
				      visible-h))
			     (send vp set-scroll-region! 0 0 canvas-w visible-h))
			    ((and (<= canvas-h visible-h)
				  (tell self 'scrollbar-present? 'vertical)
				  (> (round (/ (+ visible-w *scrollbar-width*)
					       canvas-aspect-ratio))
				     visible-h))
			     (send vp set-scroll-region! 0 0 canvas-w visible-h))
			    (else
			      (send vp set-scroll-region! 0 0 canvas-w canvas-h)))))
;;		       (printf "~a now ~a~%" (car winname) (tell self 'get-info))
		       (critical-section
			 (let ((resize
				 (lambda ()
				   (if* aspect-ratio-workaround?
				     (tell self 'repair-aspect-ratio))
				   (tell self 'resize visible-w visible-h))))
			   (if* (thread-msg-waiting? *resize-message-queue*)
			     (thread-receive-msg *resize-message-queue*))
			   (thread-send-msg *resize-message-queue* resize))))
		      (else 'done))))
		(set! resizable? #t))
	      'done)
	      ;; this method should be overridden by windows that can be resized
	    (resize (w h)
	      (printf "warning: no resize method defined~%"))
	    (get-info ()
	      (let ((hsb (if (tell self 'scrollbar-present? 'horizontal) 'h #f))
		    (vsb (if (tell self 'scrollbar-present? 'vertical) 'v #f)))
		`((visible: ,visible-w x ,visible-h)
		  (canvas: ,canvas-w x ,canvas-h)
		  (scroll: ,@(compress (list hsb vsb))))))
	    (scrollbar-present? (orientation)
	      (and (not (eq? scrolling 'none))
		   (exists? (get-scrollbar-from-frame frame orientation #f))))
	    (reposition-vertical-scrollbar ()
	      (if* (< visible-h canvas-h)
		;; port: scroll the viewport itself; a host with a scrollbar
		;; follows it (the original waited for the scrollbar to appear)
		(let* ((hidden-h (- canvas-h visible-h))
		       (hidden% (exact->inexact (/ hidden-h canvas-h))))
		  (send vp set-scroll-position! 0 hidden-h)
		  (send top set-vertical-view! hidden%)))
	      'done)
	    (set-mouse-handlers (left-press right-press)
	      (send vp set-mouse-handlers! left-press right-press)
	      'done)
	    (set-window-title (title)
	      ;; port: the host is the viewport's toplevel
	      (send top set-title! title)
	      'done)
	    (set-icon-label args 'ignored)
	    (set-icon-image args 'ignored)
	    (set-background-color (color)
	      (set! bg-color color)
	      'done)
	    (cache-mode? () cache-mode?)
	    (caching-on ()
	      (set! cache-mode? #t)
	      'done)
 	    (flush tag
	      (if* (not (null? cached-pexps))
		(if (null? tag)
		  (draw! vp (tell self 'get-cached-pexp))
		  (draw! vp (tell self 'get-cached-pexp) tag))
		(set! cached-pexps '()))
	      (swl:sync-display)
	      (set! cache-mode? #f)
	      'done)
	    (clear-pending-flush ()
	      (set! cached-pexps '())
	      (set! cache-mode? #f)
	      'done)
 	    (get-cached-pexp () `(let-sgl () ,@(reverse cached-pexps)))
	    (flash (pexp)
	      (if* (> %flash-pause% 0)
		(draw! vp `(let-sgl ((foreground-color ,bg-color)) ,pexp) 'background)
		(draw! vp pexp 'flash)
		(pause %flash-pause%)
		(send vp delete 'flash)
		(repeat* (- %num-of-flashes% 1) times
		  (pause %flash-pause%)
		  (draw! vp pexp 'flash)
		  (pause %flash-pause%)
		  (send vp delete 'flash))
		(send vp delete 'background))
	      'done)
 	    (draw (pexp . tag)
 	      (let ((pexp (if (exists? *fg-color*)
 			    `(let-sgl ((foreground-color ,*fg-color*)) ,pexp)
 			    pexp)))
		(cond
		  (cache-mode? (set! cached-pexps (cons pexp cached-pexps)))
		  ((null? tag) (draw! vp pexp))
		  (else (draw! vp pexp (car tag))))
 		'done))
	    (erase (pexp)
	      (tell self 'erase-on-background bg-color pexp))
 	    (erase-on-background (color pexp)
 	      (if cache-mode?
 		(set! cached-pexps (cons `(erase ,color ,pexp) cached-pexps))
 		(draw! vp `(erase ,color ,pexp)))
 	      'done)
	    (move (dx dy tag) (send vp move dx dy tag) 'done)
	    (move-pixels (dx dy tag) (send vp move-pixels dx dy tag) 'done)
	    (raise (tag) (send vp raise tag) 'done)
	    (unhide (tag)
	      (if* *tcl/tk-version-8_3?*
		(send vp unhide tag))
	      'done)
	    (retag (old new) (send vp retag old new) 'done)
	    (rescale (tag xfactor yfactor) (send vp rescale tag xfactor yfactor) 'done)
	    (delete (tag) (send vp delete tag) 'done)
	    (raise-window () (send top raise))
	    (lower-window () (send top lower))
	    (clear ()
	      (if cache-mode?
		(set! cached-pexps (cons `(clear ,bg-color) cached-pexps))
		(draw! vp `(clear ,bg-color)))
	      'done)
	    (get-center-coord ()
	      (list (/ (+ xmin xmax) 2) (/ (+ ymin ymax) 2)))
	    (get-x-max () xmax)
	    (get-y-max () ymax)
	    (get-visible-x-max () (+ xmin (* width-per-pixel visible-w)))
	    (get-visible-y-min () (- ymax (* height-per-pixel visible-h)))
	    (get-width-per-pixel () width-per-pixel)
	    (get-height-per-pixel () height-per-pixel)
	    ;; For a character of size WIDTH x HEIGHT pixels, the bounding-box
	    ;; coordinates relative to the text baseline offset point are given by
	    ;;    lower left corner  = (-1, -OFFSET - 1)
	    ;;    upper right corner = (WIDTH + 1, HEIGHT + 1 - OFFSET)
	    ;; The baseline offset point is at position (0, OFFSET) in the
	    ;; character's pixel matrix (for left text justification).
	    (get-character-bounding-box (char font text-origin)
              (let* ((x (1st text-origin))
		     (y (2nd text-origin))
		     (size (tell font 'get-pixel-size char))
		     (width (1st size))
		     (height (2nd size))
		     (baseline (3rd size)))
		`((,(+ x (* width-per-pixel -1))
		   ,(+ y (* height-per-pixel (- (- baseline) 1))))
		  (,(+ x (* width-per-pixel (+ width 1)))
		   ,(+ y (* height-per-pixel (- (+ height 1) baseline)))))))
	    ;; char is a one-character string
	    (get-character-width (char font)
	      (* width-per-pixel (1st (tell font 'get-pixel-size char))))
	    (get-character-height (char font)
	      (* height-per-pixel (2nd (tell font 'get-pixel-size char))))
	    (get-text-offset (font)
	      (* height-per-pixel (3rd (tell font 'get-pixel-size "M"))))
	    (get-string-width (text-string font)
	      (* width-per-pixel (tell font 'get-pixel-width text-string)))
	    (get-string-height (font)
	      (tell self 'get-character-height "M" font))
	    (destroy () (send top destroy))
	    (else (delegate msg base-object))))))))

;; For some reason, it takes some time for a scrollbar to show up among
;; the children of a frame right after creating a graphics window, so we
;; may need to wait for it if it's not yet there.  What a hack.

;; port: the window host knows its scrollbars (#f for none), so there is
;; nothing to wait for
(define get-scrollbar-from-frame
  (lambda (scrollframe orientation wait-for-scrollbar?)
    (send scrollframe get-scrollbar orientation)))

;;---------------------------------------------------------------------------

(define make-scrollable-text-window
  (lambda (visible-w visible-h canvas-h . color)
    (let* ((bg-color (if (null? color) %default-bg-color% (car color)))
	   (font %default-scrollable-text-window-font%)
	   (centering? #f)
	   ;; <paragraphs> ::= ({<skip-num>+ | <paragraph-string>} ...)
	   ;; paragraphs is a list of paragraph strings (in reverse order),
	   ;; each one separated by one or more line skip numbers
	   (paragraphs '())
	   (graphics-window
	     (make-vertical-scrollable-graphics-window
	       visible-w visible-h canvas-h bg-color)))
      (tell graphics-window 'reposition-vertical-scrollbar)
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'scrollable-text-window)
	    (get-font () font)
	    (get-paragraphs () paragraphs)
	    (set-paragraphs (new-paragraphs)
	      (set! paragraphs new-paragraphs)
	      'done)
	    (get-lines ()
	      (apply append
		(map (lambda (p)
		       (if (string? p)
			 (map remove-leading-blanks (tell self 'format-paragraph p))
			 (list p)))
		  (reverse paragraphs))))
	    (new-font (new-font)
	      (set! font new-font)
	      (tell self 'redraw)
	      (tell graphics-window 'reposition-vertical-scrollbar))
	    (default-font ()
	      (set! font %default-scrollable-text-window-font%)
	      (tell self 'redraw))
	    (centering? () centering?)
	    (centering-on ()
	      (set! centering? #t)
	      (tell self 'redraw))
	    (centering-off ()
	      (set! centering? #f)
	      (tell self 'redraw))
	    (clear ()
	      (set! paragraphs '())
	      (tell graphics-window 'clear))
	    (draw-paragraph (text-string)
	      (set! paragraphs (cons text-string paragraphs))
	      (for* each line in (tell self 'format-paragraph text-string) do
		(tell self 'draw-line line))
	      (tell self 'newline))
	    (format-paragraph (text-string)
	      (let ((max-line-length
		     (- (tell graphics-window 'get-x-max)
			(tell graphics-window 'get-character-width " " font))))
		(break-into-lines graphics-window font max-line-length text-string)))
	    (draw-line (line)
	      (let ((line-height (tell graphics-window 'get-string-height font))
		    (line-pixel-height (tell font 'get-pixel-height))
		    (line (string-append " " (remove-leading-blanks line))))
		(tell graphics-window 'move-pixels 0 (- line-pixel-height) 'all)
		(if* (> %text-scroll-pause% 0)
		  (pause %text-scroll-pause%))
		(let ((pexp `(let-sgl ((font ,font))
			       (text (0 ,(* 1/2 line-height)) ,line))))
		  (tell graphics-window 'draw
		    (if centering?
		      `(let-sgl ((text-justification center)
				 (origin (1/2 0)))
			 ,pexp)
		      pexp)))))
	    (skip (num-lines)
	      (set! paragraphs (cons num-lines paragraphs))
	      (let ((skip-height (* num-lines (tell font 'get-pixel-height))))
		(tell graphics-window 'move-pixels 0 (- skip-height) 'all)))
	    (newline () (tell self 'skip 1))
	    (resize (new-width new-height)
	      (set! visible-w new-width)
	      (set! visible-h new-height)
	      (tell self 'redraw)
	      (tell graphics-window 'reposition-vertical-scrollbar))
	    (redraw ()
	      (tell graphics-window 'retag 'all 'garbage)
	      (let ((line-height (tell graphics-window 'get-string-height font))
		    (line-pixel-height (tell font 'get-pixel-height))
		    (line-num 0))
		(for* each x in paragraphs do
		  (if (number? x)
		    (set! line-num (+ line-num x))
		    (for* each line in (reverse (tell self 'format-paragraph x)) do
		      (let ((pexp `(let-sgl ((font ,font))
				     (text (0 ,(* (+ line-num 1/2) line-height)) ,line))))
			(tell graphics-window 'draw
			  (if centering?
			    `(let-sgl ((text-justification center)
				       (origin (1/2 0)))
			       ,pexp)
			    pexp))
			(set! line-num (+ line-num 1)))))))
	      (tell graphics-window 'delete 'garbage))
	    (else (delegate msg graphics-window))))))))
