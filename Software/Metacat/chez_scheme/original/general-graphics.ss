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

(define pi 3.14159265359)
(define pi/180 (/ pi 180))
(define 180/pi (/ 180 pi))

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
	   (top (create <toplevel> with (title: "") (resizable: #f #f)
		  (destroy-request-handler: toplevel-destroy-action)))
	   (frame (if (eq? scrolling 'none)
		    (create <frame> top)
		    (create <scrollframe> top)))
	   (vp (create <viewport> frame pixel->x pixel->y x->pixel y->pixel with
		 (width: visible-w) (height: visible-h) (background-color: bg-color)
  		 (scroll-region: 0 0 canvas-w canvas-h)))
	   (resizable? #f)
	   (position #f)
	   (cache-mode? #f)
	   (cached-pexps '())
	   (aspect-ratio-workaround? #f))  ;; disabled 5/2015

;;	    ;; set-aspect-ratio-bounds! does not seem to work under Windows
;;	    ;; or Mac OS X, so we need a workaround to compensate for this
;;	    (and (or (eq? *platform* 'windows) (eq? *platform* 'macintosh))
;;		 (or (eq? scrolling 'both) (eq? scrolling 'none)))))

      (pack frame (expand: #t) (fill: 'both))
      (pack vp (expand: #t) (fill: 'both))
      (show vp)
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
		(let* ((sb (get-scrollbar-from-frame frame 'vertical #t))
		       (hidden-h (- canvas-h visible-h))
		       (hidden% (exact->inexact (/ hidden-h canvas-h))))
		  (send sb set-view! hidden% 1)
		  (send vp vscroll hidden% 'fraction)))
	      'done)
	    (set-mouse-handlers (left-press right-press)
	      (send vp set-mouse-handlers! left-press right-press)
	      'done)
	    (set-window-title (title)
	      (send (send (send vp get-parent) get-parent) set-title! title)
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

(define get-scrollbar-from-frame
  (lambda (scrollframe orientation wait-for-scrollbar?)
    (let ((scrollbar #f))
      (let try ()
	(send scrollframe for-children
	  (lambda (w)
	    (if* (and (isa? w <scrollbar>)
		      (eq? (send w get-orientation) orientation))
	      (set! scrollbar w))))
	(if* (and (not scrollbar) wait-for-scrollbar?)
	  (pause 10)
	  (try)))
      scrollbar)))

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

(define remove-leading-blanks
  (lambda (line)
    (let ((len (string-length line)))
      (letrec
	((find-beginning
	   (lambda (i)
	     (cond
	       ((= i len) line)
	       ((char-whitespace? (string-ref line i)) (find-beginning (+ i 1)))
	       (else (substring line i len))))))
	(find-beginning 0)))))

(define break-into-lines
  (lambda (g font max-line-length text-string)
    (let combine ((previous-lines '())
		  (current-line "")
		  (words-left (separate-into-words text-string))
		  (space-left max-line-length))
      (if (null? words-left)
	(filter-out
	  (lambda (s) (string=? s ""))
	  (reverse (cons current-line previous-lines)))
	(let* ((next-word (1st words-left))
	       (word-length (tell g 'get-string-width next-word font)))
	  (cond
	    ((< word-length space-left)
	     (combine
	       previous-lines
	       (string-append current-line next-word)
	       (rest words-left)
	       (- space-left word-length)))
	    ((>= word-length max-line-length)
	     (combine
	       (cons next-word (cons current-line previous-lines))
	       ""
	       (rest words-left)
	       max-line-length))
	    (else
	      (combine
		(cons current-line previous-lines)
		""
		words-left
		max-line-length))))))))

(define separate-into-words
  (lambda (text-string)
    (let* ((next-pos (find-next-space-position text-string 0))
	   (next-word (string-append " " (substring text-string 0 next-pos)))
	   (length (string-length text-string)))
      (if (= next-pos length)
	(list next-word)
	(cons next-word
	  (separate-into-words (substring text-string (+ next-pos 1) length)))))))

(define find-next-space-position
  (lambda (s i)
    (cond
      ((>= i (string-length s)) (string-length s))
      ((char=? (string-ref s i) #\space) i)
      (else (find-next-space-position s (+ i 1))))))

;;------------------------------- Circles etc. -------------------------------

(define circle
  (lambda (center diameter)
    `(arc ,center (,diameter ,diameter) 0 360)))

(define disk
  (lambda (center diameter)
    `(filled-arc ,center (,diameter ,diameter) 0 360)))

(define pie-slice
  (lambda (center diameter start-deg sweep-deg)
    `(filled-arc ,center (,diameter ,diameter) ,start-deg ,sweep-deg)))

;;--------------------------------- Boxes ------------------------------------

(define outline-box
  (lambda (x0 y0 x1 y1)
    `(rectangle (,x0 ,y0) (,x1 ,y1))))

(define solid-box
  (lambda (x0 y0 x1 y1)
    `(filled-rectangle (,x0 ,y0) (,x1 ,y1))))

(define centered-ovaloid
  (lambda (xc yc width height ovalness)
    (let* ((x1 (- xc (* 1/2 width)))
	   (x2 (+ xc (* 1/2 width)))
	   (interior-height (* height (- 1 ovalness)))
	   (y1 (- yc (* 1/2 interior-height)))
	   (y2 (+ yc (* 1/2 interior-height)))
	   (exterior-height (- height interior-height)))
      `(let-sgl ()
	 (arc (,xc ,y2) (,width ,exterior-height) 0 180)
	 (line (,x2 ,y1) (,x2 ,y2))
	 (arc (,xc ,y1) (,width ,exterior-height) 180 180)
	 (line (,x1 ,y2) (,x1 ,y1))))))

(define filled-centered-ovaloid
  (lambda (xc yc width height ovalness)
    (let* ((x1 (- xc (* 1/2 width)))
	   (x2 (+ xc (* 1/2 width)))
	   (interior-height (* height (- 1 ovalness)))
	   (y1 (- yc (* 1/2 interior-height)))
	   (y2 (+ yc (* 1/2 interior-height)))
	   (exterior-height (- height interior-height)))
      `(let-sgl ()
	 (filled-arc (,xc ,y2) (,width ,exterior-height) 0 180)
	 (filled-rectangle (,x1 ,y1) (,x2 ,y2))
	 (filled-arc (,xc ,y1) (,width ,exterior-height) 180 180)))))

(define centered-rounded-box
    (lambda (xc yc width height corner-radius)
      (let* ((left (- xc (* 1/2 width)))
	     (right (+ xc (* 1/2 width)))
	     (top (+ yc (* 1/2 height)))
	     (bottom (- yc (* 1/2 height)))
	     (x1 (+ left corner-radius))
	     (x2 (- right corner-radius))
	     (y1 (+ bottom corner-radius))
	     (y2 (- top corner-radius))
	     (corner-diameter (* 2 corner-radius)))
	`(let-sgl ()
	   (line (,x1 ,bottom) (,x2 ,bottom))
	   (arc (,x2 ,y1) (,corner-diameter ,corner-diameter) 270 90)
	   (line (,right ,y1) (,right ,y2))
	   (arc (,x2 ,y2) (,corner-diameter ,corner-diameter) 0 90)
	   (line (,x2 ,top) (,x1 ,top))
	   (arc (,x1 ,y2) (,corner-diameter ,corner-diameter) 90 90)
	   (line (,left ,y2) (,left ,y1))
	   (arc (,x1 ,y1) (,corner-diameter ,corner-diameter) 180 90)))))

(define filled-centered-rounded-box
  (lambda (xc yc width height corner-radius pixel)
      (let* ((left (- xc (* 1/2 width)))
	     (right (+ xc (* 1/2 width)))
	     (top (+ yc (* 1/2 height)))
	     (bottom (- yc (* 1/2 height)))
	     (x1 (+ left corner-radius))
	     (x2 (- right corner-radius))
	     (y1 (+ bottom corner-radius))
	     (y2 (- top corner-radius))
	     (corner-diameter (* 2 corner-radius)))
	`(let-sgl ()
	   (filled-rectangle (,left ,(- y1 pixel)) (,right ,(+ y2 pixel)))
	   (filled-rectangle (,(- x1 pixel) ,bottom) (,(+ x2 pixel) ,top))
	   (filled-arc (,x2 ,y1) (,corner-diameter ,corner-diameter) 270 90)
	   (filled-arc (,x2 ,y2) (,corner-diameter ,corner-diameter) 0 90)
	   (filled-arc (,x1 ,y2) (,corner-diameter ,corner-diameter) 90 90)
	   (filled-arc (,x1 ,y1) (,corner-diameter ,corner-diameter) 180 90)))))

;;-------------------------------- Octagons ----------------------------------

(define centered-octagon
  (lambda (xc yc width)
    (let* ((a (* 1/2 width))
	   (b (* a (/ 1 (+ 1 (sqrt 2)))))
	   (x1 (- xc a))
	   (x2 (- xc b))
	   (x3 (+ xc b))
	   (x4 (+ xc a))
	   (y1 (- yc a))
	   (y2 (- yc b))
	   (y3 (+ yc b))
	   (y4 (+ yc a)))
      `(polygon
	 (,x2 ,y1) (,x3 ,y1) (,x4 ,y2) (,x4 ,y3) (,x3 ,y4)
	 (,x2 ,y4) (,x1 ,y3) (,x1 ,y2)))))

(define filled-centered-octagon
  (lambda (xc yc width)
    (let* ((a (* 1/2 width))
	   (b (* a (/ 1 (+ 1 (sqrt 2)))))
	   (x1 (- xc a))
	   (x2 (- xc b))
	   (x3 (+ xc b))
	   (x4 (+ xc a))
	   (y1 (- yc a))
	   (y2 (- yc b))
	   (y3 (+ yc b))
	   (y4 (+ yc a)))
      `(filled-polygon
	 (,x2 ,y1) (,x3 ,y1) (,x4 ,y2) (,x4 ,y3) (,x3 ,y4)
	 (,x2 ,y4) (,x1 ,y3) (,x1 ,y2)))))

;;-------------------------- Dotted lines & boxes ----------------------------

(define %dot-interval% 1/125)

(define dotted-line
  (lambda (x1 y1 x2 y2 approx-interval-length)
    (if (or %nice-graphics% (not *tcl/tk-version-8_3?*))
      `(polypoints ,@(dotted-line-points x1 y1 x2 y2 approx-interval-length))
      `(let-sgl ((line-style dotted))
	 (line (,x1 ,y1) (,x2 ,y2))))))

(define dotted-box
  (lambda (x1 y1 x2 y2 approx-interval-length)
    (if (or %nice-graphics% (not *tcl/tk-version-8_3?*))
      `(polypoints
	 ,@(dotted-line-points x1 y1 x1 y2 approx-interval-length)
	 ,@(dotted-line-points x1 y2 x2 y2 approx-interval-length)
	 ,@(dotted-line-points x2 y1 x2 y2 approx-interval-length)
	 ,@(dotted-line-points x1 y1 x2 y1 approx-interval-length))
      `(let-sgl ((line-style dotted))
	 (rectangle (,x1 ,y1) (,x2 ,y2))))))

(define dotted-line-points
  (lambda (x1 y1 x2 y2 approx-interval-length)
    (let* ((p1 (make-rectangular x1 y1))
	   (p2 (make-rectangular x2 y2))
	   (l (- p2 p1))
	   (n (max 1 (round (/ (magnitude l) approx-interval-length))))
	   (exact-interval-length (/ (magnitude l) n))
	   (coord-transform
	     (lambda (p)
	       (if (zero? p)
		 `(,x1 ,y1)
		 (let ((p-prime
			 (+ p1 (make-polar (magnitude p) (+ (angle l) (angle p))))))
		   `(,(x-coord p-prime) ,(y-coord p-prime)))))))
      (letrec
	((dotted-line-points
	   (lambda (i points)
	     (if (< i 0)
	       points
	       (dotted-line-points
		 (sub1 i)
		 (cons (coord-transform
			 (make-rectangular (* i exact-interval-length) 0))
		   points))))))
	(dotted-line-points n '())))))

;;----------------------------- Dashed lines & boxes ----------------------------------

;; density parameter ranges from 0 (no dashes) to 1 (no spaces).
;; dash-length parameter is the length of an individual dash in world-coordinates.
;;
;; Examples of density and dash-length parameters:
;;
;;   low density, short dash-length  |--          --          --         --          --|
;;                                   |                                                 |
;;   low density, long dash-length   |--------            --------             --------|
;;                                   |                                                 |
;;   high density, short dash-length |--  --  --  --  --  --  --  --  --  --  --  -- --|
;;                                   |                                                 |
;;   high density, long dash-length  |--------  ---------  --------  --------  --------|

(define %dash-length% 1/200)
(define %dash-density% 1/2)

(define dashed-line
  (lambda (x1 y1 x2 y2 density approx-dash-length)
    (if (or %nice-graphics% (not *tcl/tk-version-8_3?*))
      `(line ,@(dashed-line-points x1 y1 x2 y2 density approx-dash-length))
      `(let-sgl ((line-style dashed))
	 (line (,x1 ,y1) (,x2 ,y2))))))

(define dashed-box
  (lambda (x1 y1 x2 y2 density approx-dash-length)
    (if (or %nice-graphics% (not *tcl/tk-version-8_3?*))
      `(line
	,@(dashed-line-points x1 y1 x1 y2 density approx-dash-length)
	,@(dashed-line-points x1 y2 x2 y2 density approx-dash-length)
	,@(dashed-line-points x2 y1 x2 y2 density approx-dash-length)
	,@(dashed-line-points x1 y1 x2 y1 density approx-dash-length))
      `(let-sgl ((line-style dashed))
	 (rectangle (,x1 ,y1) (,x2 ,y2))))))

(define dashed-line-points
  (lambda (x1 y1 x2 y2 density approx-dash-length)
    (let* ((p1 (make-rectangular x1 y1))
	   (p2 (make-rectangular x2 y2))
	   (l (- p2 p1))
	   (total-dash-length (* (magnitude l) density))
	   (total-space-length (* (magnitude l) (- 1 density)))
	   (n (max 3 (round (/ total-dash-length approx-dash-length))))
	   (dash-length (/ total-dash-length n))
	   (space-length (/ total-space-length (sub1 n)))
	   (interval-length (+ dash-length space-length))
	   (coord-transform
	     (lambda (p)
	       (if (zero? p)
		 `(,x1 ,y1)
		 (let ((p-prime
			 (+ p1 (make-polar (magnitude p) (+ (angle l) (angle p))))))
		   `(,(x-coord p-prime) ,(y-coord p-prime)))))))
      (letrec
	((dashed-line-points
	   (lambda (i points)
	     (if (< i 0)
	       points
	       (dashed-line-points
		 (sub1 i)
		 (cons (coord-transform
			 (make-rectangular (* i interval-length) 0))
		   (cons (coord-transform
			   (make-rectangular (+ (* i interval-length) dash-length) 0))
		     points)))))))
	(dashed-line-points (sub1 n) '())))))

;;---------------------------- Zigzag lines -------------------------------

(define zigzag-line
  (lambda (p1 p2 approx-zigzag-length sign)
    `(polyline ,@(zigzag-line-points p1 p2 approx-zigzag-length sign))))


(define centered-zigzag-line
  (lambda (p1 p2 approx-zigzag-length sign)
    `(polyline ,@(centered-zigzag-line-points p1 p2 approx-zigzag-length sign))))

;; zigzag-line-points:
;;
;;            /\  /\  /\  /\  /\  /\  /\  /\
;; sign + .../  \/  \/  \/  \/  \/  \/  \/  \
;;               ---- (approx-zigzag-length)
;; sign - ...
;;           \  /\  /\  /\  /\  /\  /\  /\  /
;;            \/  \/  \/  \/  \/  \/  \/  \/ 

(define zigzag-line-points
  (lambda (p1 p2 approx-zigzag-length sign)
    (let* ((l (- p2 p1))
	   (n (max 1 (round (/ (magnitude l) approx-zigzag-length))))
	   (period (make-rectangular (/ (magnitude l) n) 0))
	   (delta (/ (magnitude period) 2))
	   (zig (make-rectangular delta (sign delta)))
	   (coord-transform
	     (lambda (p)
	       (if (zero? p)
		 `(,(x-coord p1) ,(y-coord p1))
		 (let ((p-prime
			 (+ p1 (make-polar (magnitude p) (+ (angle l) (angle p))))))
		   `(,(x-coord p-prime) ,(y-coord p-prime)))))))
      (letrec
	((zigzag-line-points
	   (lambda (i sgl-points)
	     (if (< i 0)
	       sgl-points
	       (zigzag-line-points
		 (sub1 i)
		 (cons (coord-transform (* i period))
		   (cons (coord-transform (+ (* i period) zig))
		     sgl-points)))))))
	(zigzag-line-points (sub1 n) `((,(x-coord p2) ,(y-coord p2))))))))

;; centered-zigzag-line-points:
;;
;; sign + ... /\  /\  /\  /\  /\  /\  /\  /\ 
;;              \/  \/  \/  \/  \/  \/  \/  \/
;;               ---- (approx-zigzag-length)
;; sign - ...   /\  /\  /\  /\  /\  /\  /\  /\
;;            \/  \/  \/  \/  \/  \/  \/  \/ 

(define centered-zigzag-line-points
  (lambda (p1 p2 approx-zigzag-length sign)
    (let* ((l (- p2 p1))
	   (n (round (/ (magnitude l) approx-zigzag-length)))
	   (period (make-rectangular (/ (magnitude l) n) 0))
	   (delta (/ (magnitude period) 4))
	   (opp (if (eq? sign +) - +))
	   (zig (make-rectangular delta (sign delta)))
	   (zag (make-rectangular (* 3 delta) (opp delta)))
	   (coord-transform
	     (lambda (p)
	       (if (zero? p)
		 `(,(x-coord p1) ,(y-coord p1))
		 (let ((p-prime
			 (+ p1 (make-polar (magnitude p) (+ (angle l) (angle p))))))
		   `(,(x-coord p-prime) ,(y-coord p-prime)))))))
      (letrec
	((centered-zigzag-line-points
	   (lambda (i sgl-points)
	     (if (< i 0)
	       (cons `(,(x-coord p1) ,(y-coord p1)) sgl-points)
	       (centered-zigzag-line-points
		 (sub1 i)
		 (cons (coord-transform (+ (* i period) zig))
		   (cons (coord-transform (+ (* i period) zag))
		     sgl-points)))))))
	(centered-zigzag-line-points (sub1 n) `((,(x-coord p2) ,(y-coord p2))))))))

;;-------------------------------- Jagged lines --------------------------------
;; not used in Metacat

(define jagged-line
  (lambda (x1 y1 x2 y2 approx-jag-width)
    `(polyline ,@(jagged-line-points x1 y1 x2 y2 approx-jag-width))))

(define jagged-line-points
  (lambda (x1 y1 x2 y2 approx-jag-width)
    (let* ((x-len (- x2 x1))
	   (y-len (- y2 y1))
	   (short-len (min (abs x-len) (abs y-len)))
	   (n (max 5 (round (/ short-len approx-jag-width))))
	   (x-delta (/ x-len n))
	   (y-delta (/ y-len (add1 n))))
      (letrec
	((jagged-line-points
	   (lambda (i points)
	     (if (< i 0)
	       points
	       (jagged-line-points
		 (sub1 i)
		 (let ((x (+ x1 (* i x-delta)))
		       (y (+ y1 (* i y-delta))))
		   (cons `(,x ,y) (cons `(,x ,(+ y y-delta)) points))))))))
	(jagged-line-points n '())))))

;;------------------------------- Circular arcs ---------------------------------
;; not used in Metacat

;; These routines draw circular arcs clockwise from point x1,y1 to point x2,y2.
;; height is the maximum height of the arc from the line connecting the endpoints.

(define circular-arc
  (lambda (x1 y1 x2 y2 height)
    (if (<= height 0)
      `(line (,x1 ,y1) (,x2 ,y2))
      (let* ((p1 (make-rectangular x1 y1))
	     (p2 (make-rectangular x2 y2))
	     (l (- p2 p1))
	     (radius (+ (/ (^2 (magnitude l)) (* 8 height)) (/ height 2)))
	     (d (* 2 radius))
	     (alpha (* 2 (acos (- 1 (/ height radius)))))
	     (beta (/ (- pi alpha) 2))
	     (origin (+ p1 (make-polar radius (- (angle l) beta))))
	     (start (+ beta (angle l)))
	     (start-deg (* 180/pi start))
	     (alpha-deg (* 180/pi alpha)))
	`(arc (,(x-coord origin) ,(y-coord origin)) (,d ,d) ,start-deg ,alpha-deg)))))

(define dotted-circular-arc
  (lambda (x1 y1 x2 y2 height approx-interval-length)
    (cond
      ((<= height 0) (dotted-line x1 y1 x2 y2 approx-interval-length))
      ((or %nice-graphics% (not *tcl/tk-version-8_3?*))
	`(polypoints
	   ,@(circular-arc-points x1 y1 x2 y2 height approx-interval-length)))
      (else `(let-sgl ((line-style dotted))
	       ,(circular-arc x1 y1 x2 y2 height))))))

(define dashed-circular-arc
  (lambda (x1 y1 x2 y2 height)
    (cond
      ((<= height 0) (dashed-line x1 y1 x2 y2 %dash-density% %dash-length%))
      ;; dashed lines don't seem to work on the Mac even with Tcl/Tk 8.4.4,
      ;; so just use dashed-polypoints for now
      ((and *tcl/tk-version-8_3?* (not (eq? *platform* 'macintosh)))
       `(let-sgl ((line-style dashed))
	  ,(circular-arc x1 y1 x2 y2 height)))
      (else `(dashed-polypoints
	       ,@(circular-arc-points x1 y1 x2 y2 height %dot-interval%))))))

(define circular-arc-points
  (lambda (x1 y1 x2 y2 height approx-interval-length)
    (let* ((p1 (make-rectangular x1 y1))
	   (p2 (make-rectangular x2 y2))
	   (l (- p2 p1))
	   (r (+ (/ (^2 (magnitude l)) (* 8 height)) (/ height 2)))
	   (alpha (* 2 (acos (- 1 (/ height r)))))
	   (beta (/ (- pi alpha) 2))
	   (origin (+ p1 (make-polar r (- (angle l) beta))))
	   (start (+ beta (angle l)))
	   (arc-length (* r alpha))
	   (n (max 3 (round (/ arc-length approx-interval-length))))
	   (angle-delta (/ alpha n))
	   (arc-point
	     (lambda (i)
	       (let ((p (+ origin (make-polar r (+ (* i angle-delta) start)))))
		 `(,(x-coord p) ,(y-coord p))))))
      (letrec
	((circular-arc-points
	   (lambda (i points)
	     (if (< i 0)
	       points
	       (circular-arc-points (sub1 i) (cons (arc-point i) points))))))
	(circular-arc-points n '())))))

;;-------------------------- Elliptical arcs --------------------------

(define elliptical-arc
  (lambda (x1 y1 x2 y2 height)
    (let* ((y-orig (min y1 y2))
	   (y (abs (- y1 y2)))
	   (b (max y height)))
      (if (zero? b)
	`(line (,x1 ,y1) (,x2 ,y2))
	(let* ((root (sqrt (- 1 (/ (^2 y) (^2 b)))))
	       (a (/ (- x2 x1) (+ 1 root)))
	       (x-orig (if (= y1 y-orig) (+ x1 a) (- x2 a)))
	       (theta-deg (* 180/pi (acos root)))
	       (start-deg (if (= y1 y-orig) theta-deg 0)))
	  `(arc (,x-orig ,y-orig) (,(* 2 a) ,(* 2 b))
	     ,start-deg ,(- 180 theta-deg)))))))

(define dotted-elliptical-arc
  (lambda (x1 y1 x2 y2 height approx-interval-length)
    (cond
      ((and (= y1 y2) (<= height 0))
       (dotted-line x1 y1 x2 y2 approx-interval-length))
      ((or %nice-graphics% (not *tcl/tk-version-8_3?*))
	`(polypoints
	   ,@(elliptical-arc-points x1 y1 x2 y2 height approx-interval-length)))
      (else `(let-sgl ((line-style dotted))
	       ,(elliptical-arc x1 y1 x2 y2 height))))))

(define dashed-elliptical-arc
  (lambda (x1 y1 x2 y2 height)
    (cond
      ((and (= y1 y2) (<= height 0))
       (dashed-line x1 y1 x2 y2 %dash-density% %dash-length%))
      ;; dashed lines don't seem to work on the Mac even with Tcl/Tk 8.4.4,
      ;; so just use dashed-polypoints for now
      ((and *tcl/tk-version-8_3?* (not (eq? *platform* 'macintosh)))
       `(let-sgl ((line-style dashed))
	  ,(elliptical-arc x1 y1 x2 y2 height)))
      (else `(dashed-polypoints
	       ,@(elliptical-arc-points x1 y1 x2 y2 height %dot-interval%))))))

(define elliptical-arc-points
  (lambda (x1 y1 x2 y2 height approx-interval-length)
    (let* ((y-orig (min y1 y2))
	   (y (abs (- y1 y2)))
	   (b (max y height))
	   (root (sqrt (- 1 (/ (^2 y) (^2 b)))))
	   (a (/ (- x2 x1) (+ 1 root)))
	   (x-orig (if (= y1 y-orig) (+ x1 a) (- x2 a)))
	   (origin (make-rectangular x-orig y-orig))
	   (theta (acos root))
	   (alpha (- pi theta))
	   (start (if (= y1 y-orig) theta 0))
	   (arc-length (* (sqrt (* a b)) alpha))
	   (n (max 3 (round (/ arc-length approx-interval-length))))
	   (angle-delta (/ alpha n))
	   (arc-point
	     (lambda (i)
	       (let* ((phi (+ start (* i angle-delta)))
		      (p (+ origin (make-rectangular (* a (cos phi)) (* b (sin phi))))))
		 `(,(x-coord p) ,(y-coord p))))))
      (letrec
	((elliptical-arc-points
	   (lambda (i points)
	     (if (< i 0)
	       points
	       (elliptical-arc-points (sub1 i) (cons (arc-point i) points))))))
	(elliptical-arc-points n '())))))

;;------------------------------- Arrows ---------------------------------
;;
;; orientation-angle is measured counterclockwise from the horizontal
;; (0 degrees = pointing to the right); angles are specified in degrees.
;; (x0 y0) is the arrowhead tip coordinate.

(define arrowhead
  (lambda (x0 y0 orientation-angle arrowhead-length arrowhead-angle-size)
    (let* ((theta (* pi/180 orientation-angle))
	   (alpha (* pi/180 arrowhead-angle-size))
	   (half-width (* arrowhead-length (tan (/ alpha 2))))
	   (coord-transform
	    (lambda (x y)
	      (let ((x-prime (- (* x (cos theta)) (* y (sin theta))))
		    (y-prime (+ (* y (cos theta)) (* x (sin theta)))))
		`(,(+ x0 x-prime) ,(+ y0 y-prime))))))
      `(line (,x0 ,y0) ,(coord-transform (- arrowhead-length) (- half-width))
	     (,x0 ,y0) ,(coord-transform (- arrowhead-length) 0)
	     (,x0 ,y0) ,(coord-transform (- arrowhead-length) half-width)))))

(define centered-double-arrow
  (lambda (x y orientation-angle arrow-length arrow-width
		  arrowhead-length arrowhead-angle-size)
    (let* ((theta (* pi/180 orientation-angle))
	   (alpha (* pi/180 arrowhead-angle-size))
	   (half-arrowhead-width (* arrowhead-length (tan (/ alpha 2))))
	   (half-arrow-width (/ arrow-width 2))
	   (overhang (/ half-arrow-width (tan (/ alpha 2))))
	   (left (- (/ arrow-length 2)))
	   (right (/ arrow-length 2))
	   (right-line (- right overhang))
	   (coord-transform
	    (lambda (p)
	      (let ((x-prime (- (* (1st p) (cos theta)) (* (2nd p) (sin theta))))
		    (y-prime (+ (* (2nd p) (cos theta)) (* (1st p) (sin theta)))))
		`(,(+ x x-prime) ,(+ y y-prime)))))
	   (points
	    `((,left ,half-arrow-width) (,right-line ,half-arrow-width)
	      (,left ,(- half-arrow-width)) (,right-line ,(- half-arrow-width))
	      (,(- right arrowhead-length) ,half-arrowhead-width) (,right 0)
	      (,(- right arrowhead-length) ,(- half-arrowhead-width)) (,right 0))))
      `(line ,@(map coord-transform points)))))

(define centered-double-headed-double-arrow
  (lambda (x y orientation-angle arrow-length arrow-width
	    arrowhead-length arrowhead-angle-size)
    (let ((x-delta (* 1/4 arrow-length (cos (* pi/180 orientation-angle))))
	  (y-delta (* 1/4 arrow-length (sin (* pi/180 orientation-angle)))))
      `(let-sgl ()
	 ,(centered-double-arrow
	    (+ x x-delta) (+ y y-delta) orientation-angle (* 1/2 arrow-length)
	    arrow-width arrowhead-length arrowhead-angle-size)
	 ,(centered-double-arrow
	    (- x x-delta) (- y y-delta) (+ 180 orientation-angle) (* 1/2 arrow-length)
	    arrow-width arrowhead-length arrowhead-angle-size)))))

;;--------------------------------- Grids ------------------------------------
;; not used in Metacat

;; style = solid | dashed | dotted

(define grid
  (lambda (g style delta)
    (let* ((xmax (tell g 'get-x-max))
	   (ymax (tell g 'get-y-max))
	   (dot-interval (* 5 (tell g 'get-width-per-pixel)))
	   (line (lambda (x0 y0 x1 y1)
		   (case style
		     (solid `(line (,x0 ,y0) (,x1 ,y1)))
		     (dashed `(line (,x0 ,y0) (,x1 ,y1)))
		     (dotted (dotted-line x0 y0 x1 y1 dot-interval))))))
      (let grid ((x 0) (y 0) (lines '()))
	(cond
	  ((and (> x xmax) (> y ymax))
	   (if (eq? style 'dashed)
	     `(let-sgl ((line-style dashed)) ,@lines)
	     `(let-sgl () ,@lines)))
	  ((> x xmax) (grid x (+ y delta) (cons (line 0 y xmax y) lines)))
	  ((> y ymax) (grid (+ x delta) y (cons (line x 0 x ymax) lines)))
	  (else (grid (+ x delta) (+ y delta)
		  (cons (line 0 y xmax y) (cons (line x 0 x ymax) lines)))))))))
