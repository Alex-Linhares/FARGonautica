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

(define %comment-window-font% (make-mfont sans-serif 12 '(bold italic)))
(define %comment-window-reminder-font% (make-mfont serif 12 '(normal)))

(define make-comment-window
  (lambda optional-args
    (let* ((width
	     (if (null? optional-args)
	       %default-comment-window-width%
	       (1st optional-args)))
	   (height
	     (if (< (length optional-args) 2)
	       %default-comment-window-height%
	       (2nd optional-args)))
	   (window (new-comment-window width height)))
      (tell window 'initialize)
      window)))

(define new-comment-window
  (lambda (x-pixels y-pixels)
    (let ((text-window
	    (make-scrollable-text-window
	      x-pixels y-pixels %virtual-comment-window-length%
	      %comment-window-background-color%)))
      (tell text-window 'set-icon-label %comment-window-icon-label%)
      (if* (exists? %comment-window-icon-image%)
	(tell text-window 'set-icon-image %comment-window-icon-image%))
      (if* (exists? %comment-window-title%)
	(tell text-window 'set-window-title %comment-window-title%))
      (let* ((reminder-y
	       (* 1/2 (+ (tell text-window 'get-y-max)
			 (tell text-window 'get-visible-y-min))))
	     (reminder-message
	       `(let-sgl ((font ,%comment-window-reminder-font%)
			  (text-justification center))
		  (text (1/2 ,reminder-y) "(Move scroll bar to bottom)")))
	     (eliza-paragraphs '())
	     (non-eliza-paragraphs '()))
	(lambda msg
	  (let ((self (1st msg)))
	    (record-case (rest msg)
	      (object-type () 'comment-window)
	      (new-problem (initial-sym modified-sym target-sym answer-sym)
		(tell self 'add-comment
		  (if %justify-mode%
		    (list
		      (format "Let's see... \"~a\" changes to \"~a\", and"
			      initial-sym modified-sym)
		      (format " \"~a\" changes to \"~a\".  Hmm..."
			      target-sym answer-sym))
		    (list
		      (format "Okay, if \"~a\" changes to \"~a\", what"
			      initial-sym modified-sym)
		      (format " does \"~a\" change to?  Hmm..." target-sym)))
		  (if %justify-mode%
		    (list
		      (format "Beginning justify run:  \"~a\" changes to \"~a\", and"
			      initial-sym modified-sym)
		      (format " \"~a\" changes to \"~a\"..."
			      target-sym answer-sym))
		    (list
		      (format "Beginning run:  If \"~a\" changes to \"~a\", what"
			      initial-sym modified-sym)
		      (format " does \"~a\" change to?" target-sym))))
		'done)
	      (add-comment (lines1 lines2)
		(let ((paragraph1 (apply string-append lines1))
		      (paragraph2 (apply string-append lines2)))
		  (set! eliza-paragraphs (cons 1 (cons paragraph1 eliza-paragraphs)))
		  (set! non-eliza-paragraphs (cons 1 (cons paragraph2 non-eliza-paragraphs)))
		  (tell text-window 'draw-paragraph
		    (if %eliza-mode% paragraph1 paragraph2))))
	      (switch-modes ()
		(tell text-window 'set-paragraphs
		  (if %eliza-mode% eliza-paragraphs non-eliza-paragraphs))
		(tell text-window 'redraw))
	      (clear ()
		(set! eliza-paragraphs '())
		(set! non-eliza-paragraphs '())
		(tell text-window 'clear))
	      (initialize ()
		(tell self 'clear)
		(tell text-window 'new-font %comment-window-font%)
		(tell text-window 'centering-off)
		(tell text-window 'draw reminder-message))
	      (else (delegate msg text-window)))))))))
