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

(define swl-font
  (lambda (face size . style)
    (if (or (null? style) (symbol? (car style)))
      (create <font> face size style)
      (create <font> face size (car style)))))

(define *scrollbar-width* #f)
(define *scrollbar-height* #f)

;; the hidden canvas is used by fonts to compute dimensions of text
(define *mcat-logo* #f)
(define *hidden-canvas* #f)

(define create-mcat-logo
  (lambda ()
    (let* ((top (create <toplevel> with
		  (width: 110)
		  (height: 80)
		  (title: "Logo")
		  (resizable: #f #f)
		  (destroy-request-handler: toplevel-destroy-action)))
	   (frame (create <scrollframe> top))
	   (logo (create <canvas> frame with
		   (width: 110)
		   (height: 80)
		   (background-color: %logo-background-color%)))
	   (hidden (create <canvas> frame with (width: 110) (height: 80))))
      (pack frame (expand: #t) (fill: 'both))
      ;; force creation of scrollbars
      (send logo set-scroll-region! 0 0 120 90)
      ;; for some reason, need to pause before asking scrollbars for their size,
      ;; otherwise the answers returned may be incorrect.  this is an obviously
      ;; unreliable workaround.
      (pause 500)
      (set! *scrollbar-width*
	(send (get-scrollbar-from-frame frame 'vertical #t) get-width))
      (set! *scrollbar-height*
	(send (get-scrollbar-from-frame frame 'horizontal #t) get-height))
      ;; get rid of scrollbars
      (send logo set-scroll-region! 0 0 110 80)
      (swl:tcl-eval logo 'create 'text 55 50
	'-text "Metacat" '-anchor 's '-font %logo-font%)
      (set! *mcat-logo* logo)
      (set! *hidden-canvas* hidden)
      'done)))

;; positive size value indicates font size in points, negative value
;; indicates font size in pixels

(define make-mfont
  (lambda (face size style)
    (let ((font (make-fixed-font face size style)))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'mfont)
	    (resize (new-size)
	      (set! font (make-fixed-font face (- new-size) style))
	      'done)
	    (else (delegate msg font))))))))

(define get-actual-font-values
  (lambda (font)
    (call-with-values
      (lambda () (send font get-actual-values))
      list)))

(define get-actual-font-size
  (lambda (font)
    (cadr (get-actual-font-values font))))

(define make-fixed-font
  (lambda (face size style)
    (let ((font (if (string? face)
		  (swl-font (string->symbol face) size style)
		  (swl-font face size style))))
      (if (and (< (get-actual-font-size font) 7)
	       (member '|small fonts| (swl:font-families)))
	(begin
;;	  (printf "switching from ~s~n" (get-actual-font-values font))
	  (set! font (swl-font '|small fonts| (get-actual-font-size font) 'normal))
;;	  (printf "            to ~s~n" (get-actual-font-values font))
	  'done))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'fixed-font)
	    (print ()
	      (printf "Font requested: ~s~n" (list face size style))
	      (printf "Font assigned:  ~s~n" (get-actual-font-values font))
	      (if *hidden-canvas*
		(let ((size (tell self 'get-pixel-size "M")))
		  (printf "M pixel matrix: ~a x ~a, offset ~a~n"
		    (car size) (cadr size) (caddr size)))
		(printf "(pixel matrix info unavailable)~n")))
	    (get-swl-font () font)
	    (get-actual-values () (get-actual-font-values font))
	    (get-face () (car (get-actual-font-values font)))
	    (get-size () (cadr (get-actual-font-values font)))
	    (get-style () (caddr (get-actual-font-values font)))
	    (get-pixel-size (string)
	      (if *hidden-canvas*
		(let* ((bb (swl:tcl->scheme
			     (swl:tcl-eval *hidden-canvas* 'bbox
			       (swl:tcl-eval *hidden-canvas* 'create 'text 0 0
				 '-text string '-anchor 'nw '-font font))))
		       (width (- (caddr bb) (car bb)))
		       (height (- (cadddr bb) (cadr bb)))
		       (baseline-offset (round (* 1/5 height))))
		  (swl:tcl-eval *hidden-canvas* 'delete 'all)
		  (list width height baseline-offset))
		(error #f "need to run (create-mcat-logo) first")))
	    (get-pixel-width (text)
	      (car (tell self 'get-pixel-size text)))
	    (get-pixel-height ()
	      (cadr (tell self 'get-pixel-size "M")))
	    (get-baseline-offset ()
	      (caddr (tell self 'get-pixel-size "M")))
	    (show-char-info ()
	      (printf "Character widths:")
	      (let loop ((ascii 32))
		(if (< ascii 127)
		  (begin
		    (if (= (modulo ascii 4) 0) (newline))
		    (let* ((char (string (integer->char ascii)))
			   (width (tell self 'get-pixel-width char)))
		      (if (= ascii 32)
			(printf "  space ~a" width)
			(printf "      ~a ~a" char width)))
		    (loop (+ ascii 1)))
		  (newline)))
	      (printf "Character height: ~a~n" (tell self 'get-pixel-height))
	      (printf "Baseline offset: ~a~n" (tell self 'get-baseline-offset)))
	    (else (delegate msg base-object))))))))


(define *serif-faces*
  '(|times new roman|
    times))

(define *sans-serif-faces*
  '(helvetica
    arial))

(define *fancy-faces*
  '(|palatino linotype|
    palatino
    |new century schoolbook|
    |times new roman|
    times
    |bookman old style|
    georgia
    |book antiqua|))

(define select-face
  (lambda (preferences available style)
    (cond
      ((null? preferences)
;;       (printf "~nWarning: all requested ~a fonts are unavailable~n~n" style)
       (case style
	 ((serif) 'times)
	 ((sans-serif) 'helvetica)
	 ((fancy) 'times)))
      ((member (car preferences) available) (car preferences))
      (else (select-face (cdr preferences) available style)))))

(define serif (select-face *serif-faces* (swl:font-families) 'serif))
(define sans-serif (select-face *sans-serif-faces* (swl:font-families) 'sans-serif))
(define fancy (select-face *fancy-faces* (swl:font-families) 'fancy))
