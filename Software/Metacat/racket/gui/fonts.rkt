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
;; Ported to Racket, 2026: fonts.ss on racket/draw.  SWL's <font> (a Tk
;; font) becomes swl-font% below, which keeps the requested face, size and
;; style and makes the racket/draw font% it stands for.  Text is measured on
;; a private bitmap dc instead of the logo window's hidden Tk canvas, so
;; get-pixel-size works without (create-mcat-logo); the logo window itself
;; needs racket/gui and comes with the control panel.  Changes are marked
;; "port:" and listed in docs/porting-notes.md, item 12.
;;=============================================================================

(require racket/class
         (only-in racket/draw make-font make-bitmap bitmap-dc% the-font-list
                  get-face-list)
         "../compat.rkt"
         "../utilities.rkt")

(provide swl-font swl-font% swl:font-families
         make-mfont get-actual-font-values get-actual-font-size make-fixed-font
         *hidden-canvas* *scrollbar-width* *scrollbar-height* set-scrollbar-size!
         *serif-faces* *sans-serif-faces* *fancy-faces* select-face
         serif sans-serif fancy
         tk-points->pixels)

;;-----------------------------------------------------------------------------
;; port: the SWL/Tk side

;; Tk sizes: positive = points, negative = pixels.  Tk converts points at the
;; screen's resolution; the port fixes it at 96 dpi, so that offscreen
;; renderings do not depend on the display.
(define tk-points->pixels
  (lambda (size)
    (if (< size 0) (- size) (round (* size 96/72)))))

(define tk-pixels->points
  (lambda (px) (round (* px 72/96))))

;; Tk's family names, as the lower-case symbols fonts.ss compares with
(define swl:font-families
  (let ((families #f))
    (lambda ()
      (unless families
        (set! families
          (map (lambda (s) (string->symbol (string-downcase s)))
               (get-face-list 'all #:all-variants? #f))))
      families)))

(define face->family
  (lambda (face)
    (cond
      ((memq face '(times |times new roman| palatino |palatino linotype|
                    |new century schoolbook| |bookman old style| georgia
                    |book antiqua|))
       'roman)
      ((memq face '(helvetica arial |small fonts|)) 'swiss)
      ((memq face '(courier |courier new|)) 'modern)
      (else 'default))))

;; (create <font> face size style): style is a list of symbols, from
;; {normal bold italic roman underline overstrike}
(define swl-font%
  (class object%
    (init-field face size style)
    (super-new)
    (define font
      (make-font #:size (tk-points->pixels size)
                 #:size-in-pixels? #t
                 #:face (if (symbol? face) (symbol->string face) face)
                 #:family (face->family face)
                 #:weight (if (memq 'bold style) 'bold 'normal)
                 #:style (if (memq 'italic style) 'italic 'normal)
                 #:underlined? (and (memq 'underline style) #t)
                 ;; aliased, as X11's core fonts drew Tk's text in the
                 ;; dissertation's screenshots; the graphics erase text by
                 ;; drawing it again in the background colour, which leaves
                 ;; grey fringes around antialiased text (item 13)
                 #:smoothing 'unsmoothed))
    (define/public (get-font) font)
    (define/public (get-family) face)
    (define/public (get-size) size)
    (define/public (get-style) style)
    ;; Tk's `font actual': the port reports the request, sizes in points
    (define/public (get-actual-values)
      (values face
              (if (< size 0) (tk-pixels->points (- size)) size)
              (if (memq 'bold style) 'bold 'normal)))))

;; port: a bitmap dc stands for the hidden Tk canvas
(define *hidden-canvas*
  (new bitmap-dc% (bitmap (make-bitmap 4 4))))

;; Tk's `bbox' of a text item anchored nw: the layout's width and line space
(define measure-text
  (lambda (string font)
    (let-values (((w h d a) (send *hidden-canvas* get-text-extent string
                                  (send font get-font) #t)))
      (list (inexact->exact (ceiling w)) (inexact->exact (ceiling h))))))

;;-----------------------------------------------------------------------------

(define swl-font
  (lambda (face size . style)
    (if (or (null? style) (symbol? (car style)))
      (new swl-font% (face face) (size size) (style style))         ;; port: create <font>
      (new swl-font% (face face) (size size) (style (car style))))))

;; port: set by the control panel's create-mcat-logo (racket/gui)
(define *scrollbar-width* #f)
(define *scrollbar-height* #f)
(define (set-scrollbar-size! w h) (set! *scrollbar-width* w) (set! *scrollbar-height* h))

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
		(let* ((bb (measure-text string font))     ;; port: was Tk's bbox
		       (width (car bb))
		       (height (cadr bb))
		       (baseline-offset (round (* 1/5 height))))
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
