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
;; Ported to Racket, 2026: included by racket/engine.rkt (see docs/porting-notes.md,
;; item 04).  Only the model's constants are here; the graphics constants
;; (window sizes, colours, fonts, titles) are in racket/gui/constants.rktl.
;;=============================================================================

;;----------------------------------------------------------------------
;; Probability distributions

(define make-probability-distribution
  (lambda (values distribution-frequency-values)
    (lambda msg
      (record-case (rest msg)
	(object-type () 'probability-distribution)
	(choose-value () (stochastic-pick values distribution-frequency-values))
	(else (delegate msg base-object))))))


(define %very-low-translation-temperature-threshold-distribution%
  (make-probability-distribution
    '(10  20  30  40  50  60  70  80  90 100)
    '( 5  150  5   2   1   1   1   1   1   1)))


(define %low-translation-temperature-threshold-distribution%
  (make-probability-distribution
    '(10  20  30  40  50  60  70  80  90 100)
    '( 2   5  150  5   2   1   1   1   1   1)))


(define %medium-translation-temperature-threshold-distribution%
  (make-probability-distribution
    '(10  20  30  40  50  60  70  80  90 100)
    '( 1   2   5  150  5   2   1   1   1   1)))


(define %high-translation-temperature-threshold-distribution%
  (make-probability-distribution
    '(10  20  30  40  50  60  70  80  90 100)
    '( 1   1   2   5  150  5   2   1   1   1)))


(define %very-high-translation-temperature-threshold-distribution%
  (make-probability-distribution
    '(10  20  30  40  50  60  70  80  90 100)
    '( 1   1   1   2   5  150  5   2   1   1)))
