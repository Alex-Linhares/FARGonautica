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

(define temp-adjusted-probability
  (lambda (prob)
    (cond
      ((= prob 0.0) 0.0)
      ((<= prob 0.5)
       (let ((low-prob-factor (max 1.0 (truncate (abs (log10 prob))))))
	 (min 0.5 (+ prob (* (% (10- (sqrt (100- *temperature*))))
			     (- (expt 10 (1- low-prob-factor)) prob))))))
      ((> prob 0.5)
       (max 0.5 (1- (+ (1- prob) (* (% (10- (sqrt (100- *temperature*)))) prob))))))))


(define temp-adjusted-values
  (lambda (value-list)
    (let ((exponent (+ (/ (100- *temperature*) 30) 0.5)))
      (map (lambda (value) (round (expt value exponent))) value-list))))


(define current-translation-temperature-threshold-distribution
  (lambda ()
    (let* ((i-length (tell *initial-string* 'get-length))
	   (m-length (tell *modified-string* 'get-length))
	   (t-length (tell *target-string* 'get-length))
	   (bond-density
	     (if (and (= i-length 1) (= m-length 1) (= t-length 1))
	       1.0
	       (/ (+ (length (tell *initial-string* 'get-bonds))
		     (length (tell *modified-string* 'get-bonds))
		     (length (tell *target-string* 'get-bonds)))
		  (+ (sub1 i-length) (sub1 m-length) (sub1 t-length))))))
      (cond
	((>= bond-density 0.8)
	  %very-low-translation-temperature-threshold-distribution%)
	((>= bond-density 0.6)
	  %low-translation-temperature-threshold-distribution%)
	((>= bond-density 0.4)
	  %medium-translation-temperature-threshold-distribution%)
	((>= bond-density 0.2)
	  %high-translation-temperature-threshold-distribution%)
	(else %very-high-translation-temperature-threshold-distribution%)))))


(define update-temperature
  (lambda ()
    (if* (not *temperature-clamped?*)
      (let ((rule-factor
	      (if (or (and %justify-mode%
			   (tell *workspace* 'rule-possible? 'top)
			   (tell *workspace* 'supported-rule-exists? 'top)
			   (tell *workspace* 'rule-possible? 'bottom)
			   (tell *workspace* 'supported-rule-exists? 'bottom))
		      (and (not %justify-mode%)
			   (tell *workspace* 'rule-possible? 'top)
			   (tell *workspace* 'supported-rule-exists? 'top)))
		0
		100)))
	(set! *temperature*
          (round (weighted-average
                   (list (tell *workspace* 'get-average-unhappiness) rule-factor)
                   (list 70 30))))))))
