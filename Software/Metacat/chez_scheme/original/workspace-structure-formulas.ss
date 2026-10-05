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


(define length-description-probability
  (lambda (group)
    (let ((group-length (tell group 'get-group-length)))
      (cond
	((> group-length 5) 0)
	((= group-length 1) 1)
	(else (temp-adjusted-probability
		(expt 0.5 (* (^3 group-length)
			     (% (100- (tell plato-length 'get-activation)))))))))))


(define single-letter-group-probability
  (lambda (group)
    (let ((exponent (case (tell group 'get-num-of-local-supporting-groups)
		      (1 4)
		      (2 2)
		      (else 1))))
      (temp-adjusted-probability
	(expt (* (% (tell group 'get-local-support))
		 (% (tell plato-length 'get-activation)))
	  exponent)))))


(define descriptor-support
  (lambda (descriptor string)
    (let* ((groups (tell string 'get-groups))
	   (num-of-groups (length groups))
	   (num-of-described-groups
	     (count
	       (lambda (group) (tell group 'descriptor-present? descriptor))
	       groups)))
      (if (zero? num-of-groups)
	0
	(100* (/ num-of-described-groups num-of-groups))))))


(define description-type-support
  (lambda (description-type string)
    (let* ((objects (tell string 'get-objects))
	   (num-of-objects (length objects))
	   (num-of-described-objects
	     (count
	       (lambda (object)
		 (tell object 'description-type-present? description-type))
	       objects))
	   (local-support (100* (/ num-of-described-objects num-of-objects))))
      (round (average local-support (tell description-type 'get-activation))))))
