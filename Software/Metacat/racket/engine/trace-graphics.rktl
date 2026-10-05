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
;; Ported to Racket, 2026: group-event-pexp-text-string from
;; trace-graphics.ss, verbatim.  Part of the engine (racket/engine.rkt),
;; since trace.ss's make-group-event names every group event with it; the
;; rest of trace-graphics.ss (the Trace window) is in
;; racket/gui/trace-graphics.rktl (docs/porting-notes.md, item 14).

(define group-event-pexp-text-string
  (lambda (group)
    (let* ((bond-facet (tell group 'get-bond-facet))
	   (constituent-objects (tell group 'get-constituent-objects))
	   (descriptors (tell-all constituent-objects 'get-descriptor-for bond-facet))
	   (descriptor-strings
	     (map (lambda (object descriptor)
		    (cond
		      ((platonic-number? descriptor)
		       (format "~a" (platonic-number->number descriptor)))
		      ((letter? object) (tell descriptor 'get-lowercase-name))
		      ((group? object) (tell descriptor 'get-uppercase-name))))
	       constituent-objects
	       descriptors)))
      (apply string-append
	(cons (1st descriptor-strings)
	  (adjacency-map
	    (lambda (x y) (format "-~a" y))
	    descriptor-strings))))))
