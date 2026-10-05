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

(define-codelet-procedure* breaker
  (lambda ()
    (stochastic-if* (% (100- *temperature*))
      (say "Temperature is too low. Fizzling.")
      (fizzle))
    (let ((breakable-structures
	    (filter-out rule? (tell *workspace* 'get-structures))))
      (if* (null? breakable-structures)
	(say "Couldn't choose structure. Fizzling.")
	(fizzle))
      (let* ((structure (random-pick breakable-structures))
	     (enclosing-group (tell structure 'get-enclosing-group)))
	(if (and (bond? structure)
		 (exists? enclosing-group))
	    (let ((p1 (temp-adjusted-probability
			(% (tell structure 'get-weakness))))
		  (p2 (temp-adjusted-probability
			(% (tell enclosing-group 'get-weakness)))))
	      (stochastic-if* (* p1 p2)
		(break-group enclosing-group)
		(break-bond structure)))
	    (stochastic-if* (temp-adjusted-probability
			      (% (tell structure 'get-weakness)))
	      (case (tell structure 'object-type)
		(bond (break-bond structure))
		(group (break-group structure))
		(bridge (break-bridge structure)))))
	'done))))
