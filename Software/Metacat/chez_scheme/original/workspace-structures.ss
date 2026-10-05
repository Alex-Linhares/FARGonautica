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

(define make-workspace-structure
  (lambda ()
    (let ((time-stamp *codelet-count*)
	  (enclosing-group #f)
	  (strength 0)
	  (proposal-level 0)
	  (graphics-pexp #f)
	  (drawn? #f))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'workspace-structure)
	    (drawn? () drawn?)
	    (set-drawn? (new-value) (set! drawn? new-value) 'done)
	    (get-graphics-pexp () graphics-pexp)
	    (set-graphics-pexp (pexp) (set! graphics-pexp pexp) 'done)
	    (get-time-stamp () time-stamp)
	    ;; This is the time since the structure was proposed (not built):
	    (get-age () (- *codelet-count* time-stamp))
	    (get-enclosing-group () enclosing-group)
	    (get-strength () strength)
	    (get-weakness () (100- (expt strength 0.95)))
	    (get-proposal-level () proposal-level)
	    (proposed? () (< proposal-level %built%))
	    (update-proposal-level (new-level)
              (set! proposal-level new-level)
	      'done)
	    (update-enclosing-group (new-group)
	      (set! enclosing-group new-group)
	      'done)
	    (update-strength ()
	      (let* ((internal-strength (tell self 'calculate-internal-strength))
		     (external-strength (tell self 'calculate-external-strength))
		     (intrinsic-strength
		       (weighted-average
			 (list internal-strength external-strength)
			 (list internal-strength (100- internal-strength))))
		     (compatibility (tell self 'get-thematic-compatibility))
		     (thematic-weight (abs compatibility)))
		(set! strength
		  (round
		    (weighted-average
		      (list (if (> compatibility 0) 100 0) intrinsic-strength)
		      (list thematic-weight (1- thematic-weight))))))
	      'done)
	    ;; For structures that lack their own local method:
	    (get-thematic-compatibility () 0)
	    (else (delegate msg base-object))))))))


(define wins-fight?
  (lambda (challenger challenger-weight defender defender-weight)
    (tell challenger 'update-strength)
    (tell defender 'update-strength)
    (stochastic-pick
      '(#t #f)
      (temp-adjusted-values
	(list (* challenger-weight (tell challenger 'get-strength))
	      (* defender-weight (tell defender 'get-strength)))))))


(define wins-all-fights?
  (lambda (challenger challenger-weight defending-structures defender-weight/s)
    (if (list? defender-weight/s)
	(andmap
	  (lambda (defender defender-weight)
	    (wins-fight? challenger challenger-weight defender defender-weight))
	  defending-structures
	  defender-weight/s)
	(andmap
	  (lambda (defender)
	    (wins-fight? challenger challenger-weight defender defender-weight/s))
	  defending-structures))))
