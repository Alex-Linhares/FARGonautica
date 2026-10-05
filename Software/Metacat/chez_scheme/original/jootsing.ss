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

;; <theme-overlap-table> ::= ((<entry> <overlap-value>) ...)
;; <entry> ::= (<dimension> <relation>)

(define-codelet-procedure* jootser
  (lambda ()
    (if* (not %self-watching-enabled%)
      (say "Self-watching disabled. Fizzling.")
      (fizzle))
    (let ((last-event (tell *trace* 'get-last-event 'any)))
      (if* (not (exists? last-event))
	(vprintf "No last event. Fizzling.~n")
	(fizzle)))
    ;; Wait until not in a clamp period to check for repeated clamping:
    (if* (tell *trace* 'within-clamp-period?)
      (vprintf "Currently within a clamp period. Fizzling.~n")
      (fizzle))
    ;; Check for recurring clamps (manual clamps are ignored):
    (vprintf "Checking for recurring clamps...~n")
    (let ((clamps (filter-out-meth (get-most-recent-event-set 'clamp)
		    'clamp-type? 'manual-clamp)))
      (if (< (length clamps) 3)
	(vprintf "Didn't notice any recurring clamps...~n")
	(let ((clamp-type (tell (1st clamps) 'get-clamp-type))
	      (jootsing-probability (get-clamp-jootsing-probability clamps)))
	  (vprintf "Hmm...I'm beginning to notice a clamping pattern...~n")
	  (stochastic-if* jootsing-probability
	    (vprintf "Jootsing from ~as...~n" clamp-type)
	    (case clamp-type
	      (rule-codelet-clamp (joots-from-rule-codelet-clamps clamps))
	      (snag-response-clamp (joots-from-snag-response-clamps clamps))
	      (justify-clamp (joots-from-justify-clamps clamps)))
	    (fizzle))
	  (vprintf "Jootsing from clamps failed...~n"))))
    ;; Check for snags:
    (vprintf "Checking for recurring snags...~n")
    (let ((snags (get-most-recent-event-set 'snag)))
      (if* (< (length snags) 3)
	(vprintf "Didn't notice any recurring snags. Fizzling.~n")
	(fizzle))
      (let* ((num-of-snags (length snags))
	     (snag-theme-patterns (tell-all snags 'get-snag-theme-pattern))
	     (theme-overlap-table
	       (map (lambda (equivalent-entries)
		      (list (1st equivalent-entries)
			(100* (/ (length equivalent-entries) num-of-snags))))
		 (partition
		   theme-pattern-entries-equal?
		   (flatmap entries snag-theme-patterns))))
	     (max-theme-overlap (maximum (map 2nd theme-overlap-table)))
	     (jootsing-probability
	       (* (% max-theme-overlap) (% (min 100 (* 10 num-of-snags))))))
	(vprintf "Hmm...I'm beginning to notice a snag pattern...~n")
	(vprintf "Theme overlap table:~n")
	(vprintf "Max theme overlap = ~a~n" max-theme-overlap)
	(vprintf "Number of snags = ~a~n" num-of-snags)
	(vprintf "Snag jootsing probability = ~a~n"
	  (round-to-100ths jootsing-probability))
	(stochastic-if* (1- jootsing-probability)
	  (vprintf "Jootsing from snags failed. Fizzling.~n")
	  (fizzle))
	(if* (not (tell *trace* 'permission-to-clamp?))
	  (vprintf "Permission to clamp themes denied. Fizzling.~n")
	  (fizzle))
	(let* ((all-possible-pattern-entries
		 (remove-duplicates (flatmap entries snag-theme-patterns)))
	       (snag-objects (apply append (tell-all snags 'get-snag-objects)))
	       (snag-object-descriptions
		 (remq-duplicates
		   (apply append (tell-all snag-objects 'get-descriptions))))
	       (chosen-pattern-entries
		 (stochastic-filter
		   (lambda (entry)
		     (let* ((overlap (2nd (assoc entry theme-overlap-table)))
			    (snag-descriptions-for-theme
			      (filter-meth snag-object-descriptions
				'description-type? (1st entry)))
			    (average-description-depth
			      (average (tell-all snag-descriptions-for-theme
					 'get-conceptual-depth)))
			    (inclusion-probability
			      (* (% overlap) (% average-description-depth))))
		       (vprintf "~a entry: overlap = ~a, avg-dd = ~a, weight = ~a~n"
			 (reveal entry) overlap average-description-depth
			 (round-to-100ths inclusion-probability))
		       inclusion-probability))
		   all-possible-pattern-entries))
	       (negative-theme-pattern
		 (cons 'vertical-bridge
		   (map negate-theme-pattern-entry
		     chosen-pattern-entries))))
	  (if* (null? chosen-pattern-entries)
	    (vprintf "Couldn't make negative theme pattern. Fizzling.~n")
	    (fizzle))
	  (let ((clamp-event
		  (make-clamp-event 'snag-response-clamp
		    (list negative-theme-pattern %bottom-up-codelet-pattern%)
		    '() 'workspace)))
	    (tell *trace* 'add-event clamp-event)
	    (tell clamp-event 'activate)
	    (fizzle)))))))


(define get-clamp-jootsing-probability
  (lambda (clamps)
    (let ((clamp-type (tell (1st clamps) 'get-clamp-type))
	  (num-of-clamps (length clamps)))
      (let* ((elapsed-time-weights
	       (map (lambda (clamp)
		      (100* (/ (tell clamp 'get-time) *codelet-count*)))
		 clamps))
	     (average-progress
	       (round (weighted-average
			(tell-all clamps 'get-progress-achieved)
			elapsed-time-weights)))
	     (clamp-type-factor
	       (case clamp-type
		 (justify-clamp
		   (if (tell (tell *trace* 'get-last-event 'any) 'type? 'clamp) 1 0))
		 (snag-response-clamp 1)
		 (rule-codelet-clamp 0.5)))
	     (jootsing-probability
	       (* (1- (% average-progress))
		  (% (min 100 (* 10 num-of-clamps)))
		  clamp-type-factor)))
	(vprintf "progress values (last clamp first):  ~a~n"
	  (tell-all clamps 'get-progress-achieved))
	(vprintf "progress weights (last clamp first): ~a~n" elapsed-time-weights)
	(vprintf "average progress = ~a~n" average-progress)
	(vprintf "Clamp jootsing probability = ~a~n"
	  (round-to-100ths jootsing-probability))
	jootsing-probability))))


;; This function returns a list of equivalent snag or clamp events:
(define get-most-recent-event-set
  (lambda (event-type)
    (let* ((all-recent-events
	     (case event-type
	       (snag (tell *trace* 'get-new-events-since-last 'answer))
	       (clamp (tell *trace* 'get-all-events))))
	   (equivalent-event-sets
	     (partition
	       (lambda (ev1 ev2) (tell ev1 'equal? ev2))
	       (filter-meth all-recent-events 'type? event-type)))
	   (most-recent-event-set
	     (select-extreme min
	       (lambda (events) (minimum (tell-all events 'get-age)))
	       equivalent-event-sets)))
      (if (exists? most-recent-event-set)
	most-recent-event-set
	'()))))


(define joots-from-rule-codelet-clamps
  (lambda (clamps)
    (tell *comment-window* 'add-comment
      (list "I just can't seem to come up with any better rules.")
      (list "Jootsing from unsuccessful rule-codelet clamps."))
    (give-up)))


(define joots-from-snag-response-clamps
  (lambda (clamps)
    (tell *comment-window* 'add-comment
      (list "This is getting boring.  I can't think of anything else to try.")
      (list "Jootsing from unsuccessful snag-response clamps."))
    (give-up)))


(define joots-from-justify-clamps
  (lambda (clamps)
    (let ((top-rule (tell (1st clamps) 'get-rule 'top))
	  (bottom-rule (tell (1st clamps) 'get-rule 'bottom)))
      (if* (tell *memory* 'answer-present?
	     (tell *answer-string* 'get-letter-categories)
	     top-rule bottom-rule)
	(say "Already justified this answer. Fizzling.")
	(fizzle))
      ;; Try to give up:
      (if* (or (not (tell top-rule 'currently-works?))
	       (not (tell bottom-rule 'currently-works?)))
	(vprintf "Can't give up. Rule(s) don't currently work. Fizzling.~n")
	(fizzle))
      (let ((result (translate top-rule)))
	(if* (not (exists? result))
	  (vprintf "Can't give up. Couldn't translate rule. Fizzling.~n")
	  (fizzle))
	(let* ((translated-rule (1st result))
	       (supporting-vertical-bridges (2nd result))
	       (slippage-log (3rd result))
	       (vertical-mapping-supporting-groups (4th result))
	       ;; Workspace-strings (if any) have already been filtered out:
	       (top-rule-ref-objects (5th result))
	       (translated-rule-ref-objects (6th result))
	       (unjustified-slippages
		 (get-unifying-slippages translated-rule bottom-rule)))
	  (if* (null? unjustified-slippages)
	    (vprintf "No unjustified slippages. Posting answer-justifier.~n")
	    (post-codelet* urgency: %extremely-high-urgency% answer-justifier)
	    (fizzle))
	  (stochastic-if* (1- (/ 1 (length unjustified-slippages)))
	    (vprintf "Too many unjustified slippages. Fizzling.~n")
	    (fizzle))
	  ;; Time to give up:
	  (let ((all-supporting-groups
		  (remq-duplicates
		    (append
		      vertical-mapping-supporting-groups
		      (get-rule-supporting-groups top-rule bottom-rule))))
		(bottom-rule-ref-objects
		  (filter-out workspace-string?
		    (tell *target-string* 'get-all-reference-objects bottom-rule))))
	    (report-new-answer
	      *answer-string* top-rule bottom-rule supporting-vertical-bridges
	      all-supporting-groups top-rule-ref-objects bottom-rule-ref-objects
	      slippage-log unjustified-slippages)))))))


;;----------------------------------------------------------------------------

(define %satisfactory-rule-quality% 80)

;; This is the length of time that must pass after the occurrence of any event
;; before progress-watcher codelets can check on the current progress:
(define %settling-period% 250)

;; This is the maximum length of time a clamp lasts.  Clamps can be undone
;; before the end of this period by progress-watcher codelets:
(define %max-clamp-period% 750)

;; This is the length of time after unclamping during which no new clamps
;; can be made:
(define %grace-period% 100)


(define-codelet-procedure* progress-watcher
  (lambda ()
    (vprintf "Progress Watcher running (time ~a)...~n" *codelet-count*)
    (if* (not %self-watching-enabled%)
      (vprintf "Self-watching disabled. Fizzling.~n")
      (fizzle))
    (if* (tell *trace* 'within-clamp-period?)
      (let ((time-since-last-event (tell *trace* 'get-elapsed-time 'any)))
	(if* (> time-since-last-event %settling-period%)
	  (let ((last-clamp (tell *trace* 'get-last-event 'clamp)))
	    (tell *trace* 'undo-last-clamp)
	    (let ((progress-achieved (tell last-clamp 'get-progress-achieved)))
	      (vprintf "Progress achieved = ~a~n" progress-achieved)
	      (stochastic-if* (% progress-achieved)
		(post-codelet*
		  urgency: progress-achieved
		  (if %justify-mode% answer-justifier answer-finder)))
	      (fizzle))))
	(vprintf "Too soon since last event to draw any conclusions. Fizzling.~n")
	(fizzle)))
    ;; Not within a clamp period.
    (let ((current-activity (tell *workspace* 'get-activity)))
      (if* (zero? current-activity)
	(vprintf "~n*******************************~n")
	(vprintf "*  Nothing much is happening  *~n")
	(vprintf "*******************************~n")
	(vprintf "Checking on current rules...~n")
	(let* ((top-rules (tell *workspace* 'get-rules 'top))
	       (bottom-rules (tell *workspace* 'get-rules 'bottom))
	       (max-top-rule-quality
		 (maximum (tell-all top-rules 'get-quality)))
	       (max-bottom-rule-quality
		 (maximum (tell-all bottom-rules 'get-quality)))
	       (poor-top-rule-quality?
		 (< max-top-rule-quality %satisfactory-rule-quality%))
	       (poor-bottom-rule-quality?
		 (and %justify-mode%
		      (< max-bottom-rule-quality %satisfactory-rule-quality%))))
	  (if* (or poor-top-rule-quality? poor-bottom-rule-quality?)
	    (vprintf "Current rules are not good enough.~n")
	    (vprintf "Attempting to clamp codelet pattern...~n")
	    (if* (not (tell *trace* 'permission-to-clamp?))
	      (vprintf "Permission to clamp pattern denied. Fizzling.~n")
	      (fizzle))
	    (let ((clamp-probability
		    (if %justify-mode%
		      (1- (% (min max-top-rule-quality max-bottom-rule-quality)))
		      (1- (% max-top-rule-quality)))))
	      (vprintf "max top-rule quality = ~a~n" max-top-rule-quality)
	      (vprintf "max bottom-rule quality = ~a~n" max-bottom-rule-quality)
	      (vprintf "Clamp probability = ~a~n" (round-to-100ths clamp-probability))
	      (stochastic-if* clamp-probability
		(let ((how-strings-change-description
			(cond
			  ((and poor-top-rule-quality? poor-bottom-rule-quality?)
			   (format "either ~a or ~a."
			     (how-strings-change *initial-string* *modified-string*)
			     (how-strings-change *target-string* *answer-string*)))
			  (poor-top-rule-quality?
			    (format "~a."
			      (how-strings-change *initial-string* *modified-string*)))
			  (poor-bottom-rule-quality?
			    (format "~a."
			      (how-strings-change *target-string* *answer-string*))))))
		  (tell *comment-window* 'add-comment
		    (list
		      "I'm getting frustrated.  I still don't see a good way to describe "
		      how-strings-change-description)
		    (list
		      "No satisfactory rules yet exist for describing "
		      how-strings-change-description)))
		(let* ((clamped-rule-codelet-pattern
			 (against-background %very-low-urgency% %rule-codelet-pattern%))
		       (clamp-event
			 (make-clamp-event 'rule-codelet-clamp
			   ;; Pay attention to new rule events:
			   (list clamped-rule-codelet-pattern) '() 'rule)))
		  (tell *trace* 'add-event clamp-event)
		  (tell clamp-event 'activate)
		  (fizzle)))
	      (vprintf "Permission granted but attempt failed. Fizzling.~n")
	      (fizzle)))
	  (vprintf "The rules seem to be of decent quality. Fizzling.~n"))))))


(define how-strings-change
  (lambda (string1 string2)
    (format "how ~a changes to ~a"
      (quoted-string string1)
      (quoted-string string2))))
