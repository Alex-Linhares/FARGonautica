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

(define-codelet-procedure* answer-justifier
  (lambda ()
    (let ((top-map-strength (tell *workspace* 'get-mapping-strength 'top))
	  (vertical-map-strength (tell *workspace* 'get-mapping-strength 'vertical))
	  (bottom-map-strength (tell *workspace* 'get-mapping-strength 'bottom)))
      (let ((chosen-rule
	      (stochastic-pick-by-method (tell *workspace* 'get-all-supported-rules)
		'get-strength)))
	(if* (not (exists? chosen-rule))
	  (vprintf "Couldn't find a supported rule. Fizzling.~n")
	  (fizzle))

	(vprintf "Chose rule:~n")
	(vprint chosen-rule)
	(let* ((rule-type (tell chosen-rule 'get-rule-type))
	       (other-rules
		 (tell *workspace* 'get-rules (if (eq? rule-type 'top) 'bottom 'top)))
	       (result (translate chosen-rule)))

	  (if* (not (exists? result))
	    (vprintf "Couldn't translate chosen rule...~n"))
	  ;; ...and skip directly to unification section

	  (if* (exists? result)
	    (let* ((translated-rule (1st result))
		   (supporting-vertical-bridges (2nd result))
		   (slippage-log (3rd result))
		   (vertical-mapping-supporting-groups (4th result))
		   (from-ref-objs (5th result))
		   (to-ref-objs (6th result))
		   (ref-objs1 (if (eq? rule-type 'top) from-ref-objs to-ref-objs))
		   (ref-objs2 (if (eq? rule-type 'top) to-ref-objs from-ref-objs))
		   (matching-rule (select-meth other-rules 'equal? translated-rule)))

	      (vprintf "Chosen rule translated:~n")
	      (vprint translated-rule)

	      (if* (exists? matching-rule)
		(vprintf "Found a matching rule:~n")
		(vprint matching-rule)
		(let ((rule1 (if (eq? rule-type 'top) chosen-rule matching-rule))
		      (rule2 (if (eq? rule-type 'top) matching-rule chosen-rule)))
		  (if* (tell *memory* 'answer-present?
			 (tell *answer-string* 'get-letter-categories)
			 rule1 rule2)
		    (vprintf "Already found this answer. Fizzling.~n")
		    (fizzle))
		  (if* (tell matching-rule 'supported?)
		    (let ((all-supporting-groups
			    (remq-duplicates
			      (append
				vertical-mapping-supporting-groups
				(get-rule-supporting-groups rule1 rule2)))))
		      (report-new-answer
			*answer-string* rule1 rule2 supporting-vertical-bridges
			all-supporting-groups ref-objs1 ref-objs2 slippage-log '()))
		    (fizzle)))
		(vprintf "The matching rule is not currently supported...~n")
		(vprintf "Attempting to clamp rules...~n")
		(if* (tell *trace* 'permission-to-clamp?)
		  (vprintf "Permission granted. Clamping rules...~n")
		  (clamp-rules
		    (list chosen-rule matching-rule)
		    (tell *themespace* 'get-dominant-theme-pattern 'vertical-bridge)
		    (tell matching-rule 'get-concept-pattern)))
		(fizzle))

	      (vprintf "Couldn't find a matching rule...~n")
	      ;; The ref-objs1 check avoids cases in which a bottom rule gets
	      ;; translated to a top rule that works but doesn't refer to anything
	      ;; in the initial string.  Example:  xqd -> xqd; mrrjjj -> mrrjjjj
	      ;; The bottom rule "Increase length of j group by one" may get translated
	      ;; as "Increase length of letter j by one", which works (vacuously)
	      ;; when applied to xqd -> xqd.
	      (if* (and (tell translated-rule 'currently-works?)
		        (not (null? ref-objs1)))
		(vprintf "Translated rule works...~n")
		(let ((rule1 (if (eq? rule-type 'top) chosen-rule translated-rule))
		      (rule2 (if (eq? rule-type 'top) translated-rule chosen-rule)))
		  (if* (tell *memory* 'answer-present?
			 (tell *answer-string* 'get-letter-categories)
			 rule1 rule2)
		    (vprintf "Already found this answer. Fizzling.~n")
		    (fizzle))
		  (tell translated-rule 'set-quality-values)
		  ;; This sets translated-rule's supporting-horizontal-bridges
		  ;; and theme-pattern:
		  (if (eq? rule-type 'top)
		    (make-translated-string translated-rule *target-string*)
		    (make-translated-string translated-rule *initial-string*))
		  (tell *workspace* 'add-rule translated-rule)
		  (monitor-new-rules translated-rule)
		  (if* (tell translated-rule 'supported?)
		    (let ((all-supporting-groups
			    (remq-duplicates
			      (append
				vertical-mapping-supporting-groups
				(get-rule-supporting-groups rule1 rule2)))))
		      (report-new-answer
			*answer-string* rule1 rule2 supporting-vertical-bridges
			all-supporting-groups ref-objs1 ref-objs2 slippage-log '()))
		    (fizzle)))
		(vprintf "The translated rule is not currently supported...~n")
		(vprintf "Attempting to clamp rules...~n")
		(if* (tell *trace* 'permission-to-clamp?)
		  (vprintf "Permission granted. Clamping rules...~n")
		  (clamp-rules
		    (list chosen-rule translated-rule)
		    (tell *themespace* 'get-dominant-theme-pattern 'vertical-bridge)
		    (tell translated-rule 'get-concept-pattern)))
		(fizzle))
	      (vprintf "Translated rule doesn't work...~n")))

	  ;; Unification section
	  (vprintf "Attempting to unify chosen rule with some other existing rule...~n")
	  (if* (null? other-rules)
	    (vprintf "No other rules exist. Fizzling.~n")
	    (fizzle))
	  (if* (not (tell *trace* 'permission-to-clamp?))
	    (vprintf "Permission to clamp patterns denied. Fizzling.~n")
	    (fizzle))
	  (let* ((strength (tell chosen-rule 'get-strength))
		 (other-weights
		   (map
		     (lambda (r) (100- (abs (- strength (tell r 'get-strength)))))
		     other-rules))
		 (other-rule (stochastic-pick other-rules other-weights)))
	    (vprintf "Chose a rule for unification:~n")
	    (vprint other-rule)
	    (let ((unifying-theme-pattern (unify-rules chosen-rule other-rule)))
	      (if* (not (exists? unifying-theme-pattern))
		(vprintf "Couldn't unify rules. Fizzling.~n")
		(fizzle))
	      (vprintf "Rules can be unified. Clamping rules...~n")
	      (clamp-rules
		(list chosen-rule other-rule)
		(get-vertical-theme-pattern-to-clamp unifying-theme-pattern)
		(tell chosen-rule 'get-concept-pattern)
		(tell other-rule 'get-concept-pattern)))))))))


(define clamp-rules
  (lambda (rules vertical-theme-pattern . concept-patterns)
    (let* ((all-patterns
	       (append
		 (tell-all rules 'get-theme-pattern)
		 (list vertical-theme-pattern)
		 concept-patterns
		 (list
		   %top-down-codelet-pattern%
		   %thematic-codelet-pattern%)))
	   (clamp-event
	     (make-clamp-event 'justify-clamp all-patterns rules 'workspace)))
      ;; This should happen first so that concept-activation events resulting
      ;; from the clamp will appear in the trace after the clamp event:
      (tell *trace* 'add-event clamp-event)
      (tell clamp-event 'activate))))


(define unify-rules
  (lambda (from-rule to-rule)
    (if (or (tell from-rule 'verbatim?)
	    (tell to-rule 'verbatim?))
      #f
      (continuation-point* return
	(let* ((from-rule-clauses (tell from-rule 'get-rule-clauses))
	       (to-rule-clauses (tell to-rule 'get-rule-clauses))
	       (fail (lambda () (return #f)))
	       (unifying-concept-mappings
		 (traverse-rule-clauses
		   from-rule-clauses to-rule-clauses fail concept-mapping-proc))
	       (theme-pattern-entries
		 (map (lambda (cm)
			(list (tell cm 'get-CM-type) (tell cm 'get-label)))
		   (remove-whole/single-concept-mappings
		     unifying-concept-mappings))))
	  (cons 'vertical-bridge
	    theme-pattern-entries))))))


;; The slippages returned by get-unifying-slippages depend on the direction
;; of rule translation (top-to-bottom vs. bottom-to-top).  The function
;; also assumes that the given rules can be unified.

(define get-unifying-slippages
  (lambda (from-rule to-rule)
    (remove-duplicate-CMs
      (remove-whole/single-concept-mappings
	(filter-meth
	  (traverse-rule-clauses
	    (tell from-rule 'get-rule-clauses)
	    (tell to-rule 'get-rule-clauses)
	    #f concept-mapping-proc)
	  'slippage?)))))


;; single=>whole, whole=>single, single=>single, and whole=>whole
;; concept-mappings are ignored, because they would lead to clamping
;; the StrPos:diff theme, which would only confuse the program:

(define remove-whole/single-concept-mappings
  (lambda (concept-mappings)
    (remq
      (select
	(lambda (cm)
	  (and (tell cm 'CM-type? plato-string-position-category)
	       (or (eq? (tell cm 'get-descriptor1) plato-single)
		   (eq? (tell cm 'get-descriptor1) plato-whole))))
	concept-mappings)
      concept-mappings)))


(define traverse-rule-clauses
  (lambda (clauses1 clauses2 fail proc)
    (letrec
      ((walk (lambda (x1 x2 results)
	       (cond
		 ((and (null? x1) (null? x2)) results)
		 ((or (null? x1) (null? x2)) (fail))
		 ((and (list? x1) (list? x2))
		  (walk (1st x1) (1st x2) (walk (rest x1) (rest x2) results)))
		 ((or (list? x1) (list? x2)) (fail))
		 ((and (symbol? x1) (symbol? x2)) (if (eq? x1 x2) results (fail)))
		 ((eq? x1 'string) (if (eq? x2 plato-group) results (fail)))
		 ((eq? x2 'string) (if (eq? x1 plato-group) results (fail)))
		 ((or (symbol? x1) (symbol? x2)) (fail))
		 (else (proc x1 x2 fail results))))))
      (walk clauses1 clauses2 '()))))


(define verbatim-rule-clause-list?
  (lambda (rc-list)
    (and (= (length rc-list) 1)
         (verbatim-clause? (1st rc-list)))))


(define compare-rule-clause-lists
  (lambda (rc-list1 rc-list2)
    (if (and (verbatim-rule-clause-list? rc-list1)
	     (verbatim-rule-clause-list? rc-list1))
      (if (equal? rc-list1 rc-list2) '() #f)
      (continuation-point* return
	(traverse-rule-clauses
	  rc-list1
	  rc-list2
	  (lambda () (return #f))
	  rule-clause-comparison-proc)))))


(define rule-clause-comparison-proc
  (lambda (n1 n2 fail results)
    (if (eq? n1 n2)
      results
      (cons (list n1 n2) results))))


(define concept-mapping-proc
  (lambda (n1 n2 fail results)
    (cond
      ((and (not (eq? n1 n2)) (not (slip-linked? n1 n2))) (fail))
      ((not (exists? (tell n1 'get-category))) results)
      (else (cons
	      (make-concept-mapping
		#f (tell n1 'get-category) n1
		#f (tell n1 'get-category) n2)
	      results)))))


(define get-vertical-theme-pattern-to-clamp
  (lambda (unifying-pattern)
    (let* ((theme-type (1st unifying-pattern))
	   (pattern-entries (rest unifying-pattern))
	   (get-probability (retention-probability pattern-entries))
	   (final-pattern-entries
	     (add-direction-entry
	       (replace-bond-category-entry
		 (filter (compose prob? get-probability) pattern-entries))))
	   (final-theme-pattern
	     (if (null? final-pattern-entries)
	       unifying-pattern
	       (cons theme-type final-pattern-entries))))
      (vprintf "------------------------------------------------~n")
      (vprintf "Rule unification possible with the unifying pattern:~n")
      (vprintf "Retention probabilities: ~a~n"
	(map (compose round-to-100ths (retention-probability pattern-entries))
	  (rest unifying-pattern)))
      (vprintf "Final pattern:~n")
      (if (exists? final-theme-pattern)
	(vprintf "None.~n"))
      (vprintf "------------------------------------------------~n")
      final-theme-pattern)))


(define retention-probability
  (lambda (pattern-entries)
    (lambda (entry)
      (if (eq? (1st entry) plato-string-position-category)
	1
	(* (% (cd (1st entry)))
	   (% (if (eq? (2nd entry) plato-identity) 50 100)))))))


;; The following two heuristics improve the chances that a useful unifying theme 
;; pattern will get clamped:
;;
;; (1) If an entry exists in the pattern for StrPos:iden (or StrPos:opp), add an
;;     entry for Dir:iden (or Dir:opp), since the idea of opposite direction is
;;     closely related to opposite string-position.
;;
;; (2) If an entry for BondCtgy exists, replace it with the corresponding entry for
;;     GroupCtgy, since bond-category themes don't play much of a role in the
;;     creation of bridges by thematic-bridge-scout codelets.

(define add-direction-entry
  (lambda (pattern-entries)
    (let ((StrPos-entry (assq plato-string-position-category pattern-entries))
	  (Dir-entry (assq plato-direction-category pattern-entries)))
      (if (and (exists? StrPos-entry)
	       (exists? (2nd StrPos-entry))
	       (not (exists? Dir-entry)))
	(cons (list plato-direction-category (2nd StrPos-entry)) pattern-entries)
	pattern-entries))))

(define replace-bond-category-entry
  (lambda (pattern-entries)
    (let ((BondCtgy-entry (assq plato-bond-category pattern-entries))
	  (GroupCtgy-entry (assq plato-group-category pattern-entries)))
      (if (exists? BondCtgy-entry)
	(if (exists? GroupCtgy-entry)
	  (remq BondCtgy-entry pattern-entries)
	  (cons (list plato-group-category (2nd BondCtgy-entry))
	    (remq BondCtgy-entry pattern-entries)))
	pattern-entries))))
