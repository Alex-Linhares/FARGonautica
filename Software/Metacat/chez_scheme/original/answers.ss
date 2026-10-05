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

(define report-new-answer
  (lambda (answer-string top-rule bottom-rule supporting-vertical-bridges
	    supporting-groups top-rule-ref-objects bottom-rule-ref-objects
	    slippage-log unjustified-slippages)
    (update-everything)
    (if* (tell *trace* 'within-clamp-period?)
      (tell *trace* 'undo-last-clamp))
    (let ((answer-event
	    (make-answer-event
	      *initial-string* *modified-string* *target-string* answer-string
	      top-rule bottom-rule supporting-vertical-bridges supporting-groups
	      top-rule-ref-objects bottom-rule-ref-objects slippage-log
	      unjustified-slippages)))
      (tell *trace* 'add-event answer-event)
      (cond
	((not (null? unjustified-slippages))
	 (tell *comment-window* 'add-comment
	   (list
	     "Okay, I'm stumped.  This answer makes no sense to me."
	     (format "  I see no way to make the necessary ~a slippage~a here."
	       (punctuate (tell-all unjustified-slippages 'english-name))
	       (if (= (length unjustified-slippages) 1) "" "s")))
	   (list
	     (format "Run terminated.  Unable to make the necessary ~a slippage~a."
	       (punctuate (tell-all unjustified-slippages 'english-name))
	       (if (= (length unjustified-slippages) 1) "" "s")))))
	(%justify-mode%
	  (tell *comment-window* 'add-comment
	    (list
	      "Aha!  I see why this answer makes sense"
	      (let* ((quality (tell answer-event 'get-quality))
		     (quality-phrase (answer-quality-phrase quality)))
		(if (< quality 60)
		  (format ", but it's a ~a answer, in my opinion." quality-phrase)
		  (format ".  I think it's a ~a answer~a"
		    quality-phrase
		    (if (or (< quality 30) (>= quality 85)) "!" ".")))))
	    (list
	      (format "Successfully justified answer.  Answer quality = ~a."
		(tell answer-event 'get-quality)))))
	(else
	  (tell *comment-window* 'add-comment
	    (list
	      (format "The answer \"~a\" ~aoccurs to me"
		(tell answer-string 'print-name)
		(if (> (tell *trace* 'get-num-of-events 'answer) 1) "also " ""))
	      (let* ((quality (tell answer-event 'get-quality))
		     (quality-phrase (answer-quality-phrase quality))
		     (punctuation (if (or (< quality 30) (>= quality 85)) "!" ".")))
		(if (< quality 60)
		  (format ", but that's ~a~a" quality-phrase punctuation)
		  (format ".  I think this answer is ~a~a" quality-phrase punctuation))))
	    (list
	      (format "Found the answer \"~a\".  Answer quality = ~a."
		(tell answer-string 'print-name)
		(tell answer-event 'get-quality))))))
      (if* %workspace-graphics%
	(tell *workspace-window* 'draw-current-answer))
      ;; Build an abstract characterization of the answer from the information
      ;; in the workspace and the trace, and store it in memory:
      (abstract-answer-description answer-event)
      (suspend)
      (if* %workspace-graphics%
	(tell *workspace-window* 'erase-current-answer)))))


(define give-up
  (lambda ()
    (update-everything)
    (tell *comment-window* 'add-comment
      (list "Excuse me -- I think I'll go get some more punch.")
      (list "Run terminated."))
    (suspend)))


(define answer-quality-phrase
  (lambda (quality)
    (cond
      ((< quality 50) "really terrible")
      ((< quality 60) "pretty bad")
      ((< quality 70) "pretty dumb")
      ((< quality 75) "pretty mediocre")
      ((< quality 80) "halfway decent")
      ((< quality 85) "pretty good")
      ((< quality 90) "very good")
      (else "great"))))


(define most-recent-group-and-concept-mapping-events
  (lambda ()
    (let* ((group-and-concept-mapping-events
	     (filter
	       (lambda (event)
		 (and (member? (tell event 'get-type) '(concept-mapping group))
		      (tell event 'relevant-for-answer-description?)))
	       (tell *trace* 'get-all-events)))
	   (equivalent-event-clusters
	     (partition
	       (lambda (ev1 ev2) (tell ev1 'equal? ev2))
	       group-and-concept-mapping-events)))
      (map pick-most-recent-event
	equivalent-event-clusters))))


(define pick-most-recent-event
  (lambda (events)
    (select-extreme min
      (lambda (event) (tell event 'get-age))
      events)))


(define in-patterns?
  (lambda (dimension pattern/s)
    (exists? (get-entry dimension pattern/s))))


(define get-entry
  (lambda (dimension pattern/s)
    (cond
      ((null? pattern/s) #f)
      ((symbol? (1st pattern/s)) (assq dimension (entries pattern/s)))
      (else (assq dimension (flatmap entries pattern/s))))))


(define whole-string-identity-concept-mapping?
  (lambda (cm-type initial-group target-group)
    (and (exists? initial-group)
         (exists? target-group)
	 (let ((spanning-bridge
		 (get-bridge-between 'vertical initial-group target-group)))
	   (and (exists? spanning-bridge)
	        (let ((cm (tell spanning-bridge 'get-concept-mapping cm-type)))
		  (and (exists? cm) (tell cm 'identity?))))))))


(define abstract-answer-description-theme-pattern
  (lambda (important-events)
    (let* ((dominant-vertical-theme-pattern
	     (tell *themespace* 'get-dominant-theme-pattern 'vertical-bridge))
	   (whole-initial-string-event
	     (select
	       (lambda (event)
		 (and (tell event 'type? 'group)
		      (tell event 'string-type? 'initial)))
	       important-events))
	   (whole-target-string-event
	     (select
	       (lambda (event)
		 (and (tell event 'type? 'group)
		      (tell event 'string-type? 'target)))
	       important-events))
	   (initial-spanning-group
	     (if (exists? whole-initial-string-event)
	       (tell whole-initial-string-event 'get-group)
	       #f))
	   (target-spanning-group
	     (if (exists? whole-target-string-event)
	       (tell whole-target-string-event 'get-group)
	       #f))
	   (important-cm-events
	     (filter-meth important-events 'type? 'concept-mapping))
	   (important-cm-patterns
	     (tell-all important-cm-events 'get-theme-pattern))
	   (abstracted-theme-pattern-entries
	     (map-compress
	       (lambda (cm-type)
		 (cond
		   ;; Check for non-identity themes in the cm-patterns first:
		   ((in-patterns? cm-type important-cm-patterns)
		    (get-entry cm-type important-cm-patterns))
		   ((eq? cm-type plato-string-position-category)
		    (if (in-patterns? plato-direction-category important-cm-patterns)
		      ;; StringPos and Direction themes should agree if possible:
		      (list
			plato-string-position-category
			(2nd (get-entry plato-direction-category important-cm-patterns)))
		      ;; Otherwise check for a dominant StringPos theme:
		      (let ((dominant-StringPos-theme
			      (assq plato-string-position-category
				(entries dominant-vertical-theme-pattern))))
			(if (exists? dominant-StringPos-theme)
			  dominant-StringPos-theme
			  ;; Always include a string-position theme no matter what:
			  (list plato-string-position-category plato-identity)))))
		   ;; Don't include bond-facet identity themes:
		   ((eq? cm-type plato-bond-facet) #f)
		   ;; Check for whole-string identity themes:
		   ((whole-string-identity-concept-mapping?
		      cm-type initial-spanning-group target-spanning-group)
		    (list cm-type plato-identity))
		   (else #f)))
	       ;; Only themes of the following types contribute to
	       ;; answer-description thematic characterizations:
	       (list
		 plato-alphabetic-position-category
		 plato-string-position-category
		 plato-direction-category
		 plato-group-category
		 plato-bond-facet))))
      (cons 'vertical-bridge
	abstracted-theme-pattern-entries))))


(define get-theme-supporting-concept-mappings
  (lambda (theme-pattern theme-supporting-bridges)
    (let ((all-concept-mappings
	    (remove-whole/single-concept-mappings
	      (apply append
		(tell-all theme-supporting-bridges 'get-all-concept-mappings)))))
      (filter
	(lambda (cm)
	  (let ((entry (list (tell cm 'get-CM-type) (tell cm 'get-label))))
	    (or (member-equal? entry (entries theme-pattern))
	        (and (eq? (1st entry) plato-bond-category)
		     (member-equal? (list plato-group-category (2nd entry))
		       (entries theme-pattern))))))
	all-concept-mappings))))


(define get-unjustified-theme-pattern
  (lambda (unjustified-slippages)
    (let ((pattern-entries
	    (map (lambda (s) (list (tell s 'get-CM-type) (tell s 'get-label)))
	      unjustified-slippages)))
      (cons 'vertical-bridge
	(if (not (exists? (assq plato-bond-facet pattern-entries)))
	  pattern-entries
	  ;; Since a BondFacet theme implies the existence of groups, add
	  ;; accompanying (unjustified) group-category and direction themes, if
	  ;; they don't already exist.  Since the unjustified BondFacet theme
	  ;; by itself does not imply a particular group-category or direction
	  ;; relation (i.e., identity vs. opposite), identity themes are added
	  ;; in the default case:
	  (let ((GroupCtgy-entry
		  (if (exists? (assq plato-group-category pattern-entries))
		    (assq plato-group-category pattern-entries)
		    (list plato-group-category plato-identity)))
		(Direction-entry
		  (if (exists? (assq plato-direction-category pattern-entries))
		    (assq plato-direction-category pattern-entries)
		    (list plato-direction-category plato-identity))))
	    (remove-duplicates
	      (cons GroupCtgy-entry
		(cons Direction-entry
		  pattern-entries)))))))))


(define punctuate-with-commas
  (lambda (conj l)
    (cond
      ((null? l) "")
      ((= (length l) 1) (1st l))
      ((= (length l) 2) (format "~a, ~a ~a" (1st l) conj (2nd l)))
      (else (apply string-append
	      (append
		(map (lambda (x) (format "~a, " x)) (all-but-last 1 l))
		(list (format "~a ~a" conj (last l)))))))))


(define intersect-themes
  (lambda (themes1 themes2)
    (intersect-pred theme-pattern-entries-equal?
      themes1 themes2)))


(define get-snag-justified-themes
  (lambda (answer)
    (let* ((unjustified-themes
	     (tell answer 'get-unjustified-themes))
	   (all-themes (append (tell answer 'get-themes) unjustified-themes))
	   (snag-description (tell *memory* 'get-equivalent-snag answer))
	   (snag-avoiding-themes
	     (if (exists? snag-description)
	       (remove-elements
		 (tell snag-description 'get-themes)
		 all-themes)
	       '())))
      (intersect-themes
	snag-avoiding-themes
	unjustified-themes))))


(define get-snag-explanation
  (lambda (answer)
    (let ((snag (tell *memory* 'get-equivalent-snag answer)))
      (if (exists? snag)
	(tell snag 'get-explanation)
	""))))


(define explain
  (lambda (answer)
    (let* ((initial (tell answer 'get-initial-print-name))
	   (target (tell answer 'get-target-print-name))
	   (themes (tell answer 'get-themes))
	   (snag-justified-themes (get-snag-justified-themes answer))
	   (snag-explanation (get-snag-explanation answer))
	   (unjustified-themes
	     (remove-elements
	       snag-justified-themes
	       (tell answer 'get-unjustified-themes)))
	   (explanation
	     (format "This answer is based ~a."
	       (theme-phrases "on " "and" "ing"
		 themes snag-justified-themes unjustified-themes
		 initial target snag-explanation #t #f #f))))
      (tell *comment-window* 'add-comment
	(list
	  explanation
	  (format "  Personally, I think this answer is ~a."
	    (answer-quality-phrase (tell answer 'get-quality))))
	(list
	  explanation
	  (format "  Answer quality = ~a." (tell answer 'get-quality)))))))


(define theme-phrases
  (lambda (prep conj verb-ending themes snag-justified-themes unjustified-themes
	    initial target snag-explanation add-caveats? the-strings? two-strings)
    (let* ((all-themes (append themes unjustified-themes))
	   (StringPos-theme (assq plato-string-position-category all-themes))
	   ;; Direction-theme should always be the same as StringPos-theme:
	   (AlphaPos-theme (assq plato-alphabetic-position-category all-themes))
	   (GroupCtgy-theme (assq plato-group-category all-themes))
	   (BondFacet-theme (assq plato-bond-facet all-themes))
	   (strings
	     (if (exists? two-strings)
	       (format "two strings ~a" two-strings)
	       (format "~a and ~a" initial target)))
	   (caveat (lambda (theme)
		     (cond
		       ((not add-caveats?) "")
		       ((member-equal? theme unjustified-themes)
			" (although there is no good reason for doing so)")
		       ((member-equal? theme snag-justified-themes)
			(string-append
			  " (which avoids a snag that would otherwise "
			  (format "arise from the fact that ~a)" snag-explanation)))
		       (else "")))))
      (punctuate-with-commas conj
	(compress
	  (list
	    (if (exists? StringPos-theme)
	      (format "~asee~a ~a as ~a~a~a"
		prep verb-ending strings
		(cond
		  ((or (not (exists? GroupCtgy-theme))
		       (member-equal? GroupCtgy-theme unjustified-themes))
		   "")
		  ((eq? (2nd GroupCtgy-theme) plato-identity)
		   "groups of the same type ")
		  ((eq? (2nd GroupCtgy-theme) plato-opposite)
		   "symmetric predecessor and successor groups ")
		  ((eq? (2nd GroupCtgy-theme) diff)
		   "different kinds of groups "))
		(cond
		  ((eq? (2nd StringPos-theme) plato-identity)
		   "going in the same direction")
		  ((eq? (2nd StringPos-theme) plato-opposite)
		   "going in opposite directions")
		  ((eq? (2nd StringPos-theme) diff)
		   "neither going in the same nor in opposite directions"))
		(caveat StringPos-theme))
	      #f)
	    (if (and (exists? GroupCtgy-theme)
		     (not (exists? StringPos-theme))
		     (not (member-equal? GroupCtgy-theme unjustified-themes)))
	      (format "~asee~a ~a as ~a~a~a"
		prep verb-ending strings
		(cond
		  ((eq? (2nd GroupCtgy-theme) plato-identity)
		   "groups of the same type")
		  ((eq? (2nd GroupCtgy-theme) plato-opposite)
		   "symmetric predecessor and successor groups")
		  ((eq? (2nd GroupCtgy-theme) diff)
		   "different kinds of groups"))
		(if (exists? BondFacet-theme)
		  (string-append
		    " by viewing one string in terms of letters "
		    "and the other in terms of numbers")
		  "")
		(caveat GroupCtgy-theme))
	      #f)
	    (if (and (exists? BondFacet-theme)
		     (or (exists? StringPos-theme)
		         (not (exists? GroupCtgy-theme))
			 (member-equal? GroupCtgy-theme unjustified-themes)))
	      (format "~aview~a one of ~a in terms of letters ~a"
		prep verb-ending
		(if (or (exists? StringPos-theme) the-strings?)
		  "the strings"
		  strings)
		(format "and the other in terms of numbers~a"
		  (caveat BondFacet-theme)))
	      #f)
	    (if (exists? AlphaPos-theme)
	      (format "~asee~a alphabetic-position ~a between ~a~a"
		prep verb-ending
		(cond
		  ((eq? (2nd AlphaPos-theme) plato-identity) "sameness")
		  ((eq? (2nd AlphaPos-theme) plato-opposite) "symmetry"))
		(if (or (exists? StringPos-theme) the-strings?)
		  "the strings"
		  strings)
		(caveat AlphaPos-theme))
	      #f)))))))


(define compare-answers
  (lambda (answer1 answer2)
    (let ((comparison (list (get-answer-comparison-text answer1 answer2))))
      (tell *comment-window* 'add-comment comparison comparison))))


(define get-answer-comparison-text
  (lambda (self other)
    (let* ((initial1 (tell self 'get-initial-print-name))
	   (modified1 (tell self 'get-modified-print-name))
	   (target1 (tell self 'get-target-print-name))
	   (answer1 (tell self 'get-answer-print-name))
	   (initial2 (tell other 'get-initial-print-name))
	   (modified2 (tell other 'get-modified-print-name))
	   (target2 (tell other 'get-target-print-name))
	   (answer2 (tell other 'get-answer-print-name))
	   (problem1 (format "\"~a -> ~a, ~a -> ?\"" initial1 modified1 target1))
	   (problem2 (format "\"~a -> ~a, ~a -> ?\"" initial2 modified2 target2))
	   (change1 (format "the change from ~a to ~a" initial1 modified1))
	   (change2 (format "the change from ~a to ~a" initial2 modified2))
	   (full-answer1 (format "~a to the problem ~a" answer1 problem1))
	   (full-answer2 (format "~a to the problem ~a" answer2 problem2))
	   (answer1-phrase (if (string=? problem1 problem2) answer1 full-answer1))
	   (answer2-phrase (if (string=? problem1 problem2) answer2 full-answer2))
	   (both-answers-phrase
	     (format "the answer ~a and the answer ~a"
	       answer1-phrase full-answer2))
	   ;; These are all the ideas, justified or not, that underlie answer1:
	   (all-themes1
	     (append
	       (tell self 'get-themes)
	       (tell self 'get-unjustified-themes)))
	   ;; These are all the ideas, justified or not, that underlie answer2:
	   (all-themes2
	     (append
	       (tell other 'get-themes)
	       (tell other 'get-unjustified-themes)))
	   (snag-justified-themes1 (get-snag-justified-themes self))
	   (snag-justified-themes2 (get-snag-justified-themes other))
	   (unjustified-themes1
	     (remove-elements
	       snag-justified-themes1
	       (tell self 'get-unjustified-themes)))
	   (unjustified-themes2
	     (remove-elements
	       snag-justified-themes2
	       (tell other 'get-unjustified-themes)))
	   (all-unjustified-themes
	     (remove-duplicates
	       (append unjustified-themes1 unjustified-themes2)))
	   (snag-explanation1 (get-snag-explanation self))
	   (snag-explanation2 (get-snag-explanation other))
	   (common-themes
	     (intersect-themes
	       all-themes1 all-themes2))
	   ;; These are all the _types_ of ideas, justified or not, that underlie
	   ;; both answers:
	   (common-dimensions
	     (map 1st common-themes))
	   ;; These are all the _types_ of ideas, justified or not, that are common
	   ;; to both answers, but about which the answers "disagree".  Example: one
	   ;; answer based on StringPos:iden and the other based on StringPos:opp
	   (differing-dimensions
	     (remq-elements
	       common-dimensions
	       (intersect
		 (map 1st all-themes1)
		 (map 1st all-themes2))))
	   ;; These ideas, justified or not, underlie answer1 only:
	   (answer1-only-themes
	     (remove-elements common-themes all-themes1))
	   ;; These ideas, justified or not, underlie answer2 only:
	   (answer2-only-themes
	     (remove-elements common-themes all-themes2))
	   ;; These are the ideas, justified or not, that answer1 "disagrees" with
	   ;; answer2 about.  Example: if answer1 has StringPos:iden and answer2
	   ;; has StringPos:opp, StringPos:iden is one of answer1's differing themes:
	   (differing-themes1
	     (filter
	       (lambda (theme) (member? (1st theme) differing-dimensions))
	       all-themes1))
	   ;; These are the ideas, justified or not, that answer2 "disagrees" with
	   ;; answer1 about.  Example: if answer1 has StringPos:iden and answer2
	   ;; has StringPos:opp, StringPos:opp is one of answer2's differing themes:
	   (differing-themes2
	     (filter
	       (lambda (theme) (member? (1st theme) differing-dimensions))
	       all-themes2))
	   ;; These ideas, justified or not, underlie answer1 but are completely
	   ;; absent from answer2.  The answers do not even "disagree" about
	   ;; these ideas, because answer2 doesn't have an idea of the same type:
	   (unique-themes1
	     (remove-elements differing-themes1 answer1-only-themes))
	   ;; These ideas, justified or not, underlie answer2 but are completely
	   ;; absent from answer1.  The answers do not even "disagree" about
	   ;; these ideas, because answer1 doesn't have an idea of the same type:
	   (unique-themes2
	     (remove-elements differing-themes2 answer2-only-themes))
	   ;; These unjustified ideas are common to both answers:
	   (common-unjustified-themes
	     (intersect-themes
	       unjustified-themes1 unjustified-themes2))
	   ;; These unjustified ideas underlie answer1 only:
	   (answer1-only-unjustified-themes
	     (remove-elements common-unjustified-themes unjustified-themes1))
	   ;; These unjustified ideas underlie answer2 only:
	   (answer2-only-unjustified-themes
	     (remove-elements common-unjustified-themes unjustified-themes2))
	   ;; These ideas are common to both answers, but are unjustified
	   ;; only in the case of answer1:
	   (common-answer1-only-unjustified-themes
	     (remove-elements differing-themes1 answer1-only-unjustified-themes))
	   ;; These ideas are common to both answers, but are unjustified
	   ;; only in the case of answer2:
	   (common-answer2-only-unjustified-themes
	     (remove-elements differing-themes2 answer2-only-unjustified-themes))
	   ;; This is true if there exist ideas that are common to both answers,
	   ;; but which are justified for only one of the answers:
	   (justification-differences?
	     (or (not (null? common-answer1-only-unjustified-themes))
	         (not (null? common-answer2-only-unjustified-themes))))
	   (no-justification1
	     (format "in the former case, there is no compelling ~a"
	       (format "reason ~a, unlike in the latter case with ~a and ~a~a"
		 (theme-phrases "to " "or" ""
		   common-answer1-only-unjustified-themes
		   snag-justified-themes1 unjustified-themes1
		   initial1 target1 "" #f #t #f)
		 initial2 target2
		 (let ((common-unjustified1-snag-justified2-themes
			 (intersect-themes
			   common-answer1-only-unjustified-themes
			   snag-justified-themes2)))
		   (if (null? common-unjustified1-snag-justified2-themes)
		     ""
		     (format
		       ", where ~a avoids a snag that would otherwise ~a"
		       (theme-phrases "" "and" "ing"
			 common-unjustified1-snag-justified2-themes '()
			 common-unjustified1-snag-justified2-themes
			 initial2 target2 "" #f #t #f)
		       (format "arise from the fact that ~a" snag-explanation2)))))))
	   (no-justification2
	     (format "in the latter case, there is no compelling ~a"
	       (format "reason ~a, unlike in the former case with ~a and ~a~a"
		 (theme-phrases "to " "or" ""
		   common-answer2-only-unjustified-themes
		   snag-justified-themes2 unjustified-themes2
		   initial2 target2 "" #f #t #f)
		 initial1 target1
		 (let ((common-snag-justified1-unjustified2-themes
			 (intersect-themes
			   common-answer2-only-unjustified-themes
			   snag-justified-themes1)))
		   (if (null? common-snag-justified1-unjustified2-themes)
		     ""
		     (format
		       ", where ~a avoids a snag that would otherwise ~a"
		       (theme-phrases "" "and" "ing"
			 common-snag-justified1-unjustified2-themes '()
			 common-snag-justified1-unjustified2-themes
			 initial1 target1 "" #f #t #f)
		       (format "arise from the fact that ~a" snag-explanation1)))))))
	   (num-theme-differences
	     (+ (length differing-dimensions)
	        (length unique-themes1)
		(length unique-themes2)))
	   (theme-differences?
	     (not (= num-theme-differences 0)))
	   ;; Compare the rules
	   (rule1-clauses (tell self 'get-top-rule-clauses))
	   (rule2-clauses (tell other 'get-top-rule-clauses))
	   (rule1-abstractness (tell self 'get-top-rule-abstractness))
	   (rule2-abstractness (tell other 'get-top-rule-abstractness))
	   ;; <rule-differences> ::= #f | ((<slipnode1> <slipnode2>) ...)
	   (rule-differences
	     (compare-rule-clause-lists rule1-clauses rule2-clauses))
	   (num-rule-differences
	     (if (exists? rule-differences)
	       (length (filter-out
			 (lambda (nodes) (= (cd (1st nodes)) (cd (2nd nodes))))
			 rule-differences))
	       -1))
	   (rule-differences?
	     (not (= num-rule-differences 0)))
	   (rule-difference-phrase
	     (if (not (exists? rule-differences))
	       "in a completely different way"
	       (let ((difference (- rule1-abstractness rule2-abstractness)))
		 (cond
		   ((> difference 0) "in a more abstract way")
		   ((< difference 0) "in a more literal way")
		   (else "somewhat differently")))))
	   ;; In the following special case, answer2 gets mentioned before answer1
	   ;; in the output, so if both answers happen to be the same string, we
	   ;; need to refer to answer2 as the "first" answer instead of the "second"
	   ;; answer.  In all other cases answer1 gets mentioned first:
	   (answer2-mentioned-first?
	     (and theme-differences? (null? answer1-only-themes)))
	   (answer1-order (if answer2-mentioned-first? "second" "first"))
	   (answer2-order (if answer2-mentioned-first? "first" "second"))
	   ;; Example answer1-ref strings: "dyz", "the first dyz", "the second dyz"
	   (answer1-ref
	     (if (string=? answer1 answer2)
	       (format "the ~a ~a" answer1-order answer1)
	       answer1))
	   (answer2-ref
	     (if (string=? answer1 answer2)
	       (format "the ~a ~a" answer2-order answer2)
	       answer2))
	   (rule-explanation-template
	     (string-append
	       "~a difference between ~a"
	       (format " is that ~a is viewed ~a for ~a than~a in ~a."
		 change1
		 rule-difference-phrase
		 (if (string=? answer1 answer2)
		   (format "the ~a answer" answer1-order)
		   (format "the answer ~a" answer1))
		 (if (string=? change1 change2)
		   " it is"
		   (format " ~a" change2))
		 (if (string=? answer1 answer2)
		   (format "the ~a answer's case" answer2-order)
		   (format "the case of ~a" answer2)))))
	   (quality1 (tell self 'get-quality))
	   (quality2 (tell other 'get-quality))
	   (average-theme-abstractness1 (average-theme-abstractness self))
	   (average-theme-abstractness2 (average-theme-abstractness other))
	   (answer1-incoherent? (answer-incoherent? self))
	   (answer2-incoherent? (answer-incoherent? other))
	   (similarities
	     (lambda (caveats?)
	       (format "rely ~a"
		 (theme-phrases "on " "and" "ing"
		   common-themes '() all-unjustified-themes #f #f "" caveats? #f
		   (if (and (string=? initial1 initial2)
			    (string=? target1 target2))
		     (format "(~a and ~a in both cases)"
		       initial1 target1)
		     (format "(~a and ~a in one case and ~a and ~a in the other)"
		       initial1 target1 initial2 target2)))))))
      (string-append
	(cond
	  ((not theme-differences?)
	   ;; No theme differences
	   (cond
	     ((and (not rule-differences?) (not justification-differences?))
	      ;; No differences at all
	      (format "The answer ~a is essentially the same as the answer ~a.~a"
		answer1-phrase full-answer2
		(format "  Both answers ~a.  Furthermore, ~a."
		  (similarities #t)
		  (if (string=? change1 change2)
		    (format "~a is viewed in essentially the same way in both cases"
		      change1)
		    (format "~a is viewed in essentially the same way as ~a"
		      change1 change2)))))
	     ((not justification-differences?)
	      ;; Only rule differences
	      (string-append
		(format rule-explanation-template
		  "The only essential"
		  both-answers-phrase)
		(format "  Both answers ~a." (similarities #t))))
	     ;; Justification differences
	     ((null? common-answer1-only-unjustified-themes)
	      (format "The answer ~a is similar to the answer ~a, since both ~a.~a"
		answer1-phrase full-answer2 (similarities #f)
		(format "  However, ~a." no-justification2)))
	     ((null? common-answer2-only-unjustified-themes)
	      (format "The answer ~a is similar to the answer ~a, since both ~a.~a"
		answer1-phrase full-answer2 (similarities #f)
		(format "  However, ~a." no-justification1)))
	     (else
	       (format "The answer ~a is similar to the answer ~a, since both ~a.~a"
		 answer1-phrase full-answer2 (similarities #f)
		 (format "  However, ~a.  Likewise, ~a."
		   no-justification1
		   no-justification2)))))
	  ;; Theme differences
	  ((null? answer1-only-themes)
	   ;; In this case we are mentioning answer2 first:
	   (format "The answer ~a is based ~a~a."
	     full-answer2
	     (if (null? common-themes) "" "in part ")
	     (theme-phrases "on " "and" "ing"
	       answer2-only-themes snag-justified-themes2 unjustified-themes2
	       initial2 target2 snag-explanation2 #t #f #f)))
	  ((null? answer2-only-themes)
	   (format "The answer ~a is based ~a~a."
	     full-answer1
	     (if (null? common-themes) "" "in part ")
	     (theme-phrases "on " "and" "ing"
	       answer1-only-themes snag-justified-themes1 unjustified-themes1
	       initial1 target1 snag-explanation1 #t #f #f)))
	  (else
	    (format "The answer ~a is based ~a~a, while the answer ~a is based ~a~a."
	      full-answer1
	      (if (null? common-themes) "" "in part ")
	      (theme-phrases "on " "and" "ing"
		answer1-only-themes snag-justified-themes1 unjustified-themes1
		initial1 target1 snag-explanation1 #t #f #f)
	      answer2-phrase
	      (if (null? common-themes) "" "in part ")
	      (theme-phrases "on " "and" "ing"
		answer2-only-themes snag-justified-themes2 unjustified-themes2
		initial2 target2 snag-explanation2 #t #f #f))))
	(if (not (null? unique-themes1))
	  (format "~a ~a, the idea ~a does not arise."
	    (if (or (null? answer1-only-themes) (null? answer2-only-themes))
	      "  In contrast, in"
	      "  In")
	    (if (null? answer2-only-themes)
	      ;; Here we haven't explicitly mentioned the second answer yet:
	      (format "the case of the answer ~a" answer2-phrase)
	      (format "~a's case" answer2-ref))
	    (theme-phrases "of " "and" "ing"
	      unique-themes1 snag-justified-themes1 unjustified-themes1
	      initial2 target2 "" #f #f #f))
	  "")
	(if (not (null? unique-themes2))
	  (format "~a ~a, the idea ~a does not arise."
	    (if (and (or (null? answer1-only-themes) (null? answer2-only-themes))
		     (null? unique-themes1))
	      "  In contrast, in"
	      "  In")
	    (if (null? answer1-only-themes)
	      ;; Here we haven't explicitly mentioned the first answer yet:
	      (format "the case of the answer ~a" answer1-phrase)
	      (format "~a's case" answer1-ref))
	    (theme-phrases "of " "and" "ing"
	      unique-themes2 snag-justified-themes2 unjustified-themes2
	      initial1 target1 "" #f #f #f))
	  "")
	(if (or (not rule-differences?)
	        (and (not theme-differences?)
		     (not justification-differences?)))
	  ""
	  (format rule-explanation-template
	    "  Another key"
	    (if (string=? answer1 answer2)
	      (format "the two ~a answers" answer1)
	      "the answers")))
	(string-append
	  (if answer1-incoherent?
	    (string-append
	      (format
		"  The answer ~a, however, seems incoherent to me, since it involves "
		answer1)
	      (format
		"seeing~a between ~a and ~a (~a), while "
		(if (> (length all-themes1) 1)
		  " abstract similarities"
		  " an abstract similarity")
		initial1 target1
		(theme-phrases "" "and" "ing"
		  all-themes1 snag-justified-themes1 unjustified-themes1
		  initial1 target1 "" #f #f #f))
	      (format "at the same time viewing ~a in a more literal way." change1))
	    "")
	  (if answer2-incoherent?
	    (string-append
	      (format "  The answer ~a~a seems incoherent~a, since it involves "
		answer2
		(if answer1-incoherent? " also" ", however,")
		(if answer1-incoherent? "" " to me"))
	      (format
		"seeing~a between ~a and ~a (~a), while "
		(if (> (length all-themes2) 1)
		  " abstract similarities"
		  " an abstract similarity")
		initial2 target2
		(theme-phrases "" "and" "ing"
		  all-themes2 snag-justified-themes2 unjustified-themes2
		  initial2 target2 "" #f #f #f))
	      (format "at the same time viewing ~a in a more literal way." change2))
	    ""))
	(let* ((answer1-better
		 (lambda (intro reason)
		   (format "~aI'd say ~a is the better answer~a."
		     intro answer1-ref reason)))
	       (answer2-better
		 (lambda (intro reason)
		   (format "~aI'd say ~a is the better answer~a."
		     intro answer2-ref reason)))
	       (neither-better
		 (lambda (intro)
		   (let ((quality1-phrase (answer-quality-phrase quality1))
			 (quality2-phrase (answer-quality-phrase quality2)))
		     (if (string=? quality1-phrase quality2-phrase)
		       (format "~aI'd say they're both ~a answers."
			 intro quality1-phrase)
		       (format "~aI'd say ~a is ~a and ~a is ~a."
			 intro answer1-ref quality1-phrase
			 answer2-ref quality2-phrase))))))
	  (cond
	    ((and answer1-incoherent? answer2-incoherent?)
	     (cond
	       ((or (and (< average-theme-abstractness1 average-theme-abstractness2)
		         (<= (length all-themes1) (length all-themes2)))
		    (and (< (length all-themes1) (length all-themes2))
		         (<= average-theme-abstractness1 average-theme-abstractness2)))
		(answer1-better "  Overall, though, "
		  (format ", because it doesn't seem quite as incoherent as ~a"
		    answer2-ref)))
	       ((or (and (< average-theme-abstractness2 average-theme-abstractness1)
		         (<= (length all-themes2) (length all-themes1)))
		    (and (< (length all-themes2) (length all-themes1))
		         (<= average-theme-abstractness2 average-theme-abstractness1)))
		(answer2-better "  Overall, though, "
		  (format ", because it doesn't seem quite as incoherent as ~a"
		    answer1-ref)))
	       (else (neither-better "  All in all, "))))
	    ((and (not answer1-incoherent?) answer2-incoherent?)
	     (answer1-better "  All in all, "
	       ", since it is more coherent"))
	    ((and (not answer2-incoherent?) answer1-incoherent?)
	     (answer2-better "  All in all, "
	       ", since it is more coherent"))
	    ((< (length unjustified-themes1) (length unjustified-themes2))
	     (answer1-better "  All in all, "
	       (if (= (length unjustified-themes1) 0)
		 ", since it involves no unjustified ideas"
		 ", since it involves fewer unjustified ideas")))
	    ((< (length unjustified-themes2) (length unjustified-themes1))
	     (answer2-better "  All in all, "
	       (if (= (length unjustified-themes2) 0)
		 ", since it involves no unjustified ideas"
		 ", since it involves fewer unjustified ideas")))
	    ((and (not theme-differences?)
	          (not (= num-rule-differences -1))
		  (> rule1-abstractness rule2-abstractness))
	     (answer1-better "  All in all, "
	       (if (string=? change1 change2)
		 (format ", since it involves seeing ~a in a more abstract way"
		   change1)
		 (format ", since ~a is seen in a more abstract way than ~a"
		   change1 change2))))
	    ((and (not theme-differences?)
	          (not (= num-rule-differences -1))
		  (> rule2-abstractness rule1-abstractness))
	     (answer2-better "  All in all, "
	       (if (string=? change1 change2)
		 (format ", since it involves seeing ~a in a more abstract way"
		   change2)
		 (format ", since ~a is seen in a more abstract way than ~a"
		   change2 change1))))
	    ((> (length all-themes1) (length all-themes2))
	     (answer1-better "  All in all, "
	       ", since it is based on a richer set of ideas"))
	    ((> (length all-themes2) (length all-themes1))
	     (answer2-better "  All in all, "
	       ", since it is based on a richer set of ideas"))
	    (else (neither-better "  All in all, "))))))))


(define average-theme-abstractness
  (lambda (answer)
    (let ((themes
	    (append
	      (tell answer 'get-themes)
	      (tell answer 'get-unjustified-themes))))
      (round (average (map theme-abstractness themes))))))


(define theme-abstractness
  (lambda (theme)
    (let* ((dimension (1st theme))
	   (relation (2nd theme))
	   (dimension-abstractness (cd dimension))
	   (relation-abstractness
	     (cond
	       ((eq? relation plato-identity) 0)
	       ((eq? relation diff) 50)
	       (else (cd relation)))))
      (round (average
	       dimension-abstractness
	       relation-abstractness)))))


(define answer-incoherent?
  (lambda (answer)
    (let* ((theme-abstractness (average-theme-abstractness answer))
	   (rule-abstractness (tell answer 'get-top-rule-abstractness))
	   (abstractness-difference (- theme-abstractness rule-abstractness)))
      (and (> theme-abstractness 50)
	   (< rule-abstractness theme-abstractness)
	   (> abstractness-difference 25)))))


(define coherence-phrase
  (lambda (coherence)
    (cond
      ((< coherence -50) "very incoherent")
      ((< coherence -10) "incoherent")
      ((< coherence 0) "somewhat incoherent")
      ((< coherence 10) "very coherent")
      (else "coherent"))))


(define-codelet-procedure* answer-finder
  (lambda ()
    (let ((top-strength (tell *workspace* 'get-mapping-strength 'top))
	  (vertical-strength (tell *workspace* 'get-mapping-strength 'vertical)))
      (stochastic-if* (1- (^3 (* (% top-strength) (% vertical-strength))))
	(say "Mappings are not strong enough. Fizzling.")
	(fizzle))
      (say "Trying to translate a rule...")
      (say "Top mapping strength is " top-strength)
      (say "Vertical mapping strength is " vertical-strength))
    (let ((supported-rules (tell *workspace* 'get-supported-rules 'top)))
      (if* (null? supported-rules)
	(say "No supported rules exist. Fizzling.")
	(fizzle))
      (let* ((rule-weights
	       (temp-adjusted-values (tell-all supported-rules 'get-strength)))
	     (rule (stochastic-pick supported-rules rule-weights)))
	(vprintf "Answer-finder codelet chose rule using weights ~a:~n"
	  rule-weights)
	(vprint rule)
	(let ((degree-of-support (tell rule 'get-degree-of-support)))
	  (say "Degree of support is " (round degree-of-support))
	  (stochastic-if* (1- (% degree-of-support))
	    (say "Not enough support for chosen rule. Fizzling.")
	    (fizzle)))
	(if* (not (tell rule 'currently-works?))
	  (say "Chosen rule no longer works. Fizzling.")
	  (fizzle))
	;; result ::= #f
	;;          | (<translated-rule> <vertical-bridges> <slippage-log>
	;;             <vertical-mapping-supporting-groups>)
	(let ((result (translate rule)))
	  (if* (not (exists? result))
	    (say "Couldn't translate chosen rule. Fizzling.")
	    (fizzle))
	  (let ((translated-rule (1st result))
		(supporting-vertical-bridges (2nd result))
		(slippage-log (3rd result))
		(vertical-mapping-supporting-groups (4th result))
		(rule-ref-objects (5th result))
		(translated-rule-ref-objects (6th result)))
	    (say "Rule translated. Building an answer...")
	    ;; result ::= #f | ({<object-transform> | <string-transform>} ...)
	    ;; <object-transform> ::= (<ref-object> (<transform> ...))
	    ;; <string-transform> ::= (<string> (<transform> ...))
	    ;; <transform> ::= (<dimension> <descriptor>)
	    (let* ((snag-action
		     (process-snag
		       rule translated-rule supporting-vertical-bridges slippage-log
		       rule-ref-objects))
		   (result (apply-rule translated-rule *target-string* snag-action)))
	      (if* (not (exists? result))
		(fizzle))
	      (if* (tell *memory* 'answer-present?
		     (tell *target-string* 'generate-image-letters)
		     rule translated-rule)
		(say "Already found this answer. Fizzling.")
		(fizzle))
	      (tell translated-rule 'set-quality-values)
	      ;; make-translated-string sets translated-rule's supporting bridges
	      ;; and thematic-pattern:
	      (let* ((translated-string
		       (make-translated-string translated-rule *target-string*))
		     (all-supporting-groups
		       (remq-duplicates
			 (append
			   vertical-mapping-supporting-groups
			   (get-rule-supporting-groups rule translated-rule)))))
		(report-new-answer
		  translated-string rule translated-rule supporting-vertical-bridges
		  all-supporting-groups rule-ref-objects translated-rule-ref-objects
		  slippage-log '())))))))))


;; get-rule-supporting-groups gets all of the groups (but not the letters) that
;; support both of the given rules.  A group supports a rule if either:
;; (1) the rule explicitly refers to the group (i.e., the group is a reference object)
;; (2) the group is connected to a horizontal bridge that supports the rule
;; (3) the group is a nested member of a group of type (2) above

(define get-rule-supporting-groups
  (lambda (top-rule bottom-rule)
    (let* ((supporting-horizontal-bridges
	     (append
	       (tell top-rule 'get-supporting-horizontal-bridges)
	       (tell bottom-rule 'get-supporting-horizontal-bridges)))
	   (top-rule-ref-objects
	     (tell *initial-string* 'get-all-reference-objects top-rule))
	   (bottom-rule-ref-objects
	     (tell *target-string* 'get-all-reference-objects bottom-rule))
	   (all-rule-reference-groups
	     (filter-out (compose not group?)
	       (append top-rule-ref-objects bottom-rule-ref-objects))))
      (remq-duplicates
	(flatten
	  (list
	    all-rule-reference-groups
	    (map (lambda (b)
		   (let ((object1 (tell b 'get-object1))
			 (object2 (tell b 'get-object2)))
		     (list
		       (get-all-nested-groups (tell b 'get-object1))
		       (get-all-nested-groups (tell b 'get-object2)))))
	      supporting-horizontal-bridges)))))))


(define make-translated-string
  (lambda (rule string1)
    (let* ((result (apply-rule rule string1 ignore-snag))
	   (letter-categories (tell string1 'generate-image-letters))
	   (string-type
	     (case (tell string1 'get-string-type)
	       (initial 'modified)
	       (target 'answer)))
	   (string2 (new-workspace-string string-type letter-categories))
	   (object-transforms (filter-out (compose workspace-string? 1st) result))
	   (string-transform (select (compose workspace-string? 1st) result))
	   (image1 (tell string1 'get-image)))
      (tell string2 'mark-as-translated)
      (tell image1 'do-walk 'leaf-walk
	(let ((position 0))
	  (lambda (leaf-image)
	    (tell leaf-image 'instantiate-as-letter string2 position)
	    (set! position (add1 position))
	    'done)))
      (tell string2 'set-letter-list)
      (if* %workspace-graphics%
	(tell *workspace-window* 'init-translated-string-graphics
	  string2 (tell rule 'get-rule-type)))
      (tell image1 'do-walk 'postorder-interior-walk
	(lambda (interior-image)
	  (tell interior-image 'instantiate-as-group string2)))
      (attach-length-to-appropriate-groups object-transforms)
      (let* ((horizontal-bridges
	       (make-translated-rule-bridges object-transforms string-transform))
	     (irrelevant-groups
	       (filter
		 (irrelevant-translated-string-group? horizontal-bridges)
		 (tell string2 'get-groups))))
	(for* each group in irrelevant-groups do
	  (tell string2 'delete-group group))
	(tell rule 'set-translated-rule-information horizontal-bridges)
	string2))))


(define attach-length-to-appropriate-groups
  (lambda (object-transforms)
    (for* each ot in object-transforms do
      (let* ((object (1st ot))
	     (transforms (2nd ot))
	     (instantiated-object (tell object 'get-instantiated-image-object))
	     (Length-transform (assq plato-length transforms))
	     (BondFacet-transform (assq plato-bond-facet transforms)))
	(if* (and (exists? instantiated-object)
	          (group? instantiated-object))
	  (if* (exists? Length-transform)
	    (if* (and (group? object)
		      (not (tell object 'description-type-present? plato-length)))
	      (attach-length-description object))
	    (attach-length-description instantiated-object))
	  (if* (and (exists? BondFacet-transform)
		    (eq? (2nd BondFacet-transform) plato-length))
	    (for* each subobject in (tell instantiated-object 'get-constituent-objects)
	      do (attach-length-description subobject))))))))


(define make-translated-rule-bridges
  (lambda (object-transforms string-transform)
    (let* ((unmapped-string-subobjects
	     (if (exists? string-transform)
	       (remq-elements
		 (map 1st object-transforms)
		 (tell (1st string-transform) 'get-constituent-objects))
	       '()))
	   (instantiated-object-transforms
	     (filter
	       (lambda (ot)
		 (exists? (tell (1st ot) 'get-instantiated-image-object)))
	       object-transforms))
	   (horizontal-bridges
	     (append
	       (map (lambda (ot)
		      (make-horizontal-bridge
			(1st ot)
			(tell (1st ot) 'get-instantiated-image-object)
			(2nd ot)))
		 instantiated-object-transforms)
	       (map (lambda (subobject)
		      (make-horizontal-bridge
			subobject
			(tell subobject 'get-instantiated-image-object)
			'()))
		 unmapped-string-subobjects))))
      (for* each bridge in horizontal-bridges do
	(tell bridge 'mark-as-translated-rule-bridge)
	(if* %workspace-graphics%
	  (tell bridge 'update-proposal-level %built%)
	  (tell bridge 'set-graphics-pexp (make-bridge-pexp bridge %built%))))
      horizontal-bridges)))


;; Example of irrelevant group:
;; Swapping m and [jjj] in string [m][rr][jjj] results in new string [[jjj]][rr]m
;; Outer [[jjj]] group is irrelevant and should be removed from new string.

(define irrelevant-translated-string-group?
  (lambda (horizontal-bridges)
    (let* ((mapped-translated-string-objects
	     (tell-all horizontal-bridges 'get-object2))
	   (mapped-nested-groups
	     (flatmap get-all-nested-groups
	       (filter group? mapped-translated-string-objects))))
      (lambda (group)
	(not (member? group mapped-nested-groups))))))


;; <failure-result> ::=
;;   (SWAP <objects> <dimension>)
;; | (CONFLICT <object1> <dimension1> <object2> <dimension2>)
;; | (CHANGE <object> <transform>)
;; Note: The <object> of a CHANGE can be a workspace string.
;; <transform> ::= (<dimension> <descriptor>)
;;               | (<GroupCtgy> <relation> <BondFacet>)

(define process-snag
  (lambda (rule translated-rule supporting-vertical-bridges
	    slippage-log rule-ref-objects)
    (lambda (failure-result)
      (let ((snag-event
	      (make-snag-event
		failure-result rule translated-rule supporting-vertical-bridges
		slippage-log rule-ref-objects)))
	(tell *trace* 'add-event snag-event)
	(if* (not (tell *memory* 'snag-present? rule))
	  (abstract-snag-description snag-event))
	(tell *comment-window* 'add-comment
	  (list
	    (format "Uh-oh, I seem to have run into a little problem~a.  ~a."
	      (if (> (tell *trace* 'get-num-of-events 'snag) 1) " again" "")
	      (capitalize-string (tell snag-event 'get-explanation))))
	  (list
	    (format "Hit ~a snag:  ~a."
	      (if (> (tell *trace* 'get-num-of-events 'snag) 1) "another" "a")
	      (capitalize-string (tell snag-event 'get-explanation)))))
	(if* %workspace-graphics%
	  (tell *workspace-window* 'draw-current-snag)
	  (tell *workspace-window* 'erase-current-snag))
	(tell *initial-string* 'delete-all-proposed-bonds)
	(tell *initial-string* 'delete-all-proposed-groups)
	(tell *modified-string* 'delete-all-proposed-bonds)
	(tell *modified-string* 'delete-all-proposed-groups)
	(tell *target-string* 'delete-all-proposed-bonds)
	(tell *target-string* 'delete-all-proposed-groups)
	(tell *workspace* 'delete-all-proposed-bridges)
	(set! *temperature* 100)
	(set! *temperature-clamped?* #t)
	(if* %workspace-graphics%
	  (tell *temperature-window* 'update-graphics 100))
	(tell snag-event 'activate)
	;; Deleting all codelets automatically erases all proposed bonds, groups,
	;; and bridges, so we don't need to worry about explicitly erasing them
	;; when they get deleted from the workspace:
	(tell *coderack* 'delete-all-codelets)
	(post-initial-codelets)
	(update-everything)))))


;;--------------------------------- Rule Translation ----------------------------------

(define make-slippage-log
  (lambda (rule-type)
    (let ((directly-applied-slippages '())
	  ;; coattail-slippage-table is a list of the form
	  ;; ((<coattail-slippage> <inducing-slippage>) ...)
	  (coattail-slippage-table '())
	  (slippage-bridges '()))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'slippage-log)
	    (print ()
	      (printf "~nTranslation direction: ~a~n"
		(case rule-type
		  (top "top-to-bottom")
		  (bottom "bottom-to-top")))
	      (printf "Directly-applied slippages:~n")
	      (if (null? directly-applied-slippages)
		(printf "  None~n")
		(for* each slippage in directly-applied-slippages do
		  (printf "  ~a~n" (tell slippage 'print-name))))
	      (printf "Coattail-slippages:~n")
	      (if (null? coattail-slippage-table)
		(printf "  None~n")
		(for* each entry in coattail-slippage-table do
		  (printf "  ~a induced by ~a~n"
		    (tell (1st entry) 'print-name)
		    (tell (2nd entry) 'print-name))))
	      (printf "~n"))
	    (get-directly-applied-slippages () directly-applied-slippages)
	    (get-coattail-slippages () (map 1st coattail-slippage-table))
	    (get-coattail-inducing-slippages () (map 2nd coattail-slippage-table))
	    (get-applied-slippages ()
	      (remq-duplicates
		(append
		  directly-applied-slippages
		  (tell self 'get-coattail-inducing-slippages))))
	    (coattail-inducing-slippage? (slippage)
	      (ormap (lambda (x) (eq? slippage (2nd x)))
		coattail-slippage-table))
	    (get-slippage-bridges () (remq-duplicates slippage-bridges))
	    (get-bridge (slippage)
	      (select
		(lambda (b) (member? slippage (tell b 'get-slippages)))
		slippage-bridges))
	    (get-slippage-to-highlight (slippage)
	      (let* ((vertical-bridge (tell self 'get-bridge slippage))
		     (non-symmetric-slippages
		       (tell vertical-bridge 'get-non-symmetric-slippages)))
		(if (member? slippage non-symmetric-slippages)
		  slippage
		  (select-meth non-symmetric-slippages 'symmetric? slippage))))
	    (get-highlight-color (highlighted-slippage applied-slippage)
	      (if (tell self 'coattail-inducing-slippage? applied-slippage)
		(if (eq? highlighted-slippage applied-slippage)
		  %coattail-inducing-slippage-color%
		  %dim-coattail-inducing-slippage-color%)
		(if (eq? highlighted-slippage applied-slippage)
		  %vertical-slippage-color%
		  %dim-vertical-slippage-color%)))
	    (applied (slippage)
	      (set! directly-applied-slippages
		(cons slippage directly-applied-slippages))
	      (set! slippage-bridges
		(cons (tell (tell slippage 'get-object1) 'get-bridge 'vertical)
		  slippage-bridges))
	      'done)
	    (coattail (node1 label node2 inducing-slippage)
	      (let ((coattail-slippage
		      (make-concept-mapping
			'coattail (tell node1 'get-category) node1
			'coattail (tell node1 'get-category) node2)))
		(set! coattail-slippage-table
		  (cons (list coattail-slippage inducing-slippage)
		    coattail-slippage-table))
		(set! slippage-bridges
		  (cons (tell (tell inducing-slippage 'get-object1)
			  'get-bridge 'vertical)
		    slippage-bridges)))
	      'done)
	    (else (delegate msg base-object))))))))


(define translate
  (lambda (rule)
    (continuation-point* return
      (let* ((rule-type (tell rule 'get-rule-type))
	     (translation-direction
	       (case rule-type
		 (top 'top-to-bottom)
		 (bottom 'bottom-to-top)))
	     (from-string
	       (case rule-type
		 (top *initial-string*)
		 (bottom *target-string*)))
	     (to-string
	       (case rule-type
		 (top *target-string*)
		 (bottom *initial-string*)))
	     (rule-clauses (tell rule 'get-rule-clauses))
	     (slippage-log (make-slippage-log rule-type))
	     (fail (lambda () (return #f)))
	     (result
	       (map (translate-rule-clause from-string to-string slippage-log fail)
		 rule-clauses))
	     (translated-clauses (map-compress 1st result)))
	(if (not (andmap valid-rule-clause? translated-clauses))
	  #f
	  (let* ((translated-rule
		   (make-rule
		     (if (eq? rule-type 'top) 'bottom 'top)
		     translated-clauses))
		 (from-string-ref-objects
		   (filter-out workspace-string?
		     (tell from-string 'get-all-reference-objects rule)))
		 (to-string-ref-objects
		   (filter-out workspace-string?
		     (tell to-string 'get-all-reference-objects translated-rule)))
		 (supporting-vertical-bridges
		   (remq-duplicates
		     (append
		       (tell slippage-log 'get-slippage-bridges)
		       (intersect
			 (compress
			   (tell-all from-string-ref-objects 'get-bridge 'vertical))
			 (compress
			   (tell-all to-string-ref-objects 'get-bridge 'vertical))))))
		 (vertical-mapping-supporting-groups
		   (remq-duplicates
		     (flatmap
		       (lambda (b)
			 (append
			   (get-all-nested-groups (tell b 'get-object1))
			   (get-all-nested-groups (tell b 'get-object2))))
		       supporting-vertical-bridges))))
	    (if* %workspace-graphics%
	      (initialize-rule-graphics translated-rule))
	    (tell translated-rule 'mark-as-translated rule translation-direction)
	    (list
	      translated-rule
	      supporting-vertical-bridges
	      slippage-log
	      vertical-mapping-supporting-groups
	      from-string-ref-objects
	      to-string-ref-objects)))))))


;; translate-rule-clause returns a list of the form
;;    (<translated-clause> (<slippage> ...))
;; where (<slippage> ...) is a list of all the slippages applicable to the rule clause


(define translate-rule-clause
  (lambda (from-string to-string slippage-log fail)
    (lambda (rule-clause)
      (if (verbatim-clause? rule-clause)
	(list rule-clause '())
	(let* ((object-descriptions (2nd rule-clause))
	       (result (map (translate-object-description
			      from-string to-string slippage-log fail)
			 object-descriptions))
	       (translated-object-descriptions (map 1st result))
	       (applicable-object-description-slippages (flatmap 2nd result))
	       (enclosing-slippages (remq-duplicates (flatmap 3rd result)))
	       (possible-transform-slippages
		 (append applicable-object-description-slippages enclosing-slippages))
	       (ignored-dimensions
		 (filter (lambda (d) (prob? 0.4))
		   (remq-duplicates
		     (tell-all possible-transform-slippages 'get-CM-type))))
	       (applicable-transform-slippages
		 (filter-out
		   (lambda (s) (member? (tell s 'get-CM-type) ignored-dimensions))
		   possible-transform-slippages))
	       (translator
		 (case (1st rule-clause)
		   (extrinsic
		     (apply-to-dimension applicable-transform-slippages slippage-log))
		   (intrinsic
		     (apply-to-change applicable-transform-slippages slippage-log))))
	       (translated-rule-clause
		 (list
		   (1st rule-clause)
		   translated-object-descriptions
		   (map translator (3rd rule-clause))))
	       (all-applicable-slippages
		 (append
		   applicable-object-description-slippages
		   applicable-transform-slippages)))
	  (list
	    translated-rule-clause
	    all-applicable-slippages))))))


;; For an intrinsic-clause, this eliminates the (self <ObjCtgy> <group>) change if
;; the object-description denotes a group, or the (self <ObjCtgy> <letter>) change if
;; object-description denotes a letter.  For an extrinsic-clause that doesn't denote
;; subobjects, the <ObjCtgy> dimension is eliminated if the object-descriptions all
;; specify objects of the same type:

(define remove-redundant-ObjCtgy-change
  (lambda (rule-clause)
    (record-case rule-clause
      (extrinsic (object-descriptions dimensions)
	(if (and (member? plato-object-category dimensions)
	         (not (= (length object-descriptions) 1))
		 (all-same? (map 1st object-descriptions)))
	  (let ((new-dimensions (remq plato-object-category dimensions)))
	    (if (null? new-dimensions)
	      #f
	      (list 'extrinsic object-descriptions new-dimensions)))
	  rule-clause))
      (intrinsic (object-descriptions changes)
	(let ((ObjCtgy:self-change (select-change plato-object-category 'self changes))
	      (object-type (1st (1st object-descriptions))))
	  (if (and (exists? ObjCtgy:self-change)
		   (eq? (3rd ObjCtgy:self-change) object-type))
	    (let ((new-changes (remq ObjCtgy:self-change changes)))
	      (if (null? new-changes)
		#f
		(list 'intrinsic object-descriptions new-changes)))
	    rule-clause))))))


;; translate-object-description returns a list of the form
;; (<translated-object-description>
;;  <applicable-object-description-slippages>
;;  <enclosing-bond-slippages>)
;; where <applicable-object-description-slippages> is a list of slippages
;; _applicable_ to object-description (but not necessarily applied to it).
;; Whether these slippages are actually _used_ in the translation process
;; is a probabilistic decision made by the 'apply-slippages slipnode method.
;; The slippages that actually get used are recorded in the slippage-log.

(define translate-object-description
  (lambda (from-string to-string slippage-log fail)
    (lambda (object-description)
      (let ((from-objects
	      (tell from-string
		'get-object-description-ref-objects object-description)))
	(if* (null? from-objects)
	  (fail))
	(let ((vertical-bridges
		(tell-all from-objects 'get-bridge 'vertical)))
	  (if (not (all-exist? vertical-bridges))
	    (list
	      (if (whole-string-object-description? object-description)
		(translate-whole-string-object-description
		  object-description to-string)
		object-description)
	      '() '())
	    (let ((all-enclosing-groups1
		    (tell-all vertical-bridges 'get-enclosing-group1))
		  (all-enclosing-groups2
		    (tell-all vertical-bridges 'get-enclosing-group2)))
	      (if* (or (not (all-same? all-enclosing-groups1))
		       (not (all-same? all-enclosing-groups2)))
		(fail))
	      (let* ((all-bridge-slippages
		       (apply append
			 (tell-all vertical-bridges 'get-slippages)))
		     (translation-direction
		       (if (tell from-string 'top-string?) 'down 'up))
		     (symmetric-bridge-slippages
		       (apply append
			 (tell-all vertical-bridges
			   (case translation-direction
			     (down 'get-symmetric-slippages)
			     (up 'get-non-symmetric-slippages)))))
		     (applicable-object-description-slippages
		       (filter-out
			 (lambda (s)
			   (and (member? s symmetric-bridge-slippages)
			        (not (eq? (tell s 'get-label) plato-opposite))))
			 all-bridge-slippages))
		     (translated-object-description
		       (apply-to-object-description
			 applicable-object-description-slippages
			 object-description
			 slippage-log))
		     (enclosing-group1 (1st all-enclosing-groups1))
		     (enclosing-group2 (1st all-enclosing-groups2))
		     (enclosing-bridge
		       (if (and (exists? enclosing-group1)
			        (exists? enclosing-group2)
				(bridge-between? 'vertical
				  enclosing-group1 enclosing-group2))
			 (tell enclosing-group1 'get-bridge 'vertical)
			 #f))
		     (all-enclosing-bond-slippages
		       (if (exists? enclosing-bridge)
			 (tell enclosing-bridge 'get-bond-slippages)
			 '())))
		(list
		  translated-object-description
		  applicable-object-description-slippages
		  all-enclosing-bond-slippages)))))))))


;; This actually could be done with just the plato-whole line, but
;; the 'string line is included for completeness:

(define whole-string-object-description?
  (lambda (object-description)
    (or (eq? (1st object-description) 'string)
        (eq? (3rd object-description) plato-whole))))
      

(define translate-whole-string-object-description
  (lambda (object-description to-string)
    (if (tell to-string 'spanning-group-exists?)
      (list plato-group plato-string-position-category plato-whole)
      (list 'string plato-string-position-category plato-whole))))


(define apply-to-change
  (lambda (slippages slippage-log)
    (lambda (change)
      (list (1st change)
	    (tell (2nd change) 'apply-slippages slippages slippage-log)
	    (tell (3rd change) 'apply-slippages slippages slippage-log)))))


(define apply-to-dimension
  (lambda (slippages slippage-log)
    (lambda (dimension)
      (tell dimension 'apply-slippages slippages slippage-log))))


(define apply-to-object-description
  (lambda (slippages object-description slippage-log)
    (list (if (eq? (1st object-description) 'string)
	    (1st object-description)
	    (tell (1st object-description) 'apply-slippages slippages slippage-log))
          (tell (2nd object-description) 'apply-slippages slippages slippage-log)
	  (tell (3rd object-description) 'apply-slippages slippages slippage-log))))


(define valid-rule-clause?
  (lambda (rule-clause)
    (or (verbatim-clause? rule-clause)
        (and (extrinsic-clause? rule-clause)
	     (andmap valid-object-description? (2nd rule-clause))
	     (or (> (length (2nd rule-clause)) 1)
	         (not (eq? (1st (1st (2nd rule-clause))) plato-letter))))
	(and (intrinsic-clause? rule-clause)
	     (valid-object-description? (1st (2nd rule-clause)))
	     (andmap valid-change? (3rd rule-clause))))))


(define valid-object-description?
  (lambda (object-description)
    (eq? (tell (3rd object-description) 'get-category)
         (2nd object-description))))


(define valid-change?
  (lambda (change)
    (or (platonic-relation? (3rd change))
        (eq? (tell (3rd change) 'get-category) (2nd change)))))

