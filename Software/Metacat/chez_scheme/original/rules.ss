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

;; NOTE: in the code here, I use the tag symbols 'self and 'subobjects instead
;; of 'object and 'components as described in section 3.3.4 of my PhD thesis (for
;; example, in Figures 3.2 through 3.4).  I originally developed the code using
;; the self/subobjects notation, but never got around to changing the names to
;; object/components, even though I think object/components is clearer.
;;
;; Likewise, there are several other notational differences between the grammar
;; shown in Figure 3.2 and the code here (for example, a <change-description> in
;; Figure 3.2 is called a <change> here).  However, all of these differences are
;; purely syntactic.  The structure of rules is exactly the same.  One of these
;; days I'll update the code to reflect the notation used in Chapter 3.

;;------------------------------------- Rules ----------------------------------------

;; <intrinsic-rule-clause-template> ::=
;;    (intrinsic <ref-object> (<change-template> ...))
;; <extrinsic-rule-clause-template> ::=
;;    (extrinsic (<ref-object> ...) (<dimension> ...))
;;
;; Normally, the denoted objects of an extrinsic rule-clause-template are
;; just the reference objects themselves, but if the template has exactly one
;; <ref-object>, then the denoted objects are the _subobjects_ of <ref-object>
;;
;; <intrinsic-rule-clause> ::=
;;    (intrinsic (<object-description>) (<change> ...))
;; <extrinsic-rule-clause> ::=
;;    (extrinsic (<object-description> ...) (<dimension> ...))
;; <verbatim-rule-clause> ::=
;;    (verbatim (<letter-category> ...))
;;
;; <scope> ::= self | subobjects
;; <change-template> ::= (<scope> <dimension> (<descriptor> ...))
;; <change> ::= (<scope> <dimension> <descriptor>)
;; <object-description> ::= (<object-type> <object-desc-type> <object-descriptor>)
;;                        | (string <StrPosCtgy> <whole>)
;;
;; Example:
;;   intrinsic-rule-clause-template:
;;      (intrinsic [abc] ((self <DirCtgy> (<opp> <left>))
;;                        (subobjects <Length> (<succ> <two>))
;;                        (subobjects <ObjCtgy> (<group>))))
;;   intrinsic-rule-clause:
;;      (intrinsic (<group> <StrPosCtgy> <whole>) ((self <DirCtgy> <opp>)
;;                                                 (subobjects <Length> <two>)
;;                                                 (subobjects <ObjCtgy> <group>)))

(define rule-characterization
  (lambda (rule)
    (list
      (tell rule 'get-rule-type)
      (cond
	((tell rule 'verbatim?) 'verbatim)
	((tell rule 'literal?) 'literal)
	((tell rule 'abstract?) 'abstract)))))


(define make-rule
  (lambda (rule-type rule-clauses)
    (let* ((workspace-structure (make-workspace-structure))
	   (intrinsic-rule-clauses (filter intrinsic-clause? rule-clauses))
	   (extrinsic-rule-clauses (filter extrinsic-clause? rule-clauses))
	   (bridge-theme-type
	     (case rule-type
	       (top 'top-bridge)
	       (bottom 'bottom-bridge)))
	   (rule-clause-templates #f)
	   (theme-pattern #f)
	   (tagged-supporting-horizontal-bridges #f)
	   (supporting-horizontal-bridges #f)
	   (translated? #f)
	   (original-rule #f)
	   (translation-direction #f)
	   (uniformity 0)
	   (abstractness 0)
	   (succinctness 0)
	   (quality 0)
	   ;; temporary. leave this for now:
	   (intrinsic-quality 0)
	   (english-transcription (transcribe-to-english rule-type rule-clauses))
	   (clamped-graphics-pexp #f)
	   (rule-graphics-center-coord #f)
	   (rule-graphics-height #f))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'rule)
	    (get-rule-type () rule-type)
	    (type? (type) (eq? type rule-type))
	    (get-clamped-graphics-pexp () clamped-graphics-pexp)
	    (get-rule-graphics-center-coord () rule-graphics-center-coord)
	    (get-rule-graphics-height () rule-graphics-height)
	    (set-auxiliary-rule-graphics-info (pexp coord height)
	      (set! clamped-graphics-pexp pexp)
	      (set! rule-graphics-center-coord coord)
	      (set! rule-graphics-height height)
	      'done)
	    (print-english ()
	      (for* each line in english-transcription do
		(printf "~a~n" line)))
	    (print ()
	      (tell self 'print-english)
	      (printf "(Type=~a, Q=~a, RQ=~a, Uniformity=~a, Abstractness=~a, "
		rule-type quality (tell self 'get-relative-quality)
		uniformity abstractness)
	      (printf "Succinctness=~a)~n~n" succinctness))
	    (show ()
	      (for* each rc in rule-clauses do
		(printf-rule-clause rc))
	      (newline))
	    (show-all ()
	      (newline)
	      (for* each template in rule-clause-templates do
		(printf-rule-clause-template template))
	      (newline)
	      (tell self 'show)
	      (tell self 'print))
	    (mark-as-translated (rule direction)
	      (set! original-rule rule)
	      (set! translation-direction direction)
	      (set! translated? #t)
	      'done)
	    (get-original-rule () original-rule)
	    (get-translation-direction () translation-direction)
	    (set-abstracted-rule-information (templates)
	      (set! rule-clause-templates templates)
	      (set! tagged-supporting-horizontal-bridges
		(map get-tagged-supporting-horizontal-bridges templates))
	      (set! supporting-horizontal-bridges
		(apply append (map 2nd tagged-supporting-horizontal-bridges)))
	      (for* each bridge in supporting-horizontal-bridges do
		(if* (tell bridge 'slippage-type-present? plato-length)
		  (let ((object1 (tell bridge 'get-object1))
			(object2 (tell bridge 'get-object2)))
		    (if* (and (group? object1)
			      (not (tell object1 'description-type-present?
				     plato-length)))
		      (attach-length-description object1))
		    (if* (and (group? object2)
			      (not (tell object2 'description-type-present?
				     plato-length)))
		      (attach-length-description object2)))))
	      (set! theme-pattern
		(tell *themespace* 'get-dominant-theme-pattern bridge-theme-type))
	      'done)
	    (set-translated-rule-information (bridges)
	      (for* each bridge in bridges do
		(let* ((object1 (tell bridge 'get-object1))
		       (fake-object2 (tell bridge 'get-object2))
		       (real-object2 (tell *workspace* 'get-real-object fake-object2)))
		  (if* (exists? real-object2)
		    (for* each d in (tell real-object2 'get-descriptions) do
		      (let ((dtype (tell d 'get-description-type))
			    (descriptor (tell d 'get-descriptor)))
			(if* (not (tell fake-object2 'description-type-present? dtype))
			  (tell fake-object2 'attach-description dtype descriptor))))
		    (let ((bridge-CMs
			    (all-possible-bridge-CMs 'horizontal
			      object1 (tell object1 'get-descriptions)
			      real-object2 (tell real-object2 'get-descriptions))))
		      (tell bridge 'set-concept-mappings bridge-CMs)))))
	      (set! supporting-horizontal-bridges bridges)
	      (set! tagged-supporting-horizontal-bridges
		(map (lambda (bridge) (list #f (list bridge)))
		  bridges))
	      (set! theme-pattern
		(cons bridge-theme-type
		  (remove-duplicates
		    (apply append
		      (tell-all bridges 'get-associated-thematic-relations)))))
	      'done)
	    (set-verbatim-rule-information ()
	      (set! rule-clause-templates '())
	      (set! tagged-supporting-horizontal-bridges '())
	      (set! supporting-horizontal-bridges '())
	      (set! theme-pattern (list bridge-theme-type))
	      'done)
	    (set-quality-values ()
	      (set! uniformity (compute-rule-uniformity self))
	      (set! abstractness (compute-rule-abstractness self))
	      (set! succinctness (compute-rule-succinctness self))
	      ;; temporary:
	      (set! intrinsic-quality (compute-rule-intrinsic-quality self))
	      (set! quality (compute-rule-quality self))
	      'done)
	    (get-verbatim-letter-categories () (2nd (1st rule-clauses)))
	    (identity? () (null? rule-clauses))
	    (verbatim? ()
	      (and (= (length rule-clauses) 1)
		   (verbatim-clause? (1st rule-clauses))))
	    (literal? ()
	      (and (not (tell self 'verbatim?)) (ormap literal-clause? rule-clauses)))
	    (abstract? ()
	      (and (not (tell self 'verbatim?)) (not (tell self 'literal?))))
	    (get-characterization ()
	      (cond
		((tell self 'verbatim?) 'verbatim)
		((tell self 'literal?) 'literal)
		((tell self 'abstract?) 'abstract)))
	    (translated? () translated?)
	    (supported? ()
	      (andmap
		(lambda (b) (tell *workspace* 'bridge-present? b))
		supporting-horizontal-bridges))
	    (currently-works? ()
	      (let* ((string1
		       (case rule-type
			 (top *initial-string*)
			 (bottom *target-string*)))
		     (string2
		       (case rule-type
			 (top *modified-string*)
			 (bottom *answer-string*)))
		     (result (apply-rule self string1 ignore-snag)))
		(and (exists? result)
		     (equal? (tell string1 'generate-image-letters)
		             (tell string2 'get-letter-categories)))))
	    (equal? (rule) (rules-equal? self rule))
	    (get-degree-of-support ()
	      (100* (product
		      (map (lambda (b)
			     (% (tell (tell *workspace* 'get-equivalent-bridge b)
				  'get-strength)))
			supporting-horizontal-bridges))))
	    (get-quality () quality)
	    (get-relative-quality ()
	      (let ((ranked-rules
		      (sort-by-method 'get-quality <
			(tell *workspace* 'get-rules rule-type))))
		(if (member? self ranked-rules)
		  (let ((rank (+ 1 (list-index ranked-rules self))))
		    (100* (/ rank (length ranked-rules))))
		  quality)))
	    (get-uniformity () uniformity)
	    (get-abstractness () abstractness)
	    (get-succinctness () succinctness)
	    ;; temporary:
	    (get-intrinsic-quality () intrinsic-quality)
	    (get-rule-clauses () rule-clauses)
	    (get-intrinsic-rule-clauses () intrinsic-rule-clauses)
	    (get-extrinsic-rule-clauses () extrinsic-rule-clauses)
	    (get-english-transcription () english-transcription)
	    (get-rule-clause-templates () rule-clause-templates)
	    (get-tagged-supporting-horizontal-bridges ()
	      tagged-supporting-horizontal-bridges)
	    (get-supporting-horizontal-bridges () supporting-horizontal-bridges)
	    (get-theme-pattern () theme-pattern)
	    (get-concept-pattern ()
	      (cons 'concepts
		(map (lambda (node) (list node %max-activation%))
		  (remq-duplicates (filter-out symbol? (flatten rule-clauses))))))
	    (revise-abstracted-rule-information (proposed-rule)
	      (if* (ormap unsupported-self-change?
		     tagged-supporting-horizontal-bridges)
		(set! tagged-supporting-horizontal-bridges
		  (map (lambda (old-bridges new-bridges)
			 (if (and (unsupported-self-change? old-bridges)
			          (not (unsupported-self-change? new-bridges)))
			   new-bridges
			   old-bridges))
		    tagged-supporting-horizontal-bridges
		    (tell proposed-rule 'get-tagged-supporting-horizontal-bridges)))
		(set! supporting-horizontal-bridges
		  (remq-duplicates
		    (apply append (map 2nd tagged-supporting-horizontal-bridges)))))
	      (set! theme-pattern (tell proposed-rule 'get-theme-pattern))
	      'done)
	    (calculate-internal-strength () (tell self 'get-relative-quality))
	    (calculate-external-strength () (tell self 'calculate-internal-strength))
	    (else (delegate msg workspace-structure))))))))


(define unsupported-self-change? 1st)


;; This returns a list of the form (<boolean> (<bridge> ...)), where
;; (<bridge> ...) are the supporting bridges for the rule-clause-template.
;; For example, for [abc] -> [cba], a rule-clause-template with a reference
;; object of [abc] and a (self <Dir> ...) change-template would be supported
;; by the [abc]--[cba] bridge.  <boolean> = #t iff the rule-clause-template
;; specifies a self change but a bridge doesn't exist for the reference object.
;; In this case, the reference object's subobject bridges are returned.  For
;; example, if no bridge exists from [abc] yet, the subobject bridges a-a, b-b,
;; c-d are returned, with <boolean> =  #t.  If the [abc]--[cba] bridge is later
;; created, the supporting bridges can be revised from a-a, b-b, c-d to just
;; [abc]--[cba].

(define get-tagged-supporting-horizontal-bridges
  (lambda (rule-clause-template)
    (record-case rule-clause-template
      (extrinsic (ref-objects dimensions)
	(if (= (length ref-objects) 1)
	  (list #f (tell (1st ref-objects) 'get-subobject-bridges 'horizontal))
	  (list #f (tell-all ref-objects 'get-bridge 'horizontal))))
      (intrinsic (ref-object change-templates)
	(let* ((self-bridge
		 (if (workspace-string? ref-object)
		   #f
		   (tell ref-object 'get-bridge 'horizontal)))
	       (subobject-bridges
		 (if (ormap (lambda (ct) (eq? (1st ct) 'subobjects)) change-templates)
		   (tell ref-object 'get-subobject-bridges 'horizontal)
		   '()))
	       (change-self?
		 (ormap (lambda (ct) (eq? (1st ct) 'self)) change-templates))
	       (supporting-bridges
		 (if change-self?
		   (if (exists? self-bridge)
		     (cons self-bridge subobject-bridges)
		     (tell ref-object 'get-subobject-bridges 'horizontal))
		   subobject-bridges))
	       (unsupported-self-change?
		 (and change-self? (not (exists? self-bridge)))))
	  (list unsupported-self-change? supporting-bridges))))))


(define get-supporting-horizontal-bridges
  (lambda (rule-clause-template)
    (record-case rule-clause-template
      (extrinsic (ref-objects dimensions)
	(if (= (length ref-objects) 1)
	  (tell (1st ref-objects) 'get-subobject-bridges 'horizontal)
	  (tell-all ref-objects 'get-bridge 'horizontal)))
      (intrinsic (ref-object change-templates)
	(let* ((self-bridge
		 (if (workspace-string? ref-object)
		   #f
		   (tell ref-object 'get-bridge 'horizontal)))
	       (subobject-bridges
		 (if (ormap (lambda (ct) (eq? (1st ct) 'subobjects)) change-templates)
		   (tell ref-object 'get-subobject-bridges 'horizontal)
		   '()))
	       (change-self?
		 (ormap (lambda (ct) (eq? (1st ct) 'self)) change-templates)))
	  (if change-self?
	    (if (exists? self-bridge)
	      (cons self-bridge subobject-bridges)
	      (tell ref-object 'get-subobject-bridges 'horizontal))
	    subobject-bridges))))))


;; This works because all the components of the rule-clause-templates,
;; as well as the rule-clause-templates themselves, are fully sorted
;; when they get created:

(define rules-equal?
  (lambda (rule1 rule2)
    (rule-clause-lists-equal?
      (tell rule1 'get-rule-clauses)
      (tell rule2 'get-rule-clauses))))

(define rule-clause-lists-equal?
  (lambda (rc-list1 rc-list2)
    (and (= (length rc-list1) (length rc-list2))
         (andmap rule-clauses-equal? rc-list1 rc-list2))))

(define rule-clauses-equal?
  (lambda (rc1 rc2)
    (and (eq? (1st rc1) (1st rc2))
         (= (length (2nd rc1)) (length (2nd rc2)))
	 (if (verbatim-clause? rc1)
	   (equal? (2nd rc1) (2nd rc2))
	   (and (andmap object-descriptions-equal? (2nd rc1) (2nd rc2))
	        (equal? (3rd rc1) (3rd rc2)))))))

(define object-descriptions-equal?
  (lambda (od1 od2)
    (and (or (eq? (1st od1) (1st od2))
	     (and (or (eq? (1st od1) plato-group) (eq? (1st od1) 'string))
	          (or (eq? (1st od2) plato-group) (eq? (1st od2) 'string))))
         (eq? (2nd od1) (2nd od2))
	 (eq? (3rd od1) (3rd od2)))))


;;--------------------------------- Rule Codelets --------------------------------------

(define %verbatim-rule-probability% 0.01)

(define-codelet-procedure* rule-scout
  (lambda ()
    (stochastic-if* %verbatim-rule-probability%
      (let* ((rule-type (if %justify-mode% (random-pick '(top bottom)) 'top))
	     (letter-categories
	       (case rule-type
		 (top (tell *modified-string* 'get-letter-categories))
		 (bottom (tell *answer-string* 'get-letter-categories))))
	     (rule-clauses (list (list 'verbatim letter-categories)))
	     (proposed-rule (make-rule rule-type rule-clauses)))
	(say "Proposing a verbatim " rule-type " rule...")
	(tell proposed-rule 'set-quality-values)
	(tell proposed-rule 'set-verbatim-rule-information)
	(tell proposed-rule 'update-proposal-level %proposed%)
	(post-codelet* urgency: %low-urgency% rule-evaluator proposed-rule)
	(fizzle)))
    (let ((possible-rule-types (tell *workspace* 'get-possible-rule-types)))
      (if* (null? possible-rule-types)
	(say "No rule possible. Fizzling.")
	(fizzle))
      (let* ((rule-type (random-pick possible-rule-types))
	     (describable-bridges
	       (filter rule-describable-bridge?
		 (tell *workspace* 'get-bridges rule-type)))
	     (all-change-descriptions
	       (abstract-change-descriptions describable-bridges))
	     (final-change-descriptions
	       (remove-redundant-change-descriptions all-change-descriptions))
	     (rule-clause-templates
	       (sort-templates
		 (map change-descriptions->rule-clause-template
		   (partition
		     (lambda (c1 c2)
		       (and (tell c1 'same-change-type? c2)
			    (tell c1 'same-reference-objects? c2)))
		     final-change-descriptions)))))
	(if* (not (possible-to-instantiate? rule-clause-templates))
	  (say "Not all rule objects are describable. Fizzling.")
	  (fizzle))
	(let* ((rule-clauses
		 (map instantiate-rule-clause-template rule-clause-templates))
	       (proposed-rule (make-rule rule-type rule-clauses)))
	  (tell proposed-rule 'set-quality-values)
	  (tell proposed-rule 'set-abstracted-rule-information rule-clause-templates)
	  (tell proposed-rule 'update-proposal-level %proposed%)
	  (post-codelet* urgency: %high-urgency% rule-evaluator proposed-rule))))))


(define possible-to-instantiate?
  (lambda (rule-clause-templates)
    (andmap
      (lambda (template)
	(if (intrinsic-clause? template)
	  (object-description-possible? (2nd template))
	  (andmap object-description-possible? (2nd template))))
      rule-clause-templates)))


(define object-description-possible?
  (lambda (object)
    (or (workspace-string? object)
        (not (null? (tell object 'get-descriptions-for-rule))))))


(define-codelet-procedure* rule-evaluator
  (lambda (proposed-rule)
    (if* (not (tell proposed-rule 'currently-works?))
      (say "Proposed rule doesn't work. Fizzling.")
      (fizzle))
    (tell proposed-rule 'update-strength)
    (let ((strength (tell proposed-rule 'get-strength)))
      (stochastic-if* (1- (% strength))
	(say "Proposed rule not strong enough. Fizzling.")
	(fizzle))
      (tell proposed-rule 'update-proposal-level %evaluated%)
      (post-codelet* urgency: %high-urgency% rule-builder proposed-rule))))


(define-codelet-procedure* rule-builder
  (lambda (proposed-rule)
    (activate-rule-descriptors-from-workspace proposed-rule)
    (let ((equivalent-rule (tell *workspace* 'get-equivalent-rule proposed-rule)))
      (if* (exists? equivalent-rule)
	(say "This rule already exists. Fizzling.")
	(tell equivalent-rule 'revise-abstracted-rule-information proposed-rule)
	(fizzle)))
    (tell proposed-rule 'update-proposal-level %built%)
    (tell *workspace* 'add-rule proposed-rule)
    (monitor-new-rules proposed-rule)
    (vprint proposed-rule)
    (if* %workspace-graphics%
      (initialize-rule-graphics proposed-rule))
    (if %justify-mode%
      (post-codelet* urgency: %extremely-high-urgency% answer-justifier)
      (post-codelet* urgency: %extremely-high-urgency% answer-finder))))


(define activate-rule-descriptors-from-workspace
  (lambda (rule)
    (for* each clause in (tell rule 'get-rule-clauses) do
      (if* (not (verbatim-clause? clause))
	(for* each object-description in (2nd clause) do
	  (tell (3rd object-description) 'activate-from-workspace))
	(if* (intrinsic-clause? clause)
	  (for* each change in (3rd clause) do
	    (tell (3rd change) 'activate-from-workspace)))))))


;;-------------------------------- Rule Abstraction -----------------------------------

;; See section 3.3.6 of my PhD thesis for an explanation of rule abstraction.


;; rule-describable-bridge? avoids cases where a horizontal bridge cannot be
;; considered as describing a change from object1 to object2 because that change
;; is too hard to express:
;; (1) letter=>letter.  All letter->letter bridges are ok.
;; (2) letter=>group.  Example: a --> [>]:((aa)(bb))   The only type of group a
;;     letter can change into directly is a letter-category sameness group.
;;     For any other letter=>group change, the letter first has to be perceived
;;     as a group itself (maybe if images were given the group category
;;     involved they could handle such changes directly...?  Otherwise, this
;;     might be a good point to add top-down pressure to find single-letter
;;     groups of the desired group category).
;; (3) group=>letter.  To change a group into a letter, images don't need the
;;     group category, so as long as the group is based on letter-category
;;     rather than lengths, such a bridge should be ok.
;; (4) group=>group.  For bridges between groups, both groups must have
;;     the same bond-facet. Ex: [>]:mrrjjj --> [<]:mmmrrj should be ok,
;;     but [>]:abc --> [>]:mrrjjj is too hard to describe.

(define rule-describable-bridge?
  (lambda (bridge)
    (let ((object1 (tell bridge 'get-object1))
	  (object2 (tell bridge 'get-object2)))
      (or (and (letter? object1) (letter? object2))
	  (and (letter? object1) (group? object2)
	       (eq? (tell object2 'get-bond-facet) plato-letter-category)
	       (eq? (tell object2 'get-group-category) plato-samegrp))
	  (and (group? object1) (letter? object2)
	       (eq? (tell object1 'get-bond-facet) plato-letter-category))
	  (and (group? object1) (group? object2)
	       (eq? (tell object1 'get-bond-facet) (tell object2 'get-bond-facet))
	       (related? (tell object1 'get-group-category)
			 (tell object2 'get-group-category)))))))


(define abstract-change-descriptions
  (lambda (bridges)
    (let* ((all-change-descriptions '())
	   (add-extrinsic-change-description
	     (lambda (objects dim descs abstraction-possible?)
	       (let ((change-description
		       (make-extrinsic-change-description objects dim descs)))
		 (if* abstraction-possible?
		   (tell change-description 'mark-as-subobjects-swap-if-possible))
		 (set! all-change-descriptions
		   (cons change-description all-change-descriptions)))
	       'done))
	   (add-intrinsic-change-description
	     (lambda (object scope dim desc1 relation desc2)
	       ;; Disallow individual StrPosCtgy changes:
	       (if* (not (eq? dim plato-string-position-category))
		 (set! all-change-descriptions
		   (cons (make-intrinsic-change-description
			   object scope dim desc1 relation desc2)
		     all-change-descriptions)))
	       'done))
	   (swap-abstraction-probability 0.75)
	   (subobjects-abstraction-probability 0.75))
      ;; Add individual (ie., intrinsic) change-descriptions:
      (for* each b in bridges do
	(for* each slippage in (tell b 'get-non-symmetric-non-bond-slippages) do
	  (add-intrinsic-change-description (tell b 'get-object1) 'self
	    (tell slippage 'get-CM-type)
	    (tell slippage 'get-descriptor1)
	    (tell slippage 'get-label)
	    (tell slippage 'get-descriptor2))))
      ;; Add some swap (ie., extrinsic) change-descriptions:
      (let ((swaps-partition
	      (if (null? bridges)
		'()
		(bounded-random-partition
		  disjoint-left-objects?
		  bridges
		  (add1 (random (length bridges)))))))
	(for* each cluster in swaps-partition do
	  (let* ((all-swaps (get-all-swaps cluster))
		 (Length-swap (select-swap plato-length all-swaps))
		 (ObjCtgy-swap (select-swap plato-object-category all-swaps))
		 (remaining-swaps (remq Length-swap (remq ObjCtgy-swap all-swaps))))
	    ;; Add swaps probabilistically.  However, if both Length and ObjCtgy swaps
	    ;; exist, make sure that either they both get added, or neither gets added.
	    ;; This ensures better rule-clause uniformity:
	    (stochastic-if* swap-abstraction-probability
	      (let ((abstract-subobjects? (prob? subobjects-abstraction-probability)))
		(if* (exists? Length-swap)
		  (add-extrinsic-change-description
		    (swap-objs Length-swap) plato-length (swap-descs Length-swap)
		    abstract-subobjects?))
		(if* (exists? ObjCtgy-swap)
		  (add-extrinsic-change-description
		    (swap-objs ObjCtgy-swap) plato-object-category (swap-descs ObjCtgy-swap)
		    abstract-subobjects?))))
	    (for* each swap in remaining-swaps do
	      (stochastic-if* swap-abstraction-probability
		(add-extrinsic-change-description
		  (swap-objs swap) (swap-dim swap) (swap-descs swap)
		  (prob? subobjects-abstraction-probability)))))))
      ;; Try to abstract changes common to all bridges in each partition set:
      (let ((subobjects-partition
	      (partition same-left-enclosing-objects? bridges)))
	(for* each cluster in subobjects-partition do
	  (let ((left-enclosing-object (get-left-enclosing-object cluster)))
	    (if* (not (tell left-enclosing-object 'singleton-group?))
	      ;; This allows direction reversal to be abstracted based on
	      ;; StrPosCtgy:Opposite slippages:
	      (if* (and (spans-left-side? cluster)
		        (= (count-meth cluster 'StrPosCtgy:Opposite-slippage?) 2))
		(stochastic-if* subobjects-abstraction-probability
		  (add-intrinsic-change-description left-enclosing-object 'self
		    plato-direction-category #f plato-opposite #f)))
	      (for* each schema in (get-common-change-schemas cluster) do
		(if* (or (and (spans-left-side? cluster)
			      (not (common-right-enclosing-object? cluster)))
		         (and (spans-left-side? cluster)
			      (common-right-enclosing-object? cluster)
			      (spans-right-side? cluster))
			 (and (spans-left-side? cluster)
			      (common-right-enclosing-object? cluster)
			      (all-subobjects-describable?
				(get-right-enclosing-object cluster)
				(schema-dim schema)
				(schema-desc2 schema)))
			 (and (common-right-enclosing-object? cluster)
			      (spans-right-side? cluster)
			      (all-subobjects-describable?
				left-enclosing-object
				(schema-dim schema)
				(schema-desc1 schema))))
		  (stochastic-if* subobjects-abstraction-probability
		    (add-intrinsic-change-description left-enclosing-object 'subobjects
		      (schema-dim schema) (schema-desc1 schema)
		      (schema-relation schema) (schema-desc2 schema)))))))))
      all-change-descriptions)))


(define all-subobjects-describable?
  (lambda (object description-type descriptor)
    (and (exists? descriptor)
      (andmap
	(lambda (obj) (eq? (tell obj 'get-descriptor-for description-type) descriptor))
	(tell object 'get-constituent-objects)))))

(define same-left-enclosing-objects?
  (lambda (b1 b2)
    (eq? (tell b1 'get-enclosing-group1)
         (tell b2 'get-enclosing-group1))))

(define disjoint-left-objects?
  (lambda (b1 b2)
    (disjoint-objects?
      (tell b1 'get-object1)
      (tell b2 'get-object1))))

(define common-right-enclosing-object?
  (lambda (bridges)
    (all-same? (tell-all bridges 'get-enclosing-group2))))

(define get-left-enclosing-object
  (lambda (bridges)
    (get-enclosing-object (tell (1st bridges) 'get-object1))))

(define get-right-enclosing-object
  (lambda (bridges)
    (get-enclosing-object (tell (1st bridges) 'get-object2))))

(define get-enclosing-object
  (lambda (object)
    (let ((enclosing-group (tell object 'get-enclosing-group)))
      (if (exists? enclosing-group)
	enclosing-group
	(tell object 'get-string)))))

(define spans-left-side?
  (lambda (bridges)
    (sets-equal?
      (tell-all bridges 'get-object1)
      (tell (get-left-enclosing-object bridges) 'get-constituent-objects))))

(define spans-right-side?
  (lambda (bridges)
    (sets-equal?
      (tell-all bridges 'get-object2)
      (tell (get-right-enclosing-object bridges) 'get-constituent-objects))))


;;----------------------------------- Schemas -----------------------------------------

;; <schema> ::= (<schema-dim> <schema-desc1> <schema-relation> <schema-desc2>)
;; Examples:  (Length two Identity two)  for bridge [aa]-->[bb]
;;            (ObjCtgy letter #f group)           "       a-->[bb]
;;            (LettCtgy a successor b)            "       a-->[bb]


(define get-common-change-schemas
  (lambda (cluster)
    (filter-out
      (lambda (s) (eq? (schema-relation s) plato-identity))
      (get-common-schemas cluster))))


(define get-common-schemas
  (lambda (cluster)
    (let* ((num-bridges (length cluster))
	   (concept-mapping-partition
	    (partition
	     (lambda (cm1 cm2)
	       (eq? (tell cm1 'get-CM-type)
		    (tell cm2 'get-CM-type)))
	     (apply append (tell-all cluster 'get-concept-mappings)))))
      (map-compress concept-mappings->schema
	(filter-out
	  (lambda (p) (< (length p) num-bridges))
	  concept-mapping-partition)))))


(define concept-mappings->schema
  (lambda (concept-mappings)
    (let* ((cm-type (tell (1st concept-mappings) 'get-CM-type))
	   (labels (tell-all concept-mappings 'get-label))
	   (descriptor1s (tell-all concept-mappings 'get-descriptor1))
	   (descriptor2s (tell-all concept-mappings 'get-descriptor2))
	   (common-relation (if (all-same? labels) (1st labels) #f))
	   (common-descriptor1 (if (all-same? descriptor1s) (1st descriptor1s) #f))
	   (common-descriptor2 (if (all-same? descriptor2s) (1st descriptor2s) #f)))
      (if (or (exists? common-relation) (exists? common-descriptor2))
	  (list cm-type common-descriptor1 common-relation common-descriptor2)
	  #f))))


(define printf-schema
  (lambda (s)
    (printf "  ~a : ~a ~a ~a~n"
      (tell (schema-dim s) 'get-short-name)
      (if (exists? (schema-desc1 s)) (tell (schema-desc1 s) 'get-short-name) "*")
      (if (exists? (schema-relation s)) (tell (schema-relation s) 'get-short-name) "*")
      (if (exists? (schema-desc2 s)) (tell (schema-desc2 s) 'get-short-name) "*"))))


(define schema-dim 1st)
(define schema-desc1 2nd)
(define schema-relation 3rd)
(define schema-desc2 4th)


;;------------------------------------- Swaps -----------------------------------------

;; <swap> ::= ((<object> ...) <dimension> (<descriptor> <descriptor>))


(define get-all-swaps
  (lambda (cluster)
    (map-compress slippages->swap
      (partition
	(lambda (s1 s2)
	  (eq? (tell s1 'get-CM-type)
	       (tell s2 'get-CM-type)))
	(apply append (tell-all cluster 'get-non-symmetric-non-bond-slippages))))))


(define slippages->swap
  (lambda (slippages)
    (let ((from-descriptors (remq-duplicates (tell-all slippages 'get-descriptor1)))
	  (to-descriptors (remq-duplicates (tell-all slippages 'get-descriptor2))))
      (if (and (sets-equal? from-descriptors to-descriptors)
	       (= (length from-descriptors) 2))
	(list
	  (tell-all slippages 'get-object1)
	  (tell (1st slippages) 'get-CM-type)
	  from-descriptors)
	#f))))


(define select-swap
  (lambda (dimension swaps)
    (select (lambda (s) (eq? (2nd s) dimension)) swaps)))


(define swap-objs 1st)
(define swap-dim 2nd)
(define swap-descs 3rd)


;;-------------------------------------------------------------------------------------

(define change-descriptions->rule-clause-template
  (lambda (change-descriptions)
    (case (tell (1st change-descriptions) 'object-type)
      (intrinsic-change-description
	(let ((ref-object (tell (1st change-descriptions) 'get-reference-object))
	      (BondFacet-change (tell (1st change-descriptions) 'get-BondFacet-change))
	      (change-templates (tell-all change-descriptions 'make-change-template)))
	  (if (exists? BondFacet-change)
	    (list 'intrinsic ref-object (cons BondFacet-change change-templates))
	    (list 'intrinsic ref-object change-templates))))
      (extrinsic-change-description
	(list 'extrinsic
	  (if (ormap-meth change-descriptions 'subobjects-swap?)
	    (list (tell (1st change-descriptions) 'get-enclosing-object))
	    (sort-by-method 'get-left-string-pos <
	      (tell (1st change-descriptions) 'get-reference-objects)))
	  (tell-all change-descriptions 'get-dimension))))))


;; Sort intrinsic clauses by reference objects' <level of nesting>+<string position>

(define sort-templates
  (lambda (rule-clause-templates)
    (sort
      (lambda (t1 t2)
	(or (and (intrinsic-clause? t1) (extrinsic-clause? t2))
	    (and (extrinsic-clause? t1) (extrinsic-clause? t2)
	         (> (length (3rd t1)) (length (3rd t2))))
	    (and (intrinsic-clause? t1) (intrinsic-clause? t2)
	         (or (tell (2nd t2) 'nested-member? (2nd t1))
		     (and (disjoint-objects? (2nd t1) (2nd t2))
		          (< (tell (2nd t1) 'get-left-string-pos)
			     (tell (2nd t2) 'get-left-string-pos)))))))
      rule-clause-templates)))


(define intrinsic-clause?
  (lambda (rc) (eq? (1st rc) 'intrinsic)))


(define extrinsic-clause?
  (lambda (rc) (eq? (1st rc) 'extrinsic)))


(define verbatim-clause?
  (lambda (rc) (eq? (1st rc) 'verbatim)))


(define literal-clause?
  (lambda (rc)
    (record-case rc
      (intrinsic (object-descriptions changes)
	(or (ormap literal-object-description? object-descriptions)
	    (ormap literal-change? changes)))
      (extrinsic (object-descriptions dims)
	(ormap literal-object-description? object-descriptions)))))


(define literal-object-description?
  (lambda (object-description)
    (member? (2nd object-description)
      (list plato-alphabetic-position-category plato-letter-category plato-length))))


(define literal-change?
  (lambda (change)
    (platonic-literal? (3rd change))))


(define instantiate-rule-clause-template
  (lambda (rule-clause-template)
    (record-case rule-clause-template
      (intrinsic (ref-object change-templates)
	(let ((object-description (reference-object->object-description ref-object)))
	  (list 'intrinsic
	    (list object-description)
	    (map (instantiate-change-template object-description)
	      (sort-change-templates change-templates)))))
      (extrinsic (ref-objects dimensions)
	(list 'extrinsic
	  (map reference-object->object-description
	    (sort-reference-objects ref-objects))
	  (sort-rule-dimensions dimensions))))))

 
(define reference-object->object-description
  (lambda (object)
    (if (workspace-string? object)
      (list 'string plato-string-position-category plato-whole)
      (let ((chosen-description (tell object 'choose-description-for-rule)))
	(list
	  (tell object 'get-descriptor-for plato-object-category)
	  (tell chosen-description 'get-description-type)
	  (tell chosen-description 'get-descriptor))))))


(define instantiate-change-template
  (lambda (object-description)
    (lambda (change-template)
      (let* ((scope (1st change-template))
	     (dimension (2nd change-template))
	     (possible-descriptors (3rd change-template))
	     (chosen-descriptor
	       (stochastic-pick
		 possible-descriptors
		 (temp-adjusted-values
		   (tell-all possible-descriptors 'get-conceptual-depth)))))
	(list
	  scope
	  dimension
	  ;; This avoids rules such as "Change LettCtgy of object with
	  ;; LettCtgy `c' to successor" by substituting the literal
	  ;; descriptor (i.e., `d') for the relation:
	  (if (and (eq? dimension (2nd object-description))
		   (platonic-relation? chosen-descriptor))
	    (tell (3rd object-description) 'get-related-node chosen-descriptor)
	    chosen-descriptor))))))
      

(define sort-reference-objects
  (lambda (ref-objects)
    (sort-by-method 'get-left-string-pos < ref-objects)))


(define sort-rule-dimensions
  (lambda (dimensions)
    (sort-wrt-order dimensions *rule-dimension-order*)))


;; Sort change-templates by <scope>+<dimension>

(define sort-change-templates
  (lambda (change-templates)
    (sort (lambda (ct1 ct2)
	    (or (and (eq? (1st ct1) 'self) (eq? (1st ct2) 'subobjects))
	        (and (eq? (1st ct1) (1st ct2))
		     (< (list-index *rule-dimension-order* (2nd ct1))
		        (list-index *rule-dimension-order* (2nd ct2))))))
      change-templates)))


(define *rule-dimension-order*
  (list
    plato-direction-category
    plato-group-category
    plato-alphabetic-position-category
    plato-letter-category
    plato-length
    plato-object-category
    plato-string-position-category
    plato-bond-facet))


;;------------------------------- Change-descriptions ----------------------------------

(define make-extrinsic-change-description
  (lambda (reference-objects dimension descriptors)
    (let ((equivalent-intrinsic-changes
	    (map (lambda (obj)
		   (make-intrinsic-change-description
		     obj 'self dimension #f #f
		     (if (eq? (tell obj 'get-descriptor-for dimension)
			      (1st descriptors))
		       (2nd descriptors)
		       (1st descriptors))))
	      reference-objects))
	  (subobjects-swap? #f))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'extrinsic-change-description)
	    (print ()
	      (printf "CD: Swap ~a of ~a~n"
		(tell dimension 'get-short-name)
		(apply string-append
		  (cons (tell (1st reference-objects) 'ascii-name)
		    (map (lambda (obj) (format ", ~a" (tell obj 'ascii-name)))
		      (rest reference-objects))))))
	    (intrinsic? () #f)
	    (implies? (c)
	      (if (tell c 'intrinsic?)
		(extrinsic-implies-intrinsic? self c)
		(extrinsic-implies-extrinsic? self c)))
	    (get-reference-objects () reference-objects)
	    (get-dimension () dimension)
	    (get-equivalent-intrinsic-changes () equivalent-intrinsic-changes)
	    (same-change-type? (c) (not (tell c 'intrinsic?)))
	    (dimension? (dim) (eq? dim dimension))
	    (same-reference-objects? (ec)
	      (sets-equal? reference-objects (tell ec 'get-reference-objects)))
	    (common-reference-object? (ic)
	      (member? (tell ic 'get-reference-object) reference-objects))
	    (get-enclosing-object () (get-enclosing-object (1st reference-objects)))
	    (subobjects-swap? () subobjects-swap?)
	    (mark-as-subobjects-swap-if-possible ()
	      (if* (sets-equal?
		     (tell (tell self 'get-enclosing-object) 'get-constituent-objects)
		     reference-objects)
		(set! subobjects-swap? #t))
	      'done)
	    (else (delegate msg base-object))))))))


(define make-intrinsic-change-description
  (lambda (reference-object scope dimension descriptor1 relation descriptor2)
    (let ((descriptors (compress (list relation descriptor2)))
	  (enclosing-object
	    (if (workspace-string? reference-object)
	      #f
	      (get-enclosing-object reference-object))))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'intrinsic-change-description)
	    (print ()
	      (printf "CD: Change ~a of ~a~a to ~a~n"
		(tell dimension 'get-short-name)
		(if (eq? scope 'subobjects) "all subobjects of " "")
		(tell reference-object 'ascii-name)
		(tell-all descriptors 'get-short-name)))
	    (intrinsic? () #t)
	    (implies? (c)
	      (if (tell c 'intrinsic?)
		(intrinsic-implies-intrinsic? self c)
		#f))
	    (conflicts? (ic)
	      (or (intrinsic-implies-intrinsic? self ic)
		  (intrinsic-implies-intrinsic? ic self)))
	    (get-reference-object () reference-object)
	    (get-scope () scope)
	    (get-dimension () dimension)
	    (get-descriptor1 () descriptor1)
	    (get-descriptor2 () descriptor2)
	    (get-descriptors () descriptors)
	    (get-enclosing-object () enclosing-object)
	    (same-change-type? (c) (tell c 'intrinsic?))
	    (same-scope? (ic) (eq? scope (tell ic 'get-scope)))
	    (change-self? () (eq? scope 'self))
	    (change-subobjects? () (eq? scope 'subobjects))
	    (dimension? (dim) (eq? dim dimension))
	    (same-dimension? (ic) (tell ic 'dimension? dimension))
	    (LettCtgy/AlphPosCtgy-dimension? ()
	      (or (eq? dimension plato-letter-category)
		  (eq? dimension plato-alphabetic-position-category)))
	    (same-reference-objects? (ic)
	      (eq? reference-object (tell ic 'get-reference-object)))
	    (encloses? (ic)
	      (eq? reference-object (tell ic 'get-enclosing-object)))
	    (encloses-at-any-level? (ic)
	      (tell reference-object 'nested-member? (tell ic 'get-reference-object)))
	    (change-to-letter? () (eq? descriptor2 plato-letter))
	    (same-dimension-as-GroupCtgy-medium? (ic)
	      (eq? dimension (tell (tell ic 'get-reference-object) 'get-bond-facet)))
	    (implied-by-opposite-GroupCtgy? ()
	      (and (group? reference-object)
		   (eq? (tell reference-object 'get-ending-letter-category)
		        descriptor2)))
	    (get-BondFacet-change ()
	      (if (and (group? reference-object)
		       (eq? dimension plato-group-category)
		       (eq? scope 'self))
		`(self ,plato-bond-facet (,(tell reference-object 'get-bond-facet)))
		#f))
	    (make-change-template ()
	      ;; Disallow literal DirCtgy or GroupCtgy changes (ie, DirCtgy:left):
	      (if (or (eq? dimension plato-direction-category)
		      (eq? dimension plato-group-category))
		(list scope dimension (remq descriptor2 descriptors))
		(list scope dimension descriptors)))
	    (else (delegate msg base-object))))))))


;;------------------------- Change-description heuristics -----------------------------

(define remove-redundant-change-descriptions
  (lambda (change-descriptions)
    (remq-elements
      (append
	(pairwise-map get-redundant-change change-descriptions)
	(changes-implied-by-string-position-swaps change-descriptions))
      change-descriptions)))


(define changes-implied-by-string-position-swaps
  (lambda (change-descriptions)
    (let ((string-position-change-descriptions
	    (filter-meth change-descriptions
	      'dimension? plato-string-position-category)))
      (apply append
	(map (get-changes-implied-by-swap
	       (filter-meth change-descriptions 'intrinsic?)
	       (remq-elements
		 string-position-change-descriptions
		 (filter-out-meth change-descriptions 'intrinsic?)))
	  string-position-change-descriptions)))))


(define get-changes-implied-by-swap
  (lambda (intrinsic-change-descriptions other-extrinsic-change-descriptions)
    (lambda (string-position-ec)
      (append
	(filter-meth other-extrinsic-change-descriptions
	  'same-reference-objects? string-position-ec)
	(apply append
	  (pairwise-map (get-symmetric-changes string-position-ec)
	    intrinsic-change-descriptions))))))


(define get-symmetric-changes
  (lambda (string-position-ec)
    (lambda (ic1 ic2)
      (if (and (tell string-position-ec 'common-reference-object? ic1)
	       (tell string-position-ec 'common-reference-object? ic2)
	       (tell ic1 'same-dimension? ic2)
	       (eq? (tell ic1 'get-descriptor1) (tell ic2 'get-descriptor2))
	       (eq? (tell ic1 'get-descriptor2) (tell ic2 'get-descriptor1)))
	(list ic1 ic2)
	'()))))


(define get-redundant-change
  (lambda (c1 c2)
    (cond
      ((tell c1 'implies? c2) c2)
      ((tell c2 'implies? c1) c1)
      (else #f))))


;; Rule-abstraction Heuristics
;; ---------------------------
;; (see pages 110-113 of my PhD thesis for an explanation of all this)
;;
;; (intrinsic-implies-intrinsic? ic1 ic2) is true iff intrinsic change ic2 is already
;; implicit in intrinsic change ic1. The possible cases are described below:
;;
;; (1) a change to an object should be ignored if the object has an immediate
;;     enclosing group and there exists a change of the same type to the enclosing
;;     group's subobjects:
;;     [xyz] --> [[xx][yy][zz]]
;;        "[xyz] subobjects Length:succ"
;;        "x self Length:succ"  (IMPLICIT)
;;     [xyz] --> [[xx][y][zz]]
;;        "[xyz] subobjects ObjCtgy:group"
;;        "x self ObjCtgy:group"  (IMPLICIT)
;;
;; (2.1) a LettCtgy change to an object should be ignored if there exists a LettCtgy
;;       change to some other object that encloses the object at any level of nesting,
;;       since the highest-level LettCtgy change will change the letter categories of
;;       all nested objects automatically:
;;       [[aa]b] --> [[bb]c]
;;          "[[aa]b] {self || subobjects} LettCtgy:succ"
;;          "[aa] {self || subobjects} LettCtgy:succ"  (IMPLICIT)
;;          "a self LettCtgy:succ"  (IMPLICIT)
;;       If an object has a LettCtgy change both to itself and to its subobjects,
;;       only the change to the subobjects should be ignored:
;;       [[aa]b] --> [[bb]c]
;;          "[[aa]b] self LettCtgy:succ"
;;          "[[aa]b] subobjects LettCtgy:succ"  (IMPLICIT)
;;
;; (2.2) a LettCtgy change to an object should be ignored if there exists an AlphPosCtgy
;;       change to either the object itself, or to some other object that encloses it at
;;       any level of nesting, since the AlphPosCtgy change will automatically change
;;       the letter categories of all nested objects:
;;       [aa] --> zz
;;          "[aa] {self || subobjects} AlphPosCtgy:opp"
;;          "[aa] {self || subobjects} LettCtgy:z"  (IMPLICIT)
;;
;; (2.3) a LettCtgy change to an object should be ignored if the object has an
;;       enclosing group whose Length changes, since this might implicitly change
;;       the letter-categories of all the group's subobjects in a way that could
;;       be inconsistent with the object's own LettCtgy change:
;;       [abc] --> [abcd]
;;          "[abc] self Length:succ"
;;          "c self LettCtgy:succ"  (IMPLICIT)
;;
;; (2.4) a LettCtgy change to an object should be ignored if the GroupCtgy of the
;;       object's immediate enclosing group changes, or if the object itself is a
;;       group whose GroupCtgy changes, and the LettCtgy change is to the group's
;;       *ending* letter-category:
;;       [abc]> --> [cba]>
;;          "[abc] self GroupCtgy:opp"
;;          "c self LettCtgy:a"  (IMPLICIT)
;;       [abc]> --> [cba]>
;;          "[abc] self GroupCtgy:opp"
;;          "[abc] self LettCtgy:c"  (IMPLICIT)
;;
;; (3) a Length change to an object should be ignored if there exists an ObjCtgy:letter
;;     change to the same object (or if both of these changes refer to the subobjects),
;;     or if the Length change is to the object and the ObjCtgy:letter change is to its
;;     immediate-enclosing-group's subobjects, since the ObjCtgy:letter change
;;     automatically implies Length:
;;     [[aa][b][cc]] --> [abc]
;;        "[[aa][b][cc]] subobjects ObjCtgy:letter"
;;        "[aa] self Length:pred"  (IMPLICIT)
;;     [aa] --> a
;;        "[aa] self ObjCtgy:letter"
;;        "[aa] self Length:one"  (IMPLICIT)
;;     [[aa][bb][cc]] --> [abc]
;;        "[[aa][bb][cc]] subobjects ObjCtgy:letter"
;;        "[[aa][bb][cc]] subobjects Length:one"  (IMPLICIT)

(define intrinsic-implies-intrinsic?
  (lambda (ic1 ic2)
    (or (and (tell ic1 'same-dimension? ic2)
	     (or (and (tell ic1 'same-reference-objects? ic2)
		      (tell ic1 'same-scope? ic2))
	         (and (tell ic1 'encloses? ic2)
		      (tell ic1 'change-subobjects?)
		      (tell ic2 'change-self?))))
	(and (tell ic1 'LettCtgy/AlphPosCtgy-dimension?)
	     (tell ic2 'LettCtgy/AlphPosCtgy-dimension?)
	     (or (and (tell ic1 'same-reference-objects? ic2)
		      (or (and (tell ic1 'dimension? plato-alphabetic-position-category)
			       (tell ic2 'dimension? plato-letter-category))
			  (and (tell ic1 'same-dimension? ic2)
			       (tell ic2 'change-subobjects?))))
	         (tell ic1 'encloses-at-any-level? ic2)))
	(and (tell ic1 'dimension? plato-length)
	     (tell ic2 'dimension? plato-letter-category)
	     (tell ic1 'encloses? ic2)
	     (tell ic1 'change-self?))
	(and (tell ic1 'dimension? plato-group-category)
	     (tell ic1 'change-self?)
	     (tell ic2 'same-dimension-as-GroupCtgy-medium? ic1)
	     (or (tell ic1 'encloses? ic2)
	         (and (tell ic1 'same-reference-objects? ic2)
		      (tell ic2 'dimension? plato-letter-category)
		      (tell ic2 'implied-by-opposite-GroupCtgy?))))
	(and (tell ic1 'dimension? plato-string-position-category)
	     (tell ic2 'dimension? plato-direction-category)
	     (tell ic2 'encloses? ic1)
	     (tell ic2 'change-self?))
	(and (tell ic1 'change-to-letter?)
	     (tell ic2 'dimension? plato-length)
	     (or (and (tell ic1 'same-reference-objects? ic2)
		      (tell ic1 'same-scope? ic2))
	         (and (tell ic1 'encloses? ic2)
		      (tell ic1 'change-subobjects?)
		      (tell ic2 'change-self?)))))))


(define extrinsic-implies-intrinsic?
  (lambda (ec ic)
    (ormap-meth (tell ec 'get-equivalent-intrinsic-changes) 'conflicts? ic)))


(define extrinsic-implies-extrinsic?
  (lambda (ec1 ec2)
    (let ((equivalent-ic2-changes (tell ec2 'get-equivalent-intrinsic-changes)))
      (andmap
	(lambda (ic1)
	  (ormap
	    (lambda (ic2) (tell ic1 'implies? ic2))
	    equivalent-ic2-changes))
	(tell ec1 'get-equivalent-intrinsic-changes)))))


;;--------------------------------- Rule Application -----------------------------------

(define ignore-snag
  (lambda (failure-result) 'done))


;; apply-rule returns a list of the form ((<object> (<transform> ...)) ...),
;; where <object> is either a workspace-object or a workspace-string, or #f
;; if the rule application fails:

(define apply-rule
  (lambda (rule string failure-action)
    (if (tell rule 'verbatim?)
      (begin
	(tell (tell string 'get-image)
	  'new-appearance (tell rule 'get-verbatim-letter-categories))
	'())
      (continuation-point* abort
	(tell string 'reset-string-image)
	(let* ((extrinsic-transforms
		 (get-extrinsic-transforms rule string
		   (lambda (objects dimension)
		     (failure-action (list 'SWAP objects dimension))
		     (abort #f))))
	       (string-position-swaps
		 (filter
		   (lambda (x) (eq? (1st x) plato-string-position-category))
		   extrinsic-transforms))
	       (extrinsic-object-transform-pairs
		 (remq-elements string-position-swaps extrinsic-transforms))
	       (intrinsic-object-transform-pairs
		 (get-intrinsic-transforms rule string))
	       (all-object-transform-pairs
		 (append
		   extrinsic-object-transform-pairs
		   intrinsic-object-transform-pairs)))
	  (check-for-conflicts all-object-transform-pairs
	    (lambda (object1 dimension1 object2 dimension2)
	      (failure-action
		(list 'CONFLICT object1 dimension1 object2 dimension2))
	      (abort #f)))
	  (let ((transforms-grouped-by-object
		  (sort (lambda (ot1 ot2)
			  (> (tell (1st ot1) 'get-nesting-level)
			     (tell (1st ot2) 'get-nesting-level)))
		    (map (lambda (p) (list (1st (1st p)) (map 2nd p)))
		      (partition
			(lambda (ot1 ot2) (eq? (1st ot1) (1st ot2)))
			all-object-transform-pairs))))
		(fail (lambda (object)
			(lambda (transform)
			  (lambda ()
			    (failure-action (list 'CHANGE object transform))
			    (abort #f))))))
	    ;; transforms-grouped-by-object is a list of the form
	    ;;    ((<object> (<transform> ...)) ...)
	    ;; where each (<transform> ...) is a list of all the transforms that
	    ;; apply to <object> in the string according to the changes specified
	    ;; in the rule, including all StrPosCtgy changes.  Deepest-level objects
	    ;; are transformed first ("inside-out" order).
	    (for* each l in transforms-grouped-by-object do
	      (let ((object (1st l))
		    (transforms (2nd l)))
		(apply-transforms transforms (tell object 'get-image) (fail object))))
	    (for* each swap in string-position-swaps do
	      (let ((object1 (2nd swap))
		    (object2 (3rd swap)))
		(apply-string-position-swap object1 object2)))
	    transforms-grouped-by-object))))))


(define check-for-conflicts
  (lambda (object-transform-pairs fail)
    (let ((equivalent-change-descriptions
	    (map (lambda (ot)
		   (make-intrinsic-change-description
		     (1st ot) 'self (1st (2nd ot)) #f #f (2nd (2nd ot))))
	      object-transform-pairs)))
      (pairwise-map
	(lambda (ic1 ic2)
	  (if (tell ic1 'conflicts? ic2)
	    (fail
	      (tell ic1 'get-reference-object)
	      (tell ic1 'get-dimension)
	      (tell ic2 'get-reference-object)
	      (tell ic2 'get-dimension))
	    #f))
	equivalent-change-descriptions)
      'done)))


(define apply-transforms
  (lambda (transforms image fail)
    (let ((ordered-transforms
	    (sort (apply-before? (tell image 'get-length)) transforms)))
      (for* each transform in ordered-transforms do
	(if (eq? (1st transform) plato-group-category)
	  (let* ((medium (2nd (assq plato-bond-facet transforms)))
		 (GroupCtgy-transform (list plato-group-category plato-opposite medium)))
	    (transform-image image GroupCtgy-transform (fail GroupCtgy-transform)))
	  (transform-image image transform (fail transform)))))))


;; Special case:  BondFacet "transforms" are allowed in a rule, but have no effect,
;; and are included in a rule solely in conjunction with (<GroupCtgy> <Opposite>)
;; transforms to indicate which medium (i.e., letter-category or length) is being
;; reversed in the group.  Without this extra "transform", the bond-facet information
;; would be lost whenever a rule specifying a GroupCtgy reversal for a group is
;; abstracted (i.e., predgrp=(Opp)=>succgrp or succgrp=(Opp)=>predgrp).  Whenever
;; transform-image is invoked with a GroupCtgy transform, the transform is first
;; augmented with the bond-facet information gotten from the BondFacet "transform".
;; Thus, in transform-image,
;;
;; transform ::= (<dimension> <descriptor>) | (<GroupCtgy> <Opposite> <bond-facet>)


(define transform-image
  (lambda (image transform fail)
    (let ((dimension (1st transform))
	  (descriptor (2nd transform)))
      (case (tell dimension 'get-name-symbol)
	(plato-object-category
	  (if (eq? descriptor plato-letter)
	    (tell image 'letter fail)
	    (tell image 'group fail)))
	(plato-letter-category
	  (tell image 'new-start-letter descriptor fail))
	(plato-length
	  (tell image 'new-length descriptor fail))
	(plato-direction-category
	  (tell image 'reverse-direction fail))
	(plato-group-category
	  (tell image 'reverse-medium (3rd transform) fail))
	(plato-alphabetic-position-category
	  (tell image 'new-alpha-position-category descriptor fail))
	;; BondFacet "transforms" have no effect:
	(plato-bond-facet 'done)
	;; StrPosCtgy swaps are handled separately:
	(plato-string-position-category 'done)))))


(define apply-before?
  (lambda (platonic-image-length)
    (lambda (t1 t2)
      (or (eq? (1st t1) plato-group-category)
	  (eq? (2nd t1) plato-letter)
	  (and (eq? (1st t1) plato-length)
	       (change-length-first? (2nd t1) platonic-image-length))
	  (and (eq? (1st t2) plato-length)
	       (not (change-length-first? (2nd t2) platonic-image-length)))))))


(define apply-string-position-swap
  (lambda (object1 object2)
    (let* ((image1 (tell object1 'get-image))
	   (image2 (tell object2 'get-image))
	   (image-state1 (tell image1 'get-state))
	   (image-state2 (tell image2 'get-state)))
      (tell image1 'new-state image-state2)
      (tell image2 'new-state image-state1)
      (tell image1 'update-swapped-image image2)
      (tell image2 'update-swapped-image image1))))


;; get-extrinsic-transforms returns a list of the form (<element> ...)
;;
;; <element> ::= (<object> <transform>) | (<StrPosCtgy> <object> <object>)
;; <transform> ::= (<dimension> <descriptor>)

(define get-extrinsic-transforms
  (lambda (rule string fail)
    (apply append
      (map (lambda (rule-clause)
	     (let* ((reference-objects
		      (tell string 'get-reference-objects rule-clause))
		    (denoted-objects
		      (if (= (length (2nd rule-clause)) 1)
			(apply append
			  (tell-all reference-objects 'get-constituent-objects))
			reference-objects))
		    (dimensions (3rd rule-clause)))
	       (cond
		 ((< (length denoted-objects) 2) '())
		 ((not (pairwise-andmap disjoint-objects? denoted-objects))
		  (fail denoted-objects (1st dimensions)))
		 (else (apply append
			 (map (get-dimension-transforms denoted-objects fail)
			   dimensions))))))
	(tell rule 'get-extrinsic-rule-clauses)))))


(define get-dimension-transforms
  (lambda (denoted-objects fail)
    (lambda (dimension)
      (if (eq? dimension plato-string-position-category)
	(if (> (length denoted-objects) 2)
	  (fail denoted-objects dimension)
	  (StrPosCtgy-transforms (1st denoted-objects) (2nd denoted-objects)))
	(let ((denoted-object-descriptors
		;; If a Length swap is attempted for a group without a Length
		;; description, then one is attached (if possible), since this
		;; amounts to explicitly noticing the group's Length.  If the
		;; object is a letter, a descriptor is returned (plato-one) for
		;; the letter, but a Length description is not attached:
		(map (lambda (object)
		       (cond
			 ((not (eq? dimension plato-length))
			  (tell object 'get-descriptor-for dimension))
			 ((letter? object) plato-one)
			 ((tell object 'description-type-present? plato-length)
			  (tell object 'get-descriptor-for plato-length))
			 (else (attach-length-description object)
			       (tell object 'get-descriptor-for plato-length))))
		  denoted-objects)))
	  (if (not (all-exist? denoted-object-descriptors))
	    (fail denoted-objects dimension)
	    (let ((swap-descriptors (remq-duplicates denoted-object-descriptors)))
	      (if (> (length swap-descriptors) 2)
		(fail denoted-objects dimension)
		(let ((swap-descriptor1 (1st swap-descriptors))
		      (swap-descriptor2 (if (null? (rest swap-descriptors))
					  (1st swap-descriptors)
					  (2nd swap-descriptors))))
		  (map (lambda (object descriptor)
			 (list object
			   (list dimension
			     (if (eq? descriptor swap-descriptor1)
			       swap-descriptor2
			       swap-descriptor1))))
		    denoted-objects
		    denoted-object-descriptors))))))))))


;; StrPosCtgy-transforms returns both a "swap transform" of the
;; form (<StrPosCtgy> <object1> <object2>), and two regular
;; object-transform pairs -- one for each object -- of the form
;; (<object> (<StrPosCtgy> <descriptor>))

(define StrPosCtgy-transforms
  (lambda (object1 object2)
    (list
      (list plato-string-position-category object1 object2)
      (list object1
	(list plato-string-position-category
	  (tell object2 'get-descriptor-for plato-string-position-category)))
      (list object2
	(list plato-string-position-category
	  (tell object1 'get-descriptor-for plato-string-position-category))))))


;; get-intrinsic-transforms pairs all string objects denoted by the rule's intrinsic
;; clauses with all changes that apply to the objects.  It returns a list of the form
;; ((<object> <transform>) ...)
;;
;; <transform> ::= (<dimension> <descriptor>)
;;
;; TERMINOLOGY: A "reference object" in a string is an object directly specified
;; by the <obj-type> <obj-desc-type> <obj-desc> attributes of an object-description.
;; A "denoted object" may either be a reference object (in the case of a 'self change),
;; or one of the subobjects of a reference object (in the case of a 'subobjects change).
;; Example: in the string [abc], the rule clause
;;
;; (intrinsic ((<group> <StrPosCtgy> <whole>))
;;            ((self <LettCtgy> <succ>)
;;             (subobjects <Length> <succ>)))
;;
;; names the group [abc] as the *reference* object, while the *denoted* objects
;; are both the group [abc] (via self), and the letters a, b, c (via subobjects).


(define get-intrinsic-transforms
  (lambda (rule string)
    (apply append
      (map (lambda (rule-clause)
	     (let* ((ref-objects (tell string 'get-reference-objects rule-clause))
		    (changes (3rd rule-clause))
		    (self-transforms
		      (filter-map (lambda (c) (eq? (1st c) 'self)) rest changes))
		    (subobject-transforms
		      (filter-map (lambda (c) (eq? (1st c) 'subobjects)) rest changes)))
	       (if (null? subobject-transforms)
		 (cross-product ref-objects self-transforms)
		 (let ((all-subobjects
			 (apply append
			   (tell-all ref-objects 'get-constituent-objects))))
		   (append
		     (cross-product ref-objects self-transforms)
		     (cross-product all-subobjects subobject-transforms))))))
	(tell rule 'get-intrinsic-rule-clauses)))))



;;----------------------------------- Rule Quality -------------------------------------

(define compute-rule-quality
  (lambda (rule)
    (round (* (% (tell rule 'get-uniformity))
	      (weighted-average
		(list (tell rule 'get-abstractness) (tell rule 'get-succinctness))
		(list 3 2))))))


(define compute-rule-uniformity
  (lambda (rule)
    (if (or (tell rule 'identity?) (tell rule 'verbatim?))
      100
      (let* ((rule-clauses (tell rule 'get-rule-clauses))
	     (intrinsic-clauses (tell rule 'get-intrinsic-rule-clauses))
	     (extrinsic-clauses (tell rule 'get-extrinsic-rule-clauses))
	     (changes
	       (filter-out
		 (lambda (c)
		   (or (eq? (2nd c) plato-object-category)
		       (eq? (2nd c) plato-bond-facet)))
		 (flatmap 3rd intrinsic-clauses)))
	     (changes-grouped-by-dimension
	       (partition
		 (lambda (c1 c2) (eq? (2nd c1) (2nd c2)))
		 changes))
	     (all-intrinsic-object-descriptions
	       (flatmap 2nd intrinsic-clauses))
	     (intrinsic-clauses-uniformity
	       (if (null? intrinsic-clauses)
		 1
		 (average
		   (product (map change-abstractness-uniformity
			      changes-grouped-by-dimension))
		   (object-description-uniformity
		     all-intrinsic-object-descriptions))))
	     (extrinsic-clauses-uniformity
	       (if (null? extrinsic-clauses)
		 1
		 (average (map (compose ^3 object-description-uniformity)
			    (map 2nd extrinsic-clauses)))))
	     (clause-type-uniformity
	       (/ (max (count intrinsic-clause? rule-clauses)
		       (count extrinsic-clause? rule-clauses))
		 (length rule-clauses)))
	     (raw-uniformity
	       (weighted-average
		 (list
		   intrinsic-clauses-uniformity
		   extrinsic-clauses-uniformity
		   clause-type-uniformity)
		 (list 5 5 1)))
	     (adjusted-uniformity (exp (* 4 (- raw-uniformity 1)))))
	(100* adjusted-uniformity)))))


(define compute-rule-abstractness
  (let ((adjust-depth (sigmoid 3 40)))
    (lambda (rule)
      (cond
	((tell rule 'identity?) 100)
	((tell rule 'verbatim?) 0)
	(else
	  (let* ((object-descriptions
		   (apply append (map 2nd (tell rule 'get-rule-clauses))))
		 (intrinsic-changes
		   (apply append (map 3rd (tell rule 'get-intrinsic-rule-clauses))))
		 (swap-dimensions
		   (apply append (map 3rd (tell rule 'get-extrinsic-rule-clauses))))
		 (average-object-description-type-depth
		   (average (tell-all (map 2nd object-descriptions)
			      'get-conceptual-depth)))
		 (average-change-descriptor-depth
		   (average (tell-all (map 3rd intrinsic-changes)
			      'get-conceptual-depth)))
		 (average-swap-dimension-depth
		   (average (tell-all swap-dimensions 'get-conceptual-depth))))
	    (100* (adjust-depth (average
				  (filter-out zero?
				    (list
				      average-object-description-type-depth
				      average-change-descriptor-depth
				      average-swap-dimension-depth)))))))))))


(define compute-rule-succinctness
  (lambda (rule)
    (if (or (tell rule 'identity?) (tell rule 'verbatim?))
      100
      (100* (/ 4 (+ 3 (sum (map (lambda (rule-clause)
				  (cond
				    ((intrinsic-clause? rule-clause) 1)
				    ((> (length (2nd rule-clause)) 1) 2)
				    (else 1)))
			     (tell rule 'get-rule-clauses)))))))))


(define object-description-uniformity
  (lambda (object-descriptions)
    (let ((description-types (map 2nd object-descriptions)))
      (/ (maximum (map length (partition eq? description-types)))
	 (length description-types)))))


;; This returns a value in [0,1].  When changes have all relation descriptors or
;; all literal descriptors, value = 1; when changes are evenly mixed, value = 0:

(define change-abstractness-uniformity
  (lambda (changes)
    (let ((descriptors (map 3rd changes)))
      (* 2 (abs (- (/ (count platonic-relation? descriptors)
		      (length descriptors)) 1/2))))))


;; temporary. leave this for now:
(define compute-rule-intrinsic-quality
  (lambda (rule)
    (cond
      ((tell rule 'identity?) 100)
      ((tell rule 'verbatim?) 10)
      (else
	(let* ((rule-clauses (tell rule 'get-rule-clauses))
	       (intrinsic-clauses (tell rule 'get-intrinsic-rule-clauses))
	       (extrinsic-clauses (tell rule 'get-extrinsic-rule-clauses))
	       (object-description-types
		 (map 2nd (apply append (map 2nd rule-clauses))))
	       (object-description-depth-factor
		 ((sigmoid 3 30)
		  (average (tell-all object-description-types 'get-conceptual-depth))))
	       (changes
		 (filter-out
		   (lambda (c) (eq? (2nd c) plato-object-category))
		   (apply append (map 3rd intrinsic-clauses))))
	       (change-descriptors (map 3rd changes))
	       (change-descriptor-abstractness
		 (if (null? change-descriptors)
		   1
		   (/ (count platonic-relation? change-descriptors)
		      (length change-descriptors))))
	       (changes-grouped-by-dimension
		 (partition
		   (lambda (c1 c2) (eq? (2nd c1) (2nd c2)))
		   changes))
	       (intrinsic-object-descriptions
		 (apply append (map 2nd intrinsic-clauses)))
	       (intrinsic-clauses-uniformity
		 (if (null? intrinsic-clauses)
		   1
		   (* (product (map change-abstractness-uniformity
				 changes-grouped-by-dimension))
		      (^2 (object-description-uniformity
			    intrinsic-object-descriptions)))))
	       (extrinsic-clauses-uniformity
		 (if (null? extrinsic-clauses)
		   1
		   (average (map (compose ^3 object-description-uniformity)
			      (map 2nd extrinsic-clauses)))))
	       (raw-uniformity
		 (average intrinsic-clauses-uniformity extrinsic-clauses-uniformity))
	       (cohesion-factor (exp (* 5 (- raw-uniformity 1)))))
	  (100* (* cohesion-factor
		  (average
		    object-description-depth-factor
		    change-descriptor-abstractness))))))))


;;------------------------------ English Transcription -------------------------------

(define %maximum-rule-line-length% 60)

(define transcribe-to-english
  (lambda (rule-type rule-clauses)
    (let* ((string
	     (case rule-type
	       (top *initial-string*)
	       (bottom *target-string*)))
	   (clause-strings
	     (if (null? rule-clauses)
	       (list "Don't change anything")
	       (apply append
		 (map (get-rule-clause-phrases string)
		   rule-clauses))))
	   (adjusted-line-lengths
	     (map (lambda (s)
		    (let ((len (string-length s)))
		      (ceiling (/ len (ceiling (/ len %maximum-rule-line-length%))))))
	       clause-strings))
	   (max-line-length (maximum adjusted-line-lengths)))
      (apply append
	(map (separate-into-multiple-lines max-line-length "  ")
	  clause-strings)))))


(define separate-into-multiple-lines
  (lambda (max-length indent)
    (lambda (s)
      (let ((next-pos (find-next-space-position s max-length)))
	(if (= next-pos (string-length s))
	  (list s)
	  (cons (substring s 0 next-pos)
	    ((separate-into-multiple-lines max-length indent)
	     (string-append indent (string-suffix s next-pos)))))))))


(define get-rule-clause-phrases
  (lambda (string)
    (lambda (rule-clause)
      (record-case rule-clause
	(verbatim (letter-categories)
	  (list (get-verbatim-phrase letter-categories)))
	(extrinsic (object-descriptions dimensions)
	  (list (get-swap-phrase object-descriptions dimensions string)))
	(intrinsic (object-descriptions changes)
	  (let* ((plural? (plural-object-phrase? (1st object-descriptions) string))
		 (object-phrase
		   (get-object-phrase (1st object-descriptions) plural? string))
		 (ObjCtgy/Length/LettCtgy-phrases
		   (get-ObjCtgy/Length/LettCtgy-change-phrases
		     object-phrase plural? changes))
		 (BondFacet-change (select-change plato-bond-facet 'self changes))
		 (other-changes
		   (filter-out ObjCtgy/Length-change?
		     (remq BondFacet-change
		       (remove-LettCtgy-change-if-necessary changes))))
		 (other-change-phrases
		   (map (get-change-phrase object-phrase plural? BondFacet-change)
		     other-changes)))
	    (append other-change-phrases ObjCtgy/Length/LettCtgy-phrases)))))))


(define get-verbatim-phrase
  (lambda (letter-categories)
    (format "Change string to \"~a\""
      (apply string-append (tell-all letter-categories 'get-lowercase-name)))))


(define get-swap-phrase
  (lambda (object-descriptions dimensions string)
    (let ((objects-phrase
	    (punctuate
	      (map (lambda (od)
		     (get-object-phrase od (plural-object-phrase? od string) string))
		object-descriptions)))
	  (dimensions-phrase
	    (punctuate
	      (map (lambda (dim) (get-dimension-phrase dim #t))
		dimensions))))
      (if (= (length object-descriptions) 1)
	(format "Swap ~a of all objects in ~a" dimensions-phrase objects-phrase)
	(format "Swap ~a of ~a" dimensions-phrase objects-phrase)))))


(define plural-object-phrase?
  (lambda (object-description string)
    (> (length (tell string 'get-object-description-ref-objects object-description))
       1)))


(define get-dimension-phrase
  (lambda (dimension plural?)
    (case (tell dimension 'get-name-symbol)
      (plato-direction-category (if plural? "directions" "direction"))
      (plato-group-category (if plural? "group-types" "group-type"))
      (plato-alphabetic-position-category
	(if plural? "alphabetic-positions" "alphabetic-position"))
      (plato-letter-category (if plural? "letter-categories" "letter-category"))
      (plato-length (if plural? "lengths" "length"))
      (plato-object-category (if plural? "object-types" "object-type"))
      (plato-string-position-category (if plural? "positions" "position")))))


(define punctuate
  (lambda (l)
    (cond
      ((null? l) "")
      ((= (length l) 1) (1st l))
      ((= (length l) 2) (format "~a and ~a" (1st l) (2nd l)))
      (else (apply string-append
	      (append
		(map (lambda (x) (format "~a, " x)) (all-but-last 1 l))
		(list (format "and ~a" (last l)))))))))


(define get-object-phrase
  (lambda (object-description plural? string)
    (let ((object-type (1st object-description))
	  (object-descriptor (3rd object-description)))
      (cond
	((or (eq? object-type 'string) (eq? object-descriptor plato-whole))
	 (if (tell string 'whole-group?) "whole group" "string"))
	((platonic-letter? object-descriptor)
	 (cond
	   (plural?
	     (format "all `~a' ~as"
	       (tell object-descriptor 'get-lowercase-name)
	       (tell object-type 'get-lowercase-name)))
	   ((eq? object-type plato-letter)
	    (format "letter `~a'" (tell object-descriptor 'get-lowercase-name)))
	   (else (format "`~a' group" (tell object-descriptor 'get-lowercase-name)))))
	(plural?
	  (format "all ~a ~as"
	    (tell object-descriptor 'get-lowercase-name)
	    (tell object-type 'get-lowercase-name)))
	(else
	  (format "~a ~a"
	    (tell object-descriptor 'get-lowercase-name)
	    (tell object-type 'get-lowercase-name)))))))


(define get-change-phrase
  (lambda (object-phrase plural? BondFacet-change)
    (lambda (change)
      (let ((scope (1st change))
	    (dimension (2nd change))
	    (descriptor (3rd change)))
	(cond
	  ((eq? dimension plato-direction-category)
	   (format "Reverse ~a of ~a~a"
	     (get-dimension-phrase dimension (or plural? (eq? scope 'subobjects)))
	     (if (eq? scope 'subobjects) "all objects in " "")
	     object-phrase))
	  ((eq? dimension plato-group-category)
	   (if (eq? scope 'self)
	     (format "Reverse starting and ending ~a of ~a"
	       (cond
		 ((eq? (3rd BondFacet-change) plato-letter-category) "letter-categories")
		 ((eq? (3rd BondFacet-change) plato-length) "lengths"))
	       object-phrase)
	     (format "Reverse starting and ending points of all objects in ~a"
	       object-phrase)))
	  (else
	    (format "Change ~a of ~a~a to ~a"
	      (get-dimension-phrase dimension (or plural? (eq? scope 'subobjects)))
	      (if (eq? scope 'subobjects) "all objects in " "")
	      object-phrase
	      (if (platonic-letter? descriptor)
		(format "`~a'" (tell descriptor 'get-lowercase-name))
		(tell descriptor 'get-lowercase-name)))))))))


(define ObjCtgy/Length-change?
  (lambda (c)
    (or (eq? (2nd c) plato-object-category)
	(eq? (2nd c) plato-length))))


; Form of ObjCtgy/Length/LettCtgy-change-phrases for all possible combinations of
; ObjCtgy and Length changes:
;
; (*) denotes cases in which LettCtgy change may be incorporated into the phrase.
;
; <object-phrase> may be either singular or plural.
; (Example: "rightmost group" versus "all `c' groups")
;
; --------------------------------------------------------------------------------------
; <ObjCtgy:self>   <Length:self>
; if ObjCtgy:self = letter
;(*)"Change <object-phrase> to (a) letter(s) {<LettCtgy:self>}"
; if ObjCtgy:self = group & Length:self is relation
;   "Change/Increase/Decrease length(s) of <object-phrase> to/by <Length:self>/one"
; if ObjCtgy:self = group & Length:self is literal
;(*)"Change <object-phrase> to (a) {<LettCtgy:self>} group(s) of length <Length:self>"
; --------------------------------------------------------------------------------------
; <ObjCtgy:self>
;(*)"Change <object-phrase> to (a) {<LettCtgy:self>} <ObjCtgy:self>"
; --------------------------------------------------------------------------------------
; <ObjCtgy:subs>   <Length:self>   <Length:subs>
; if ObjCtgy:subs = letter
;(*)"Change/Increase/Decrease length(s) of <object-phrase> to/by <Length:self>/one"
;   "Change all objects in <object-phrase> to (a) letter(s) {<LettCtgy:subs>}"
; if ObjCtgy:subs = group & Length:subs is relation
;   "Change/Increase/Decrease length(s) of <object-phrase> to/by <Length:self>/one"
;   "Change/Increase/Decrease lengths of all objects in <object-phrase>
;      to/by <Length:subs>/one"
; if ObjCtgy:subs = group & Length:subs is literal
;(*)"Change/Increase/Decrease length(s) of <object-phrase> to/by <Length:self>/one"
;   "Change all objects in <object-phrase> to (a) {<LettCtgy:subs>} group(s)
;      of length <Length:subs>"
; --------------------------------------------------------------------------------------
; <ObjCtgy:subs>   <Length:self>
;(*)"Change/Increase/Decrease length(s) of <object-phrase> to/by <Length:self>/one"
;   "Change all objects in <object-phrase> to (a) {<LettCtgy:subs>} <ObjCtgy:subs>"
; --------------------------------------------------------------------------------------
; <ObjCtgy:subs>   <Length:subs>
; if ObjCtgy:subs = letter
;(*)"Change all objects in <object-phrase> to (a) letter(s) {<LettCtgy:subs>}"
; if ObjCtgy:subs = group & Length:subs is relation
;   "Change/Increase/Decrease lengths of all objects in <object-phrase>
;      to/by <Length:subs>/one"
; if ObjCtgy:subs = group & Length:subs is literal
;(*)"Change all objects in <object-phrase> to (a) {<LettCtgy:subs>} group(s)
;      of length <Length:subs>"
; --------------------------------------------------------------------------------------
; <ObjCtgy:subs>
;(*)"Change all objects in <object-phrase> to (a) {<LettCtgy:subs>} <ObjCtgy:subs>"
; --------------------------------------------------------------------------------------
; <Length:self>   <Length:subs>
;   "Change/Increase/Decrease length(s) of <object-phrase> to/by <Length:self>/one"
;   "Change/Increase/Decrease lengths of all objects in <object-phrase>
;      to/by <Length:subs>/one"
; --------------------------------------------------------------------------------------
; <Length:self>
;   "Change/Increase/Decrease length(s) of <object-phrase> to/by <Length:self>/one"
; --------------------------------------------------------------------------------------
; <Length:subs>
;   "Change/Increase/Decrease lengths of all objects in <object-phrase>
;      to/by <Length:subs>/one"
; --------------------------------------------------------------------------------------


(define get-ObjCtgy/Length/LettCtgy-change-phrases
  (lambda (object-phrase plural? changes)
    (let* ((present? (lambda l (andmap exists? l)))
	   (ObjCtgy:self (select-descriptor plato-object-category 'self changes))
	   (ObjCtgy:subs (select-descriptor plato-object-category 'subobjects changes))
	   (Length:self (select-descriptor plato-length 'self changes))
	   (Length:subs (select-descriptor plato-length 'subobjects changes))
	   (LettCtgy:self (select-descriptor plato-letter-category 'self changes))
	   (LettCtgy:subs (select-descriptor plato-letter-category 'subobjects changes)))
      (cond
       ((present? ObjCtgy:self Length:self)
	(cond
	  ((eq? ObjCtgy:self plato-letter)
	   `(,(change-to-object-type
		object-phrase plural? plato-letter LettCtgy:self #f)))
	  ((platonic-relation? Length:self)
	   `(,(change-length-of object-phrase plural? Length:self #f)))
	  (else `(,(change-to-group-of-length
		     object-phrase plural? Length:self LettCtgy:self #f)))))
       ((present? ObjCtgy:self)
	`(,(change-to-object-type object-phrase plural? ObjCtgy:self LettCtgy:self #f)))
       ((present? ObjCtgy:subs Length:self Length:subs)
	`(,(change-length-of object-phrase plural? Length:self #f)
	  ,(cond
	     ((eq? ObjCtgy:subs plato-letter)
	      (change-to-object-type
		object-phrase plural? plato-letter LettCtgy:subs #t))
	     ((platonic-relation? Length:subs)
	      (change-length-of object-phrase plural? Length:subs #t))
	     (else (change-to-group-of-length
		     object-phrase plural? Length:subs LettCtgy:subs #t)))))
       ((present? ObjCtgy:subs Length:self)
	`(,(change-length-of object-phrase plural? Length:self #f)
	  ,(change-to-object-type object-phrase plural? ObjCtgy:subs LettCtgy:subs #t)))
       ((present? ObjCtgy:subs Length:subs)
	(cond
	 ((eq? ObjCtgy:subs plato-letter)
	  `(,(change-to-object-type
	       object-phrase plural? plato-letter LettCtgy:subs #t)))
	 ((platonic-relation? Length:subs)
	  `(,(change-length-of object-phrase plural? Length:subs #t)))
	 (else `(,(change-to-group-of-length
		    object-phrase plural? Length:subs LettCtgy:subs #t)))))
       ((present? ObjCtgy:subs)
	`(,(change-to-object-type object-phrase plural? ObjCtgy:subs LettCtgy:subs #t)))
       ((present? Length:self Length:subs)
	`(,(change-length-of object-phrase plural? Length:self #f)
	  ,(change-length-of object-phrase plural? Length:subs #t)))
       ((present? Length:self)
	`(,(change-length-of object-phrase plural? Length:self #f)))
       ((present? Length:subs)
	`(,(change-length-of object-phrase plural? Length:subs #t)))
       (else '())))))


(define remove-LettCtgy-change-if-necessary
  (lambda (changes)
    (let ((ObjCtgy-change (select-change plato-object-category #f changes)))
      (if (not (exists? ObjCtgy-change))
	changes
	(let ((same-scope-LettCtgy-change
		(select-change plato-letter-category (1st ObjCtgy-change) changes))
	      (same-scope-Length-change
		(select-change plato-length (1st ObjCtgy-change) changes)))
	  (if (and (exists? same-scope-LettCtgy-change)
		   (platonic-literal? (3rd same-scope-LettCtgy-change))
		   (or (not (exists? same-scope-Length-change))
		       (platonic-literal? (3rd same-scope-Length-change))
		       (eq? (3rd ObjCtgy-change) plato-letter)))
	    (remq same-scope-LettCtgy-change changes)
	    changes))))))


(define select-descriptor
  (lambda (dimension scope changes)
    (let ((change (select-change dimension scope changes)))
      (if (exists? change)
	(3rd change)
	#f))))


(define select-change
  (lambda (dimension scope changes)
    (select
      (lambda (c)
	(if (exists? scope)
	  (and (eq? (1st c) scope) (eq? (2nd c) dimension))
	  (eq? (2nd c) dimension)))
      changes)))


(define change-to-object-type
  (lambda (object-phrase plural? object-type LettCtgy-descriptor subobjects?)
    (format "Change ~a~a to ~a"
      (if subobjects? "all objects in " "")
      object-phrase
      (new-object-phrase object-type LettCtgy-descriptor (or plural? subobjects?)))))


(define change-to-group-of-length
  (lambda (object-phrase plural? length LettCtgy-descriptor subobjects?)
    (format "Change ~a~a to ~a of length ~a"
      (if subobjects? "all objects in " "")
      object-phrase
      (new-object-phrase plato-group LettCtgy-descriptor (or plural? subobjects?))
      (tell length 'get-lowercase-name))))


(define new-object-phrase
  (let ((an-letters (list plato-a plato-e plato-f plato-h plato-i plato-l
		      plato-m plato-n plato-o plato-r plato-s plato-x)))
    (lambda (new-object-type LettCtgy-descriptor plural?)
      (cond
	((eq? new-object-type plato-letter)
	 (if (and (exists? LettCtgy-descriptor)
	          (platonic-literal? LettCtgy-descriptor))
	   (format "the letter `~a'" (tell LettCtgy-descriptor 'get-lowercase-name))
	   (if plural? "letters" "a letter")))
	((eq? new-object-type plato-group)
	 (if (and (exists? LettCtgy-descriptor)
	          (platonic-literal? LettCtgy-descriptor))
	   (if plural?
	     (format "`~a' groups" (tell LettCtgy-descriptor 'get-lowercase-name))
	     (format "~a `~a' group"
	       (if (member? LettCtgy-descriptor an-letters) "an" "a")
	       (tell LettCtgy-descriptor 'get-lowercase-name)))
	   (if plural? "groups" "a group")))))))


(define change-length-of
  (lambda (object-phrase plural? length subobjects?)
    (cond
      ((eq? length plato-successor)
       (format "Increase ~a of ~a~a by one"
	 (get-dimension-phrase plato-length (or plural? subobjects?))
	 (if subobjects? "all objects in " "")
	 object-phrase))
      ((eq? length plato-predecessor)
       (format "Decrease ~a of ~a~a by one"
	 (get-dimension-phrase plato-length (or plural? subobjects?))
	 (if subobjects? "all objects in " "")
	 object-phrase))
      (else
	(format "Change ~a of ~a~a to ~a"
	  (get-dimension-phrase plato-length (or plural? subobjects?))
	  (if subobjects? "all objects in " "")
	  object-phrase
	  (tell length 'get-lowercase-name))))))


;;----------------------------------- Rule Printing -----------------------------------

(define printf-object-description
  (lambda (object-description)
    (printf "~a~n" (format-object-description object-description))))

(define printf-rule-clause-template
  (lambda (rule-clause-template)
    (record-case rule-clause-template
      (intrinsic (ref-obj change-templates)
	(printf "intrinsic ~a~n" (tell ref-obj 'ascii-name))
	(for* each ct in change-templates do
	  (printf "   ~a~n" (format-change-template ct))))
      (extrinsic (ref-objs dimensions)
	(printf "extrinsic ~a ~a~n"
	  (tell-all ref-objs 'ascii-name)
	  (map format-slipnode dimensions))))))

(define printf-rule-clause
  (lambda (rule-clause)
    (record-case rule-clause
      (verbatim (letter-categories)
	(printf "VERBATIM ~a~n"
	  (tell-all letter-categories 'get-lowercase-name)))
      (intrinsic (object-descriptions changes)
	(printf "CHANGE ~a~n"
	  (format-object-description (1st object-descriptions)))
	(for* each change in changes do
	  (printf "   (~a ~a ~a)~n"
	    (1st change)
	    (format-slipnode (2nd change))
	    (format-slipnode (3rd change)))))
      (extrinsic (object-descriptions dimensions)
	(printf "SWAP ~a of~n"
	  (apply string-append
	    (cons (format-slipnode (1st dimensions))
	      (map (lambda (dim) (format ", ~a" (format-slipnode dim)))
		(rest dimensions)))))
	(if (= (length object-descriptions) 1)
	  (printf "   subobjects ~a~n"
	    (format-object-description (1st object-descriptions)))
	  (for* each obj-desc in object-descriptions do
	    (printf "   ~a~n" (format-object-description obj-desc))))))))

(define format-change-template
  (lambda (ct)
    (format "(~a ~a ~a)"
      (1st ct)
      (format-slipnode (2nd ct))
      (map format-slipnode (3rd ct)))))

(define format-object-description
  (lambda (od)
    (format "(~a ~a ~a)"
      (if (eq? (1st od) 'string) 'string (format-slipnode (1st od)))
      (format-slipnode (2nd od))
      (format-slipnode (3rd od)))))

(define format-slipnode
  (lambda (node)
    (format "<~a>" (tell node 'get-short-name))))
