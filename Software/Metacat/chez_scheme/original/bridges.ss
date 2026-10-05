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

;; Horizontal bridges should have at least one _distinguishing_
;; CM/slippage with a <relation>. This prevents stupid mappings such
;; as b--d in abc->abcd, which is based on SLIPPAGE:StrPosCtgy:middle=>rmost
;; and SLIPPAGE:LettCtgy:b=>d

;; Symmetric concept-mappings are important.  They're used in
;; important-object-bridge-scout codelets in the 'get-all-slippages
;; call.  Example: abc ddbbaa cba with a horizontal bridge already
;; made between a and aa (with cm StrPos:lmost=>rmost).  If
;; description StrPos:rmost is chosen for c, applicable-slippage
;; selected is the *symmetric* slippage StrPos:rmost=>lmost, which
;; then yields lmost as object2-descriptor.  Then objects with
;; description lmost in the modified string get focused on.
;; Otherwise, only way to make this bridge is via
;; bottom-up-bridge-scout codelets.

(define make-horizontal-bridge
  (lambda (object1 object2 concept-mappings)
    (let* ((bridge-type
	     (case (tell object1 'which-string)
	       (initial 'top)
	       (target 'bottom)))
	   (theme-type (bridge-type->theme-type bridge-type))
	   (workspace-structure (make-workspace-structure))
	   ;; bond-CMs = BondCtgy and BondFacet concept-mappings only
	   ;; all-CMs = CMs + bond-CMs
	   ;; symmetric-slippages = symmetric versions of all-CMs _slippages_
	   (bond-concept-mappings '())
	   (all-concept-mappings concept-mappings)
	   (symmetric-slippages '())
	   (flipped-group1? #f)
	   (original-group1 #f)
	   (flipped-group2? #f)
	   (original-group2 #f)
	   (spanning-bridge? (both-spanning-objects? object1 object2))
	   ;; group-spanning-bridge? is only for graphics purposes:
	   (group-spanning-bridge? (both-spanning-groups? object1 object2))
	   (translated-rule-bridge? #f)
	   (from-graphics-coord
	    (cond
	     ((not %workspace-graphics%) #f)
	     (group-spanning-bridge?
	      (tell object1 'get-group-spanning-bridge-graphics-coord 'horizontal))
	     (else (tell object1 'get-bridge-graphics-coord 'horizontal))))
	   (to-graphics-coord
	    (cond
	     ((not %workspace-graphics%) #f)
	     (group-spanning-bridge?
	      (tell object2 'get-group-spanning-bridge-graphics-coord 'horizontal))
	     (else (tell object2 'get-bridge-graphics-coord 'horizontal)))))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'bridge)
	    (get-bridge-type () bridge-type)
	    (get-theme-type () theme-type)
	    (bridge-type? (type) (eq? type bridge-type))
	    (get-orientation () 'horizontal)
	    (print ()
              (printf "~a from ~a to ~a"
		(if spanning-bridge?
		  "Spanning horizontal bridge"
		  "Horizontal bridge")
		(tell object1 'ascii-name)
		(tell object2 'ascii-name))
	      (if* (< (tell self 'get-proposal-level) %built%)
		(printf (if (and %workspace-graphics% (not (tell self 'drawn?)))
			  " (~a, not drawn)"
			  " (~a)")
		  (case (tell self 'get-proposal-level)
		    (0 "new")
		    (1 "proposed")
		    (2 "evaluated"))))
	      (newline)
	      (for* each cm in all-concept-mappings do
		(printf "     ")
		(print cm)))
	    (short-print ()
	      (printf "~a from ~a to ~a"
		(if spanning-bridge?
		  "Spanning horizontal bridge"
		  "Horizontal bridge")
		(tell object1 'ascii-name)
		(tell object2 'ascii-name))
	      (if* (< (tell self 'get-proposal-level) %built%)
		(printf (if (and %workspace-graphics% (not (tell self 'drawn?)))
			  " (~a, not drawn)"
			  " (~a)")
		  (case (tell self 'get-proposal-level)
		    (0 "new")
		    (1 "proposed")
		    (2 "evaluated"))))
	      (newline))
	    (spanning-bridge? () spanning-bridge?)
	    (group-spanning-bridge? () group-spanning-bridge?)
	    (translated-rule-bridge? () translated-rule-bridge?)
	    (mark-as-translated-rule-bridge ()
	      (set! translated-rule-bridge? #t)
	      'done)
	    (get-from-graphics-coord () from-graphics-coord)
	    (get-to-graphics-coord () to-graphics-coord)
	    (set-new-from-graphics-coord (from-group)
	      (set! from-graphics-coord
		(tell from-group 'get-group-spanning-bridge-graphics-coord 'horizontal))
	      'done)
	    (set-new-to-graphics-coord (to-group)
              (set! to-graphics-coord
		(tell to-group 'get-group-spanning-bridge-graphics-coord 'horizontal))
	      'done)
	    ;; Currently no concept-mapping graphics for horizontal bridges:
	    (activate-concept-mapping-graphics () 'done)
	    (concept-mapping-graphics-active? () #f)
	    (get-object1 () object1)
	    (get-object2 () object2)
	    (get-enclosing-group1 () (tell object1 'get-enclosing-group))
	    (get-enclosing-group2 () (tell object2 'get-enclosing-group))
	    (get-original-object1 ()
              (if flipped-group1?
		original-group1
		object1))
	    (get-original-object2 ()
              (if flipped-group2?
		original-group2
		object2))
	    ;; This is used to set concept-mappings for horizontal bridges created
	    ;; by translating a rule:
	    (set-concept-mappings (CM-list)
	      (set! bond-concept-mappings (filter-meth CM-list 'bond-concept-mapping?))
	      (set! concept-mappings (remq-elements bond-concept-mappings CM-list))
	      (set! all-concept-mappings
		(append concept-mappings bond-concept-mappings))
	      (set! symmetric-slippages
		(tell-all (filter-meth all-concept-mappings 'slippage?)
		  'symmetric-mapping))
	      'done)
	    (get-concept-mapping-types ()
	      (tell-all all-concept-mappings 'get-CM-type))
	    (get-concept-mapping (description-type)
	      (select-meth all-concept-mappings 'CM-type? description-type))
	    (get-concept-mappings () concept-mappings)
	    (get-bond-concept-mappings () bond-concept-mappings)
	    (get-all-concept-mappings () all-concept-mappings)
	    (get-non-symmetric-slippages ()
	      (filter-meth all-concept-mappings 'slippage?))
	    (get-non-symmetric-non-bond-slippages ()
	      (filter-meth concept-mappings 'slippage?))
	    (get-symmetric-slippages () symmetric-slippages)
	    (get-slippages ()
	      (append
		(tell self 'get-non-symmetric-slippages)
		symmetric-slippages))
	    (get-bond-slippages ()
	      (filter-meth (tell self 'get-slippages) 'bond-concept-mapping?))
	    (get-other-object (object)
	      (if (eq? object object1) object2 object1))
	    (get-covered-letters ()
	      (append (tell object1 'get-letters)
		      (tell object2 'get-letters)))
	    (get-letter-span ()
	      (+ (tell object1 'get-letter-span)
		 (tell object2 'get-letter-span)))
	    (flipped-group1? () flipped-group1?)
	    (flipped-group2? () flipped-group2?)
	    (get-original-group1 () original-group1)
	    (get-original-group2 () original-group2)
	    (mark-flipped-group1 (original-group)
	      (set! flipped-group1? #t)
	      (set! original-group1 original-group)
	      'done)
	    (mark-flipped-group2 (original-group)
	      (set! flipped-group2? #t)
	      (set! original-group2 original-group)
	      'done)
	    (get-drawn-coincident-bridge ()
              (select-meth
		(tell *workspace* 'get-all-other-coincident-bridges
		  self object1 object2)
		'drawn?))
	    (get-highest-level-coincident-bridge ()
              (let ((current-max 0)
		    (highest-level-bridge #f))
		(for* each b in (tell *workspace* 'get-all-other-coincident-bridges
				  self object1 object2) do
		  (let ((proposal-level (tell b 'get-proposal-level)))
		    (if* (> proposal-level current-max)
		      (set! current-max proposal-level)
		      (set! highest-level-bridge b))))
		highest-level-bridge))
	    (add-concept-mapping (cm)
	      (tell self 'add-concept-mappings (list cm)))
	    (add-concept-mappings (cm-list)
	      (set! concept-mappings (append cm-list concept-mappings))
	      (set! all-concept-mappings (append cm-list all-concept-mappings))
	      'done)
	    (delete-concept-mapping-type (type)
	      (let ((cm (select-meth all-concept-mappings 'CM-type? type))
		    (ss (select-meth symmetric-slippages 'CM-type? type)))
		(set! concept-mappings (remq cm concept-mappings))
		(set! all-concept-mappings (remq cm all-concept-mappings))
		(if* (exists? ss)
		  (set! symmetric-slippages (remq ss symmetric-slippages))))
	      'done)
	    (add-bond-concept-mapping (bond-cm)
	      (set! bond-concept-mappings (cons bond-cm bond-concept-mappings))
	      (set! all-concept-mappings (cons bond-cm all-concept-mappings))
	      'done)
	    (add-symmetric-slippage (slippage)
	      (let ((symmetric-slippage (tell slippage 'symmetric-mapping)))
		(set! symmetric-slippages
		  (cons symmetric-slippage symmetric-slippages)))
	      'done)
	    (concept-mapping-present? (concept-mapping)
	      (ormap (lambda (cm) (CMs-equal? cm concept-mapping))
		all-concept-mappings))
	    (CM-type-present? (type)
	      (ormap-meth all-concept-mappings 'CM-type? type))
	    (slippage-type-present? (type)
	      (let ((cm (select-meth all-concept-mappings 'CM-type? type)))
		(and (exists? cm) (tell cm 'slippage?))))
	    (supports-theme-pattern? (pattern)
	      (cross-product-ormap
		(lambda (entry cm)
		  (and (eq? (tell cm 'get-CM-type) (1st entry))
		       (eq? (tell cm 'get-label) (2nd entry))))
		(entries pattern)
		(remove-whole/single-concept-mappings
		  all-concept-mappings)))
	    (get-relevant-CMs ()
	      (filter-meth concept-mappings 'relevant?))
	    (get-distinguishing-CMs ()
	      (filter-meth concept-mappings 'distinguishing?))
	    (get-relevant-distinguishing-CMs ()
	      (filter-meth concept-mappings 'relevant-distinguishing?))
	    ;; Themes:
	    (incompatible-with-theme? (theme)
	      (or (check-descriptions object1 object2 conflicts-with-theme? theme)
		  (let* ((dimension (tell theme 'get-dimension))
			 (object1-description-possible?
			   (tell dimension 'description-possible? object1))
			 (object2-description-possible?
			   (tell dimension 'description-possible? object2)))
		    (or (and object1-description-possible?
			     (not object2-description-possible?))
		        (and (not object1-description-possible?)
			     object2-description-possible?)
			(and (eq? dimension plato-string-position-category)
			     (not object1-description-possible?)
			     (not object2-description-possible?))))))
	    (supported-by-theme? (theme)
	      (check-descriptions object1 object2 supported-by-theme? theme))
	    (get-thematic-compatibility ()
	      (bridge-theme-compatibility-sigmoid
		(tell self 'get-average-theme-support)))
	    (get-average-theme-support ()
	      (let* ((support-values (tell self 'get-theme-support-values))
		     (neg-weight (* 2 (length support-values)))
		     (support-weights
		       (map (lambda (n) (* (if (< n 0) neg-weight 1) (abs n)))
			 support-values)))
		(weighted-average support-values support-weights)))
	    (get-theme-support-values ()
	      (map (lambda (theme)
		     (cond
		       ((tell self 'incompatible-with-theme? theme)
			(- (% (tell theme 'get-activation))))
		       ((tell self 'supported-by-theme? theme)
			(% (tell theme 'get-activation)))
		       (else 0)))
		(tell *themespace* 'get-active-themes theme-type)))
	    (boost-themespace-activations ()
	      (tell self 'boost-themes)
	      (tell *themespace* 'update-dominant-themes theme-type)
	      (tell *themespace-window* 'update-graphics theme-type)
	      'done)
	    ;; This method just boosts theme activations.
	    ;; It does not update dominant themes or graphics:
	    (boost-themes ()
	      (let ((strength (tell self 'get-strength)))
		(cross-product-for-each
		  (lambda (d1 d2)
		    (if* (descriptions-affect-themespace? d1 d2)
		      (let ((theme (tell *themespace* 'add-theme-if-possible
				     theme-type
				     (tell d1 'get-description-type)
				     (get-label
				       (tell d1 'get-descriptor)
				       (tell d2 'get-descriptor)))))
			(if* (exists? theme)
			  (if spanning-bridge?
			    (tell theme 'boost-activation (* 2 strength))
			    (tell theme 'boost-activation strength))))))
		  (tell object1 'get-descriptions)
		  (tell object2 'get-descriptions)))
	      'done)
	    (get-associated-thematic-relations ()
	      (cross-product-filter-map
		descriptions-affect-themespace?
		(lambda (d1 d2)
		  (list
		    (tell d1 'get-description-type)
		    (get-label (tell d1 'get-descriptor) (tell d2 'get-descriptor))))
		(tell object1 'get-descriptions)
		(tell object2 'get-descriptions)))

	    (get-incompatible-bridges ()
	      (remq-duplicates
		(append
		  (filter
		    (lambda (b) (incompatible-horizontal-bridges? b self))
		    (tell *workspace* 'get-bridges bridge-type))
		  (group-incompatible-bridges 'horizontal object1 object2)
		  (if (both-spanning-groups? object1 object2)
		    (let ((direction-CM (select-meth concept-mappings
					  'CM-type? plato-direction-category)))
		      (if (exists? direction-CM)
			(direction-incompatible-bridges 'horizontal
			  object1 object2 direction-CM)
			'()))
		    '()))))
	    (get-incompatible-bond ()
	      (let ((bond1 (if (tell object1 'leftmost-in-string?)
			       (tell object1 'get-right-bond)
			       (tell object1 'get-left-bond)))
		    (bond2 (if (tell object2 'leftmost-in-string?)
			       (tell object2 'get-right-bond)
			       (tell object2 'get-left-bond))))
		(if (and (exists? bond1)
			 (exists? bond2)
			 (directed? bond1)
			 (directed? bond2)
			 (let ((direction-category-CM
				 (make-concept-mapping
				   bond1
				   plato-direction-category
				   (tell bond1 'get-direction)
				   bond2
				   plato-direction-category
				   (tell bond2 'get-direction))))
			   (incompatible-with-any-horizontal-CM?
			     direction-category-CM concept-mappings)))
		    bond2
		    #f)))
	    (internally-coherent? ()
	      (let ((relevant-distinguishing-CMs
		      (tell self 'get-relevant-distinguishing-CMs)))
		(cross-product-ormap
		  (lambda (cm1 cm2)
		    (and (not (eq? cm1 cm2))
			 (supporting-horizontal-CMs? cm1 cm2)))
		  relevant-distinguishing-CMs
		  relevant-distinguishing-CMs)))
	    (StrPosCtgy:Opposite-slippage? ()
	      (let ((string-position-CM
		      (select-meth concept-mappings
			'CM-type? plato-string-position-category)))
		(and (exists? string-position-CM)
		     (eq? (tell string-position-CM 'get-label) plato-opposite))))
	    (calculate-internal-strength ()
	      (let ((relevant-distinguishing-CMs
		      (tell self 'get-relevant-distinguishing-CMs)))
		(if (null? relevant-distinguishing-CMs)
		  0
		  (let* ((average-strength
			   (average (tell-all relevant-distinguishing-CMs 'get-strength)))
			 (num-of-concept-mappings
			   (length relevant-distinguishing-CMs))
			 (num-of-concept-mappings-factor
			   (cond
			     ((= num-of-concept-mappings 1) 0.8)
			     ((= num-of-concept-mappings 2) 1.2)
			     (else 1.6)))
			 (internal-coherence-factor
			   (if (tell self 'internally-coherent?) 2.5 1.0))
			 (singleton-factor
			   (singleton-letter-factor object1 object2)))
		    (min 100 (round (* average-strength
				       num-of-concept-mappings-factor
				       internal-coherence-factor
				       ;; Only used for horizontal bridges:
				       singleton-factor)))))))
	    (calculate-external-strength ()
	      (if (or (and (letter? object1) (tell object1 'spans-whole-string?))
		      (and (letter? object2) (tell object2 'spans-whole-string?)))
		  100
		  (let* ((supporting-bridges
			   (filter
			     (lambda (b) (supporting-horizontal-bridges? self b))
			     (remq self (tell *workspace* 'get-bridges bridge-type))))
			 (total-support
			   (sum (tell-all supporting-bridges 'get-strength))))
		    (round (* (min 100 total-support))))))
	    (else (delegate msg workspace-structure))))))))


(define make-vertical-bridge
  (lambda (object1 object2 concept-mappings)
    (let* ((bridge-type 'vertical)
	   (theme-type (bridge-type->theme-type bridge-type))
	   (workspace-structure (make-workspace-structure))
	   ;; bond-CMs = BondCtgy and BondFacet concept-mappings only
	   ;; all-CMs = CMs + bond-CMs
	   ;; symmetric-slippages = symmetric versions of all-CMs _slippages_
	   (bond-concept-mappings '())
	   (all-concept-mappings concept-mappings)
	   (symmetric-slippages '())
	   (flipped-group1? #f)
	   (original-group1 #f)
	   (flipped-group2? #f)
	   (original-group2 #f)
	   (spanning-bridge? (both-spanning-objects? object1 object2))
	   ;; group-spanning-bridge? is only for graphics purposes:
	   (group-spanning-bridge? (both-spanning-groups? object1 object2))
	   (from-graphics-coord
	    (cond
	     ((not %workspace-graphics%) #f)
	     (group-spanning-bridge?
	      (tell object1 'get-group-spanning-bridge-graphics-coord 'vertical))
	     (else (tell object1 'get-bridge-graphics-coord 'vertical))))
	   (to-graphics-coord
	    (cond
	     ((not %workspace-graphics%) #f)
	     (group-spanning-bridge?
	      (tell object2 'get-group-spanning-bridge-graphics-coord 'vertical))
	     (else (tell object2 'get-bridge-graphics-coord 'vertical))))
	   (concept-mapping-graphics-active? #f)
	   (concept-mapping-list-coord #f)
	   (bridge-label-number #f))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'bridge)
	    (get-bridge-type () bridge-type)
	    (get-theme-type () theme-type)
	    (bridge-type? (type) (eq? type bridge-type))
	    (get-orientation () 'vertical)
	    (print ()
              (printf "~a from ~a to ~a"
		(if spanning-bridge?
		  "Spanning vertical bridge"
		  "Vertical bridge")
		(tell object1 'ascii-name)
		(tell object2 'ascii-name))
	      (cond
		((< (tell self 'get-proposal-level) %built%)
		 (printf (if (and %workspace-graphics% (not (tell self 'drawn?)))
			   " (~a, not drawn)"
			   " (~a)")
		   (case (tell self 'get-proposal-level)
		     (0 "new")
		     (1 "proposed")
		     (2 "evaluated"))))
		((and %workspace-graphics% (not group-spanning-bridge?))
		 (printf " (#~a)" bridge-label-number)))
	      (newline)
	      (for* each cm in all-concept-mappings do
		(printf "     ")
		(print cm)))
	    (short-print ()
	      (printf "~a from ~a to ~a"
		(if spanning-bridge?
		  "Spanning vertical bridge"
		  "Vertical bridge")
		(tell object1 'ascii-name)
		(tell object2 'ascii-name))
	      (if* (< (tell self 'get-proposal-level) %built%)
		(printf (if (and %workspace-graphics% (not (tell self 'drawn?)))
			  " (~a, not drawn)"
			  " (~a)")
		  (case (tell self 'get-proposal-level)
		    (0 "new")
		    (1 "proposed")
		    (2 "evaluated"))))
	      (newline))
	    (spanning-bridge? () spanning-bridge?)
	    (group-spanning-bridge? () group-spanning-bridge?)
	    (get-from-graphics-coord () from-graphics-coord)
	    (get-to-graphics-coord () to-graphics-coord)
	    (set-new-from-graphics-coord (from-group)
              (set! from-graphics-coord
		(tell from-group 'get-group-spanning-bridge-graphics-coord 'vertical))
	      'done)
	    (set-new-to-graphics-coord (to-group)
              (set! to-graphics-coord
		(tell to-group 'get-group-spanning-bridge-graphics-coord 'vertical))
	      'done)
	    (concept-mapping-graphics-active? () concept-mapping-graphics-active?)
	    (get-cm-list-coord () concept-mapping-list-coord)
	    (get-bridge-label-number () bridge-label-number)
	    (activate-concept-mapping-graphics ()
              (set! concept-mapping-graphics-active? #t)
	      (set! bridge-label-number
		(if group-spanning-bridge?
		  0
		  (new-bridge-label-number self)))
	      (set! concept-mapping-list-coord
		(if (not group-spanning-bridge?)
		  (tell *workspace-window* 'get-cm-list-coord bridge-label-number)
		  (let ((spanning-cm-list-x
			  (+ (tell *workspace-window*
			       'get-spanning-vertical-bridge-right-x)
			     (tell *workspace-window* 'get-spanning-cm-list-offset)))
			(spanning-cm-list-y
			  (* 1/2 (+ (y-coord from-graphics-coord)
				    (y-coord to-graphics-coord)))))
		    `(,spanning-cm-list-x ,spanning-cm-list-y))))
	      (tell self 'set-concept-mapping-pexps all-concept-mappings 0)
	      'done)
	    (set-concept-mapping-pexps (cm-list starting-index-num)
              (for* i from 0 to (sub1 (length cm-list)) do
		(let* ((cm (nth i cm-list))
		       (n (+ i starting-index-num))
		       (line-offset (* 1 (if group-spanning-bridge?
					     (sub1 (* (expt -1 n) (ceiling (/ n 2))))
					     (- n)))))
		  (tell cm 'set-graphics-pexp
		    `(let-sgl ((origin ,concept-mapping-list-coord))
		       (text (text-relative (0 ,line-offset)) ,(tell cm 'print-name))))))
	      'done)
	    (get-object1 () object1)
	    (get-object2 () object2)
	    (get-enclosing-group1 () (tell object1 'get-enclosing-group))
	    (get-enclosing-group2 () (tell object2 'get-enclosing-group))
	    (get-original-object1 ()
              (if flipped-group1?
		  original-group1
		  object1))
	    (get-original-object2 ()
              (if flipped-group2?
		  original-group2
		  object2))
	    (get-concept-mapping-types ()
	      (tell-all all-concept-mappings 'get-CM-type))
	    (get-concept-mapping (description-type)
	      (select-meth all-concept-mappings 'CM-type? description-type))
	    (get-concept-mappings () concept-mappings)
	    (get-bond-concept-mappings () bond-concept-mappings)
	    (get-all-concept-mappings () all-concept-mappings)
	    (get-non-symmetric-slippages ()
	      (filter-meth all-concept-mappings 'slippage?))
	    (get-symmetric-slippages () symmetric-slippages)
	    (get-slippages ()
	      (append
		(tell self 'get-non-symmetric-slippages)
		symmetric-slippages))
	    (get-bond-slippages ()
	      (filter-meth (tell self 'get-slippages) 'bond-concept-mapping?))
	    (get-other-object (object)
	      (if (eq? object object1) object2 object1))
	    (get-covered-letters ()
	      (append
		(tell object1 'get-letters)
		(tell object2 'get-letters)))
	    (get-letter-span ()
	      (+ (tell object1 'get-letter-span)
		 (tell object2 'get-letter-span)))
	    (flipped-group1? () flipped-group1?)
	    (flipped-group2? () flipped-group2?)
	    (get-original-group1 () original-group1)
	    (get-original-group2 () original-group2)
	    (mark-flipped-group1 (original-group)
	      (set! flipped-group1? #t)
	      (set! original-group1 original-group)
	      'done)
	    (mark-flipped-group2 (original-group)
	      (set! flipped-group2? #t)
	      (set! original-group2 original-group)
	      'done)
	    (get-drawn-coincident-bridge ()
              (select-meth
	       (tell *workspace* 'get-all-other-coincident-bridges self object1 object2)
	       'drawn?))
	    (get-highest-level-coincident-bridge ()
              (let ((current-max 0)
		    (highest-level-bridge #f))
		(for* each b in (tell *workspace* 'get-all-other-coincident-bridges
				  self object1 object2) do
		  (let ((proposal-level (tell b 'get-proposal-level)))
		    (if* (> proposal-level current-max)
		      (set! current-max proposal-level)
		      (set! highest-level-bridge b))))
		highest-level-bridge))
	    (add-concept-mapping (cm)
	      (tell self 'add-concept-mappings (list cm)))
	    (add-concept-mappings (cm-list)
              (if* %workspace-graphics%
		(tell self 'set-concept-mapping-pexps
		  cm-list (length all-concept-mappings)))
	      (set! concept-mappings (append cm-list concept-mappings))
	      (set! all-concept-mappings (append cm-list all-concept-mappings))
	      'done)
	    (delete-concept-mapping-type (type)
	      (let ((cm (select-meth all-concept-mappings 'CM-type? type))
		    (ss (select-meth symmetric-slippages 'CM-type? type)))
		(set! concept-mappings (remq cm concept-mappings))
		(set! all-concept-mappings (remq cm all-concept-mappings))
		(if* (exists? ss)
		  (set! symmetric-slippages (remq ss symmetric-slippages)))
		(if* %workspace-graphics%
		  (tell *workspace-window* 'caching-on)
		  (tell *workspace-window* 'erase-concept-mapping cm)
		  (for* each cm in all-concept-mappings do
		    (tell *workspace-window* 'erase-concept-mapping cm))
		  (tell self 'set-concept-mapping-pexps all-concept-mappings 0)
		  (for* each cm in all-concept-mappings do
		    (tell *workspace-window* 'draw-concept-mapping cm))
		  (tell *workspace-window* 'flush)))
	      'done)
	    (add-bond-concept-mapping (bond-cm)
	      (set! bond-concept-mappings (cons bond-cm bond-concept-mappings))
	      (set! all-concept-mappings (cons bond-cm all-concept-mappings))
	      'done)
	    (add-symmetric-slippage (slippage)
	      (let ((symmetric-slippage (tell slippage 'symmetric-mapping)))
		(set! symmetric-slippages (cons symmetric-slippage symmetric-slippages)))
	      'done)
	    (concept-mapping-present? (concept-mapping)
	      (ormap (lambda (cm) (CMs-equal? cm concept-mapping))
		all-concept-mappings))
	    (CM-type-present? (type)
	      (ormap-meth all-concept-mappings 'CM-type? type))
	    (slippage-type-present? (type)
	      (let ((cm (select-meth all-concept-mappings 'CM-type? type)))
		(and (exists? cm) (tell cm 'slippage?))))
	    (supports-theme-pattern? (pattern)
	      (cross-product-ormap
		(lambda (entry cm)
		  (and (eq? (tell cm 'get-CM-type) (1st entry))
		       (eq? (tell cm 'get-label) (2nd entry))))
		(entries pattern)
		(remove-whole/single-concept-mappings
		  all-concept-mappings)))
	    (get-relevant-CMs ()
	      (filter-meth concept-mappings 'relevant?))
	    (get-distinguishing-CMs ()
	      (filter-meth concept-mappings 'distinguishing?))
	    (get-relevant-distinguishing-CMs ()
	      (filter-meth concept-mappings 'relevant-distinguishing?))
	    ;; Themes:
	    (incompatible-with-theme? (theme)
	      (or (check-descriptions object1 object2 conflicts-with-theme? theme)
		  (let* ((dimension (tell theme 'get-dimension))
			 (object1-description-possible?
			   (tell dimension 'description-possible? object1))
			 (object2-description-possible?
			   (tell dimension 'description-possible? object2)))
		    (or (and object1-description-possible?
			     (not object2-description-possible?))
		        (and (not object1-description-possible?)
			     object2-description-possible?)
			(and (eq? dimension plato-string-position-category)
			     (not object1-description-possible?)
			     (not object2-description-possible?))))))
	    (supported-by-theme? (theme)
	      (check-descriptions object1 object2 supported-by-theme? theme))
	    (get-thematic-compatibility ()
	      (bridge-theme-compatibility-sigmoid
		(tell self 'get-average-theme-support)))
	    (get-average-theme-support ()
	      (let* ((support-values (tell self 'get-theme-support-values))
		     (neg-weight (* 2 (length support-values)))
		     (support-weights
		       (map (lambda (n) (* (if (< n 0) neg-weight 1) (abs n)))
			 support-values)))
		(weighted-average support-values support-weights)))
	    (get-theme-support-values ()
	      (map (lambda (theme)
		     (cond
		       ((tell self 'incompatible-with-theme? theme)
			(- (% (tell theme 'get-activation))))
		       ((tell self 'supported-by-theme? theme)
			(% (tell theme 'get-activation)))
		       (else 0)))
		(tell *themespace* 'get-active-themes theme-type)))
	    (boost-themespace-activations ()
	      (tell self 'boost-themes)
	      (tell *themespace* 'update-dominant-themes theme-type)
	      (tell *themespace-window* 'update-graphics theme-type)
	      'done)
	    ;; This method just boosts theme activations.
	    ;; It does not update dominant themes or graphics:
	    (boost-themes ()
	      (let ((strength (tell self 'get-strength)))
		(cross-product-for-each
		  (lambda (d1 d2)
		    (if* (descriptions-affect-themespace? d1 d2)
		      (let ((theme (tell *themespace* 'add-theme-if-possible
				     theme-type
				     (tell d1 'get-description-type)
				     (get-label
				       (tell d1 'get-descriptor)
				       (tell d2 'get-descriptor)))))
			(if* (exists? theme)
			  (if spanning-bridge?
			    (tell theme 'boost-activation (* 2 strength))
			    (tell theme 'boost-activation strength))))))
		  (tell object1 'get-descriptions)
		  (tell object2 'get-descriptions)))
	      'done)
	    (get-associated-thematic-relations ()
	      (cross-product-filter-map
		descriptions-affect-themespace?
		(lambda (d1 d2)
		  (list
		    (tell d1 'get-description-type)
		    (get-label (tell d1 'get-descriptor) (tell d2 'get-descriptor))))
		(tell object1 'get-descriptions)
		(tell object2 'get-descriptions)))

	    (get-incompatible-bridges ()
	      (remq-duplicates
		(append
		  (filter (lambda (b) (incompatible-vertical-bridges? b self))
		    (tell *workspace* 'get-bridges bridge-type))
		  (group-incompatible-bridges 'vertical object1 object2)
		  (if (both-spanning-groups? object1 object2)
		    (let ((direction-CM (select-meth concept-mappings
					  'CM-type? plato-direction-category)))
		      (if (exists? direction-CM)
			(direction-incompatible-bridges 'vertical
			  object1 object2 direction-CM)
			'()))
		    '()))))
	    (get-incompatible-bond ()
	      (let ((bond1 (if (tell object1 'leftmost-in-string?)
			       (tell object1 'get-right-bond)
			       (tell object1 'get-left-bond)))
		    (bond2 (if (tell object2 'leftmost-in-string?)
			       (tell object2 'get-right-bond)
			       (tell object2 'get-left-bond))))
		(if (and (exists? bond1)
			 (exists? bond2)
			 (directed? bond1)
			 (directed? bond2)
			 (let ((direction-category-CM
				 (make-concept-mapping
				   bond1
				   plato-direction-category
				   (tell bond1 'get-direction)
				   bond2
				   plato-direction-category
				   (tell bond2 'get-direction))))
			   (incompatible-with-any-vertical-CM?
			     direction-category-CM concept-mappings)))
		    bond2
		    #f)))
	    (internally-coherent? ()
	      (let ((relevant-distinguishing-CMs
		      (tell self 'get-relevant-distinguishing-CMs)))
		(cross-product-ormap
		  (lambda (cm1 cm2)
		    (and (not (eq? cm1 cm2))
			 (supporting-vertical-CMs? cm1 cm2)))
		  relevant-distinguishing-CMs
		  relevant-distinguishing-CMs)))

	    (calculate-internal-strength ()
	      (let ((relevant-distinguishing-CMs
		      (tell self 'get-relevant-distinguishing-CMs)))
		(if (null? relevant-distinguishing-CMs)
		  0
		  (let* ((average-strength
			   (average (tell-all relevant-distinguishing-CMs 'get-strength)))
			 (num-of-concept-mappings
			   (length relevant-distinguishing-CMs))
			 (num-of-concept-mappings-factor
			   (cond
			     ((= num-of-concept-mappings 1) 0.8)
			     ((= num-of-concept-mappings 2) 1.2)
			     (else 1.6)))
			 (internal-coherence-factor
			   (if (tell self 'internally-coherent?) 2.5 1.0)))
		    (min 100 (round (* average-strength
				       num-of-concept-mappings-factor
				       internal-coherence-factor)))))))
	    (calculate-external-strength ()
	      (if (or (and (letter? object1) (tell object1 'spans-whole-string?))
		      (and (letter? object2) (tell object2 'spans-whole-string?)))
		100
		(let* ((supporting-bridges
			 (filter
			   (lambda (b) (supporting-vertical-bridges? self b))
			   (remq self (tell *workspace* 'get-bridges bridge-type))))
		       (total-support
			 (sum (tell-all supporting-bridges 'get-strength))))
		  (round (* (min 100 total-support))))))
	    (else (delegate msg workspace-structure))))))))


(define singleton-letter-factor
  (lambda (object1 object2)
    (cond
      ((singleton-letter? object1) (if (letter? object2) 1 0.1))
      ((singleton-letter? object2) (if (letter? object1) 1 0.1))
      ((tell object1 'singleton-group?) (if (group? object2) 1 0.1))
      ((tell object2 'singleton-group?) (if (group? object1) 1 0.1))
      (else 1))))


(define singleton-letter?
  (lambda (object)
    (let ((enclosing-group (tell object 'get-enclosing-group)))
      (and (letter? object)
	   (exists? enclosing-group)
	   (tell enclosing-group 'singleton-group?)))))


(define group-incompatible-bridges
  (lambda (bridge-orientation object1 object2)
    (let ((enclosing-group1 (tell object1 'get-enclosing-group))
	  (enclosing-group2 (tell object2 'get-enclosing-group)))
      (append
	(if (group? object1)
	  (let ((subobject-bridges
		  (tell object1 'get-subobject-bridges bridge-orientation)))
	    (if (letter? object2)
	      subobject-bridges
	      (filter-out
		(lambda (b) (tell object2 'top-level-member? (tell b 'get-object2)))
		subobject-bridges)))
	  '())
	(if (group? object2)
	  (let ((subobject-bridges
		  (tell object2 'get-subobject-bridges bridge-orientation)))
	    (if (letter? object1)
	      subobject-bridges
	      (filter-out
		(lambda (b) (tell object1 'top-level-member? (tell b 'get-object1)))
		subobject-bridges)))
	  '())
	(if (and (exists? enclosing-group1)
	         (exists? (tell enclosing-group1 'get-bridge bridge-orientation)))
	  (let* ((group-bridge (tell enclosing-group1 'get-bridge bridge-orientation))
		 (other-object (tell group-bridge 'get-object2)))
	    (if (or (not (exists? enclosing-group2))
		    (not (eq? enclosing-group2 other-object)))
	      (list group-bridge)
	      '()))
	  '())
	(if (and (exists? enclosing-group2)
	         (exists? (tell enclosing-group2 'get-bridge bridge-orientation)))
	  (let* ((group-bridge (tell enclosing-group2 'get-bridge bridge-orientation))
		 (other-object (tell group-bridge 'get-object1)))
	    (if (or (not (exists? enclosing-group1))
		    (not (eq? enclosing-group1 other-object)))
	      (list group-bridge)
	      '()))
	  '())))))


(define direction-incompatible-bridges
  (lambda (bridge-orientation group1 group2 direction-CM)
    (let* ((direction-label (tell direction-CM 'get-label))
	   (partition-function
	     (lambda (pred1? pred2?)
	       (lambda (b1 b2)
		 (let ((b1-pos1 (tell (tell b1 'get-object1) 'get-left-string-pos))
		       (b1-pos2 (tell (tell b1 'get-object2) 'get-left-string-pos))
		       (b2-pos1 (tell (tell b2 'get-object1) 'get-left-string-pos))
		       (b2-pos2 (tell (tell b2 'get-object2) 'get-left-string-pos)))
		   (or (and (< b1-pos1 b2-pos1) (pred1? b1-pos2 b2-pos2))
		       (and (> b1-pos1 b2-pos1) (pred2? b1-pos2 b2-pos2)))))))
	   (subobject-bridges
	     (intersect
	       (tell group1 'get-subobject-bridges bridge-orientation)
	       (tell group2 'get-subobject-bridges bridge-orientation)))
	   (mutually-compatible-bridges
	     (select-longest-list
	       (partition
		 (cond
		   ((eq? direction-label plato-identity) (partition-function < >))
		   ((eq? direction-label plato-opposite) (partition-function > <)))
		 subobject-bridges))))
      (remq-elements mutually-compatible-bridges subobject-bridges))))


(define-codelet-procedure* bottom-up-bridge-scout
  (lambda ()
    (let* ((bridge-types
	     (if %justify-mode%
	       '(top vertical bottom)
	       '(top vertical)))
	   (bridge-type-weights
	     (map (lambda (bridge-type)
		    (100- (tell *workspace* 'get-mapping-strength bridge-type)))
	       bridge-types))
	   (bridge-type
	     (stochastic-pick bridge-types bridge-type-weights))
	   (bridge-orientation (bridge-type->orientation bridge-type))
	   (strings
	     (case bridge-type
	       (top *top-strings*)
	       (bottom *bottom-strings*)
	       (vertical *vertical-strings*)))
	   (object1 (tell (1st strings) 'choose-object
		      'get-inter-string-salience bridge-orientation))
	   (object2 (tell (2nd strings) 'choose-object
		      'get-inter-string-salience bridge-orientation)))
      (say "Chose " bridge-type " objects "
	(tell object1 'ascii-name) " and " (tell object2 'ascii-name))
      (if* (lone-spanning-object? object1 object2)
	(say "One object spans string, other doesn't. Fizzling.")
	(fizzle))
      (if* %workspace-graphics%
	(draw-bridge-grope bridge-orientation object1 object2))
      (let* ((possible-CMs
	       (all-possible-bridge-CMs bridge-orientation
		 object1 (tell object1 'get-relevant-descriptions)
		 object2 (tell object2 'get-relevant-descriptions)))
	     (slippabilities
	       (map (compose temp-adjusted-probability %)
		 (tell-all possible-CMs 'get-slippability))))
	(say "Possible CMs:")
	(if* %verbose% (print possible-CMs))
	(say "Slippabilities: ")
	(say (map round-to-100ths slippabilities))
	(stochastic-if* (product (map 1- slippabilities))
	  (say "Couldn't make necessary slippages. Fizzling.")
	  (fizzle))
	;; Bridges that don't have at least one relevant distinguishing CM with
	;; an Identity/Opposite relation should not be proposed.  This prevents
	;; making bridges between objects without _some_ a priori justification.
	;; However, active horizontal-bridge themes can override this.
	;; Example: in abc->abcd, b--d bridge CMs are ObjCtgy:letter=>letter,
	;; StrPosCtgy:middle=>lmost, LettCtgy:b=>d.  The ObjCtgy CM is not
	;; distinguishing.  Bridge should not be made based only on StrPosCtgy
	;; and LettCtgy slippages (if an active horizontal-bridge theme such as
	;; StrPosCtgy:different exists, such a bridge may be proposed by
	;; thematic-bridge-scout codelets):
	(let ((distinguishing-CMs
		(filter-meth possible-CMs 'distinguishing-identity/opposite?)))
	  (if* (null? distinguishing-CMs)
	    (say "No distinguishing identity/opposite concept-mappings. Fizzling.")
	    (fizzle))
	  (let* ((proposed-bridge
		   (propose-bridge bridge-orientation object1 #f object2
		     (and (both-spanning-groups? object1 object2)
		          (reverse-direction-orientation? possible-CMs))))
		 (average-distinguishing-CM-strength
		   (average
		     (tell-all (tell proposed-bridge 'get-distinguishing-CMs)
		       'get-strength))))
	    (post-codelet*
	      urgency: average-distinguishing-CM-strength
	      bridge-evaluator proposed-bridge)))))))


(define-codelet-procedure* important-object-bridge-scout
  (lambda ()
    (let* ((bridge-types
	     (if %justify-mode%
	       '(top vertical bottom)
	       '(top vertical)))
	   (bridge-type-weights
	     (map (lambda (bridge-type)
		    (100- (tell *workspace* 'get-mapping-strength bridge-type)))
	       bridge-types))
	   (bridge-type
	     (stochastic-pick bridge-types bridge-type-weights))
	   (bridge-orientation (bridge-type->orientation bridge-type))
	   (strings
	     (case bridge-type
	       (top *top-strings*)
	       (bottom *bottom-strings*)
	       (vertical *vertical-strings*)))
	   (object1 (tell (1st strings) 'choose-object 'get-relative-importance))
	   (object1-description
	    (tell object1 'choose-relevant-distinguishing-description-by-depth)))
      (say "Chose " (tell object1 'ascii-name) " in "
	(tell (1st strings) 'generic-name) ".")
      (if* (not (exists? object1-description))
	(say "No relevant distinguishing descriptions. Fizzling.")
	(fizzle))
      (let* ((object1-descriptor (tell object1-description 'get-descriptor))
	     (applicable-slippage
	       (select
		 (lambda (s) (eq? (tell s 'get-descriptor1) object1-descriptor))
		 (tell *workspace* 'get-all-slippages bridge-type)))
	     (object2-descriptor
	       (if (exists? applicable-slippage)
		 (tell applicable-slippage 'get-descriptor2)
		 object1-descriptor))
	     (object2-candidates
	       (filter
		 (lambda (object)
		   (ormap
		     (lambda (d) (eq? (tell d 'get-descriptor) object2-descriptor))
		     (tell object 'get-relevant-descriptions)))
		 (tell (2nd strings) 'get-objects))))
	(say "Chose description " object1-description)
	(say "Looking for " (tell (2nd strings) 'generic-name)
	  " object with descriptor " object2-descriptor)
	(if* (null? object2-candidates)
	  (say "No object with proper descriptor. Fizzling.")
	  (fizzle))
	(let ((object2 (stochastic-pick-by-method object2-candidates
			 'get-inter-string-salience bridge-orientation)))
	  (say "Chose object2 to be " (tell object2 'ascii-name))
	  (if* (lone-spanning-object? object1 object2)
	    (say "One object spans string, other doesn't. Fizzling.")
	    (fizzle))
	  (if* %workspace-graphics%
	    (draw-bridge-grope bridge-orientation object1 object2))
	  (let* ((possible-CMs
		   (all-possible-bridge-CMs bridge-orientation
		     object1 (tell object1 'get-relevant-descriptions)
		     object2 (tell object2 'get-relevant-descriptions)))
		 (slippabilities
		   (map (compose temp-adjusted-probability %)
		     (tell-all possible-CMs 'get-slippability))))
	    (stochastic-if* (product (map 1- slippabilities))
	      (say "Couldn't make necessary slippages. Fizzling.")
	      (fizzle))
	    ;; Bridges that don't have at least one relevant distinguishing CM with
	    ;; an Identity/Opposite relation should not be proposed.  This prevents
	    ;; making bridges between objects without _some_ a priori justification.
	    ;; However, active bridge themes can override this.  Example:
	    ;; In abc->abcd, b--d bridge CMs are ObjCtgy:letter=>letter,
	    ;; StrPosCtgy:middle=>lmost, LettCtgy:b=>d.  The ObjCtgy CM is not
	    ;; distinguishing.  Bridge should not be made based only on StrPosCtgy
	    ;; and LettCtgy slippages (if an active horizontal-bridge theme such as
	    ;; StrPosCtgy:different exists, such a bridge may be proposed by
	    ;; thematic-bridge-scout codelets):
	    (let ((distinguishing-CMs
		    (filter-meth possible-CMs 'distinguishing-identity/opposite?)))
	      (if* (null? distinguishing-CMs)
		(say "No distinguishing identity/opposite concept-mappings. Fizzling.")
		(fizzle))
	      (let* ((proposed-bridge
		       (propose-bridge bridge-orientation object1 #f object2
			 (and (both-spanning-groups? object1 object2)
			      (reverse-direction-orientation? possible-CMs))))
		     (average-distinguishing-CM-strength
		       (average
			 (tell-all (tell proposed-bridge 'get-distinguishing-CMs)
			   'get-strength))))
		(post-codelet*
		  urgency: average-distinguishing-CM-strength
		  bridge-evaluator proposed-bridge)))))))))


(define reverse-direction-orientation?
  (lambda (concept-mappings)
    (and (ormap-meth concept-mappings 'CM-type? plato-direction-category)
         (andmap-meth
	   (filter-meth concept-mappings 'reversible-CM-type?)
	   'opposite-mapping?)
	 (not (fully-active? plato-opposite)))))


;; Good example of the contradictory assumptions that are deeply embedded within
;; the fabric of Copycat:  When proposing a new bridge, only _relevant_ descriptions
;; are used to create the new bridge's concept-mappings.  So if a particular concept
;; such as Direction-Category happens to be momentarily inactive, then no
;; Direction-Category concept-mapping will be included with the bridge.  Suppose that
;; a spanning bridge >abc>---<cba< currently exists, along with a-a, b-b, and c-c.
;; If the proposed bridge is between >abc> and a flipped version of <cba<, that is,
;; >abc>--->cba>, it will not be judged incompatible with a-a, b-b, and c-c, due to
;; the lack of a Direction:right=>right concept-mapping.  These latter three bridges
;; therefore will not be broken.  The end result will be that the original >abc>--<cba<
;; spanning bridge is replaced by >abc>--->cba>, but the original three a-a, b-b, and
;; c-c bridges are still intact, resulting in a contradictory situation.  This problem
;; will crop up rarely, but completely unpredictably (depending on the ever-changing
;; activations of slipnodes).


(define propose-bridge
  (lambda (bridge-orientation object1 flip1? object2 flip2?)
    (let* ((obj1 (if flip1? (tell object1 'make-flipped-version) object1))
	   (obj2 (if flip2? (tell object2 'make-flipped-version) object2))
	   (concept-mappings
	     (all-possible-bridge-CMs bridge-orientation
	       ;; This is a hack in order to get around the problem described above:
	       obj1 (if (tell obj1 'string-spanning-group?)
		      (tell obj1 'get-descriptions)
		      (tell obj1 'get-relevant-descriptions))
	       obj2 (if (tell obj2 'string-spanning-group?)
		      (tell obj2 'get-descriptions)
		      (tell obj2 'get-relevant-descriptions))))
	   (proposed-bridge
	     (case bridge-orientation
	       (horizontal (make-horizontal-bridge obj1 obj2 concept-mappings))
	       (vertical (make-vertical-bridge obj1 obj2 concept-mappings)))))
      (if* flip1? (tell proposed-bridge 'mark-flipped-group1 object1))
      (if* flip2? (tell proposed-bridge 'mark-flipped-group2 object2))
      (if* %workspace-graphics%
	(if* flip1? (tell proposed-bridge 'set-new-from-graphics-coord object1))
	(if* flip2? (tell proposed-bridge 'set-new-to-graphics-coord object2)))
      (tell *workspace* 'add-proposed-bridge proposed-bridge)
      (tell proposed-bridge 'update-proposal-level %proposed%)
      (for* each cm in concept-mappings do
	(tell cm 'activate-descriptions))
      (if* %workspace-graphics%
	(bridge-graphics 'set-pexp-and-draw proposed-bridge))
      (cond
	((and flip1? flip2?)
	 (say "Proposing " bridge-orientation " bridge with both flipped groups:"))
	(flip1? (say "Proposing " bridge-orientation " bridge with flipped group1:"))
	(flip2? (say "Proposing " bridge-orientation " bridge with flipped group2:"))
	(else (say "Proposing " bridge-orientation " bridge:")))
      (if* %verbose% (print proposed-bridge))
      (if* (and (eq? bridge-orientation 'horizontal)
	        (not (eq? (tell object1 'get-platonic-length)
		          (tell object2 'get-platonic-length))))
	(tell plato-length 'activate-from-workspace)
	(post-codelet*
	  urgency: %very-high-urgency%
	  top-down-description-scout
	  plato-length (tell object1 'get-string))
	(post-codelet*
	  urgency: %very-high-urgency%
	  top-down-description-scout
	  plato-length (tell object2 'get-string))
	(if* (and (letter? object1) (group? object2))
	  (tell (tell object2 'get-group-category) 'activate-from-workspace)
	  (post-codelet*
	    urgency: %very-high-urgency%
	    top-down-group-scout:category
	    (tell object2 'get-group-category)
	    (tell object1 'get-string)))
	(if* (and (group? object1) (letter? object2))
	  (tell (tell object1 'get-group-category) 'activate-from-workspace)
	  (post-codelet*
	    urgency: %very-high-urgency%
	    top-down-group-scout:category
	    (tell object1 'get-group-category)
	    (tell object2 'get-string))))
      proposed-bridge)))


(define-codelet-procedure* bridge-evaluator
  (lambda (proposed-bridge)
    (let ((bridge-orientation (tell proposed-bridge 'get-orientation)))
      (if* %verbose% (print proposed-bridge))
      (if* (not (and (tell *workspace* 'object-exists?
		       (tell proposed-bridge 'get-original-object1))
		     (tell *workspace* 'object-exists?
		       (tell proposed-bridge 'get-original-object2))))
	(say "One or both objects no longer exist. Fizzling.")
	;; Erasing the bridge is unnecessary, since it was
	;; erased at the time object1 or object2 got broken.
	(fizzle))
      (if* %workspace-graphics%
	(bridge-graphics 'flash proposed-bridge))
      (tell proposed-bridge 'update-strength)
      (let ((strength (tell proposed-bridge 'get-strength)))
	(say "Strength is " (round strength))
	(stochastic-if* (1- (temp-adjusted-probability (% strength)))
	  (say "Bridge not strong enough. Fizzling.")
	  (tell *workspace* 'delete-proposed-bridge proposed-bridge)
	  (if* %workspace-graphics%
	    (bridge-graphics 'erase proposed-bridge))
	  (fizzle))
	(let ((concept-mappings (tell proposed-bridge 'get-concept-mappings)))
	  (for* each cm in concept-mappings do
	    (tell cm 'activate-descriptions))
	  (tell proposed-bridge 'update-proposal-level %evaluated%)
	  (if* %workspace-graphics%
	    (bridge-graphics 'update-level proposed-bridge))
	  (post-codelet*
	    urgency: strength
	    bridge-builder proposed-bridge))))))


(define-codelet-procedure* bridge-builder
  (lambda (proposed-bridge)
    (if* %verbose% (print proposed-bridge))
    (if* (not (and (tell *workspace* 'object-exists?
		     (tell proposed-bridge 'get-original-object1))
		   (tell *workspace* 'object-exists?
		     (tell proposed-bridge 'get-original-object2))))
      (say "One or both objects no longer exist. Fizzling.")
      ;; Erasing the bridge is unnecessary, since it was
      ;; erased at the time object1 or object2 got broken.
      (fizzle))
    (tell *workspace* 'delete-proposed-bridge proposed-bridge)
    (let ((bridge-orientation (tell proposed-bridge 'get-orientation))
	  (object1 (tell proposed-bridge 'get-object1))
	  (object2 (tell proposed-bridge 'get-object2))
	  (concept-mappings (tell proposed-bridge 'get-concept-mappings)))
      ;; This is necessary because StrPosCtgy:middle descriptions can now be deleted:
      (if* (not (andmap (lambda (type)
			  (and (tell object1 'description-type-present? type)
			       (tell object2 'description-type-present? type)))
		  (tell proposed-bridge 'get-concept-mapping-types)))
	(say "Not all necessary descriptions still exist. Fizzling.")
	(if* %workspace-graphics%
	  (bridge-graphics 'erase proposed-bridge))
	(fizzle))
      (if* (bridge-between? bridge-orientation object1 object2)
	(say "This bridge already exists.")
	(say "Adding any new concept-mappings and fizzling...")
	(for* each cm in concept-mappings do
	  (tell cm 'activate-label))
	(let* ((existing-bridge (tell object1 'get-bridge bridge-orientation))
	       (concept-mappings-to-be-added
		 (filter-out
		   (lambda (cm)
		     (tell existing-bridge 'concept-mapping-present? cm))
		   concept-mappings)))
	  (if* (not (null? concept-mappings-to-be-added))
	    (tell existing-bridge 'add-concept-mappings concept-mappings-to-be-added)
	    (monitor-new-concept-mappings
	      concept-mappings-to-be-added existing-bridge)
	    (if* %verbose% (print concept-mappings-to-be-added))
	    (if* %workspace-graphics%
	      (bridge-graphics 'flash existing-bridge)
	      ;; Currently only vertical bridges show concept-mappings:
	      (if* (tell proposed-bridge 'bridge-type? 'vertical)
		(tell *workspace-window* 'caching-on)
		(for* each cm in concept-mappings-to-be-added do
		  (tell *workspace-window* 'draw-concept-mapping cm))
		(tell *workspace-window* 'flush))))
	  (fizzle)))
      (if* (not (andmap-meth concept-mappings 'relevant?))
	(say "Not all concept-mappings are still relevant. Fizzling.")
	(if* %workspace-graphics%
	  (bridge-graphics 'erase proposed-bridge))
	(fizzle))
      (if* %workspace-graphics%
	(bridge-graphics 'flash proposed-bridge))
      (say "About to fight against incompatible structures...")
      (let ((incompatible-bridges
	      (tell proposed-bridge 'get-incompatible-bridges)))
	(if* (and %verbose% (not (null? incompatible-bridges)))
	  (say "Fighting against incompatible bridges:")
	  (print incompatible-bridges)
	  (say "Weight is " (tell proposed-bridge 'get-letter-span))
	  (say "Opposing weights are "
	    (tell-all incompatible-bridges 'get-letter-span)))
	(if* (and (not (null? incompatible-bridges))
	          (not (wins-all-fights?
			 proposed-bridge
			 (tell proposed-bridge 'get-letter-span)
			 incompatible-bridges
			 (tell-all incompatible-bridges 'get-letter-span))))
	  (say "Lost to incompatible bridge. Fizzling.")
	  (if* %workspace-graphics%
	    (bridge-graphics 'erase proposed-bridge))
	  (fizzle))
	(let ((incompatible-bond
		(if (and (or (tell object1 'leftmost-in-string?)
			     (tell object1 'rightmost-in-string?))
		         (or (tell object2 'leftmost-in-string?)
			     (tell object2 'rightmost-in-string?)))
		  (tell proposed-bridge 'get-incompatible-bond)
		  #f)))
	  (if* (and %verbose% (exists? incompatible-bond))
	    (say "Fighting against incompatible bond:")
	    (print incompatible-bond))
	  (if* (and (exists? incompatible-bond)
		    (not (wins-fight?
			   proposed-bridge 3
			   incompatible-bond 2)))
	    (say "Lost to incompatible bond. Fizzling.")
	    (if* %workspace-graphics%
	      (bridge-graphics 'erase proposed-bridge))
	    (fizzle))
	  (let ((incompatible-group
		  (if (exists? incompatible-bond)
		    (tell incompatible-bond 'get-enclosing-group)
		    #f)))
	    (if* (and %verbose% (exists? incompatible-group))
	      (say "Fighting against incompatible group:")
	      (print incompatible-group))
	    (if* (and (exists? incompatible-group)
		      (not (wins-fight?
			     proposed-bridge 1
			     incompatible-group 1)))
	      (say "Lost to incompatible group. Fizzling.")
	      (if* %workspace-graphics%
		(bridge-graphics 'erase proposed-bridge))
	      (fizzle))
	    (if* (and %verbose% (tell proposed-bridge 'flipped-group1?))
	      (say "Fighting against existing group1:")
	      (print (tell proposed-bridge 'get-original-group1)))
	    (if* (and (tell proposed-bridge 'flipped-group1?)
		      (not (wins-fight?
			     proposed-bridge 1
			     (tell proposed-bridge 'get-original-group1) 1)))
	      (say "Lost to existing group1. Fizzling.")
	      (if* %workspace-graphics%
		(bridge-graphics 'erase proposed-bridge))
	      (fizzle))
	    (if* (and %verbose% (tell proposed-bridge 'flipped-group2?))
	      (say "Fighting against existing group2:")
	      (print (tell proposed-bridge 'get-original-group2)))
	    (if* (and (tell proposed-bridge 'flipped-group2?)
		      (not (wins-fight?
			     proposed-bridge 1
			     (tell proposed-bridge 'get-original-group2) 1)))
	      (say "Lost to existing group2. Fizzling.")
	      (if* %workspace-graphics%
		(bridge-graphics 'erase proposed-bridge))
	      (fizzle))
	    (say "Won against all incompatible structures.")
	    (if* (not (null? incompatible-bridges))
	      (for* each bridge in incompatible-bridges do
		(break-bridge bridge)))
	    (if* (exists? incompatible-bond)
	      (break-bond incompatible-bond))
	    (if* (exists? incompatible-group)
	      (break-group incompatible-group))
	    (if* (tell proposed-bridge 'flipped-group1?)
	      (say "*** flipping group1 ***")
	      (let ((original-group
		      (tell proposed-bridge 'get-original-group1)))
		(break-group original-group)
		(for* each bond in (tell original-group 'get-constituent-bonds) do
		  (break-bond bond))
		(for* each bond in (tell object1 'get-constituent-bonds) do
		  (build-bond bond))
		(build-group object1 #t)
		(if* %workspace-graphics%
		  (group-graphics 'set-pexp-and-draw object1)
		  (tell proposed-bridge 'set-new-from-graphics-coord object1))))
	    (if* (tell proposed-bridge 'flipped-group2?)
	      (say "*** flipping group2 ***")
	      (let ((original-group
		      (tell proposed-bridge 'get-original-group2)))
		(break-group original-group)
		(for* each bond in (tell original-group 'get-constituent-bonds) do
		  (break-bond bond))
		(for* each bond in (tell object2 'get-constituent-bonds) do
		  (build-bond bond))
		(build-group object2 #t)
		(if* %workspace-graphics%
		  (group-graphics 'set-pexp-and-draw object2)
		  (tell proposed-bridge 'set-new-to-graphics-coord object2))))
	    (build-bridge bridge-orientation proposed-bridge)
	    ;; This must happen _after_ the bridge is built, since building the
	    ;; bridge may cause bond-concept-mappings to be added to it (and
	    ;; possibly an ObjCtgy concept-mapping, for horizontal bridges):
	    (tell proposed-bridge 'boost-themespace-activations)))))))


(define build-bridge
  (lambda (bridge-orientation bridge)
    (say "Building bridge:")
    (if* %verbose% (print bridge))
    (let ((object1 (tell bridge 'get-object1))
	  (object2 (tell bridge 'get-object2)))
      (tell object1 'update-bridge bridge-orientation bridge)
      (tell object2 'update-bridge bridge-orientation bridge)
      (tell *workspace* 'add-bridge bridge)

      (if* (eq? bridge-orientation 'horizontal)
	;; Need to ensure that an ObjCtgy CM/slippage exists for every horizontal bridge,
	;; whether or not it's relevant, so as to avoid problems with rule abstraction.
	;; Example: ab --> aabb, a-->aa bridge has ObjCtgy:letter=>group slippage
	;; but b-->bb bridge does not.  This leads to the following rule:
	;;     Increase length of each object in string by one
	;;     Change leftmost letter to a group
	(if* (not (ormap-meth (tell bridge 'get-concept-mappings)
		    'CM-type? plato-object-category))
	  (let ((ObjCtgy-CM (make-concept-mapping
			      object1 plato-object-category
			      (tell object1 'get-descriptor-for plato-object-category)
			      object2 plato-object-category
			      (tell object2 'get-descriptor-for plato-object-category))))
	    (tell bridge 'add-concept-mapping ObjCtgy-CM)
	    (if* (tell ObjCtgy-CM 'slippage?)
	      (tell bridge 'add-symmetric-slippage ObjCtgy-CM)))))

      (for* each slippage in
	(filter-meth (tell bridge 'get-concept-mappings) 'slippage?)
	do (tell bridge 'add-symmetric-slippage slippage))
      (if* (and (group? object1) (group? object2))
	(for* each bond-cm in
	  (all-possible-bridge-CMs bridge-orientation
	    object1 (tell object1 'get-bond-descriptions)
	    object2 (tell object2 'get-bond-descriptions)) do
	  (tell bridge 'add-bond-concept-mapping bond-cm)
	  (if* (tell bond-cm 'slippage?)
	    (tell bridge 'add-symmetric-slippage bond-cm))))

      (if* (eq? bridge-orientation 'horizontal)
	;; Attach length description(s) if lengths are different, and add a Length
	;; slippage to bridge's concept-mappings, but don't active Length itself.
	;; Special case: if bridge is from a letter to a singleton group, attach
	;; a Length description (if one doesn't already exist) to the singleton group
	;; and add a Length:one=>one concept-mapping. 
	;; Another special case:  if bridge is between a letter and a group,
	;; try to propose a singleton group based on the letter, with the same
	;; group category and direction as the corresponding group.
	(let ((length1 (tell object1 'get-platonic-length))
	      (length2 (tell object2 'get-platonic-length)))
	  (if* (and (not (eq? length1 length2))
		    (not (tell bridge 'CM-type-present? plato-length)))
	      (tell bridge 'add-concept-mapping
		(make-concept-mapping
		  object1 plato-length length1
		  object2 plato-length length2)))))

      (for* each cm in (tell bridge 'get-concept-mappings) do
	(tell cm 'activate-label))
      (tell bridge 'update-proposal-level %built%)
      (monitor-new-concept-mappings (tell bridge 'get-all-concept-mappings) bridge)
      (if* %workspace-graphics%
	(tell bridge 'activate-concept-mapping-graphics)
	(bridge-graphics 'update-level bridge)))))


(define propose-singleton-group
  (lambda (letter group-category direction)
    (propose-group (list letter) '() group-category direction)))


(define try-to-propose-singleton-group
  (lambda (letter group-category direction)
    (let ((singleton-group
	    (make-group (tell letter 'get-string) group-category
	      plato-letter-category direction letter letter (list letter) '())))
      (stochastic-if* (single-letter-group-probability singleton-group)
	(say "Local support strong enough. Proposing single-letter group.")
	(propose-group (list letter) '() group-category direction)))))


(define break-bridge
  (lambda (bridge)
    (say "Breaking bridge:")
    (if* %verbose% (print bridge))
    (let ((bridge-orientation (tell bridge 'get-orientation))
	  (object1 (tell bridge 'get-object1))
	  (object2 (tell bridge 'get-object2)))
      (tell object1 'update-bridge bridge-orientation #f)
      (tell object2 'update-bridge bridge-orientation #f)
      (tell *workspace* 'delete-bridge bridge)
      (if* %workspace-graphics%
	(bridge-graphics 'erase bridge)))
    'done))


(define bridge-type->orientation
  (lambda (bridge-type)
    (case bridge-type
      ((top bottom) 'horizontal)
      (vertical 'vertical))))


(define enclosing-bridge?
  (lambda (b1 b2)
    (and (tell (tell b1 'get-object1) 'nested-member? (tell b2 'get-object1))
	 (tell (tell b1 'get-object2) 'nested-member? (tell b2 'get-object2)))))


(define bridges-equal?
  (lambda (bridge1 bridge2)
    (and (eq? (tell bridge1 'get-object1) (tell bridge2 'get-object1))
	 (eq? (tell bridge1 'get-object2) (tell bridge2 'get-object2)))))


(define bridge-between?
  (lambda (bridge-orientation object1 object2)
    (let ((bridge (tell object1 'get-bridge bridge-orientation)))
      (and (exists? bridge)
	   (eq? (tell bridge 'get-object2) object2)))))


(define get-bridge-between
  (lambda (bridge-orientation object1 object2)
    (let ((bridge (tell object1 'get-bridge bridge-orientation)))
      (if (and (exists? bridge)
	       (eq? (tell bridge 'get-object2) object2))
	bridge
	#f))))


(define letter-category/length-slippage?
  (lambda (cm)
    (or (and (tell cm 'CM-type? plato-letter-category)
	     (tell cm 'slippage?))
	(and (tell cm 'CM-type? plato-length)
	     (tell cm 'slippage?)))))


(define all-possible-bridge-CMs
  (lambda (bridge-orientation object1 object1-descriptions object2 object2-descriptions)
    (cross-product-filter-map
      (case bridge-orientation
	(horizontal (horizontal-mappable-descriptions? object1 object2))
	(vertical (vertical-mappable-descriptions? object1 object2)))
      (lambda (description1 description2)
	(make-concept-mapping
	  object1
	  (tell description1 'get-description-type)
	  (tell description1 'get-descriptor)
	  object2
	  (tell description2 'get-description-type)
	  (tell description2 'get-descriptor)))
      object1-descriptions object2-descriptions)))


;; object1 and object2 will never be Bfacet:length groups because such
;; groups do not have LettCtgy descriptions:

(define letter-category-mappable-objects?
  (lambda (object1 object2)
    (or (and (letter? object1) (letter? object2))
	(and (letter? object1) (group? object2) (tell object2 'all-letter-group?))
	(and (group? object1) (tell object1 'all-letter-group?) (letter? object2))
	(and (group? object1) (group? object2)
	     (related? (tell object1 'get-group-category)
		       (tell object1 'get-group-category))))))

;;--------------------- Code specific to horizontal bridges ------------------------

(define horizontal-mappable-descriptions?
  (lambda (object1 object2)
    (lambda (description1 description2)
      (let ((description-type1 (tell description1 'get-description-type))
	    (description-type2 (tell description2 'get-description-type)))
	(and (eq? description-type1 description-type2)
	     (if (eq? description-type1 plato-letter-category)
		 (letter-category-mappable-objects? object1 object2)
		 (let ((descriptor1 (tell description1 'get-descriptor))
		       (descriptor2 (tell description2 'get-descriptor)))
		   (or (eq? description-type1 plato-string-position-category)
		       (eq? description-type1 plato-length)
		       (eq? descriptor1 descriptor2)
		       (slip-linked? descriptor1 descriptor2)))))))))


(define supporting-horizontal-bridges?
  (lambda (b1 b2)
    (and (not (incompatible-horizontal-bridges? b1 b2))
	 (cross-product-ormap
	   supporting-horizontal-CMs?
	   (tell b1 'get-distinguishing-CMs)
	   (tell b2 'get-distinguishing-CMs)))))


(define incompatible-horizontal-bridges?
  (lambda (b1 b2)
    (or (eq? (tell b1 'get-object1) (tell b2 'get-object1))
	(eq? (tell b1 'get-object2) (tell b2 'get-object2))
	;; This is to avoid certain CM "incompatibilities" which are
	;; not really incompatible. For example, in abc -> abcc we
	;; don't want the spanning bridge to be incompatible with
	;; c-->cc based on the Length CMs 3=>3 and 1=>2. In aabc ->
	;; aabcc, we don't want aa-->aa (Length:2=>2) to be incompatible
	;; with c-->cc (Length:1=>2).  Don't need to do this for vertical
	;; bridges, since letter-category or length *slippages* cannot
	;; underlie vertical bridges. Also, as for vertical bridges,
	;; DirCtgy concept-mappings should only be considered if the
	;; bridges are nested. Example: [[xy][fg]] --> [[fg][xy]]
	;; (see incompatible-vertical-bridges? for more info):
	(let ((b1-concept-mappings
		(filter-out letter-category/length-slippage?
		  (tell b1 'get-concept-mappings)))
	      (b2-concept-mappings
		(filter-out letter-category/length-slippage?
		  (tell b2 'get-concept-mappings))))
	  (incompatible-horizontal-CM-lists?
	    (if (enclosing-bridge? b1 b2)
	      b1-concept-mappings
	      (filter-out-meth b1-concept-mappings
		'CM-type? plato-direction-category))
	    (if (enclosing-bridge? b2 b1)
	      b2-concept-mappings
	      (filter-out-meth b2-concept-mappings
		'CM-type? plato-direction-category)))))))


(define incompatible-horizontal-CM-lists?
  (lambda (l1 l2)
    (cross-product-ormap incompatible-horizontal-CMs? l1 l2)))


(define incompatible-with-any-horizontal-CM?
  (lambda (cm l)
    (cross-product-ormap incompatible-horizontal-CMs? (list cm) l)))


(define supporting-horizontal-CMs?
  (lambda (cm1 cm2)
    (or (CMs-equal? cm1 cm2)
	(and (or (related? (tell cm1 'get-descriptor1) (tell cm2 'get-descriptor1))
		 (related? (tell cm1 'get-descriptor2) (tell cm2 'get-descriptor2)))
	     (exists? (tell cm1 'get-label))
	     (exists? (tell cm2 'get-label))
	     (eq? (tell cm1 'get-label) (tell cm2 'get-label))))))


(define incompatible-horizontal-CMs?
  (lambda (cm1 cm2)
    (let ((cm1-desc1 (tell cm1 'get-descriptor1))
	  (cm1-desc2 (tell cm1 'get-descriptor2))
	  (cm2-desc1 (tell cm2 'get-descriptor1))
	  (cm2-desc2 (tell cm2 'get-descriptor2)))
      (and (or (related? cm1-desc1 cm2-desc1)
	       (related? cm1-desc2 cm2-desc2))
	   (exists? (tell cm1 'get-label))
	   (exists? (tell cm2 'get-label))
	   (not (eq? (tell cm1 'get-label) (tell cm2 'get-label)))
	   (not (eq? (get-label cm1-desc1 cm2-desc1)
		     (get-label cm1-desc2 cm2-desc2)))))))


;;-------------------- Code specific to vertical bridges ------------------------

(define vertical-mappable-descriptions?
  (lambda (object1 object2)
    (lambda (description1 description2)
      (let ((description-type1 (tell description1 'get-description-type))
	    (description-type2 (tell description2 'get-description-type)))
	(and (eq? description-type1 description-type2)
	     (let ((descriptor1 (tell description1 'get-descriptor))
		   (descriptor2 (tell description2 'get-descriptor)))
	       (or (eq? descriptor1 descriptor2)
		   (slip-linked? descriptor1 descriptor2))))))))


(define supporting-vertical-bridges?
  (lambda (b1 b2)
    (and (not (incompatible-vertical-bridges? b1 b2))
	 (cross-product-ormap
	   supporting-vertical-CMs?
	   (tell b1 'get-distinguishing-CMs)
	   (tell b2 'get-distinguishing-CMs)))))


(define incompatible-vertical-bridges?
  (lambda (b1 b2)
    (or (eq? (tell b1 'get-object1) (tell b2 'get-object1))
	(eq? (tell b1 'get-object2) (tell b2 'get-object2))
	;; Direction-category-CM of a bridge should only be considered
	;; if the bridge encloses the other bridge.  This prevents
	;; unnecessary incompatibilities between directed-group
	;; bridges.  Example:        [[xy][fg]] -> ...
	;;                               \ /
	;;                                X
	;;                               / \
	;;                           [[fg][xy]] -> ?
	;; "DirCtgy:right=>right" CMs would be incompatible with the
	;; "StrPosCtgy:lmost=(opp)=>rmost" and "StrPosCtgy:rmost=(opp)=>lmost"
	;; CMs.  However, if the whole-string groups are directed the same way,
	;; the spanning-bridge DirCtgy CM should be included to make
	;; the StrPosCtgy mappings incompatible:
	(let ((b1-concept-mappings (tell b1 'get-concept-mappings))
	      (b2-concept-mappings (tell b2 'get-concept-mappings)))
	  (incompatible-vertical-CM-lists?
	   (if (enclosing-bridge? b1 b2)
	     b1-concept-mappings
	     (filter-out-meth b1-concept-mappings
	       'CM-type? plato-direction-category))
	   (if (enclosing-bridge? b2 b1)
	     b2-concept-mappings
	     (filter-out-meth b2-concept-mappings
	       'CM-type? plato-direction-category)))))))


(define incompatible-vertical-CM-lists?
  (lambda (l1 l2)
    (cross-product-ormap incompatible-vertical-CMs? l1 l2)))


(define incompatible-with-any-vertical-CM?
  (lambda (cm l)
    (cross-product-ormap incompatible-vertical-CMs? (list cm) l)))


(define supporting-vertical-CMs?
  (lambda (cm1 cm2)
    (or (CMs-equal? cm1 cm2)
	(and (or (related? (tell cm1 'get-descriptor1) (tell cm2 'get-descriptor1))
		 (related? (tell cm1 'get-descriptor2) (tell cm2 'get-descriptor2)))
	     (exists? (tell cm1 'get-label))
	     (exists? (tell cm2 'get-label))
	     (eq? (tell cm1 'get-label) (tell cm2 'get-label))))))


(define incompatible-vertical-CMs?
  (lambda (cm1 cm2)
    (let ((cm1-desc1 (tell cm1 'get-descriptor1))
	  (cm1-desc2 (tell cm1 'get-descriptor2))
	  (cm2-desc1 (tell cm2 'get-descriptor1))
	  (cm2-desc2 (tell cm2 'get-descriptor2)))
      (and (or (related? cm1-desc1 cm2-desc1)
	       (related? cm1-desc2 cm2-desc2))
	   (exists? (tell cm1 'get-label))
	   (exists? (tell cm2 'get-label))
	   (not (eq? (tell cm1 'get-label) (tell cm2 'get-label)))
	   (not (eq? (get-label cm1-desc1 cm2-desc1)
		     (get-label cm1-desc2 cm2-desc2)))))))

