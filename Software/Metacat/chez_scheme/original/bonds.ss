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

(define make-bond
  (lambda (from-object to-object bond-category bond-facet
	    from-object-descriptor to-object-descriptor)
    (let* ((workspace-structure (make-workspace-structure))
	   (left-object
	     (if (< (tell from-object 'get-left-string-pos)
		    (tell to-object 'get-left-string-pos))
	       from-object
	       to-object))
	   (right-object
	     (if (eq? left-object from-object)
	       to-object
	       from-object))
	   (string (tell left-object 'get-string))
	   (direction
	     (cond
	       ((eq? bond-category plato-sameness) #f)
	       ((eq? left-object from-object) plato-right)
	       (else plato-left)))
	   (left-string-pos (tell left-object 'get-left-string-pos))
	   (right-string-pos (tell right-object 'get-right-string-pos))
	   (bond-importance (if (exists? direction) 50 100)))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'bond)
	    (print ()
	      (printf "Bond ~a ~a ~a in ~a"
		(tell left-object 'ascii-name)
		(format
		  (cond
		    ((eq? bond-category plato-sameness) "=~a=")
		    ((eq? direction plato-left) "<-~a-")
		    ((eq? direction plato-right) "-~a->"))
		  (cond
		    ((eq? bond-category plato-sameness) "same")
		    ((eq? bond-category plato-successor) "succ")
		    ((eq? bond-category plato-predecessor) "pred")))
		(tell right-object 'ascii-name)
		(tell string 'generic-name))
	      (if* (< (tell self 'get-proposal-level) %built%)
		(printf " (~a)" (case (tell self 'get-proposal-level)
				  (0 "new")
				  (1 "proposed")
				  (2 "evaluated"))))
	      (newline))
	    (get-string () string)
	    (get-from-object () from-object)
	    (get-to-object () to-object)
	    (get-left-object () left-object)
	    (get-right-object () right-object)
	    (get-bond-category () bond-category)
	    (get-direction () direction)
	    (get-bond-facet () bond-facet)
	    (get-from-object-descriptor () from-object-descriptor)
	    (get-to-object-descriptor () to-object-descriptor)
	    (leftmost-in-string? () (= left-string-pos 0))
	    (rightmost-in-string? ()
	      (= right-string-pos (sub1 (tell string 'get-length))))
	    (get-incompatible-bonds ()
	      (remq-duplicates
		(compress
		  (list (tell left-object 'get-right-bond)
			(tell right-object 'get-left-bond)))))
	    (get-incompatible-bridges (bridge-orientation)
	      (compress
		(list
		  (tell self 'get-incompatible-bridge left-object bridge-orientation)
		  (tell self 'get-incompatible-bridge right-object bridge-orientation))))
	    (get-incompatible-bridge (object bridge-orientation)
	      (continuation-point* return
		(let ((bridge (tell object 'get-bridge bridge-orientation)))
		  (if* (not (exists? bridge)) (return #f))
		  (let ((string-position-CM
			  (select-meth (tell bridge 'get-concept-mappings)
			    'CM-type? plato-string-position-category)))
		    (if* (not (exists? string-position-CM))
		      (return #f))
		    (let ((other-object (tell bridge 'get-other-object object)))
		      (if* (not (or (tell other-object 'leftmost-in-string?)
				    (tell other-object 'rightmost-in-string?)))
			(return #f))
		      (let ((other-bond
			      (if (tell other-object 'leftmost-in-string?)
				(tell other-object 'get-right-bond)
				(tell other-object 'get-left-bond))))
			(if* (not (and (exists? other-bond) (directed? other-bond)))
			  (return #f))
			(let ((bond-direction-CM
				(make-concept-mapping
				  self
				  plato-direction-category
				  direction
				  other-bond
				  plato-direction-category
				  (tell other-bond 'get-direction)))
			      (incompatible?
				(case bridge-orientation
				  (horizontal incompatible-horizontal-CMs?)
				  (vertical incompatible-vertical-CMs?))))
			  (if (incompatible? bond-direction-CM string-position-CM)
			    bridge
			    #f))))))))
	    (make-flipped-version ()
	      (make-bond
		to-object from-object
		(tell bond-category 'get-related-node plato-opposite)
		bond-facet to-object-descriptor from-object-descriptor))
	    (get-num-of-local-supporting-bonds ()
	      (count
		(lambda (other-bond)
		  (and (disjoint-objects? left-object (tell other-bond 'get-left-object))
		       (disjoint-objects? right-object (tell other-bond 'get-right-object))
		       (eq? (tell other-bond 'get-bond-category) bond-category)
		       (eq? (tell other-bond 'get-direction) direction)))
		(remq self (tell string 'get-bonds))))
	    (get-local-density ()
	      (letrec
		((neighbors
		   (lambda (object choose-method)
		     (let ((neighbor (tell object choose-method)))
		       (if (not (exists? neighbor))
			 '()
			 (cons neighbor (neighbors neighbor choose-method)))))))
		(let* ((left-neighbors (neighbors left-object 'choose-left-neighbor))
		       (right-neighbors (neighbors right-object 'choose-right-neighbor))
		       (num-of-bond-slots
			 (+ (length left-neighbors) (length right-neighbors)))
		       (bond-counter
			 (lambda (get-bond-method)
			   (lambda (object)
			     (let ((bond (tell object get-bond-method)))
			       (and (exists? bond)
				    (eq? (tell bond 'get-bond-category) bond-category)
				    (eq? (tell bond 'get-direction) direction))))))
		       (num-of-similar-bonds
			 (+ (count (bond-counter 'get-right-bond) left-neighbors)
			    (count (bond-counter 'get-left-bond) right-neighbors))))
		  (if (zero? num-of-bond-slots)
		    100
		    (round (* 100 (/ num-of-similar-bonds num-of-bond-slots)))))))
	    (get-local-support ()
	      (let ((number (tell self 'get-num-of-local-supporting-bonds)))
		(if (zero? number)
		  0
		  (let* ((density (tell self 'get-local-density))
			 (adjusted-density (* 100 (sqrt (% density))))
			 (number-factor (min 1 (expt 0.6 (/ 1 (^3 number))))))
		    (round (* adjusted-density number-factor))))))
	    (calculate-internal-strength ()
	      (let ((compatibility-factor
		      (if (eq? (tell from-object 'object-type)
			       (tell to-object 'object-type))
			1.0
			0.7))
		    (bond-facet-factor
		      (if (eq? bond-facet plato-letter-category)
			1.0
			0.7)))
		(round (* compatibility-factor
			  bond-facet-factor
			  (bond-degree-of-assoc bond-category)))))
	    (calculate-external-strength ()
	      (tell self 'get-local-support))
	    (else (delegate msg workspace-structure))))))))


(define-codelet-procedure* bottom-up-bond-scout
  (lambda ()
    (let* ((from-object (tell *workspace* 'choose-object 'get-intra-string-salience))
	   (to-object (tell from-object 'choose-neighbor)))
      (say "Chose object " (tell from-object 'ascii-name))
      (if* (not (exists? to-object))
	(say "Chosen object has no neighbor. Fizzling.")
	(fizzle))
      (say "Chose neighbor " (tell to-object 'ascii-name))
      (let ((bond-facet (choose-bond-facet from-object to-object)))
	(if* (not (exists? bond-facet))
	  (say "No possible bond facet. Fizzling.")
	  (fizzle))
	(let* ((from-object-descriptor (tell from-object 'get-descriptor-for bond-facet))
	       (to-object-descriptor (tell to-object 'get-descriptor-for bond-facet))
	       (bond-category
		 (get-bond-category from-object-descriptor to-object-descriptor)))
	  (say "Chose descriptors " from-object-descriptor " and " to-object-descriptor)
	  (cond
	    ((not (exists? bond-category))
	     (say "No bond possible between descriptors. Fizzling.")
	     (fizzle))
	    ((incompatible-bond-candidates?
	       from-object to-object bond-facet bond-category)
	     (say "Incompatible bond-candidate objects. Fizzling.")
	     (fizzle))
	    (else (propose-bond from-object to-object bond-category bond-facet
		    from-object-descriptor to-object-descriptor))))))))


(define-codelet-procedure* top-down-bond-scout:category
  (lambda (bond-category scope)
    (if (workspace? scope)
      (say "Scope is entire Workspace.")
      (say "Focusing on " (tell scope 'generic-name) "..."))
    (let* ((string
	     (if (workspace-string? scope)
	       scope
	       (stochastic-pick
		 (if %justify-mode% *all-strings* *non-answer-strings*)
		 (list
		   (average
		     (tell *initial-string* 'get-bond-category-relevance bond-category)
		     (tell *initial-string* 'get-average-intra-string-unhappiness))
		   (average
		     (tell *modified-string* 'get-bond-category-relevance bond-category)
		     (tell *modified-string* 'get-average-intra-string-unhappiness))
		   (average
		     (tell *target-string* 'get-bond-category-relevance bond-category)
		     (tell *target-string* 'get-average-intra-string-unhappiness))
		   (if %justify-mode%
		     (average
		       (tell *answer-string* 'get-bond-category-relevance bond-category)
		       (tell *answer-string* 'get-average-intra-string-unhappiness))
		     0)))))
	   (object1 (tell string 'choose-object 'get-intra-string-salience))
	   (object2 (tell object1 'choose-neighbor)))
      (if* (not (exists? object2))
	(say "Chosen object has no neighbor. Fizzling.")
	(fizzle))
      (let ((bond-facet (choose-bond-facet object1 object2)))
	(if* (not (exists? bond-facet))
	  (say "No possible bond-facet. Fizzling.")
	  (fizzle))
	(let ((descriptor1 (tell object1 'get-descriptor-for bond-facet))
	      (descriptor2 (tell object2 'get-descriptor-for bond-facet)))
	  (cond
	    ((incompatible-bond-candidates?
	       object1 object2 bond-facet bond-category)
	     (say "Incompatible bond-candidate objects. Fizzling.")
	     (fizzle))
	    ((eq? (get-bond-category descriptor1 descriptor2) bond-category)
	     (propose-bond
	       object1 object2 bond-category bond-facet descriptor1 descriptor2))
	    ((eq? (get-bond-category descriptor2 descriptor1) bond-category)
	     (propose-bond
	       object2 object1 bond-category bond-facet descriptor2 descriptor1))
	    (else
	      (say "No possible bond. Fizzling.")
	      (fizzle))))))))


(define-codelet-procedure* top-down-bond-scout:direction
  (lambda (direction scope)
    (if (workspace? scope)
      (say "Scope is entire Workspace.")
      (say "Focusing on " (tell scope 'generic-name) "..."))
    (let* ((string
	     (if (workspace-string? scope)
	       scope
	       (stochastic-pick
		 (if %justify-mode% *all-strings* *non-answer-strings*)
		 (list
		   (average
		     (tell *initial-string* 'get-direction-relevance direction)
		     (tell *initial-string* 'get-average-intra-string-unhappiness))
		   (average
		     (tell *modified-string* 'get-direction-relevance direction)
		     (tell *modified-string* 'get-average-intra-string-unhappiness))
		   (average
		     (tell *target-string* 'get-direction-relevance direction)
		     (tell *target-string* 'get-average-intra-string-unhappiness))
		   (if %justify-mode%
		     (average
		       (tell *answer-string* 'get-direction-relevance direction)
		       (tell *answer-string* 'get-average-intra-string-unhappiness))
		     0)))))
	   (from-object (tell string 'choose-object 'get-intra-string-salience))
	   (to-object (tell from-object (if (eq? direction plato-left)
					  'choose-left-neighbor
					  'choose-right-neighbor))))
      (if* (not (exists? to-object))
	(say "Chosen object lacks appropriate left/right neighbor. Fizzling.")
	(fizzle))
      (let ((bond-facet (choose-bond-facet from-object to-object)))
	(if* (not (exists? bond-facet))
	  (say "No possible bond-facet. Fizzling.")
	  (fizzle))
	(let* ((from-descriptor (tell from-object 'get-descriptor-for bond-facet))
	       (to-descriptor (tell to-object 'get-descriptor-for bond-facet))
	       (bond-category (get-bond-category from-descriptor to-descriptor)))
	  (cond
	    ((or (not (exists? bond-category))
	         (eq? bond-category plato-sameness))
	     (say "No possible bond in this direction. Fizzling.")
	     (fizzle))
	    ((incompatible-bond-candidates?
	       from-object to-object bond-facet bond-category)
	     (say "Incompatible bond-candidate objects. Fizzling.")
	     (fizzle))
	    (else (propose-bond from-object to-object bond-category bond-facet
		    from-descriptor to-descriptor))))))))


(define propose-bond
  (lambda (from-object to-object bond-category bond-facet
	    from-object-descriptor to-object-descriptor)
    (say "Proposing bond:")
    (tell from-object-descriptor 'activate-from-workspace)
    (tell to-object-descriptor 'activate-from-workspace)
    (tell bond-facet 'activate-from-workspace)
    (let ((proposed-bond (make-bond from-object to-object bond-category bond-facet
			   from-object-descriptor to-object-descriptor))
	  (string (tell from-object 'get-string)))
      (if* %verbose% (print proposed-bond))
      (tell string 'add-proposed-bond proposed-bond)
      (tell proposed-bond 'update-proposal-level %proposed%)
      (post-codelet*
	urgency: (bond-degree-of-assoc bond-category)
	bond-evaluator proposed-bond))))


(define-codelet-procedure* bond-evaluator
  (lambda (proposed-bond)
    (tell proposed-bond 'update-strength)
    (let ((strength (tell proposed-bond 'get-strength)))
      (stochastic-if* (1- (temp-adjusted-probability (% strength)))
	(say "Bond not strong enough. Fizzling.")
	(tell (tell proposed-bond 'get-string) 'delete-proposed-bond proposed-bond)
	(fizzle))
      (tell (tell proposed-bond 'get-from-object-descriptor) 'activate-from-workspace)
      (tell (tell proposed-bond 'get-to-object-descriptor) 'activate-from-workspace)
      (tell (tell proposed-bond 'get-bond-facet) 'activate-from-workspace)
      (tell proposed-bond 'update-proposal-level %evaluated%)
      (post-codelet* urgency: strength bond-builder proposed-bond))))


(define-codelet-procedure* bond-builder
  (lambda (proposed-bond)
    (let* ((from-object (tell proposed-bond 'get-from-object))
	   (to-object (tell proposed-bond 'get-to-object))
	   (string (tell from-object 'get-string)))
      (if* (or (not (tell *workspace* 'object-exists? from-object))
	       (not (tell *workspace* 'object-exists? to-object)))
	(say "One or both of the objects no longer exist. Fizzling.")
	(fizzle))
      (tell string 'delete-proposed-bond proposed-bond)
      (if* (tell string 'bond-present? proposed-bond)
	(say "This bond already exists. Fizzling.")
	(tell (tell proposed-bond 'get-bond-category) 'activate-from-workspace)
	(if* (directed? proposed-bond)
	  (tell (tell proposed-bond 'get-direction) 'activate-from-workspace))
	(fizzle))
      (let ((incompatible-bonds (tell proposed-bond 'get-incompatible-bonds)))
	(if* (and (not (null? incompatible-bonds))
		  (not (wins-all-fights?
			 proposed-bond 1
			 incompatible-bonds 1)))
	  (say "Lost to incompatible bond. Fizzling.")
	  (fizzle))
	(let ((incompatible-groups (get-common-groups from-object to-object)))
	  (if* (and (not (null? incompatible-groups))
		    (not (wins-all-fights?
			   proposed-bond 1
			   incompatible-groups
			   (maximum (tell-all incompatible-groups 'get-letter-span)))))
	    (say "Lost to incompatible group. Fizzling.")
	    (fizzle))
	  (let ((incompatible-bridges
		  (if (and (directed? proposed-bond)
			   (or (tell proposed-bond 'leftmost-in-string?)
			       (tell proposed-bond 'rightmost-in-string?)))
		      (append
		       (tell proposed-bond 'get-incompatible-bridges 'horizontal)
		       (tell proposed-bond 'get-incompatible-bridges 'vertical))
		      '())))
	    (if* (and (not (null? incompatible-bridges))
		      (not (wins-all-fights?
			     proposed-bond 2
			     incompatible-bridges 3)))
	      (say "Lost to incompatible bridge. Fizzling.")
	      (fizzle))
	    (say "Won against all incompatible structures.")
	    (for* each group in incompatible-groups do
	      (if* (tell *workspace* 'object-exists? group)
		(break-group group)))
	    (for* each bond in incompatible-bonds do
	      (break-bond bond))
	    (for* each bridge in incompatible-bridges do
	      (break-bridge bridge))
	    (build-bond proposed-bond)))))))


(define build-bond
  (lambda (proposed-bond)
    (say "Building bond:")
    (if* %verbose% (print proposed-bond))
    (tell (tell proposed-bond 'get-string) 'add-bond proposed-bond)
    (tell (tell proposed-bond 'get-from-object) 'add-outgoing-bond proposed-bond)
    (tell (tell proposed-bond 'get-to-object) 'add-incoming-bond proposed-bond)
    (tell (tell proposed-bond 'get-left-object) 'update-right-bond proposed-bond)
    (tell (tell proposed-bond 'get-right-object) 'update-left-bond proposed-bond)
    (tell (tell proposed-bond 'get-bond-category) 'activate-from-workspace)
    (if* (directed? proposed-bond)
      (tell (tell proposed-bond 'get-direction) 'activate-from-workspace))
    (tell proposed-bond 'update-proposal-level %built%)))


(define break-bond
  (lambda (bond)
    (say "Breaking bond:")
    (if* %verbose% (print bond))
    (tell (tell bond 'get-string) 'delete-bond bond)
    (tell (tell bond 'get-from-object) 'remove-outgoing-bond bond)
    (tell (tell bond 'get-to-object) 'remove-incoming-bond bond)
    (tell (tell bond 'get-left-object) 'update-right-bond #f)
    (tell (tell bond 'get-right-object) 'update-left-bond #f)))


(define bonds-equal?
  (lambda (bond1 bond2)
    (and (eq? (tell bond1 'get-from-object) (tell bond2 'get-from-object))
	 (eq? (tell bond1 'get-to-object) (tell bond2 'get-to-object))
	 (same-bond-category? bond1 bond2)
	 (same-direction? bond1 bond2))))


(define directed?
  (lambda (bond)
    (exists? (tell bond 'get-direction))))


(define same-bond-category?
  (lambda (bond1 bond2)
    (eq? (tell bond1 'get-bond-category) (tell bond2 'get-bond-category))))


(define same-bond-direction?
  (lambda (bond1 bond2)
    (eq? (tell bond1 'get-direction) (tell bond2 'get-direction))))


(define opposite-bond-category?
  (lambda (bond1 bond2)
    (and (directed? bond1)
	 (directed? bond2)
	 (eq? (tell bond1 'get-bond-category)
	      (tell (tell bond2 'get-bond-category) 'get-related-node plato-opposite)))))


(define opposite-bond-direction?
  (lambda (bond1 bond2)
    (and (directed? bond1)
	 (directed? bond2)
	 (eq? (tell bond1 'get-direction)
	      (tell (tell bond2 'get-direction) 'get-related-node plato-opposite)))))


(define choose-bond-facet
  (lambda (object1 object2)
    (let* ((string (tell object1 'get-string))
	   (object1-bond-facets (get-bond-facets object1))
	   (object2-bond-facets (get-bond-facets object2))
	   (bond-facets (intersect object1-bond-facets object2-bond-facets))
	   (support-values
	     (map (lambda (description-type)
		    (description-type-support description-type string))
		  bond-facets)))
      (if (null? bond-facets)
	#f
	(stochastic-pick bond-facets support-values)))))


(define bond-degree-of-assoc
  (lambda (bond-category)
    (min 100 (round (* 11 (sqrt (tell bond-category 'get-degree-of-assoc)))))))


(define instance-of?
  (lambda (node category)
    (let ((super-category (tell node 'get-category)))
      (and (exists? super-category) (eq? super-category category)))))


(define get-bond-facets
  (lambda (object)
    (filter-map
      (lambda (d) (instance-of? (tell d 'get-description-type) plato-bond-facet))
      (lambda (d) (tell d 'get-description-type))
      (tell object 'get-descriptions))))


(define get-bond-category
  (lambda (from-descriptor to-descriptor)
    (if (eq? from-descriptor to-descriptor)
      plato-sameness
      (get-label from-descriptor to-descriptor))))


(define bonded?
  (lambda (object1 object2)
    (or (and (exists? (tell object1 'get-right-bond))
	     (eq? (tell (tell object1 'get-right-bond) 'get-right-object)
		  object2))
	(and (exists? (tell object1 'get-left-bond))
	     (eq? (tell (tell object1 'get-left-bond) 'get-left-object)
		  object2)))))


;; Since succ and pred groups now are allowed to have LettCtgy
;; descriptions, sameness bonds between succ and pred groups based on
;; letter-category are thus possible, whereas before such bonds could
;; only be built between letters or sameness groups.  Also, a directed
;; bond is now possible between a pred grp and a succ grp (or vice
;; versa).  For example, now it is possible for the rightward SUCC
;; group [ab> to get bonded to the leftward SUCC group <ba] to form
;; the "sameness" group A:[[ab><ba]].  The following test disallows
;; this, and similar cases.  Also, bonds based on length rather than
;; letter-category are allowed as long as they're not between two
;; directed groups going in opposite directions, such as <ab][d>.

(define incompatible-bond-candidates?
  (lambda (object1 object2 bond-facet bond-category)
    (cond
      ((eq? bond-facet plato-length)
       (and (directed-group? object1)
	    (directed-group? object2)
	    (not (same-group-direction? object1 object2))))
      ((eq? bond-category plato-sameness)
       (or (directed-group? object1) (directed-group? object2)))
      ((and (directed-group? object1) (directed-group? object2))
       (or (not (same-group-category? object1 object2))
	   (not (same-group-direction? object1 object2))))
      (else (or (directed-group? object1) (directed-group? object2))))))
