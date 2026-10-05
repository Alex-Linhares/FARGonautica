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

(define make-group
  (lambda (string group-category group-bond-facet direction
	    left-object right-object objects bonds)
    (let ((group (new-group
		   string group-category group-bond-facet direction
		   left-object right-object objects bonds
		   (tell left-object 'get-left-string-pos)
		   (tell right-object 'get-right-string-pos))))
      (tell group 'new-description plato-object-category plato-group)
      (tell group 'new-description plato-group-category group-category)
      (tell group 'new-bond-description
	plato-bond-category (tell group 'get-bond-category))
      (if* (exists? direction)
	(tell group 'new-description plato-direction-category direction))
      (cond
	((tell group 'spans-whole-string?)
	 (tell group 'new-description plato-string-position-category plato-whole))
	((tell group 'leftmost-in-string?)
	 (tell group 'new-description plato-string-position-category plato-leftmost))
	((tell group 'middle-in-string?)
	 (tell group 'new-description plato-string-position-category plato-middle))
	((tell group 'rightmost-in-string?)
	 (tell group 'new-description plato-string-position-category plato-rightmost)))
      (tell group 'new-bond-description plato-bond-facet group-bond-facet)
      ;; Attaching LettCtgy descriptions to groups even if they are successor or
      ;; predecessor groups allows horizontal bridges such as [abc] --> [bcd]:
      (if* (eq? group-bond-facet plato-letter-category)
	(let ((initial-letter-category (tell group 'get-initial-letter-category)))
	  (tell group 'new-description plato-letter-category initial-letter-category)
	  (if* (eq? group-category plato-samegrp)
	    (tell group 'set-print-name
	      (tell initial-letter-category 'get-uppercase-name)))))
      (tell group 'set-ascii-name)
      (if* %workspace-graphics%
	(tell group 'set-graphics-parameters))
      group)))


(define new-group
  (lambda (string group-category group-bond-facet direction left-object
	    right-object objects bonds left-string-pos right-string-pos)
    (let* ((workspace-object
	     (make-workspace-object string left-string-pos right-string-pos))
	   (workspace-structure (make-workspace-structure))
	   (middle-object
	     (select
	       (lambda (object)
		 (eq? (tell object 'get-descriptor-for plato-string-position-category)
		   plato-middle))
	       objects))
	   (ordered-objects
	     (if (eq? direction plato-left)
	       (reverse objects)
	       objects))
	   ;; This is mainly for efficiency, to avoid lots of searching
	   ;; through descriptions every time a new group is created:
	   (initial-letter-category
	     (tell (1st ordered-objects) 'get-descriptor-for plato-letter-category))
	   (bond-descriptions '())
	   (bond-category (tell group-category 'get-related-node plato-bond-category))
	   (group-length (length objects))
	   (platonic-length (number->platonic-number group-length))
	   (all-letter-group? (andmap letter? objects))
	   (letters (apply append (tell-all objects 'get-letters)))
	   (image (make-image
		    initial-letter-category
		    group-bond-facet
		    (if (> group-length 1)
		      (relationship-between
			(tell-all ordered-objects 'get-initial-letter-category))
		      (if (eq? bond-category plato-sameness)
			plato-identity
			bond-category))
		    (if (> group-length 1)
		      (relationship-between
			(tell-all ordered-objects 'get-platonic-length))
		      plato-identity)
		    (if (exists? direction) direction plato-right)
		    (tell-all ordered-objects 'get-image)))
	   (print-name #f)
	   (ascii-name #f)
	   (length-graphics-pexp #f)
	   (letcat-graphics-pexp #f)
	   (length-graphics-enabled? #f)
	   (shrunk-singleton? #f)
	   (shrunk-singleton-pexp #f))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'group)
	    (print-name () print-name)
	    (ascii-name () ascii-name)
	    (set-print-name (pn) (set! print-name pn) 'done)
	    (set-ascii-name ()
              (set! ascii-name
		(format "[~a]:~a"
		  (if (exists? print-name)
		    print-name
		    (cond
		      ((eq? direction plato-right) ">")
		      ((eq? direction plato-left) "<")
		      (else group-length)))
		  (if (tell self 'spans-whole-string?)
		    "*"
		    (format "~a,~a" left-string-pos right-string-pos))))
	      'done)
	    (get-graphics-pexp ()
              (if shrunk-singleton?
		shrunk-singleton-pexp
		(tell workspace-structure 'get-graphics-pexp)))
	    (set-shrunk-singleton? (new-value) (set! shrunk-singleton? new-value) 'done)
	    (length-graphics-enabled? () length-graphics-enabled?)
	    (length-graphics-active? ()
              (and length-graphics-enabled?
		   (= (tell self 'get-proposal-level) %built%)))
	    (enable-length-graphics () (set! length-graphics-enabled? #t) 'done)
	    (get-length-graphics-pexp () length-graphics-pexp)
	    (get-letcat-graphics-pexp () letcat-graphics-pexp)
	    (set-graphics-parameters ()
	      (let* ((left-letter (1st (tell left-object 'get-letters)))
		     (right-letter (last (tell right-object 'get-letters)))
		     (x1 (tell left-letter 'get-graphics-x1))
		     (y1 (tell left-letter 'get-graphics-y1))
		     (x2 (tell right-letter 'get-graphics-x2))
		     (y2 (tell right-letter 'get-graphics-y2))
		     (x-delta (tell string 'get-group-enclosure-x-delta))
		     (y-delta (tell string 'get-group-enclosure-y-delta))
		     (sizing-factor (max 1 (sub1 (tell self 'get-letter-span))))
		     (shrink-factor
		       (cond
			 ;; This special case is due to the fact that singleton groups
			 ;; have the same sizing-factor as letter-span-2 groups, which
			 ;; causes graphics problems for groups consisting of a singleton
			 ;; group and a letter (or another singleton group):
			 ((and (= (tell self 'get-letter-span) 2)
			       (or (tell left-object 'singleton-group?)
				   (tell right-object 'singleton-group?)))
			  +1/4)
			 ((eq? direction plato-right) -1/2)
			 (else 0)))
		     (new-x1 (- x1 (* sizing-factor x-delta) (* shrink-factor x-delta)))
		     (new-x2 (+ x2 (* sizing-factor x-delta) (* shrink-factor x-delta)))
		     (new-y1 (- y1 (* sizing-factor y-delta) (* shrink-factor y-delta)))
		     (new-y2 (+ y2 (* sizing-factor y-delta) (* shrink-factor y-delta)))
		     (mid-x (/ (+ new-x1 new-x2) 2))
		     (y2-lowered (- new-y2 (* 3/4 y-delta)))
		     (length-string (format "~a" (length objects)))
		     (length-x
		       (if (exists? print-name)
			 (+ mid-x
			   (* 1/2 (tell *workspace-window* 'get-character-width
				    print-name %group-letter-category-font%))
			   (* 1/2 (tell *workspace-window* 'get-character-width
				    " " %group-letter-category-font%))
			   (* 1/2 (tell *workspace-window* 'get-character-width
				    length-string %relevant-group-length-font%)))
			 (let ((offset-factor
				 (cond
				   ((eq? direction plato-right) +7/8)
				   ((eq? direction plato-left) -7/8)
				   (else 0))))
			   (+ mid-x (* offset-factor
				      (tell *workspace-window* 'get-character-width
					length-string %relevant-group-length-font%))))))
		     (length-coord `(,length-x ,y2-lowered))
		     (letcat-coord `(,mid-x ,y2-lowered))
		     (top-y (if (not (exists? print-name))
			      new-y2
			      (let ((bbox (tell *workspace-window*
					    'get-character-bounding-box print-name
					    %group-letter-category-font% letcat-coord))
				    (letter-height
				      (tell *workspace-window* 'get-character-height
					print-name %group-letter-category-font%)))
				(+ (* -1/8 letter-height) (2nd (2nd bbox)))))))
		(tell self 'set-graphics-coords `(,new-x1 ,new-y1) `(,new-x2 ,new-y2))
		(tell self 'set-bridge-graphics-coords
		  (coord mid-x (if (tell string 'top-string?) new-y1 top-y))
		  (coord mid-x top-y))
		(if* (tell self 'spans-whole-string?)
		  (tell self 'set-group-spanning-bridge-graphics-coords
		    (coord new-x2 (/ (+ new-y1 new-y2) 2))
		    (coord mid-x top-y)))
		(set! letcat-graphics-pexp
		  (if (exists? print-name)
		    `(let-sgl ((font ,%group-letter-category-font%)
			       (text-justification center)
			       (text-mode image))
		       (text ,letcat-coord ,print-name))
		    #f))
		(set! length-graphics-pexp
		  `(let-sgl ((text-justification center)
			     (text-mode image))
		     (text ,length-coord ,length-string)))
		(if* (tell self 'singleton-group?)
		  (set! shrunk-singleton-pexp
		    (if (exists? print-name)
		      `(let-sgl ()
			 ,letcat-graphics-pexp
			 ,(outline-box x1 y1 x2 y2))
		      `(let-sgl ()
			 ,(arrowhead
			    mid-x y2 (if (eq? direction plato-right) 0 180)
			    %small-group-arrowhead-length% %group-arrowhead-angle%)
			 ,(outline-box x1 y1 x2 y2)))))
		'done))
	    (get-image () image)
	    (get-initial-letter-category () initial-letter-category)
	    (get-ending-letter-category ()
	      (tell (last ordered-objects) 'get-descriptor-for plato-letter-category))
	    (get-platonic-length () platonic-length)
	    (get-group-category () group-category)
	    (get-direction () direction)
	    (get-leftmost-object () left-object)
	    (get-middle-object () middle-object)
	    (get-rightmost-object () right-object)
	    (get-constituent-objects () objects)
	    (get-constituent-bonds () bonds)
	    (get-bond-facet () group-bond-facet)
	    (get-bond-descriptions () bond-descriptions)
	    (get-bond-category () bond-category)
	    (get-letters () letters)
	    (get-group-length () group-length)
	    (get-subobject-bridges (bridge-orientation)
	      (compress (tell-all objects 'get-bridge bridge-orientation)))
	    (get-highest-level-coincident-group ()
              (let ((current-max 0)
		    (highest-level-group #f))
		(for* each g in (tell string 'get-all-other-coincident-groups
				  self left-string-pos right-string-pos direction) do
				  (let ((proposal-level (tell g 'get-proposal-level)))
				    (if* (> proposal-level current-max)
				      (set! current-max proposal-level)
				      (set! highest-level-group g))))
		highest-level-group))
	    (get-drawn-coincident-group ()
              (select
		(lambda (g) (tell g 'drawn?))
		(tell string 'get-all-other-coincident-groups
		  self left-string-pos right-string-pos direction)))
	    (get-drawn-overlapping-groups ()
              (filter
		(lambda (g)
		  (and (not (eq? g self))
		    (tell g 'drawn?)
		    (<= left-string-pos (tell g 'get-right-string-pos))
		    (>= right-string-pos (tell g 'get-left-string-pos))))
		(tell string 'get-all-groups)))
	    (all-letter-group? () all-letter-group?)
	    (singleton-group? () (= group-length 1))
	    (top-level-member? (object) (member? object objects))
	    (nested-member? (object)
	      (or (member? object objects)
		  (ormap-meth objects 'nested-member? object)))
	    (new-bond-description (description-type descriptor)
	      (let ((bond-description
		      (make-description self description-type descriptor)))
		(tell bond-description 'update-proposal-level %built%)
		(tell self 'add-bond-description bond-description)))
	    (add-bond-description (description)
	      (set! bond-descriptions (cons description bond-descriptions))
	      'done)
	    (set-new-constituent-bonds (new-bonds) (set! bonds new-bonds) 'done)
	    (get-incompatible-groups ()
	      (remq self
		(remq-duplicates
		  (compress (tell-all objects 'get-enclosing-group)))))
	    (get-incompatible-bridges (bridge-orientation)
              (if (not (exists? direction))
		'()
		(map-compress
		  (lambda (object)
		    (tell self 'get-incompatible-bridge object bridge-orientation))
		  objects)))
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
			(let ((group-direction-CM
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
			  (if (incompatible? group-direction-CM string-position-CM)
			    bridge
			    #f))))))))
	    (make-flipped-version ()
	      (if (eq? group-category plato-samegrp)
		self
		(let* ((flipped-bonds
			 (tell-all bonds 'make-flipped-version))
		       (flipped-group
			 (make-group
			   string
			   (tell group-category 'get-related-node plato-opposite)
			   group-bond-facet
			   (tell direction 'get-related-node plato-opposite)
			   left-object right-object objects flipped-bonds)))
		  ;; This is necessary to ensure that all bridges to this flipped
		  ;; group will be stored in the same place in the workspace's
		  ;; proposed-bridges-table as bridges to the unflipped version:
		  (tell flipped-group 'set-id-num (tell self 'get-id-num))
		  (if* (tell self 'description-type-present? plato-length)
		    (attach-length-description flipped-group))
		  flipped-group)))
	    (get-num-of-local-supporting-groups ()
	      (count
		(lambda (other-group)
		  (and (disjoint-objects? self other-group)
		       (eq? (tell other-group 'get-group-category) group-category)
		       (eq? (tell other-group 'get-direction) direction)))
		(remq self (tell string 'get-groups))))
	    (get-local-density ()
	      (if (tell self 'spans-whole-string?)
		  100
		  (letrec
		    ((neighbors
		       (lambda (object choose-method)
			 (let ((neighbor (tell object choose-method)))
			   (if (not (exists? neighbor))
			       '()
			       (let ((group (tell neighbor 'get-enclosing-group)))
				 (if (not (and (letter? neighbor) (exists? group)))
				     (cons neighbor (neighbors neighbor choose-method))
				     (cons group (neighbors group choose-method)))))))))
		    (let* ((other-objects
			     (append
			       (neighbors self 'choose-left-neighbor)
			       (neighbors self 'choose-right-neighbor)))
			   (num-of-objects (length other-objects))
			   (num-of-similar-groups
			     (count
			       (lambda (object)
				 (and (group? object)
				      (disjoint-objects? self object)
				      (eq? (tell object 'get-group-category)
					   group-category)
				      (eq? (tell object 'get-direction) direction)))
			       other-objects)))
		      (if (zero? num-of-objects)
			  100
			  (round (* 100 (/ num-of-similar-groups num-of-objects))))))))
	    (get-local-support ()
	      (let ((num (tell self 'get-num-of-local-supporting-groups)))
		(if (zero? num)
		    0
		    (let* ((density (tell self 'get-local-density))
			   (adjusted-density (* 100 (sqrt (% density))))
			   (num-factor (min 1 (expt 0.6 (/ 1 (^3 num))))))
		      (round (* adjusted-density num-factor))))))
	    (calculate-internal-strength ()
	      (let* ((bond-factor
		       (* (tell bond-category 'get-degree-of-assoc)
			  (if (and (exists? group-bond-facet)
				   (eq? group-bond-facet plato-letter-category))
			    1
			    1/2)))
		     (length-factor
		       (cond
			 ((= group-length 1) 5)
			 ;; Original ccat value of group-length 2 case = 20
			 ((= group-length 2) 40)
			 ((= group-length 3) 60)
			 (else 90)))
		     (bond-factor-weight (expt bond-factor 0.98))
		     (length-factor-weight (100- bond-factor-weight)))
		(round (weighted-average
			 (list bond-factor length-factor)
			 (list bond-factor-weight length-factor-weight)))))
	    (calculate-external-strength ()
	      (if (tell self 'spans-whole-string?)
		  100
		  (tell self 'get-local-support)))
	    (else (delegate msg workspace-object workspace-structure))))))))


(define-codelet-procedure* top-down-group-scout:category
  (lambda (group-category scope)
    (if (workspace? scope)
      (say "Scope is entire Workspace.")
      (say "Focusing on " (tell scope 'generic-name) "..."))
    (let* ((bond-category (tell group-category 'get-related-node plato-bond-category))
	   (string
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
	   (object (tell string 'choose-object 'get-intra-string-salience)))
      (if* (tell object 'spans-whole-string?)
	(say "Chosen object spans whole string. Fizzling.")
	(fizzle))
      (let* ((direction-to-scan
	       (cond
		 ((tell object 'leftmost-in-string?) plato-right)
		 ((tell object 'rightmost-in-string?) plato-left)
		 (else (stochastic-pick-by-method
			 (list plato-right plato-left) 'get-activation))))
	     (number-to-scan (tell string 'get-num-of-bonds-to-scan))
	     (initial-bond (if (eq? direction-to-scan plato-left)
			     (tell object 'get-left-bond)
			     (tell object 'get-right-bond))))
	(cond
	  ((and (exists? initial-bond)
	        (eq? (tell initial-bond 'get-bond-category) bond-category))
	   (let* ((bonds (scan-bonds number-to-scan direction-to-scan initial-bond))
		  (objects (cons (tell (1st bonds) 'get-left-object)
			         (tell-all bonds 'get-right-object)))
		  (direction (tell initial-bond 'get-direction)))
	     (propose-group objects bonds group-category direction)))
	  ((group? object)
	   (say "Can't make singleton group from a group. Fizzling.")
	   (fizzle))
	  (else
	    (let* ((singleton-direction
		     (if (eq? group-category plato-samegrp)
			 #f
			 (let ((left-support (descriptor-support plato-left string))
			       (right-support (descriptor-support plato-right string)))
			   (stochastic-pick
			     (list plato-left plato-right)
			     (list left-support right-support)))))
		   (objects (list object))
		   (bonds '())
		   (singleton-group
		     (make-group string group-category plato-letter-category
		       singleton-direction object object objects bonds)))
	      (stochastic-if* (1- (single-letter-group-probability singleton-group))
		(say "Local support not strong enough. Fizzling.")
		(fizzle))
	      (propose-group objects bonds group-category singleton-direction))))))))


(define-codelet-procedure* top-down-group-scout:direction
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
	   (object (tell string 'choose-object 'get-intra-string-salience)))
      (if* (tell object 'spans-whole-string?)
	(say "Chosen object spans whole string. Fizzling.")
	(fizzle))
      (let* ((direction-to-scan
	       (cond
		 ((tell object 'leftmost-in-string?) plato-right)
		 ((tell object 'rightmost-in-string?) plato-left)
		 (else (stochastic-pick-by-method
			 (list plato-right plato-left) 'get-activation))))
	     (number-to-scan (tell string 'get-num-of-bonds-to-scan))
	     (initial-bond (if (eq? direction-to-scan plato-left)
			     (tell object 'get-left-bond)
			     (tell object 'get-right-bond))))
	(if* (or (not (exists? initial-bond))
		 (not (eq? (tell initial-bond 'get-direction) direction)))
	  (say "No appropriate bond in this direction. Fizzling.")
	  (fizzle))
	(let* ((bond-facet (tell initial-bond 'get-bond-facet))
	       (bond-category (tell initial-bond 'get-bond-category))
	       (opposite-bond-category
		 (tell bond-category 'get-related-node plato-opposite))
	       (opposite-direction (tell direction 'get-related-node plato-opposite))
	       (group-category
		 (tell bond-category 'get-related-node plato-group-category))
	       (bonds (scan-bonds number-to-scan direction-to-scan initial-bond))
	       (objects (cons (tell (1st bonds) 'get-left-object)
			      (tell-all bonds 'get-right-object))))
	  (propose-group objects bonds group-category direction))))))


(define-codelet-procedure* group-scout:whole-string
  (lambda ()
    (let ((string
	    (stochastic-pick
	      (if %justify-mode% *all-strings* *non-answer-strings*)
	      (list
		(tell *initial-string* 'get-average-intra-string-unhappiness)
		(tell *modified-string* 'get-average-intra-string-unhappiness)
		(tell *target-string* 'get-average-intra-string-unhappiness)
		(if %justify-mode%
		  (tell *answer-string* 'get-average-intra-string-unhappiness)
		  0)))))
      (if* (null? (tell string 'get-bonds))
	(say "No bonds in chosen string. Fizzling.")
	(fizzle))
      (let* ((leftmost-object (tell string 'choose-leftmost-object))
	     (right-bonds (right-adjacent-bonds leftmost-object))
	     (bonded-objects (right-adjacent-objects leftmost-object)))
	(if* (or (null? right-bonds)
		 (not (tell (last bonded-objects) 'rightmost-in-string?)))
	  (say "Bonds do not span string. Fizzling.")
	  (fizzle))
	(let* ((chosen-bond (random-pick right-bonds))
	       (bond-facet (tell chosen-bond 'get-bond-facet))
	       (bond-category (tell chosen-bond 'get-bond-category))
	       (direction (tell chosen-bond 'get-direction))
	       (polarized-bonds
		 (polarize-bonds right-bonds bond-facet bond-category direction)))
	  (if* (null? polarized-bonds)
	    (say "No possible group. Fizzling.")
	    (fizzle))
	  (let ((group-category
		  (tell bond-category 'get-related-node plato-group-category)))
	    (propose-group
	      bonded-objects polarized-bonds group-category direction)))))))


(define-codelet-procedure* group-evaluator
  (lambda (proposed-group)
    (if* %workspace-graphics% (group-graphics 'flash proposed-group))
    (tell proposed-group 'update-strength)
    (let ((strength (tell proposed-group 'get-strength)))
      (say "Evaluating group:")
      (if* %verbose% (print proposed-group))
      (say "Strength of proposed group is " strength)
      (let ((evaluation-prob (group-evaluation-probability strength)))
	(say "Group evaluation probability of survival is "
	  (format "~a%" (round (100* evaluation-prob))))
	(stochastic-if* (1- evaluation-prob)
	  (say "Group not strong enough. Fizzling.")
	  (tell (tell proposed-group 'get-string) 'delete-proposed-group proposed-group)
	  (if* %workspace-graphics% (group-graphics 'erase proposed-group))
	  (fizzle))
	(tell (tell proposed-group 'get-bond-category) 'activate-from-workspace)
	(let ((direction (tell proposed-group 'get-direction)))
	  (if* (exists? direction)
	       (tell direction 'activate-from-workspace)))
	(tell proposed-group 'update-proposal-level %evaluated%)
	(if* %workspace-graphics% (group-graphics 'update-level proposed-group))
	(post-codelet* urgency: strength group-builder proposed-group)))))


;; Copycat has problems building weak groups in strings such as xwyxzy
;; or abijxy, especially in the beginning of a run.  The following
;; is a different evaluation test.  At higher temperatures, the
;; probability is strongly boosted toward 1 for all but the lowest
;; values of x.  At lower temperatures, the probability approaches
;; a linear identity function.  Domain is 0..100, range is 0.0..1.0
;; This function is equivalent to:
;;   f(x) = T/100 * tanh(x/10) + (1 - T/100) * x/100

(define group-evaluation-probability
  (lambda (x)
    (+ (* (% *temperature*) (- (/ 2 (1+ (exp (/ (- x) 5)))) 1))
       (* (1- (% *temperature*)) (% x)))))


(define-codelet-procedure* group-builder
  (lambda (proposed-group)
    (let* ((string (tell proposed-group 'get-string))
	   (group-category (tell proposed-group 'get-group-category))
	   (direction (tell proposed-group 'get-direction))
	   (equivalent-group (tell string 'get-equivalent-group proposed-group))
	   (constituent-bonds (tell proposed-group 'get-constituent-bonds))
	   (constituent-objects (tell proposed-group 'get-constituent-objects)))
      (tell string 'delete-proposed-group proposed-group)
      (if* (exists? equivalent-group)
	   (say "This group already exists. Fizzling.")
	   (for* each description in (tell equivalent-group 'get-descriptions) do
	     (tell (tell description 'get-descriptor) 'activate-from-workspace))
	   (for* each description in (tell proposed-group 'get-descriptions) do
	     (if* (not (tell equivalent-group 'description-present? description))
		  (build-description
		    (make-description
		      equivalent-group
		      (tell description 'get-description-type)
		      (tell description 'get-descriptor)))))
	   (fizzle))
      (if* (not (andmap
		  (lambda (bond)
		    (or (tell string 'bond-present? bond)
			(tell string 'flipped-bond-present? bond)))
		  constituent-bonds))
	   (say "Not all the bonds in this group still exist. Fizzling.")
	   (if* %workspace-graphics% (group-graphics 'erase proposed-group))
	   (fizzle))
      (if* %workspace-graphics% (group-graphics 'flash proposed-group))
      (let ((bonds-to-be-flipped
	      (if (eq? group-category plato-samegrp)
		  '()
		  (map-compress
		    (lambda (bond) (tell string 'get-equivalent-flipped-bond bond))
		    constituent-bonds))))
	(if* (and (not (null? bonds-to-be-flipped))
		  (not (wins-all-fights?
			 proposed-group (tell proposed-group 'get-letter-span)
			 bonds-to-be-flipped 1)))
	     (say "Lost to existing bond. Fizzling.")
	     (if* %workspace-graphics% (group-graphics 'erase proposed-group))
	     (fizzle))
	(let ((incompatible-groups (tell proposed-group 'get-incompatible-groups)))
	  (if* (and (not (null? incompatible-groups))
		    (not (andmap
			   (lambda (incompatible-group)
			     (if (and (eq? (tell incompatible-group 'get-group-category)
					   group-category)
				      (eq? (tell incompatible-group 'get-direction)
					   direction))
				 (wins-fight?
				   proposed-group
				   (tell proposed-group 'get-group-length)
				   incompatible-group
				   (tell incompatible-group 'get-group-length))
				 (wins-fight?
				   proposed-group 1
				   incompatible-group 1)))
			   incompatible-groups)))
	       (say "Lost to incompatible group. Fizzling.")
	       (if* %workspace-graphics% (group-graphics 'erase proposed-group))
	       (fizzle))
	  (let ((incompatible-bridges
		 (append
		  (tell proposed-group 'get-incompatible-bridges 'vertical)
		  (tell proposed-group 'get-incompatible-bridges 'horizontal))))
	    (if* (and (not (null? incompatible-bridges))
		      (not (wins-all-fights?
			     proposed-group 1
			     incompatible-bridges 1)))
		 (say "Lost to incompatible bridge. Fizzling.")
		 (if* %workspace-graphics% (group-graphics 'erase proposed-group))
		 (fizzle))
	    (say "Won against all incompatible structures.")
	    (for* each group in incompatible-groups do
	      (break-group group))
	    (for* each bridge in incompatible-bridges do
	      (break-bridge bridge))
	    
	    ;; If a letter-sameness group is proposed that contains
	    ;; other letter-sameness groups, then consolidate all
	    ;; letters into one group.  A slightly better variation
	    ;; would be to make this a probabilistic decision (not
	    ;; sure how to bias it, though):
	    (cond
	      ((and (eq? group-category plato-samegrp)
		    (eq? (tell proposed-group 'get-bond-facet) plato-letter-category)
		    (ormap group? constituent-objects))
	       (let ((letters (tell proposed-group 'get-letters)))
		 (for* each group in (filter group? constituent-objects) do
		   (break-group group))
		 (let ((letter-bonds
			 (adjacency-map
			   (lambda (l1 l2)
			     (if (bonded? l1 l2)
			       (tell l1 'get-right-bond)
			       (let ((new-bond
				       (make-bond l1 l2 plato-sameness
					 plato-letter-category
					 (tell l1 'get-letter-category)
					 (tell l2 'get-letter-category))))
				 (build-bond new-bond)
				 new-bond)))
			   letters)))
		   (group-graphics 'erase proposed-group)
		   (let ((new-group
			   (make-group
			     string group-category plato-letter-category direction
			     (1st letters) (last letters) letters letter-bonds)))
		     (if* (tell proposed-group 'description-type-present? plato-length)
		       (attach-length-description new-group))
		     (set! proposed-group new-group)))))

	      ((and (eq? group-category plato-samegrp)
		    (eq? (tell proposed-group 'get-bond-facet) plato-length)
		    (ormap length-group? constituent-objects))
	       (let ((new-constituent-groups
		       (flatmap
			 (lambda (group)
			   (if (length-group? group)
			     (tell group 'get-constituent-objects)
			     (list group)))
			 constituent-objects)))
		 (for* each group in (filter length-group? constituent-objects) do
		   (break-group group))
		 (if* (not (all-same? (tell-all new-constituent-groups 'get-platonic-length)))
		   (say "New constituent groups have different lengths. Fizzling.")
		   (if* %workspace-graphics% (group-graphics 'erase proposed-group))
		   (fizzle))
		 (let ((group-bonds
			 (adjacency-map
			   (lambda (g1 g2)
			     (if (bonded? g1 g2)
			       (tell g1 'get-right-bond)
			       (let ((new-bond
				       (make-bond g1 g2 plato-sameness
					 plato-length
					 (tell g1 'get-platonic-length)
					 (tell g2 'get-platonic-length))))
				 (build-bond new-bond)
				 new-bond)))
			   new-constituent-groups)))
		   (group-graphics 'erase proposed-group)
		   (let ((new-group
			   (make-group
			     string group-category plato-length direction
			     (1st new-constituent-groups) (last new-constituent-groups)
			     new-constituent-groups group-bonds)))
		     (attach-length-description new-group)
		     (set! proposed-group new-group)))))

	      (else
		(let ((new-bonds
			(map
			  (lambda (bond)
			    (if (tell string 'bond-present? bond)
			      (tell string 'get-equivalent-bond bond)
			      (let ((flipped-bond
				      (tell string 'get-equivalent-flipped-bond bond)))
				(break-bond flipped-bond)
				(build-bond bond)
				bond)))
			  constituent-bonds)))
		  (tell proposed-group 'set-new-constituent-bonds new-bonds))))

	    (build-group proposed-group #f)
	    (if* %workspace-graphics%
	      (group-graphics 'update-level proposed-group))))))))


(define length-group?
  (lambda (group)
    (eq? (tell group 'get-bond-facet) plato-length)))


;; Propose-group assumes that objects are in string-position-order from left to right:

(define propose-group
  (lambda (objects bonds group-category direction)
    (say "Proposing group:")
    (if* %verbose% (print objects))
    (let* ((left-object (1st objects))
	   (right-object (last objects))
	   (string (tell left-object 'get-string))
	   (bond-category (tell group-category 'get-related-node plato-bond-category))
	   (group-bond-facet
	     (if (null? bonds)
	       plato-letter-category
	       (tell (1st bonds) 'get-bond-facet)))
	   (proposed-group
	     (make-group
	       string group-category group-bond-facet direction
	       left-object right-object objects bonds)))
      (stochastic-if* (length-description-probability proposed-group)
	(say "Attaching length " (length objects) " description (if possible).")
	(attach-length-description proposed-group))
      (tell bond-category 'activate-from-workspace)
      (if* (exists? direction)
	(tell direction 'activate-from-workspace))
      (tell string 'add-proposed-group proposed-group)
      (tell proposed-group 'update-proposal-level %proposed%)
      (if* %workspace-graphics%
	(draw-group-grope proposed-group)
	(group-graphics 'set-pexp-and-draw proposed-group))
      (post-codelet* urgency: (bond-degree-of-assoc bond-category)
	group-evaluator proposed-group))))


(define attach-length-description
  (lambda (group)
    (if* (not (exists? (tell group 'get-descriptor-for plato-length)))
      (let ((platonic-length (tell group 'get-platonic-length)))
	;; Check for groups longer than five objects:
	(if* (exists? platonic-length)
	  (tell group 'new-description plato-length platonic-length))))))


(define right-adjacent-bonds
  (lambda (object)
    (let ((right-bond (tell object 'get-right-bond)))
      (if (not (exists? right-bond))
	'()
	(cons right-bond
	  (right-adjacent-bonds (tell right-bond 'get-right-object)))))))


(define right-adjacent-objects
  (lambda (object)
    (let ((right-bond (tell object 'get-right-bond)))
      (if (not (exists? right-bond))
	(list object)
	(cons object
	  (right-adjacent-objects (tell right-bond 'get-right-object)))))))


(define polarize-bonds
  (lambda (bonds bond-facet bond-category direction)
    (continuation-point* return
      (map (lambda (bond)
	     (let ((this-bond-facet (tell bond 'get-bond-facet))
		   (this-bond-category (tell bond 'get-bond-category))
		   (this-bond-direction (tell bond 'get-direction)))
	       (cond
		 ((not (eq? this-bond-facet bond-facet)) (return '()))
		 ((and (eq? this-bond-category bond-category)
		       (eq? this-bond-direction direction))
		  bond)
		 ((and (eq? (tell this-bond-category 'get-related-node plato-opposite)
			    bond-category)
		       (eq? (tell this-bond-direction 'get-related-node plato-opposite)
			    direction))
		  (tell bond 'make-flipped-version))
		 (else (return '())))))
	   bonds))))


(define scan-bonds
  (lambda (max-num-to-scan direction-to-scan initial-bond)
    (let* ((bond-facet (tell initial-bond 'get-bond-facet))
	   (bond-category (tell initial-bond 'get-bond-category))
	   (opposite-bond-category
	     (tell bond-category 'get-related-node plato-opposite))
	   (direction (tell initial-bond 'get-direction))
	   (opposite-direction (if (exists? direction)
				 (tell direction 'get-related-node plato-opposite)
				 #f)))
      (letrec
	((scan (lambda (n bond)
		 (if (or (zero? n) (not (exists? bond)))
		   '()
		   (cond
		     ((and (eq? (tell bond 'get-bond-facet) bond-facet)
			   (eq? (tell bond 'get-bond-category) bond-category)
			   (eq? (tell bond 'get-direction) direction))
		      (cons bond
			(scan (sub1 n) (get-next-bond bond direction-to-scan))))
		     ((and (eq? (tell bond 'get-bond-facet) bond-facet)
			   (eq? (tell bond 'get-bond-category) opposite-bond-category)
			   (eq? (tell bond 'get-direction) opposite-direction))
		      (cons (tell bond 'make-flipped-version)
			(scan (sub1 n) (get-next-bond bond direction-to-scan))))
		     (else '()))))))
	(if (eq? direction-to-scan plato-right)
	  (scan max-num-to-scan initial-bond)
	  (reverse (scan max-num-to-scan initial-bond)))))))


(define get-next-bond
  (lambda (bond direction-to-scan)
    (cond
      ((eq? direction-to-scan plato-left)
       (tell (tell bond 'get-left-object) 'get-left-bond))
      ((eq? direction-to-scan plato-right)
       (tell (tell bond 'get-right-object) 'get-right-bond)))))


(define build-group
  (lambda (proposed-group flipped?)
    (vprintf "Building ~agroup:~n" (if flipped? "flipped " ""))
    (vprint proposed-group)
    (let ((string (tell proposed-group 'get-string)))
      (tell string 'add-group proposed-group)
      (for* each object in (tell proposed-group 'get-constituent-objects) do
	(tell object 'update-enclosing-group proposed-group))
      (for* each bond in (tell proposed-group 'get-constituent-bonds) do
	(tell bond 'update-enclosing-group proposed-group))
      (for* each description in (tell proposed-group 'get-descriptions) do
	(tell (tell description 'get-descriptor) 'activate-from-workspace))
      (tell proposed-group 'update-proposal-level %built%)
      ;; Building a non-spanning group may invalidate StrPosCtgy:middle descriptions
      ;; for some objects in the string.  Need to remove such descriptions from any
      ;; objects that are no longer describable as "middle":
      (if* (not (tell proposed-group 'spans-whole-string?))
	(tell string 'delete-invalid-string-position-middle-descriptions))
      (if* (and %workspace-graphics%
	        (= (tell proposed-group 'get-letter-span) 2))
	(let ((left-object (tell proposed-group 'get-leftmost-object))
	      (right-object (tell proposed-group 'get-rightmost-object)))
	  (tell *workspace-window* 'caching-on)
	  (if* (tell left-object 'singleton-group?)
	    (tell *workspace-window* 'erase-group left-object)
	    (tell left-object 'set-shrunk-singleton? #t)
	    (tell *workspace-window* 'draw-group left-object))
	  (if* (tell right-object 'singleton-group?)
	    (tell *workspace-window* 'erase-group right-object)
	    (tell right-object 'set-shrunk-singleton? #t)
	    (tell *workspace-window* 'draw-group right-object))
	  (tell *workspace-window* 'flush)))
      (monitor-new-groups proposed-group flipped?))))


(define break-group
  (lambda (group)
    (say "Breaking group:")
    (if* %verbose% (print group))
    (let ((string (tell group 'get-string))
	  (vertical-bridge (tell group 'get-bridge 'vertical))
	  (horizontal-bridge (tell group 'get-bridge 'horizontal))
	  (enclosing-group (tell group 'get-enclosing-group)))
      (if* (exists? enclosing-group)
	(break-group enclosing-group))
      (tell string 'delete-group group)
      (tell string 'delete-proposed-bonds group)
      (for* each bond in (tell group 'get-incident-bonds) do
	(break-bond bond))
      (if* %workspace-graphics%
	(tell *workspace-window* 'caching-on)
	(for* each v in (tell *workspace* 'get-proposed-vertical-bridges group)
	  do (if* (tell v 'drawn?)
	       ;; Can't use (bridge-graphics 'erase ...) here because
	       ;; we don't want any pending bridges to be re-drawn.
	       ;; Also, probably should repair damage to bridge's other object.
	       (tell *workspace-window* 'erase-bridge v)))
	(for* each h in (tell *workspace* 'get-proposed-horizontal-bridges group)
	  do (if* (tell h 'drawn?)
	       (tell *workspace-window* 'erase-bridge h)))
	(tell *workspace-window* 'flush))
      (tell *workspace* 'delete-proposed-vertical-bridges group)
      (tell *workspace* 'delete-proposed-horizontal-bridges group)
      (if* (exists? vertical-bridge)
	(break-bridge vertical-bridge))
      (if* (exists? horizontal-bridge)
	(break-bridge horizontal-bridge))
      (for* each object in (tell group 'get-constituent-objects) do
	(tell object 'update-enclosing-group #f))
      (for* each bond in (tell group 'get-constituent-bonds) do
	(tell bond 'update-enclosing-group #f))
      ;; Breaking a non-spanning group may invalidate StrPosCtgy:middle descriptions
      ;; for some objects in the string.  Need to remove such descriptions from any
      ;; objects that are no longer describable as "middle":
      (if* (not (tell group 'spans-whole-string?))
	(tell string 'delete-invalid-string-position-middle-descriptions))
      (if* %workspace-graphics%
	(group-graphics 'erase group)
	(if* (= (tell group 'get-letter-span) 2)
	  (let ((left-object (tell group 'get-leftmost-object))
		(right-object (tell group 'get-rightmost-object)))
	    (tell *workspace-window* 'caching-on)
	    (if* (tell left-object 'singleton-group?)
	      (tell *workspace-window* 'erase-group left-object)
	      (tell left-object 'set-shrunk-singleton? #f)
	      (tell *workspace-window* 'draw-group left-object)
	      ;; Repair any damage done to workspace letter:
	      (tell *workspace-window* 'draw
		(tell (tell left-object 'get-leftmost-object)
		  'get-graphics-pexp)))
	    (if* (tell right-object 'singleton-group?)
	      (tell *workspace-window* 'erase-group right-object)
	      (tell right-object 'set-shrunk-singleton? #f)
	      (tell *workspace-window* 'draw-group right-object)
	      ;; Repair any damage done to workspace letter:
	      (tell *workspace-window* 'draw
		(tell (tell right-object 'get-leftmost-object)
		  'get-graphics-pexp)))
	    (tell *workspace-window* 'flush))))
      'done)))


(define contains?
  (lambda (object1 object2)
    (and (group? object1) (tell object1 'nested-member? object2))))


(define get-common-groups
  (lambda (object1 object2)
    (filter
      (lambda (group)
	(and (tell group 'nested-member? object1)
	     (tell group 'nested-member? object2)))
      (tell (tell object1 'get-string) 'get-groups))))


(define same-group-category?
  (lambda (group1 group2)
    (eq? (tell group1 'get-group-category) (tell group2 'get-group-category))))


(define same-group-direction?
  (lambda (group1 group2)
    (eq? (tell group1 'get-direction) (tell group2 'get-direction))))


(define same-letter-category?
  (lambda (object1 object2)
    (eq? (tell object1 'get-letter-category) (tell object2 'get-letter-category))))


(define directed-group?
  (lambda (object)
    (and (group? object)
	 (let ((group-category (tell object 'get-group-category)))
	   (or (eq? group-category plato-succgrp)
	       (eq? group-category plato-predgrp))))))


(define get-all-nested-groups
  (lambda (object)
    (if (letter? object)
      '()
      (cons object
	(apply append
	  (map get-all-nested-groups
	    (tell object 'get-constituent-objects)))))))
