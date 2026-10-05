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


(define make-workspace-string
  (lambda (string-type symbol)
    (let* ((letter-categories (symbol->letter-categories symbol))
	   (string (new-workspace-string string-type letter-categories)))
      (for* position from 0 to (sub1 (length letter-categories)) do
	(tell string 'add-letter
	  (make-letter string (list-ref letter-categories position) position)
	  position))
      (tell string 'set-letter-list)
      (tell string 'set-string-image (make-string-image string plato-right))
      string)))


(define new-workspace-string
  (lambda (string-type letter-categories)
    (let* ((number-of-letters (length letter-categories))
	   (max-object-capacity (* 2 number-of-letters))
	   (next-id-num 0)
	   (letter-vector (make-vector number-of-letters))
	   (left-edge-group-vector (make-vector number-of-letters '()))
	   (right-edge-group-vector (make-vector number-of-letters '()))
	   (group-vector (make-vector max-object-capacity #f))
	   (proposed-group-table
	     (make-table max-object-capacity max-object-capacity '()))
	   (from-to-bond-table
	     (make-table max-object-capacity max-object-capacity #f))
	   (left-right-bond-table
	     (make-table max-object-capacity max-object-capacity #f))
	   (proposed-bond-table
	     (make-table max-object-capacity max-object-capacity '()))
	   (letter-list #f)
	   (group-list '())
	   (proposed-group-list '())
	   (bond-list '())
	   (average-intra-string-unhappiness #f)
	   (bond-scan-distribution
	     (make-probability-distribution
	       (ascending-index-list number-of-letters)
	       (map ^2 (ascending-index-list number-of-letters))))
	   (translated? #f)
	   (string-image #f)
	   (print-name #f)
	   (group-enclosure-x-delta #f)
	   (group-enclosure-y-delta #f)
	   (graphics-width #f)
	   (spanning-group-x1 #f)
	   (spanning-group-y1 #f)
	   (spanning-group-x2 #f)
	   (spanning-group-y2 #f))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'workspace-string)
	    (print-name () print-name)
	    (ascii-name () (format "\"~a\"" print-name))
	    (generic-name ()
	      (format "~a~a string" (if translated? "translated " "") string-type))
	    (symbol-name () (string->symbol print-name))
	    (print ()
	      (printf "~aWorkspace string \"~a\"~%"
		(if translated? "Translated " "")
		print-name))
	    (get-group-enclosure-x-delta () group-enclosure-x-delta)
	    (get-group-enclosure-y-delta () group-enclosure-y-delta)
	    (get-graphics-width () graphics-width)
	    (get-spanning-group-x1 () spanning-group-x1)
	    (get-spanning-group-y1 () spanning-group-y1)
	    (get-spanning-group-x2 () spanning-group-x2)
	    (get-spanning-group-y2 () spanning-group-y2)
	    (set-graphics-info (x-delta y-delta width span-x1 span-y1 span-x2 span-y2)
	      (set! group-enclosure-x-delta x-delta)
	      (set! group-enclosure-y-delta y-delta)
	      (set! graphics-width width)
	      (set! spanning-group-x1 span-x1)
	      (set! spanning-group-y1 span-y1)
	      (set! spanning-group-x2 span-x2)
	      (set! spanning-group-y2 span-y2)
	      'done)
	    (get-max-object-capacity () max-object-capacity)
	    (get-random-letter () (random-pick letter-list))
	    (get-letter (position) (vector-ref letter-vector position))
	    (get-letter-categories () letter-categories)
	    (get-left-edged-groups (position)
	      (vector-ref left-edge-group-vector position))
	    (get-right-edged-groups (position)
	      (vector-ref right-edge-group-vector position))
	    (get-length () number-of-letters)
	    (get-letters () letter-list)
	    (get-groups () group-list)
	    (get-all-groups () (append group-list proposed-group-list))
	    (get-bonds () bond-list)
	    (get-all-bonds ()
	      (append bond-list (apply append (table->list proposed-bond-table))))
	    (get-objects () (append letter-list group-list))
	    (get-all-objects () (append letter-list group-list proposed-group-list))
	    (get-average-intra-string-unhappiness () average-intra-string-unhappiness)
	    (get-num-of-bonds-to-scan () (tell bond-scan-distribution 'choose-value))
	    (get-string-type () string-type)
	    (string-type? (type) (eq? string-type type))
	    (top-string? () (member? string-type '(initial modified)))
	    (vertical-string? () (member? string-type '(initial target)))
	    (bottom-string? () (member? string-type '(target answer)))
	    (translated? () translated?)
	    (mark-as-translated ()
	      (set! translated? #t)
	      'done)
	    (bond-present? (bond)
	      (exists? (tell self 'get-equivalent-bond bond)))
	    (get-equivalent-bond (bond)
	      (let* ((i (tell (tell bond 'get-from-object) 'get-id-num))
		     (j (tell (tell bond 'get-to-object) 'get-id-num))
		     (equivalent-bond (table-ref from-to-bond-table i j)))
		(if (and (exists? equivalent-bond)
		      (same-bond-category? bond equivalent-bond)
		      (same-bond-direction? bond equivalent-bond))
		  equivalent-bond
		  #f)))
	    (flipped-bond-present? (bond)
	      (exists? (tell self 'get-equivalent-flipped-bond bond)))
	    (get-equivalent-flipped-bond (bond)
	      (let* ((i (tell (tell bond 'get-from-object) 'get-id-num))
		     (j (tell (tell bond 'get-to-object) 'get-id-num))
		     (equivalent-flipped-bond (table-ref from-to-bond-table j i)))
		(if (and (exists? equivalent-flipped-bond)
		      (opposite-bond-category? bond equivalent-flipped-bond)
		      (opposite-bond-direction? bond equivalent-flipped-bond))
		  equivalent-flipped-bond
		  #f)))
	    (assign-id-num (object)
	      (tell object 'set-id-num next-id-num)
	      (set! next-id-num (add1 next-id-num))
	      (if* (= next-id-num max-object-capacity)
		(tell self 'expand-internal-storage))
	      'done)
	    (add-letter (letter position)
	      (tell self 'assign-id-num letter)
	      (vector-set! letter-vector position letter)
	      'done)
	    (set-letter-list ()
	      (set! letter-list (vector->list letter-vector))
	      (set! print-name (apply string-append (tell-all letter-list 'print-name)))
	      'done)
	    (add-bond (bond)
	      (let ((i1 (tell (tell bond 'get-from-object) 'get-id-num))
		    (j1 (tell (tell bond 'get-to-object) 'get-id-num))
		    (i2 (tell (tell bond 'get-left-object) 'get-id-num))
		    (j2 (tell (tell bond 'get-right-object) 'get-id-num)))
		(table-set! from-to-bond-table i1 j1 bond)
		(table-set! left-right-bond-table i2 j2 bond)
		(if* (eq? (tell bond 'get-bond-category) plato-sameness)
		  (table-set! from-to-bond-table j1 i1 bond)
		  (table-set! left-right-bond-table j2 i2 bond))
		(set! bond-list (cons bond bond-list))
		'done))
	    (delete-bond (bond)
	      (let ((i1 (tell (tell bond 'get-from-object) 'get-id-num))
		    (j1 (tell (tell bond 'get-to-object) 'get-id-num))
		    (i2 (tell (tell bond 'get-left-object) 'get-id-num))
		    (j2 (tell (tell bond 'get-right-object) 'get-id-num)))
		(table-set! from-to-bond-table i1 j1 #f)
		(table-set! left-right-bond-table i2 j2 #f)
		(if* (eq? (tell bond 'get-bond-category) plato-sameness)
		  (table-set! from-to-bond-table j1 i1 #f)
		  (table-set! left-right-bond-table j2 i2 #f))
		(set! bond-list (remq bond bond-list))
		'done))
	    (add-proposed-bond (bond)
	      (let* ((i (tell (tell bond 'get-from-object) 'get-id-num))
		     (j (tell (tell bond 'get-to-object) 'get-id-num))
		     (proposed-bonds (table-ref proposed-bond-table i j)))
		(table-set! proposed-bond-table i j (cons bond proposed-bonds)))
	      'done)
	    (delete-proposed-bond (bond)
	      (let* ((i (tell (tell bond 'get-from-object) 'get-id-num))
		     (j (tell (tell bond 'get-to-object) 'get-id-num))
		     (proposed-bonds (table-ref proposed-bond-table i j)))
		(table-set! proposed-bond-table i j (remq bond proposed-bonds)))
	      'done)
	    (delete-proposed-bonds (object)
	      (let ((i (tell object 'get-id-num)))
		(initialize-row! proposed-bond-table i '())
		(initialize-column! proposed-bond-table i '()))
	      'done)
	    (delete-all-proposed-bonds ()
	      (for-each-vector-element* (proposed-bond-table i) do
		(initialize-row! proposed-bond-table i '()))
	      'done)
	    (group-present? (group)
	      (exists? (tell self 'get-equivalent-group group)))
	    (get-equivalent-object (object)
	      (if (letter? object)
		(tell self 'get-equivalent-letter object)
		(tell self 'get-equivalent-group object)))
	    ;; get-equivalent-letter and get-equivalent-group assume that self
	    ;; and (tell letter/group 'get-string) are strings consisting of
	    ;; exactly the same letter-categories.  These strings may or may
	    ;; not be eq? (i.e., one could be a translated-string):
	    (get-equivalent-letter (letter)
	      (if (member? letter letter-list)
		letter
		(let ((equivalent-letter
			(vector-ref letter-vector (tell letter 'get-string-pos))))
		  (if (eq? (tell letter 'get-letter-category)
			   (tell equivalent-letter 'get-letter-category))
		    equivalent-letter
		    #f))))
	    (get-equivalent-group (group)
	      (if (member? group group-list)
		group
		(let ((i (tell (tell group 'get-leftmost-object) 'get-id-num)))
		  (if (>= i max-object-capacity)
		    #f
		    (let ((equivalent-group (vector-ref group-vector i)))
		      (if (and (exists? equivalent-group)
			       (same-group-category? group equivalent-group)
			       (same-group-direction? group equivalent-group)
			       (= (tell group 'get-group-length)
				  (tell equivalent-group 'get-group-length)))
			equivalent-group
			#f))))))
	    (get-all-other-coincident-groups (group left-pos right-pos direction)
              (filter
		(lambda (g)
		  (and (not (eq? g group))
		       (= left-pos (tell g 'get-left-string-pos))
		       (= right-pos (tell g 'get-right-string-pos))
		       (or (eq? direction (tell g 'get-direction))
			   ;; This is for the extremely unlikely but theoretically
			   ;; possible case of a sameness group <=> left-directed
			   ;; group coincidence:
			   (and (not (eq? direction plato-right))
			        (not (eq? (tell g 'get-direction) plato-right))))))
		(append group-list proposed-group-list)))
	    (add-group (group)
	      (tell self 'assign-id-num group)
	      (let* ((left-pos (tell group 'get-left-string-pos))
		     (right-pos (tell group 'get-right-string-pos))
		     (left-edge-groups (vector-ref left-edge-group-vector left-pos))
		     (right-edge-groups (vector-ref right-edge-group-vector right-pos))
		     (i (tell (tell group 'get-leftmost-object) 'get-id-num)))
		(vector-set! group-vector i group)
		(vector-set! left-edge-group-vector
		  left-pos (cons group left-edge-groups))
		(vector-set! right-edge-group-vector
		  right-pos (cons group right-edge-groups))
		(set! group-list (cons group group-list))
		'done))
	    (delete-group (group)
	      (let* ((left-pos (tell group 'get-left-string-pos))
		     (right-pos (tell group 'get-right-string-pos))
		     (left-edge-groups (vector-ref left-edge-group-vector left-pos))
		     (right-edge-groups (vector-ref right-edge-group-vector right-pos))
		     (i (tell (tell group 'get-leftmost-object) 'get-id-num)))
		(vector-set! group-vector i #f)
		(vector-set! left-edge-group-vector
		  left-pos (remq group left-edge-groups))
		(vector-set! right-edge-group-vector
		  right-pos (remq group right-edge-groups))
		(set! group-list (remq group group-list))
		'done))
	    (add-proposed-group (group)
	      (let* ((i (tell (tell group 'get-leftmost-object) 'get-id-num))
		     (j (tell (tell group 'get-rightmost-object) 'get-id-num))
		     (proposed-groups (table-ref proposed-group-table i j)))
		(table-set! proposed-group-table i j (cons group proposed-groups))
		(set! proposed-group-list (cons group proposed-group-list)))
	      'done)
	    (delete-proposed-group (group)
	      (let* ((i (tell (tell group 'get-leftmost-object) 'get-id-num))
		     (j (tell (tell group 'get-rightmost-object) 'get-id-num))
		     (proposed-groups (table-ref proposed-group-table i j)))
		(table-set! proposed-group-table i j (remq group proposed-groups))
		(set! proposed-group-list (remq group proposed-group-list)))
	      'done)
	    (delete-all-proposed-groups ()
	      (for-each-vector-element* (proposed-group-table i) do
		(initialize-row! proposed-group-table i '()))
	      (set! proposed-group-list '())
	      'done)
	    (delete-invalid-string-position-middle-descriptions ()
	      (for* each object in (tell self 'get-all-objects) do
		(if* (and (tell object 'descriptor-present? plato-middle)
		          (not (tell object 'middle-in-string?)))
		  (tell object 'delete-description-type plato-string-position-category)
		  (let ((vertical-bridge (tell object 'get-bridge 'vertical))
			(horizontal-bridge (tell object 'get-bridge 'horizontal)))
		    (if* (and (exists? vertical-bridge)
			      (tell vertical-bridge 'CM-type-present?
				plato-string-position-category))
		      (tell vertical-bridge 'delete-concept-mapping-type
			plato-string-position-category)
		      (if* (null? (tell vertical-bridge 'get-all-concept-mappings))
			(break-bridge vertical-bridge)))
		    (if* (and (exists? horizontal-bridge)
			      (tell horizontal-bridge 'CM-type-present?
				plato-string-position-category))
		      (tell horizontal-bridge 'delete-concept-mapping-type
			plato-string-position-category)
		      (if* (null? (tell horizontal-bridge 'get-all-concept-mappings))
			(break-bridge horizontal-bridge))))))
	      'done)
	    (update-all-relative-importances ()
	      (let* ((objects (tell self 'get-objects))
		     (total-raw-importance
		       (sum (tell-all objects 'get-raw-importance))))
		(if (= total-raw-importance 0)
		  (let ((importance (100* (/ 1 (length objects)))))
		    (for* each object in objects do
		      (tell object 'update-relative-importance importance)))
		  (for* each object in objects do
		    (let ((raw-importance (tell object 'get-raw-importance)))
		      (tell object 'update-relative-importance
			(100* (/ raw-importance total-raw-importance))))))
		'done))
	    (update-average-intra-string-unhappiness ()
	      (set! average-intra-string-unhappiness
		(round (average (tell-all (tell self 'get-objects)
				  'get-intra-string-unhappiness))))
	      'done)
	    (choose-object message
	      (let* ((objects (tell self 'get-objects))
		     (weights (apply tell-all (cons objects message))))
		(stochastic-pick objects (temp-adjusted-values weights))))
	    (choose-object-with-description-type (description-type . message)
	      (let* ((objects (filter-meth (tell self 'get-objects)
				'description-type-present? description-type))
		     (weights (apply tell-all (cons objects message))))
		(if (null? objects)
		  #f
		  (stochastic-pick objects (temp-adjusted-values weights)))))
	    (choose-leftmost-object ()
	      (let ((leftmost-objects
		      (filter (lambda (object)
				(eq? (tell object 'get-descriptor-for
				       plato-string-position-category)
				  plato-leftmost))
			(tell self 'get-objects))))
		(stochastic-pick-by-method
		  leftmost-objects 'get-relative-importance)))
	    (get-relevance (get-category-method-name category)
	      (let ((non-spanning-objects
		      (filter-out
			(lambda (object) (tell object 'spans-whole-string?))
			(tell self 'get-objects))))
		(if (null? non-spanning-objects)
		  0
		  (let ((num-of-bonded-objects
			  (count
			    (lambda (object)
			      (let ((right-bond (tell object 'get-right-bond)))
				(and (exists? right-bond)
				     (eq? (tell right-bond get-category-method-name)
				       category))))
			    non-spanning-objects)))
		    (100* (/ num-of-bonded-objects
			     (sub1 (length non-spanning-objects))))))))
	    (get-bond-category-relevance (bond-category)
	      (tell self 'get-relevance 'get-bond-category bond-category))
	    (get-direction-relevance (direction)
	      (tell self 'get-relevance 'get-direction direction))
	    (get-image () string-image)
	    (set-string-image (image) (set! string-image image) 'done)
	    (generate-image-letters () (flatten (tell string-image 'generate)))
	    (reset-string-image () (tell string-image 'reset))
	    ;;-----------------------------------------------------------------------
	    ;; The following methods allow workspace-strings to behave as spanning
	    ;; groups, so that a workspace-string can be used as the reference-object
	    ;; of an intrinsic change-description or rule-clause-template:
	    (get-bridge (bridge-type) #f)
	    (get-constituent-objects ()
	      (sort-by-method 'get-left-string-pos <
		(tell self 'get-top-level-objects)))
	    (get-top-level-objects ()
	      (filter-out
		(lambda (obj) (exists? (tell obj 'get-enclosing-group)))
		(append letter-list group-list)))
	    (nested-member? (object)
	      (let ((top-level-objects (tell self 'get-top-level-objects)))
		(or (member? object top-level-objects)
		    (ormap-meth top-level-objects 'nested-member? object))))
	    (singleton-group? ()
	      (= (length (tell self 'get-top-level-objects)) 1))
	    (get-subobject-bridges (bridge-orientation)
	      (compress
		(tell-all (tell self 'get-top-level-objects)
		  'get-bridge bridge-orientation)))
	    (get-left-string-pos () 0)
	    (get-right-string-pos () (- number-of-letters 1))
	    (get-nesting-level () 0)
	    (get-bond-facet () plato-letter-category)
	    ;;-----------------------------------------------------------------------
	    (spanning-group-exists? ()
	      (ormap-meth group-list 'spans-whole-string?))
	    (get-spanning-group ()
	      (select-meth group-list 'spans-whole-string?))
	    (get-all-reference-objects (rule)
	      (apply append
		(map (lambda (rc) (tell self 'get-reference-objects rc))
		  (tell rule 'get-rule-clauses))))
	    (get-reference-objects (rule-clause)
	      (if (verbatim-clause? rule-clause)
		'()
		(apply append
		  (map (lambda (od)
			 (tell self 'get-object-description-ref-objects od))
		    (2nd rule-clause)))))
	    (whole-group? ()
	      (exists? (select-meth group-list 'descriptor-present? plato-whole)))
	    (get-object-description-ref-objects (object-description)
	      (let ((object-type (1st object-description))
		    (description-type (2nd object-description))
		    (descriptor (3rd object-description)))
		(if (eq? object-type 'string)
		  ;; In abc->aaa, if group [abc] doesn't exist when a rule is created, rule
		  ;; will will be CHANGE (string <StrPos> <whole>) (subobjs <LetCat> <a>)
		  ;; but then if [abc] is subsequently created, the rule will no
		  ;; longer work, since string "abc" is retrieved, and subobjects
		  ;; are ([abc]).  Hence, for a "string" rule, need to retrieve the
		  ;; whole group if one exists (else just retrieve the string):
		  (let ((whole-group
			  (select-meth group-list 'descriptor-present? plato-whole)))
		    (if (exists? whole-group)
		      (list whole-group)
		      (list self)))
		  (let ((all-candidates
			  (filter-meth
			    (if (eq? object-type plato-letter) letter-list group-list)
			    'descriptor-present? descriptor)))
		    (if (and (eq? description-type plato-string-position-category)
			     (> (length all-candidates) 1))
		      (let ((objects-with-vertical-bridges
			      (filter
				(lambda (obj)
				  (exists? (tell obj 'get-bridge 'vertical)))
				all-candidates)))
			(if (null? objects-with-vertical-bridges)
			  (list (lowest-level-object all-candidates))
			  (list (lowest-level-object objects-with-vertical-bridges))))
		      all-candidates)))))
	    (expand-internal-storage ()
	      (set! max-object-capacity (* 2 max-object-capacity))
	      (let ((new-group-vector
		      (make-vector max-object-capacity #f))
		    (new-proposed-group-table
		      (make-table max-object-capacity max-object-capacity '()))
		    (new-from-to-bond-table
		      (make-table max-object-capacity max-object-capacity #f))
		    (new-left-right-bond-table
		      (make-table max-object-capacity max-object-capacity #f))
		    (new-proposed-bond-table
		      (make-table max-object-capacity max-object-capacity '())))
		(copy-vector-contents! group-vector new-group-vector)
		(copy-table-contents! proposed-group-table new-proposed-group-table)
		(copy-table-contents! from-to-bond-table new-from-to-bond-table)
		(copy-table-contents! left-right-bond-table new-left-right-bond-table)
		(copy-table-contents! proposed-bond-table new-proposed-bond-table)
		(set! group-vector new-group-vector)
		(set! proposed-group-table new-proposed-group-table)
		(set! from-to-bond-table new-from-to-bond-table)
		(set! left-right-bond-table new-left-right-bond-table)
		(set! proposed-bond-table new-proposed-bond-table)
		(case string-type
		  (initial
		    (tell *workspace* 'reallocate-top-bridges-storage)
		    (tell *workspace* 'reallocate-vertical-bridges-storage))
		  (modified
		    (tell *workspace* 'reallocate-top-bridges-storage))
		  (target
		    (tell *workspace* 'reallocate-vertical-bridges-storage)
		    (if* %justify-mode%
		      (tell *workspace* 'reallocate-bottom-bridges-storage)))
		  (answer
		    (if* (not translated?)
		      (tell *workspace* 'reallocate-bottom-bridges-storage)))))
	      'done)
	    (else (delegate msg base-object))))))))
