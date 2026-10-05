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

(define *initial-string* #f)
(define *modified-string* #f)
(define *target-string* #f)
(define *answer-string* #f)
(define *top-strings* #f)
(define *bottom-strings* #f)
(define *vertical-strings* #f)
(define *non-answer-strings* #f)
(define *all-strings* #f)

(define %expiration-period% 500)
(define %num-youngest-structures% 3)

;; Workspace structure proposal levels:
(define %proposed% 1)
(define %evaluated% 2)
(define %built% 3)

(define make-workspace
  (lambda ()
    (let ((initial-string #f)
	  (modified-string #f)
	  (target-string #f)
	  (answer-string #f)
	  (top-strings #f)
	  (bottom-strings #f)
	  (vertical-strings #f)
	  (top-bridges #f)
	  (top-bridge-list '())
	  (proposed-top-bridge-table #f)
	  (bottom-bridges #f)
	  (bottom-bridge-list '())
	  (proposed-bottom-bridge-table #f)
	  (vertical-bridges #f)
	  (vertical-bridge-list '())
	  (proposed-vertical-bridge-table #f)
	  (average-intra-string-unhappiness 0)
	  (average-top-inter-string-unhappiness 0)
	  (average-bottom-inter-string-unhappiness 0)
	  (average-vertical-inter-string-unhappiness 0)
	  (average-unhappiness 0)
	  (top-mapping-strength 0)
	  (bottom-mapping-strength 0)
	  (vertical-mapping-strength 0)
	  (clamped-rule-list '())
	  (top-rule-list '())
	  (bottom-rule-list '())
	  (top-rule-possible? #f)
	  (bottom-rule-possible? #f))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'workspace)
	    (initialize (initial modified target answer)
	      (set! initial-string initial)
	      (set! modified-string modified)
	      (set! target-string target)
	      (set! answer-string answer)
	      (set! top-strings (list initial modified))
	      (set! bottom-strings (list target answer))
	      (set! vertical-strings (list initial target))
	      (set! top-bridges
		(make-vector (tell initial-string 'get-max-object-capacity) #f))
	      (set! top-bridge-list '())
	      (set! proposed-top-bridge-table
		(make-table
		  (tell initial-string 'get-max-object-capacity)
		  (tell modified-string 'get-max-object-capacity)
		  '()))
	      (set! bottom-bridges
		(if %justify-mode%
		  (make-vector (tell target-string 'get-max-object-capacity) #f)
		  #f))
	      (set! bottom-bridge-list '())
	      (set! proposed-bottom-bridge-table
		(if %justify-mode%
		  (make-table
		    (tell target-string 'get-max-object-capacity)
		    (tell answer-string 'get-max-object-capacity)
		    '())
		  #f))
	      (set! vertical-bridges
		(make-vector (tell initial-string 'get-max-object-capacity) #f))
	      (set! vertical-bridge-list '())
	      (set! proposed-vertical-bridge-table
		(make-table
		  (tell initial-string 'get-max-object-capacity)
		  (tell target-string 'get-max-object-capacity)
		  '()))
	      (set! average-intra-string-unhappiness 0)
	      (set! average-top-inter-string-unhappiness 0)
	      (set! average-bottom-inter-string-unhappiness 0)
	      (set! average-vertical-inter-string-unhappiness 0)
	      (set! average-unhappiness 0)
	      (set! top-mapping-strength 0)
	      (set! bottom-mapping-strength 0)
	      (set! vertical-mapping-strength 0)
	      (set! top-rule-list '())
	      (set! bottom-rule-list '())
	      (set! clamped-rule-list '())
	      (set! top-rule-possible? #f)
	      (set! bottom-rule-possible? #f)
	      (tell *EEG* 'initialize)
	      'done)
	    (get-bonds ()
	      (append
		(tell initial-string 'get-bonds)
		(tell modified-string 'get-bonds)
		(tell target-string 'get-bonds)
		(if %justify-mode% (tell answer-string 'get-bonds) '())))
	    (get-groups ()
	      (append
		(tell initial-string 'get-groups)
		(tell modified-string 'get-groups)
		(tell target-string 'get-groups)
		(if %justify-mode% (tell answer-string 'get-groups) '())))
	    (get-all-letters ()
	      (append
		(tell initial-string 'get-letters)
		(tell modified-string 'get-letters)
		(tell target-string 'get-letters)
		(if %justify-mode% (tell answer-string 'get-letters) '())))
	    (get-all-groups ()
	      (append
		(tell initial-string 'get-all-groups)
		(tell modified-string 'get-all-groups)
		(tell target-string 'get-all-groups)
		(if %justify-mode% (tell answer-string 'get-all-groups) '())))
	    (get-bridges (bridge-type)
	      (case bridge-type
		(top top-bridge-list)
		(bottom bottom-bridge-list)
		(vertical vertical-bridge-list)))
	    (get-all-bridges ()
	      (append top-bridge-list bottom-bridge-list vertical-bridge-list))
	    (get-objects ()
	      (append
		(tell initial-string 'get-objects)
		(tell modified-string 'get-objects)
		(tell target-string 'get-objects)
		(if %justify-mode% (tell answer-string 'get-objects) '())))
	    (get-other-string (string bridge-orientation)
	      (case (tell string 'get-string-type)
		(initial
		  (case bridge-orientation
		    (horizontal modified-string)
		    (vertical target-string)))
		(modified initial-string)
		(target
		  (case bridge-orientation
		    (horizontal answer-string)
		    (vertical initial-string)))
		(answer target-string)))
	    (get-possible-bridge-objects (bridge-type)
	      (apply append
		(case bridge-type
		  (top (tell-all top-strings 'get-objects))
		  (bottom (tell-all bottom-strings 'get-objects))
		  (vertical (tell-all vertical-strings 'get-objects)))))
	    (get-activity ()
	      (let ((average-age
		      (tell self 'get-youngest-structures-average-age)))
		(100- (100* (min 1.0 (/ average-age %expiration-period%))))))
	    (get-youngest-structures-average-age ()
	      (let* ((structures (tell self 'get-structures))
		     (youngest-structures
		       (get-first
			 (min (length structures) %num-youngest-structures%)
			 (sort-by-method 'get-age < structures))))
		(if (null? youngest-structures)
		  0
		  (/ (apply + (tell-all youngest-structures 'get-age))
		     (length youngest-structures)))))
	    (get-structures ()
	      (append
		(tell self 'get-bonds)
		(tell self 'get-groups)
		top-bridge-list
		bottom-bridge-list
		vertical-bridge-list
		top-rule-list
		bottom-rule-list))
	    (get-proposed-bridges (bridge-type)
	      (remq-duplicates
		(flatten
		  (map vector->list
		    (vector->list
		      (case bridge-type
			(top proposed-top-bridge-table)
			(vertical proposed-vertical-bridge-table)
			(bottom (if %justify-mode%
				  proposed-bottom-bridge-table
				  '#()))))))))
	    (get-all-proposed-bridges ()
	      (remq-duplicates
		(flatten
		  (list
		    (map vector->list (vector->list proposed-vertical-bridge-table))
		    (map vector->list (vector->list proposed-top-bridge-table))
		    (if %justify-mode%
		      (map vector->list (vector->list proposed-bottom-bridge-table))
		      '())))))
	    (get-proposed-vertical-bridges (object)
              (let ((id (tell object 'get-id-num))
		    (string (tell object 'get-string)))
		(cond
		  ((eq? string initial-string)
		   (apply append (get-row proposed-vertical-bridge-table id)))
		  ((eq? string target-string)
		   (apply append (get-column proposed-vertical-bridge-table id)))
		  (else '()))))
	    (get-proposed-horizontal-bridges (object)
              (let ((id (tell object 'get-id-num))
		    (string (tell object 'get-string)))
		(cond
		  ((eq? string initial-string)
		   (apply append (get-row proposed-top-bridge-table id)))
		  ((eq? string modified-string)
		   (apply append (get-column proposed-top-bridge-table id)))
		  ((and %justify-mode% (eq? string target-string))
		   (apply append (get-row proposed-bottom-bridge-table id)))
		  ((and %justify-mode% (eq? string answer-string))
		   (apply append (get-column proposed-bottom-bridge-table id)))
		  (else '()))))
	    (get-all-other-coincident-bridges (bridge object1 object2)
              (let* ((i (tell object1 'get-id-num))
		     (j (tell object2 'get-id-num))
		     (proposed-bridges
		       (remq bridge
			 (case (tell bridge 'get-bridge-type)
			   (top (table-ref proposed-top-bridge-table i j))
			   (bottom (table-ref proposed-bottom-bridge-table i j))
			   (vertical (table-ref proposed-vertical-bridge-table i j)))))
		     (built-bridge
		       (case (tell bridge 'get-bridge-type)
			 (top (vector-ref top-bridges i))
			 (bottom (vector-ref bottom-bridges i))
			 (vertical (vector-ref vertical-bridges i)))))
		(if (and (exists? built-bridge)
		         (not (eq? built-bridge bridge))
			 (eq? (tell built-bridge 'get-object1)
			      (tell bridge 'get-original-object1))
			 (eq? (tell built-bridge 'get-object2)
			      (tell bridge 'get-original-object2)))
		  (cons built-bridge proposed-bridges)
		  proposed-bridges)))
	    (bridge-present? (bridge)
	      (exists? (tell self 'get-equivalent-bridge bridge)))
	    (get-equivalent-bridge (bridge)
		(let* ((bridge-type (tell bridge 'get-bridge-type))
		       (bridge-orientation (tell bridge 'get-orientation))
		       (bridge-list
			 (case bridge-type
			   (top top-bridge-list)
			   (bottom bottom-bridge-list)
			   (vertical vertical-bridge-list)))
		       (string1
			 (case bridge-type
			   (top initial-string)
			   (bottom target-string)
			   (vertical initial-string)))
		       (string2
			 (case bridge-type
			   (top modified-string)
			   (bottom answer-string)
			   (vertical target-string))))
		  (if (member? bridge bridge-list)
		    bridge
		    (let ((equivalent-object1
			    (tell string1 'get-equivalent-object
			      (tell bridge 'get-object1)))
			  (equivalent-object2
			    (if (exists? string2)
			      (tell string2 'get-equivalent-object
				(tell bridge 'get-object2))
			      #f)))
		      (if (and (exists? equivalent-object1)
			       (exists? equivalent-object2)
			       (bridge-between? bridge-orientation
				 equivalent-object1 equivalent-object2))
			(tell equivalent-object1 'get-bridge bridge-orientation)
			#f)))))
	    (get-all-slippages (bridge-type)
	      (apply append
		(tell-all (tell self 'get-bridges bridge-type) 'get-slippages)))
	    (get-all-non-symmetric-slippages (bridge-type)
	      (apply append
		(tell-all (tell self 'get-bridges bridge-type)
		  'get-non-symmetric-slippages)))
	    (get-all-vertical-CMs ()
	      (apply append
		(tell-all vertical-bridge-list 'get-all-concept-mappings)))
	    (object-exists? (object)
	      (member? object (tell self 'get-objects)))
	    (spanning-bridge-exists? (bridge-type)
	      (ormap-meth (tell self 'get-bridges bridge-type) 'spanning-bridge?))
	    (get-spanning-bridge (bridge-type)
	      (select-meth (tell self 'get-bridges bridge-type) 'spanning-bridge?))
	    (add-bridge (bridge)
	      (let ((i (tell (tell bridge 'get-object1) 'get-id-num)))
		(case (tell bridge 'get-bridge-type)
		  (top
		    (vector-set! top-bridges i bridge)
		    (set! top-bridge-list (cons bridge top-bridge-list)))
		  (bottom
		    (vector-set! bottom-bridges i bridge)
		    (set! bottom-bridge-list (cons bridge bottom-bridge-list)))
		  (vertical
		    (vector-set! vertical-bridges i bridge)
		    (set! vertical-bridge-list (cons bridge vertical-bridge-list)))))
	      'done)
	    (add-proposed-bridge (proposed-bridge)
	      (let* ((table (case (tell proposed-bridge 'get-bridge-type)
			      (top proposed-top-bridge-table)
			      (bottom proposed-bottom-bridge-table)
			      (vertical proposed-vertical-bridge-table)))
		     (i (tell (tell proposed-bridge 'get-object1) 'get-id-num))
		     (j (tell (tell proposed-bridge 'get-object2) 'get-id-num))
		     (bridge-list (table-ref table i j)))
		(table-set! table i j (cons proposed-bridge bridge-list)))
	      'done)
	    (delete-bridge (bridge)
	      (let ((i (tell (tell bridge 'get-object1) 'get-id-num)))
		(case (tell bridge 'get-bridge-type)
		  (top
		    (set! top-bridge-list (remq bridge top-bridge-list))
		    (vector-set! top-bridges i #f))
		  (bottom
		    (set! bottom-bridge-list (remq bridge bottom-bridge-list))
		    (vector-set! bottom-bridges i #f))
		  (vertical
		    (set! vertical-bridge-list (remq bridge vertical-bridge-list))
		    (vector-set! vertical-bridges i #f))))
	      'done)
	    (delete-proposed-bridge (proposed-bridge)
	      (let* ((table (case (tell proposed-bridge 'get-bridge-type)
			      (top proposed-top-bridge-table)
			      (bottom proposed-bottom-bridge-table)
			      (vertical proposed-vertical-bridge-table)))
		     (i (tell (tell proposed-bridge 'get-object1) 'get-id-num))
		     (j (tell (tell proposed-bridge 'get-object2) 'get-id-num))
		     (bridge-list (table-ref table i j)))
		(table-set! table i j (remq proposed-bridge bridge-list)))
	      'done)
	    (delete-proposed-vertical-bridges (object)
	      (let ((id (tell object 'get-id-num))
		    (string (tell object 'get-string)))
		(cond
		  ((eq? string initial-string)
		   (initialize-row! proposed-vertical-bridge-table id '()))
		  ((eq? string target-string)
		   (initialize-column! proposed-vertical-bridge-table id '()))))
	      'done)
	    (delete-proposed-horizontal-bridges (object)
              (let ((id (tell object 'get-id-num))
		    (string (tell object 'get-string)))
		(cond
		  ((eq? string initial-string)
		   (initialize-row! proposed-top-bridge-table id '()))
		  ((eq? string modified-string)
		   (initialize-column! proposed-top-bridge-table id '()))
		  ((and %justify-mode% (eq? string target-string))
		   (initialize-row! proposed-bottom-bridge-table id '()))
		  ((and %justify-mode% (eq? string answer-string))
		   (initialize-column! proposed-bottom-bridge-table id '()))))
	      'done)
	    (delete-all-proposed-bridges ()
	      (for-each-vector-element* (proposed-vertical-bridge-table i) do
		(initialize-row! proposed-vertical-bridge-table i '()))
	      (for-each-vector-element* (proposed-top-bridge-table i) do
		(initialize-row! proposed-top-bridge-table i '()))
	      (if* %justify-mode%
		(for-each-vector-element* (proposed-bottom-bridge-table i) do
		  (initialize-row! proposed-bottom-bridge-table i '())))
	      'done)
	    (delete-proposed-structure (struc)
	      (case (tell struc 'object-type)
		(bond
		  (tell (tell struc 'get-string) 'delete-proposed-bond struc))
		(group
		  (tell (tell struc 'get-string) 'delete-proposed-group struc)
		  (if* %workspace-graphics% (group-graphics 'erase struc)))
		(bridge
		  (tell self 'delete-proposed-bridge struc)
		  (if* %workspace-graphics%
		    (bridge-graphics 'erase struc))))
	      'done)
	    (get-real-object (fake-object)
	      (select
		(lambda (obj) (equivalent-workspace-objects? obj fake-object))
		(tell self 'get-objects)))
	    (get-clamped-rules () clamped-rule-list)
	    (clamp-rule (rule)
	      (set! clamped-rule-list (cons rule clamped-rule-list))
	      (if* %workspace-graphics%
		(tell *workspace-window* 'draw (tell rule 'get-clamped-graphics-pexp)))
	      'done)
	    (unclamp-rule (rule)
	      (set! clamped-rule-list (remq rule clamped-rule-list))
	      (if* %workspace-graphics%
		(tell *workspace-window* 'erase (tell rule 'get-clamped-graphics-pexp)))
	      'done)
	    (unclamp-rules ()
	      (if* (not (null? clamped-rule-list))
		(if* %workspace-graphics%
		  (for* each rule in clamped-rule-list do
		    (tell *workspace-window* 'erase
		      (tell rule 'get-clamped-graphics-pexp))))
		(set! clamped-rule-list '()))
	      'done)
	    (get-all-rules ()
	      (append top-rule-list bottom-rule-list))
	    (get-rules (rule-type)
	      (case rule-type
		(top top-rule-list)
		(bottom bottom-rule-list)))
	    (get-all-supported-rules ()
	      (filter-meth (tell self 'get-all-rules) 'supported?))
	    (get-supported-rules (rule-type)
	      (filter-meth (tell self 'get-rules rule-type) 'supported?))
	    (rule-exists? (rule-type)
	      (not (null? (tell self 'get-rules rule-type))))
	    (supported-rule-exists? (rule-type)
	      (ormap-meth (tell self 'get-rules rule-type) 'supported?))
	    (get-possible-rule-types ()
	      (cond
		((and top-rule-possible? bottom-rule-possible?) '(top bottom))
		(top-rule-possible? '(top))
		(bottom-rule-possible? '(bottom))
		(else '())))
	    (rule-possible? (rule-type)
	      (case rule-type
		(top top-rule-possible?)
		(bottom bottom-rule-possible?)))
	    (check-if-rules-possible ()
	      (set! top-rule-possible?
		(subset?
		  (append
		    (tell initial-string 'get-letters)
		    (tell modified-string 'get-letters))
		  (apply append
		    (tell-all (filter rule-describable-bridge? top-bridge-list)
		      'get-covered-letters))))
	      (if* %justify-mode%
		(set! bottom-rule-possible?
		  (subset?
		    (append
		      (tell target-string 'get-letters)
		      (tell answer-string 'get-letters))
		    (apply append
		      (tell-all (filter rule-describable-bridge? bottom-bridge-list)
			'get-covered-letters)))))
	      'done)
	    (rule-present? (rule)
	      (exists? (tell self 'get-equivalent-rule rule)))
	    (get-equivalent-rule (rule)
	      (select-meth
		(tell self 'get-rules (tell rule 'get-rule-type))
		'equal? rule))
	    (add-rule (rule)
	      (case (tell rule 'get-rule-type)
		(top (set! top-rule-list (cons rule top-rule-list)))
		(bottom (set! bottom-rule-list (cons rule bottom-rule-list))))
	      'done)
	    ;; unused:
	    (delete-rule (rule)
	      (case (tell rule 'get-rule-type)
		(top (set! top-rule-list (remq rule top-rule-list)))
		(bottom (set! bottom-rule-list (remq rule bottom-rule-list))))
	      'done)
	    ;; Themes:
	    ;;
	    ;; Don't need to update dominant themes or graphics here, since they get
	    ;; updated automatically after activation spreads in the themespace, which
	    ;; happens immediately after spread-activation-to-themespace (see run.ss):
	    (spread-activation-to-themespace ()
	      (for* each bridge in (tell self 'get-all-bridges) do
		(tell bridge 'boost-themes))
	      'done)
	    (choose-object message
	      (let* ((objects (tell self 'get-objects))
		     (weights (apply tell-all (cons objects message))))
		(stochastic-pick objects (temp-adjusted-values weights))))
	    (get-average-intra-string-unhappiness () average-intra-string-unhappiness)
	    (get-average-inter-string-unhappiness (bridge-type)
	      (case bridge-type
		(top average-top-inter-string-unhappiness)
		(bottom average-bottom-inter-string-unhappiness)
		(vertical average-vertical-inter-string-unhappiness)))
	    (get-max-inter-string-unhappiness ()
	      (if %justify-mode%
		(max average-top-inter-string-unhappiness
		     average-bottom-inter-string-unhappiness
		     average-vertical-inter-string-unhappiness)
		(max average-top-inter-string-unhappiness
		     average-vertical-inter-string-unhappiness)))
	    (get-average-unhappiness () average-unhappiness)
	    (get-mapping-strength (bridge-type)
	      (case bridge-type
		(top top-mapping-strength)
		(bottom bottom-mapping-strength)
		(vertical vertical-mapping-strength)))
	    (get-min-mapping-strength ()
	      (if %justify-mode%
		(min top-mapping-strength
		     bottom-mapping-strength
		     vertical-mapping-strength)
		(min top-mapping-strength
		     vertical-mapping-strength)))
	    (maximal-mapping? (bridge-type)
	      (let ((strings
		      (case bridge-type
			(top (list initial-string modified-string))
			(vertical (list initial-string target-string))
			(bottom (list target-string answer-string)))))
		(sets-equal?
		  (remq-duplicates
		    (apply append
		      (tell-all (tell self 'get-bridges bridge-type)
			'get-covered-letters)))
		  (apply append (tell-all strings 'get-letters)))))
	    (update-average-unhappiness-values ()
	      (let ((all-objects (tell self 'get-objects))
		    (top-objects
		      (append
			(tell initial-string 'get-objects)
			(tell modified-string 'get-objects)))
		    (bottom-objects
		      (if %justify-mode%
			(append
			  (tell target-string 'get-objects)
			  (tell answer-string 'get-objects))
			#f))
		    (vertical-objects
		      (append
			(tell initial-string 'get-objects)
			(tell target-string 'get-objects))))
		(set! average-intra-string-unhappiness
		  (round
		    (weighted-average
		      (tell-all all-objects 'get-intra-string-unhappiness)
		      (tell-all all-objects 'get-relative-importance))))
		(set! average-top-inter-string-unhappiness
		  (round
		    (weighted-average
		      (tell-all top-objects
			'get-inter-string-unhappiness 'horizontal)
		      (tell-all top-objects 'get-relative-importance))))
		(if* %justify-mode%
		  (set! average-bottom-inter-string-unhappiness
		    (round
		      (weighted-average
			(tell-all bottom-objects
			  'get-inter-string-unhappiness 'horizontal)
			(tell-all bottom-objects 'get-relative-importance)))))
		(set! average-vertical-inter-string-unhappiness
		  (round
		    (weighted-average
		      (tell-all vertical-objects
			'get-inter-string-unhappiness 'vertical)
		      (tell-all vertical-objects 'get-relative-importance))))
		(set! average-unhappiness
		  (round
		    (weighted-average
		      (tell-all all-objects 'get-average-unhappiness)
		      (tell-all all-objects 'get-relative-importance))))
		(let ((raw-top-strength
			(100- average-top-inter-string-unhappiness))
		      (raw-bottom-strength
			(if %justify-mode%
			  (100- average-bottom-inter-string-unhappiness)
			  #f))
		      (raw-vertical-strength
			(100- average-vertical-inter-string-unhappiness)))
		  (set! top-mapping-strength
		    (cond
		      ((tell self 'spanning-bridge-exists? 'top)
		       raw-top-strength)
		      ((and (spanning-group-possible? initial-string)
			    (spanning-group-possible? modified-string))
		       (round (* 1/2 raw-top-strength)))
		      ((tell self 'maximal-mapping? 'top)
		       (100* (tanh (* 1/40 raw-top-strength))))
		      (else raw-top-strength)))
		  (if* %justify-mode%
		    (set! bottom-mapping-strength
		      (cond
			((tell self 'spanning-bridge-exists? 'bottom)
			 raw-bottom-strength)
			((and (spanning-group-possible? target-string)
			      (spanning-group-possible? answer-string))
			 (round (* 1/2 raw-bottom-strength)))
			((tell self 'maximal-mapping? 'bottom)
			 (100* (tanh (* 1/40 raw-bottom-strength))))
			(else raw-bottom-strength))))
		  (set! vertical-mapping-strength
		    (cond
		      ((tell self 'spanning-bridge-exists? 'vertical)
		       raw-vertical-strength)
		      ((and (spanning-group-possible? initial-string)
			    (spanning-group-possible? target-string))
		       (round (* 1/2 raw-vertical-strength)))
		      ((tell self 'maximal-mapping? 'vertical)
		       (100* (tanh (* 1/40 raw-vertical-strength))))
		      (else raw-vertical-strength))))))
	    (get-rough-num-of-unrelated-objects ()
	      (rough-num-of-objects (count unrelated? (tell self 'get-objects))))
	    (get-rough-num-of-ungrouped-objects ()
	      (rough-num-of-objects (count ungrouped? (tell self 'get-objects))))
	    (get-rough-num-of-unmapped-objects ()
	      (rough-num-of-objects (count unmapped? (tell self 'get-objects))))
	    (reallocate-top-bridges-storage ()
	      (let* ((max-i-capacity (tell initial-string 'get-max-object-capacity))
		     (max-m-capacity (tell modified-string 'get-max-object-capacity))
		     (new-vector (make-vector max-i-capacity #f))
		     (new-table (make-table max-i-capacity max-m-capacity '())))
		(copy-vector-contents! top-bridges new-vector)
		(copy-table-contents! proposed-top-bridge-table new-table)
		(set! top-bridges new-vector)
		(set! proposed-top-bridge-table new-table))
	      'done)
	    (reallocate-bottom-bridges-storage ()
	      (let* ((max-t-capacity (tell target-string 'get-max-object-capacity))
		     (max-a-capacity (tell answer-string 'get-max-object-capacity))
		     (new-vector (make-vector max-t-capacity #f))
		     (new-table (make-table max-t-capacity max-a-capacity '())))
		(copy-vector-contents! bottom-bridges new-vector)
		(copy-table-contents! proposed-bottom-bridge-table new-table)
		(set! bottom-bridges new-vector)
		(set! proposed-bottom-bridge-table new-table))
	      'done)
	    (reallocate-vertical-bridges-storage ()
	      (let* ((max-i-capacity (tell initial-string 'get-max-object-capacity))
		     (max-t-capacity (tell target-string 'get-max-object-capacity))
		     (new-vector (make-vector max-i-capacity #f))
		     (new-table (make-table max-i-capacity max-t-capacity '())))
		(copy-vector-contents! vertical-bridges new-vector)
		(copy-table-contents! proposed-vertical-bridge-table new-table)
		(set! vertical-bridges new-vector)
		(set! proposed-vertical-bridge-table new-table))
	      'done)
	    (else (delegate msg base-object))))))))


(define spanning-group-possible?
  (lambda (string)
    (or (tell string 'spanning-group-exists?)
        (let ((objects (tell string 'get-constituent-objects)))
	  (ormap
	    (lambda (bond-facet)
	      (let ((relations
		      (adjacency-map
			(lambda (obj1 obj2)
			  (let ((desc1 (tell obj1 'get-descriptor-for bond-facet))
				(desc2 (tell obj2 'get-descriptor-for bond-facet)))
			    (if (and (exists? desc1) (exists? desc2))
			      (get-label desc1 desc2)
			      #f)))
			objects)))
		(and (all-exist? relations) (all-same? relations))))
	    (tell plato-bond-facet 'get-instance-nodes))))))


(define rough-num-of-objects
  (lambda (num-of-objects)
    (cond
      ((< num-of-objects (~ 2)) 'few)
      ((< num-of-objects (~ 4)) 'some)
      (else 'many))))


(define unrelated?
  (lambda (object)
    (and (ungrouped? object)
	 (let ((num-of-incident-bonds (tell object 'get-num-of-incident-bonds)))
	   (if (or (tell object 'leftmost-in-string?)
		   (tell object 'rightmost-in-string?))
	       (= num-of-incident-bonds 0)
	       (< num-of-incident-bonds 2))))))


(define ungrouped?
  (lambda (object)
    (and (not (tell object 'spans-whole-string?))
	 (not (exists? (tell object 'get-enclosing-group))))))


(define unmapped?
  (lambda (object)
    (case (tell object 'which-string)
      (initial (not (tell object 'mapped? 'both)))
      (modified (not (tell object 'mapped? 'horizontal)))
      (target
	(if %justify-mode%
	  (not (tell object 'mapped? 'both))
	  (not (tell object 'mapped? 'vertical))))
      (answer (not (tell object 'mapped? 'horizontal))))))


(define *workspace* (make-workspace))
