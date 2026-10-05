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


(define make-letter
  (lambda (string letter-category string-pos)
    (let ((letter (new-letter string letter-category string-pos)))
      (tell letter 'new-description plato-object-category plato-letter)
      (tell letter 'new-description plato-letter-category letter-category)
      letter)))


(define new-letter
  (lambda (string letter-category string-pos)
    (let* ((workspace-object (make-workspace-object string string-pos string-pos))
	   (image (make-letter-image letter-category))
	   (print-name (tell letter-category 'get-lowercase-name))
	   (ascii-name (format "~a:~a" print-name string-pos))
	   ;; The graphics-text-coord of a letter is NOT the same as its
	   ;; bottom-left-graphics-coord, the latter being the lower left
	   ;; corner of the bounding-box.  Nor is it necessarily the lower left
	   ;; corner of the letter's bit matrix.  The actual text origin relative
	   ;; to the bit matrix depends on the particular font.
	   (graphics-pexp #f)
	   (graphics-text-coord #f))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'letter)
	    (print-name () print-name)
	    (ascii-name () ascii-name)
	    (get-string-pos () string-pos)
	    (get-graphics-pexp () graphics-pexp)
	    (get-graphics-text-coord () graphics-text-coord)
	    (set-graphics-pexp (pexp) (set! graphics-pexp pexp) 'done)
	    (set-graphics-text-coord (coord) (set! graphics-text-coord coord) 'done)
	    (get-image () image)
	    ;; get-initial-letter-category is included to be consistent with groups,
	    ;; and is used by images:
	    (get-initial-letter-category () letter-category)
	    (get-platonic-length () plato-one)
	    (get-letter-category () letter-category)
	    (get-letters () (list self))
	    (nested-member? (object) #f)
	    (singleton-group? () #f)
	    (make-flipped-version () self)
	    (else (delegate msg workspace-object))))))))


(define make-workspace-object
  (lambda (string left-string-pos right-string-pos)
    (let ((id-num 0)
	  (raw-importance 0)
	  (relative-importance 0)
	  (intra-string-unhappiness 0)
	  (horizontal-inter-string-unhappiness 0)
	  (vertical-inter-string-unhappiness 0)
	  (average-unhappiness 0)
	  (intra-string-salience 0)
	  (horizontal-inter-string-salience 0)
	  (vertical-inter-string-salience 0)
	  (average-salience 0)
	  (descriptions '())
	  (outgoing-bonds '())
	  (incoming-bonds '())
	  (left-bond #f)
	  (right-bond #f)
	  (enclosing-group #f)
	  (horizontal-bridge #f)
	  (vertical-bridge #f)
	  (new-answer-letter? #f)
	  (salience-clamped? #f)
	  (graphics-x1 #f)
	  (graphics-y1 #f)
	  (graphics-x2 #f)
	  (graphics-y2 #f)
	  (horizontal-bridge-graphics-coord #f)
	  (vertical-bridge-graphics-coord #f)
	  (horizontal-group-spanning-bridge-graphics-coord #f)
	  (vertical-group-spanning-bridge-graphics-coord #f))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'workspace-object)
	    (print ()
              (printf "~a ~a in ~a"
		(full-workspace-object-name self)
		(tell self 'ascii-name)
		(tell string 'generic-name))
	      (if* (and (group? self) (< (tell self 'get-proposal-level) %built%))
		(printf (if (and %workspace-graphics% (not (tell self 'drawn?)))
			  " (~a, not drawn)"
			  " (~a)")
		  (case (tell self 'get-proposal-level)
		    (0 "new")
		    (1 "proposed")
		    (2 "evaluated"))))
	      (newline))
	    (get-id-num () id-num)
	    (set-id-num (new-id) (set! id-num new-id) 'done)
	    (get-graphics-x1 () graphics-x1)
	    (get-graphics-y1 () graphics-y1)
	    (get-graphics-x2 () graphics-x2)
	    (get-graphics-y2 () graphics-y2)
	    (set-graphics-coords (left-bottom right-top)
              (set! graphics-x1 (1st left-bottom))
	      (set! graphics-y1 (2nd left-bottom))
	      (set! graphics-x2 (1st right-top))
	      (set! graphics-y2 (2nd right-top))
	      'done)
	    (get-bridge-graphics-coord (bridge-orientation)
	      (case bridge-orientation
		(horizontal horizontal-bridge-graphics-coord)
		(vertical vertical-bridge-graphics-coord)))
	    (get-group-spanning-bridge-graphics-coord (bridge-orientation)
	      (case bridge-orientation
		(horizontal horizontal-group-spanning-bridge-graphics-coord)
		(vertical vertical-group-spanning-bridge-graphics-coord)))
	    (set-bridge-graphics-coords (v h)
	      (set! vertical-bridge-graphics-coord v)
	      (set! horizontal-bridge-graphics-coord h)
	      'done)
	    (set-group-spanning-bridge-graphics-coords (v h)
	      (set! vertical-group-spanning-bridge-graphics-coord v)
	      (set! horizontal-group-spanning-bridge-graphics-coord h)
	      'done)
	    (get-instantiated-image-object ()
	      (let* ((image (tell self 'get-image))
		     (swapped-image (tell image 'get-swapped-image)))
		(if (exists? swapped-image)
		  (tell swapped-image 'get-instantiated-object)
		  (tell image 'get-instantiated-object))))
	    (get-string () string)
	    (which-string () (tell string 'get-string-type))
	    (in-string? (s) (eq? s string))
	    (get-left-string-pos () left-string-pos)
	    (get-right-string-pos () right-string-pos)
	    (get-left-bond () left-bond)
	    (get-right-bond () right-bond)
	    (get-enclosing-group () enclosing-group)
	    (get-bridge (bridge-orientation)
	      (case bridge-orientation
		(horizontal horizontal-bridge)
		(vertical vertical-bridge)))
	    (get-descriptions () descriptions)
	    (get-all-descriptions ()
	      (if (group? self)
		(append descriptions (tell self 'get-bond-descriptions))
		descriptions))
	    (get-incident-bonds () (compress (list left-bond right-bond)))
	    (get-num-of-incident-bonds () (length (tell self 'get-incident-bonds)))
	    (get-concept-pattern ()
	      (cons 'concepts
		(map
		  (lambda (description)
		    (let ((descriptor (tell description 'get-descriptor))
			  (relevance
			    (if (tell description 'relevant?) %max-activation% 0)))
		      (list descriptor relevance)))
		  (tell self 'get-all-descriptions))))
	    (clamp-salience () (set! salience-clamped? #t) 'done)
	    (unclamp-salience () (set! salience-clamped? #f) 'done)
	    (mapped? (orientation)
	      (case orientation
		(vertical (exists? vertical-bridge))
		(horizontal (exists? horizontal-bridge))
		(both (and (exists? vertical-bridge) (exists? horizontal-bridge)))))
	    (update-enclosing-group (new-group)
	      (set! enclosing-group new-group)
	      'done)
	    (update-right-bond (new-bond)
	      (set! right-bond new-bond)
	      'done)
	    (update-left-bond (new-bond)
	      (set! left-bond new-bond)
	      'done)
	    (add-outgoing-bond (bond)
	      (set! outgoing-bonds (cons bond outgoing-bonds))
	      (if* (eq? (tell bond 'get-bond-category) plato-sameness)
		   (set! incoming-bonds (cons bond incoming-bonds)))
	      'done)
	    (add-incoming-bond (bond)
	      (set! incoming-bonds (cons bond incoming-bonds))
	      (if* (eq? (tell bond 'get-bond-category) plato-sameness)
		   (set! outgoing-bonds (cons bond outgoing-bonds)))
	      'done)
	    (remove-outgoing-bond (bond)
	      (set! outgoing-bonds (remq bond outgoing-bonds))
	      (if* (eq? (tell bond 'get-bond-category) plato-sameness)
		   (set! incoming-bonds (remq bond incoming-bonds)))
	      'done)
	    (remove-incoming-bond (bond)
	      (set! incoming-bonds (remq bond incoming-bonds))
	      (if* (eq? (tell bond 'get-bond-category) plato-sameness)
		   (set! outgoing-bonds (remq bond outgoing-bonds)))
	      'done)
	    (get-all-description-types ()
	      (tell-all descriptions 'get-description-type))
	    (get-all-descriptors ()
	      (tell-all descriptions 'get-descriptor))
	    (get-descriptor-for (description-type)
	      (let ((description
		      (select-meth descriptions 'description-type? description-type)))
		(if (exists? description)
		    (tell description 'get-descriptor)
		    #f)))
	    (distinguishing-descriptor? (descriptor)
	      (if (or (eq? descriptor plato-letter)
		      (eq? descriptor plato-group)
		      (member? descriptor *slipnet-numbers*))
		  #f
		  (let* ((other-objects
			   (cond
			     ((letter? self)
			      (remq self (tell string 'get-letters)))
			     ((group? self)
			      (let ((supergroup (tell self 'get-enclosing-group))
				    (subgroups (filter group?
						 (tell self 'get-constituent-objects))))
				(filter (lambda (group)
					  (not (or (eq? group self)
						   (eq? group supergroup)
						   (member? group subgroups))))
				  (tell string 'get-groups))))))
			 (other-descriptors
			   (tell-all
			     (apply append (tell-all other-objects 'get-descriptions))
			     'get-descriptor)))
		    (not (member? descriptor other-descriptors)))))

	    (get-relevant-descriptions ()
	      (filter (lambda (d) (tell d 'relevant?)) descriptions))

	    (get-distinguishing-descriptions ()
	      (filter (lambda (d)
			(tell self 'distinguishing-descriptor?
			  (tell d 'get-descriptor)))
		descriptions))

	    (get-relevant-distinguishing-descriptions ()
	      (filter (lambda (dd) (tell dd 'relevant?))
		(tell self 'get-distinguishing-descriptions)))

            (get-descriptions-for-rule ()
              (filter
		(lambda (rdd)
		  (or (tell rdd 'description-type? plato-string-position-category)
		      (tell rdd 'description-type? plato-alphabetic-position-category)
		      (and (tell rdd 'description-type? plato-letter-category)
			   (or (letter? self)
			       (eq? (tell self 'get-group-category) plato-samegrp)))))
		(tell self 'get-relevant-distinguishing-descriptions)))

	    (choose-description-for-rule ()
	      (let ((possible-descriptions (tell self 'get-descriptions-for-rule)))
		(stochastic-pick
		  possible-descriptions
		  (temp-adjusted-values
		    (tell-all possible-descriptions 'get-conceptual-depth)))))

	    (description-type-present? (description-type)
	      (ormap-meth (tell self 'get-all-descriptions)
		'description-type? description-type))

	    (description-present? (description)
	      (description-member? description (tell self 'get-all-descriptions)))

	    (descriptor-present? (descriptor)
	      (ormap (lambda (d) (eq? (tell d 'get-descriptor) descriptor))
		(tell self 'get-all-descriptions)))

	    (all-description-types-present? (description-types)
	      (andmap (lambda (description-type)
			(tell self 'description-type-present? description-type))
		description-types))

	    (add-description (new-description)
	      (set! descriptions (cons new-description descriptions))
	      (if* (and %workspace-graphics%
			(tell new-description 'description-type? plato-length)
			(group? self))
		   (tell self 'enable-length-graphics)
		   (if* (tell self 'drawn?)
		     (tell *workspace-window* 'draw-group-length self)))
	      'done)

	    (new-description (description-type descriptor)
	      (let ((description (make-description self description-type descriptor)))
		(tell description 'update-proposal-level %built%)
		(tell self 'add-description description)))

	    ;; Used only when creating horizontal bridges for translated rules:
	    (attach-description (description-type descriptor)
	      (let ((description (make-description self description-type descriptor)))
		(tell description 'update-proposal-level %built%)
		(set! descriptions (cons description descriptions)))
	      'done)

	    (delete-description-type (description-type)
	      (let ((description
		      (select-meth descriptions 'description-type? description-type)))
		(set! descriptions (remq description descriptions))
		'done))

	    (update-bridge (bridge-orientation new-bridge)
	      (case bridge-orientation
		(horizontal (set! horizontal-bridge new-bridge))
		(vertical (set! vertical-bridge new-bridge)))
	      'done)

	    (get-raw-importance () raw-importance)
	    (get-relative-importance () relative-importance)
	    (get-intra-string-unhappiness () intra-string-unhappiness)
	    (get-inter-string-unhappiness (bridge-orientation)
	      (case bridge-orientation
		(horizontal horizontal-inter-string-unhappiness)
		(vertical vertical-inter-string-unhappiness)))
	    (get-average-unhappiness () average-unhappiness)
	    (get-intra-string-salience () intra-string-salience)
	    (get-inter-string-salience (bridge-orientation)
	      (case bridge-orientation
		(horizontal horizontal-inter-string-salience)
		(vertical vertical-inter-string-salience)))
	    (get-average-salience () average-salience)

	    (get-nesting-level ()
	      (if (exists? enclosing-group)
		(add1 (tell enclosing-group 'get-nesting-level))
		0))

	    (get-letter-span ()
	      (add1 (- right-string-pos left-string-pos)))

	    (spans-whole-string? ()
	      (= (tell self 'get-letter-span) (tell string 'get-length)))

	    (string-spanning-group? ()
	      (and (group? self) (tell self 'spans-whole-string?)))

	    (get-num-of-spanning-bridges ()
	      (if (tell self 'spans-whole-string?)
		(count exists? (list horizontal-bridge vertical-bridge))
		0))

	    (leftmost-in-string? ()
	      (= left-string-pos 0))

	    (middle-in-string? ()
	      (let ((left-neighbor (tell self 'get-ungrouped-left-neighbor))
		    (right-neighbor (tell self 'get-ungrouped-right-neighbor)))
		(and (exists? left-neighbor)
		     (exists? right-neighbor)
		     (tell left-neighbor 'leftmost-in-string?)
		     (tell right-neighbor 'rightmost-in-string?))))

	    (rightmost-in-string? ()
	      (= right-string-pos (sub1 (tell string 'get-length))))
					
	    (get-all-left-neighbors ()
	      (if (tell self 'leftmost-in-string?)
		'()
		(let ((left-pos (sub1 left-string-pos)))
		  (cons (tell string 'get-letter left-pos)
		    (tell string 'get-right-edged-groups left-pos)))))

	    (get-all-right-neighbors ()
	      (if (tell self 'rightmost-in-string?)
		'()
		(let ((right-pos (add1 right-string-pos)))
		  (cons (tell string 'get-letter right-pos)
		    (tell string 'get-left-edged-groups right-pos)))))

	    (get-ungrouped-left-neighbor ()
	      (select (lambda (n)
			(let ((enclosing-group (tell n 'get-enclosing-group)))
			  (or (not (exists? enclosing-group))
			      (tell enclosing-group 'nested-member? self))))
		(tell self 'get-all-left-neighbors)))
	    
	    (get-ungrouped-right-neighbor ()
	      (select (lambda (n)
			(let ((enclosing-group (tell n 'get-enclosing-group)))
			  (or (not (exists? enclosing-group))
			      (tell enclosing-group 'nested-member? self))))
		(tell self 'get-all-right-neighbors)))

	    (choose-left-neighbor ()
	      (let ((left-neighbors (tell self 'get-all-left-neighbors)))
		(if (null? left-neighbors)
		  #f
		  (stochastic-pick-by-method
		    left-neighbors 'get-intra-string-salience))))

	    (choose-right-neighbor ()
	      (let ((right-neighbors (tell self 'get-all-right-neighbors)))
		(if (null? right-neighbors)
		  #f
		  (stochastic-pick-by-method
		    right-neighbors 'get-intra-string-salience))))

	    (choose-neighbor ()
	      (let ((neighbors (append (tell self 'get-all-left-neighbors)
				 (tell self 'get-all-right-neighbors))))
		(if (null? neighbors)
		  #f
		  (stochastic-pick-by-method
		    neighbors 'get-intra-string-salience))))

	    (update-raw-importance ()
	      (set! raw-importance
		(let* ((relevant-descriptions
			 (tell self 'get-relevant-descriptions))
		       (result (min 300 (sum (tell-all relevant-descriptions
					       'get-descriptor-activation)))))
		  (if (exists? enclosing-group)
		    (* 2/3 result)
		    result)))
	      'done)
	    
	    (update-relative-importance (new-value)
	      (set! relative-importance new-value)
	      'done)

	    (update-intra-string-unhappiness ()
	      (set! intra-string-unhappiness
		(cond
		  ((tell self 'spans-whole-string?) 0)
		  ((exists? enclosing-group)
		   (100- (tell enclosing-group 'get-strength)))
		  (else
		    (let ((bonds (tell self 'get-incident-bonds)))
		      (cond
			((null? bonds) 100)
			((or (tell self 'leftmost-in-string?)
			     (tell self 'rightmost-in-string?))
			 (100- (round (* 1/3 (tell (1st bonds) 'get-strength)))))
			(else (100- (round (* 1/6 (sum (tell-all bonds
							 'get-strength)))))))))))
	      'done)

	    (update-inter-string-unhappiness ()
	      (let ((horizontal-weakness
		      (cond
			((exists? horizontal-bridge)
			 (100- (tell horizontal-bridge 'get-strength)))
			((exists? enclosing-group)
			 (let ((h (tell enclosing-group 'get-bridge 'horizontal)))
			   (if (exists? h)
			     (100- (* 1/2 (tell h 'get-strength)))
			     100)))
			(else 100)))
		    (vertical-weakness
		      (cond
			((exists? vertical-bridge)
			 (100- (tell vertical-bridge 'get-strength)))
			((exists? enclosing-group)
			 (let ((v (tell enclosing-group 'get-bridge 'vertical)))
			   (if (exists? v)
			     (100- (* 1/2 (tell v 'get-strength)))
			     100)))
			(else 100))))
		(case (tell string 'get-string-type)
		  (initial
		    (set! horizontal-inter-string-unhappiness horizontal-weakness)
		    (set! vertical-inter-string-unhappiness vertical-weakness))
		  (modified
		    (set! horizontal-inter-string-unhappiness horizontal-weakness))
		  (target
		    (set! vertical-inter-string-unhappiness vertical-weakness)
		    (if* %justify-mode%
		      (set! horizontal-inter-string-unhappiness horizontal-weakness)))
		  (answer
		    (set! horizontal-inter-string-unhappiness horizontal-weakness))))
	      'done)

	    (update-average-unhappiness ()
	      (set! average-unhappiness
		(round (case (tell string 'get-string-type)
			 (initial
			   (average
			     intra-string-unhappiness
			     horizontal-inter-string-unhappiness
			     vertical-inter-string-unhappiness))
			 (modified
			   (average
			     intra-string-unhappiness
			     horizontal-inter-string-unhappiness))
			 (target
			   (if %justify-mode%
			     (average
			       intra-string-unhappiness
			       vertical-inter-string-unhappiness
			       horizontal-inter-string-unhappiness)
			     (average
			       intra-string-unhappiness
			       vertical-inter-string-unhappiness)))
			 (answer
			  (average
			    intra-string-unhappiness
			    horizontal-inter-string-unhappiness)))))
	      'done)

	    ;; intra/inter-string-salience =
	    ;;    intra/inter-string-unhappiness weighted by relative-importance
	    ;; Heavily weighted by importance for inter-string-salience,
	    ;; lightly weighted by importance for intra-string-salience.

	    (update-intra-string-salience ()
	      (set! intra-string-salience
		(if salience-clamped?
		  100
		  (round (+ (80% intra-string-unhappiness)
			    (20% relative-importance)))))
	      'done)

	    (update-inter-string-salience ()
	      (cond
		(salience-clamped?
		  (set! horizontal-inter-string-salience 100)
		  (set! vertical-inter-string-salience 100))
		((tell string 'string-type? 'initial)
		 (set! horizontal-inter-string-salience
		   (round (+ (20% horizontal-inter-string-unhappiness)
			     (80% relative-importance))))
		 (set! vertical-inter-string-salience
		   (round (+ (20% vertical-inter-string-unhappiness)
			     (80% relative-importance)))))
		((tell string 'string-type? 'modified)
		 (set! horizontal-inter-string-salience
		   (round (+ (20% horizontal-inter-string-unhappiness)
			     (80% relative-importance)))))
		((tell string 'string-type? 'target)
		 (set! vertical-inter-string-salience
		   (round (+ (20% vertical-inter-string-unhappiness)
			     (80% relative-importance))))
		 (if* %justify-mode%
		   (set! horizontal-inter-string-salience
		     (round (+ (20% horizontal-inter-string-unhappiness)
			       (80% relative-importance))))))
		((tell string 'string-type? 'answer)
		 (set! horizontal-inter-string-salience
		   (round (+ (20% horizontal-inter-string-unhappiness)
			     (80% relative-importance))))))
	      'done)

	    (update-average-salience ()
	      (set! average-salience
		(round (case (tell string 'get-string-type)
			 (initial
			   (average
			     intra-string-salience
			     horizontal-inter-string-salience
			     vertical-inter-string-salience))
			 (modified
			   (average
			     intra-string-salience
			     horizontal-inter-string-salience))
			 (target
			   (if %justify-mode%
			     (average
			       intra-string-salience
			       vertical-inter-string-salience
			       horizontal-inter-string-salience)
			     (average
			       intra-string-salience
			       vertical-inter-string-salience)))
			 (answer
			   (average
			     intra-string-salience
			     horizontal-inter-string-salience)))))
	      'done)

	    (update-description-strengths ()
	      (for* each description in descriptions do
		(tell description 'update-strength))
	      'done)

	    (update-object-values ()
	      (tell self 'update-intra-string-unhappiness)
	      (tell self 'update-inter-string-unhappiness)
	      (tell self 'update-average-unhappiness)
	      (tell self 'update-intra-string-salience)
	      (tell self 'update-inter-string-salience)
	      (tell self 'update-average-salience)
	      (tell self 'update-description-strengths))

	    (choose-relevant-description-by-activation ()
	      (let ((relevant-descriptions (tell self 'get-relevant-descriptions)))
		(if (null? relevant-descriptions)
		    #f
		    (stochastic-pick-by-method
		      relevant-descriptions
		      'get-descriptor-activation))))

	    (choose-relevant-distinguishing-description-by-depth ()
	      (let ((relevant-distinguishing-descriptions
		      (tell self 'get-relevant-distinguishing-descriptions)))
		(if (null? relevant-distinguishing-descriptions)
		    #f
		    (stochastic-pick-by-method
		      relevant-distinguishing-descriptions
		      'get-conceptual-depth))))

	    (else (delegate msg base-object))))))))



(define lowest-level-object
  (lambda (objects)
    (let ((max-nesting-level (maximum (tell-all objects 'get-nesting-level))))
      (select
	(lambda (object)
	  (= (tell object 'get-nesting-level) max-nesting-level))
	objects))))



(define highest-level-object
  (lambda (objects)
    (let ((min-nesting-level (minimum (tell-all objects 'get-nesting-level))))
      (select
	(lambda (object)
	  (= (tell object 'get-nesting-level) min-nesting-level))
	objects))))



(define disjoint-objects?
  (lambda (object1 object2)
    (or (< (tell object1 'get-right-string-pos)
	   (tell object2 'get-left-string-pos))
	(> (tell object1 'get-left-string-pos)
	   (tell object2 'get-right-string-pos)))))



(define lone-spanning-object?
  (lambda (object1 object2)
    (or (and (tell object1 'spans-whole-string?)
	     (not (tell object2 'spans-whole-string?)))
        (and (tell object2 'spans-whole-string?)
	     (not (tell object1 'spans-whole-string?))))))



(define both-spanning-groups?
  (lambda (object1 object2)
    (and (tell object1 'string-spanning-group?)
         (tell object2 'string-spanning-group?))))



(define both-spanning-objects?
  (lambda (object1 object2)
    (and (tell object1 'spans-whole-string?)
         (tell object2 'spans-whole-string?))))
