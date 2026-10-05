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

(define make-description
  (lambda (object description-type descriptor)
    (let ((string (tell object 'get-string))
	  (workspace-structure (make-workspace-structure)))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'description)
	    ;; This is solely for the purposes of (say <description>):
	    (print-name ()
	      (format "~a:~a"
		(tell description-type 'get-short-name)
		(tell descriptor 'get-short-name)))
	    (print ()
              (printf "Description of ~a~a in ~a: ~a:~a"
		(tell object 'ascii-name)
		(if (or (letter? object)
		        (= (tell object 'get-proposal-level) %built%))
		  ""
		  (format (if (and %workspace-graphics% (not (tell object 'drawn?)))
			    " (~a, not drawn)"
			    " (~a)")
		    (case (tell object 'get-proposal-level)
		      (1 "proposed")
		      (2 "evaluated"))))
		(tell string 'generic-name)
		(tell description-type 'get-short-name)
		(tell descriptor 'get-lowercase-name))
	      (if* (< (tell self 'get-proposal-level) %built%)
		(printf " (~a)"
		  (case (tell self 'get-proposal-level)
		    (1 "proposed")
		    (2 "evaluated"))))
	      (newline))
	    (get-theme-types ()
	      (case (tell object 'which-string)
		(initial '(top-bridge vertical-bridge))
		(modified '(top-bridge))
		(target (if %justify-mode%
			  '(vertical-bridge bottom-bridge)
			  '(vertical-bridge)))
		(answer '(bottom-bridge))))
	    (get-object () object)
	    (get-description-type () description-type)
	    (get-descriptor () descriptor)
	    (get-descriptor-activation () (tell descriptor 'get-activation))
	    (description-type? (type) (eq? type description-type))
	    (relevant? () (fully-active? description-type))
	    (get-conceptual-depth () (tell descriptor 'get-conceptual-depth))
	    (bond-description? ()
	      (or (eq? description-type plato-bond-category)
		  (eq? description-type plato-bond-facet)))
	    ;; Themes:
	    (get-thematic-compatibility () (tell self 'get-max-theme-support))
	    (get-max-theme-support ()
	      (maximum (tell self 'get-theme-support-values)))
	    (get-theme-support-values ()
	      (map (lambda (theme)
		     (if (eq? description-type (tell theme 'get-dimension))
		       (% (tell theme 'get-absolute-activation))
		       0))
		(tell *themespace* 'get-active-themes (tell self 'get-theme-types))))
	    (calculate-internal-strength ()
	      (tell descriptor 'get-conceptual-depth))
	    (calculate-external-strength ()
	      (average (tell self 'calculate-local-support)
		(tell description-type 'get-activation)))
	    (calculate-local-support ()
	      (let ((number-of-supporting-objects
		      (count (lambda (other-object)
			       (and (not (contains? object other-object))
				    (not (contains? other-object object))
				    (member? description-type
				      (tell-all (tell other-object 'get-descriptions)
					'get-description-type))))
			(remq object (tell string 'get-objects)))))
		(case number-of-supporting-objects
		  (0 0)
		  (1 20)
		  (2 60)
		  (3 90)
		  (else 100))))
	    (else (delegate msg workspace-structure))))))))


(define-codelet-procedure* bottom-up-description-scout
  (lambda ()
    (let* ((chosen-object (tell *workspace* 'choose-object 'get-average-salience))
	   (chosen-description
	     (tell chosen-object 'choose-relevant-description-by-activation)))
      (if* (not (exists? chosen-description))
	(say "Couldn't choose a description. Fizzling.")
	(fizzle))
      (let* ((chosen-descriptor (tell chosen-description 'get-descriptor))
	     (property-links (tell chosen-descriptor 'get-similar-property-links)))
	(if* (null? property-links)
	  (say "No short-enough property links. Fizzling.")
	  (fizzle))
	(let* ((properties (tell-all property-links 'get-to-node))
	       (property-activations (tell-all properties 'get-activation))
	       (degrees-of-assoc (tell-all property-links 'get-degree-of-assoc))
	       (chosen-property (stochastic-pick properties
				  (map * degrees-of-assoc property-activations))))
	  (propose-description
	    chosen-object (tell chosen-property 'get-category) chosen-property))))))


(define-codelet-procedure* top-down-description-scout
  (lambda (description-type scope)
    (if (workspace? scope)
      (say "Scope is entire Workspace.")
      (say "Focusing on " (tell scope 'generic-name) "..."))
    (let* ((chosen-object (tell scope 'choose-object 'get-average-salience))
	   (possible-descriptors
	     (tell description-type 'get-possible-descriptors chosen-object)))
      (if* (null? possible-descriptors)
	(say "Couldn't make description. Fizzling.")
	(fizzle))
      (let ((chosen-descriptor
	      (stochastic-pick-by-method possible-descriptors 'get-activation)))
	(propose-description chosen-object description-type chosen-descriptor)))))


(define-codelet-procedure* description-evaluator
  (lambda (proposed-description)
    (let ((descriptor (tell proposed-description 'get-descriptor)))
      (tell descriptor 'activate-from-workspace)
      (tell proposed-description 'update-strength)
      (let ((strength (tell proposed-description 'get-strength)))
	(stochastic-if* (1- (temp-adjusted-probability (% strength)))
	  (say "Description not strong enough. Fizzling.")
	  (fizzle))
	(tell proposed-description 'update-proposal-level %evaluated%)
	(post-codelet* urgency: strength description-builder proposed-description)))))


(define-codelet-procedure* description-builder
  (lambda (proposed-description)
    (let ((object (tell proposed-description 'get-object))
	  (description-type (tell proposed-description 'get-description-type))
	  (descriptor (tell proposed-description 'get-descriptor)))
      (if* (not (tell *workspace* 'object-exists? object))
	(say "This object no longer exists. Fizzling.")
	(fizzle))
      (if* (tell object 'description-present? proposed-description)
	(say "This description already exists. Fizzling.")
	(tell description-type 'activate-from-workspace)
	(tell descriptor 'activate-from-workspace)
	(fizzle))
      (build-description proposed-description))))


(define propose-description
  (lambda (object description-type descriptor)
    (let ((proposed-description (make-description object description-type descriptor)))
      (tell descriptor 'activate-from-workspace)
      (tell proposed-description 'update-proposal-level %proposed%)
      (post-codelet*
	urgency: (tell description-type 'get-activation)
	description-evaluator proposed-description))))


(define build-description
  (lambda (proposed-description)
    (let ((object (tell proposed-description 'get-object))
	  (description-type (tell proposed-description 'get-description-type))
	  (descriptor (tell proposed-description 'get-descriptor)))
      (if (tell proposed-description 'bond-description?)
	(tell object 'add-bond-description proposed-description)
	(tell object 'add-description proposed-description))
      (tell description-type 'activate-from-workspace)
      (tell descriptor 'activate-from-workspace)
      (tell proposed-description 'update-proposal-level %built%))))


(define descriptions-equal?
  (lambda (d1 d2)
    (and (eq? (tell d1 'get-description-type) (tell d2 'get-description-type))
	 (eq? (tell d1 'get-descriptor) (tell d2 'get-descriptor)))))


(define description-member?
  (lambda (d l)
    (and (not (null? l))
	 (or (descriptions-equal? d (1st l))
	     (description-member? d (rest l))))))
