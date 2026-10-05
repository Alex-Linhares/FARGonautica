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

(define make-concept-mapping
  (lambda (object1 description-type1 descriptor1
	    object2 description-type2 descriptor2)
    (let* ((label (get-label descriptor1 descriptor2))
	   (identity? (eq? descriptor1 descriptor2))
	   (slipnet-link
	     ;; slipnet-link is usually a lateral-sliplink; however, it can be a
	     ;; lateral-link in the case of LettCtgy/Length pred/succ "slippages"
	     ;; such as LettCtgy:a=(succ)=>b or Length:two=(pred)=>one:
	     (if identity?
	       #f
	       (select
		 (lambda (link) (eq? (tell link 'get-to-node) descriptor2))
		 (if (or (eq? description-type1 plato-letter-category)
		         (eq? description-type1 plato-length))
		   (tell descriptor1 'get-lateral-links)
		   (tell descriptor1 'get-lateral-sliplinks)))))
	   (print-name
	     (string-append
	       (tell descriptor1 'get-CM-short-name)
	       "=>" (tell descriptor2 'get-CM-short-name)))
	   (english-name
	     (string-append
	       (tell descriptor1 'get-lowercase-name) " <=> "
	       (tell descriptor2 'get-lowercase-name)))
	   (graphics-pexp #f)
	   (previously-relevant? #f))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'concept-mapping)
	    (get-object1 () object1)
	    (get-object2 () object2)
	    (print-name () print-name)
	    (english-name () english-name)
	    (long-name ()
	      (format "~a ~a:~a~a~a"
		(cond
		  (identity? "CM")
		  ((eq? object1 'coattail) "COATTAIL SLIPPAGE")
		  (else "SLIPPAGE"))
		(tell description-type1 'get-short-name)
		(tell descriptor1 'get-short-name)
		(if (or (not (exists? label)) (eq? label plato-identity))
		  "=>"
		  (format "=(~a)=>" (tell label 'get-short-name)))
		(tell descriptor2 'get-short-name)))
	    (print ()
	      (printf "~a~n" (tell self 'long-name)))
	    (get-graphics-pexp () graphics-pexp)
	    (set-graphics-pexp (pexp) (set! graphics-pexp pexp) 'done)
	    (previously-relevant? () previously-relevant?)
	    (update-previously-relevant? (new-value)
              (set! previously-relevant? new-value)
	      'done)
	    ;; could use description-type2 here instead:
	    (get-CM-type () description-type1)
	    (get-slipnet-link () slipnet-link)
	    (CM-type? (description-type)
	      (eq? description-type1 description-type))
	    (bond-concept-mapping? ()
	      (or (eq? description-type1 plato-bond-category)
		  (eq? description-type1 plato-bond-facet)))
	    ;; Used in deciding whether to re-perceive spanning groups as flipped:
	    (reversible-CM-type? ()
	      (or (eq? description-type1 plato-direction-category)
		  (eq? description-type1 plato-bond-category)
		  (eq? description-type1 plato-group-category)))
	    (get-descriptor1 () descriptor1)
	    (get-descriptor2 () descriptor2)
	    (get-label () label)
	    ;; Previously defined as (exists? sliplink).  However, this
	    ;; fails to classify a=(succ)=>b type CMs as slippages.
	    ;; Since CMs can now be of the form LettCtgy:a=(succ)=>b
	    ;; (but only in a horizontal-bridge's concept-mappings list),
	    ;; defining 'slippage? as (exists? sliplink) won't suffice.
	    ;; (not identity?) will classify CMs as slippages whenever
	    ;; desc1 != desc2.  This includes ObjCtgy:let=>grp slippages
	    ;; too (i.e., non-labeled concept-mappings):
	    (slippage? () (not identity?))
	    (identity? () identity?)
	    (opposite-mapping? () (eq? label plato-opposite))
	    (identity/opposite-mapping? ()
	      (or (eq? label plato-identity)
		  (eq? label plato-opposite)
		  (and (eq? descriptor1 plato-whole) (eq? descriptor2 plato-single))
		  (and (eq? descriptor1 plato-single) (eq? descriptor2 plato-whole))))
	    (relevant? ()
	      (and (fully-active? description-type1)
		   (fully-active? description-type2)))
	    (distinguishing? ()
	      (and (not (and identity? (eq? descriptor1 plato-whole)))
		   (tell object1 'distinguishing-descriptor? descriptor1)
		   (tell object2 'distinguishing-descriptor? descriptor2)))
	    (relevant-distinguishing? ()
	      (and (tell self 'relevant?) (tell self 'distinguishing?)))
	    (distinguishing-identity/opposite? ()
	      (and (tell self 'distinguishing?)
		   (tell self 'identity/opposite-mapping?)))
	    (get-degree-of-assoc ()
	      (cond
		(identity? 100)
		((exists? slipnet-link) (tell slipnet-link 'get-degree-of-assoc))
		;; Else the CM is an unlabeled LettCtgy/Length "slippage"
		;; such as LettCtgy:m=>j or Length:one=>three:
		(else 5)))
	    (get-conceptual-depth ()
	      (average
		(tell descriptor1 'get-conceptual-depth)
		(tell descriptor2 'get-conceptual-depth)))
	    (get-strength ()
	      (let ((degree-of-assoc (tell self 'get-degree-of-assoc)))
		(if (= degree-of-assoc 100)
		  100
		  (round (* degree-of-assoc
			    (+ 1 (^2 (% (tell self 'get-conceptual-depth)))))))))
	    (get-slippability ()
	      (let ((degree-of-assoc (tell self 'get-degree-of-assoc)))
		(if (= degree-of-assoc 100)
		  100
		  (round (* degree-of-assoc
			    (1- (^2 (% (tell self 'get-conceptual-depth)))))))))
	    (get-concept-pattern ()
	      (compress
		(list 'concepts
		  (list description-type1 %max-activation%)
		  (list descriptor1 %max-activation%)
		  (list descriptor2 %max-activation%)
		  (if (exists? label)
		    (list label %max-activation%)
		    #f))))
	    (symmetric? (cm)
	      (and (eq? (tell cm 'get-descriptor1) descriptor2)
		   (eq? (tell cm 'get-descriptor2) descriptor1)))
	    (symmetric-mapping ()
	      (if identity?
		self
		(make-concept-mapping
		  object1 description-type2 descriptor2
		  object2 description-type1 descriptor1)))
	    (activate-descriptions ()
	      (tell description-type1 'activate-from-workspace)
	      (tell descriptor1 'activate-from-workspace)
	      (tell description-type2 'activate-from-workspace)
	      (tell descriptor2 'activate-from-workspace))
	    (activate-label ()
	      (if* (exists? label)
		(tell label 'activate-from-workspace)
		;; Do this so that the activation of the label node will show up
		;; in the trace before the concept-mapping that caused it:
		(tell label 'flush-activation-buffer))
	      'done)
	    (else (delegate msg base-object))))))))


(define CMs-equal?
  (lambda (cm1 cm2)
    (and (eq? (tell cm1 'get-descriptor1) (tell cm2 'get-descriptor1))
	 (eq? (tell cm1 'get-descriptor2) (tell cm2 'get-descriptor2)))))


(define remove-duplicate-CMs
  (remove-duplicates-pred CMs-equal?))
