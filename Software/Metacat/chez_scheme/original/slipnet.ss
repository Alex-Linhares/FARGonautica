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

(define %max-activation% 100)
(define %workspace-activation% 100)
(define %full-activation-threshold% 50)

(define make-slipnode
  (lambda (name-symbol short-name conceptual-depth)
    (let* ((activation 0)
	   (activation-buffer 0)
	   (frozen? #f)
	   (changed-frozen? #f)
	   (rate-of-decay #f)
	   (intrinsic-link-length 0)
	   (shrunk-link-length 0)
	   (top-down-codelet-types '())
	   (incoming-links '())
	   (category-links '())
	   (instance-links '())
	   (property-links '())
	   (lateral-links '())
	   (lateral-sliplinks '())
	   (links-labeled-by-node '())
	   (descriptor-predicate? (lambda (object) #f))
	   (full-lowercase-name
	    (string-downcase
	     (string-suffix (symbol->string name-symbol) 6)))
	   (full-uppercase-name (string-upcase full-lowercase-name))
	   (graphics-coord #f)
	   (graphics-label-coord #f))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'slipnode)
	    (get-name-symbol () name-symbol)
	    (get-lowercase-name () full-lowercase-name)
	    (get-uppercase-name () full-uppercase-name)
	    (get-short-name () short-name)
	    (get-CM-short-name ()
	      (if (eq? self plato-letter-category)
		;; This avoids occasional graphics problems with overlapping CMs:
		"LettCtgy"
		(tell self 'get-short-name)))
	    ;; This is solely for the purposes of (say <slipnode>):
	    (print-name () short-name)
	    (print () (printf "Slipnode \"~a\"~%" short-name))
	    (draw-activation-graphics (slipnet-window)
	      (tell slipnet-window 'draw-activation
		graphics-coord activation frozen?))
	    (reset ()
              (set! activation 0)
	      (set! activation-buffer 0)
	      (set! frozen? #f)
	      (set! changed-frozen? #f)
	      (set! rate-of-decay
		(1- (expt (% conceptual-depth) (/ %update-cycle-length% 15))))
	      'done)
	    (get-graphics-coord () graphics-coord)
	    (get-graphics-label-coord () graphics-label-coord)
	    (set-graphics-coord (coord) (set! graphics-coord coord) 'done)
	    (set-graphics-label-coord (coord) (set! graphics-label-coord coord) 'done)
	    (get-conceptual-depth () conceptual-depth)
	    (frozen? () frozen?)
	    (get-activation () activation)
	    (get-intrinsic-link-length () intrinsic-link-length)
	    (get-shrunk-link-length () shrunk-link-length)
	    (get-incoming-links () incoming-links)
	    (get-category-links () category-links)
	    (get-lateral-links () lateral-links)
	    (get-lateral-sliplinks () lateral-sliplinks)
	    (get-property-links () property-links)
	    (get-links-labeled-by-node () links-labeled-by-node)
	    (get-degree-of-assoc ()
	      (100- (if (fully-active? self) shrunk-link-length intrinsic-link-length)))

	    (category? () (not (null? instance-links)))
	    (instance? () (not (null? category-links)))

	    (get-category ()
	      (if (null? category-links)
		  #f
		  (tell (1st category-links) 'get-to-node)))

	    (get-outgoing-links ()
	      (append category-links instance-links property-links
		lateral-links lateral-sliplinks))

	    (get-instance-nodes ()
	      (tell-all instance-links 'get-to-node))

	    (get-similar-property-links ()
	      (filter (lambda (link)
			(prob? (temp-adjusted-probability
				 (% (tell link 'get-degree-of-assoc)))))
		property-links))

	    (get-related-node (relation)
	      (if (eq? relation plato-identity)
		self
		(let ((related-nodes
			(tell-all
			  (filter
			    (lambda (link) (eq? (tell link 'get-label-node) relation))
			    (tell self 'get-outgoing-links))
			  'get-to-node)))
		  (cond
		    ((null? related-nodes) #f)
		    ((null? (rest related-nodes)) (1st related-nodes))
		    (else (select
			    (lambda (node)
			      (eq? (tell node 'get-category) (tell self 'get-category)))
			    related-nodes))))))

	    (freeze ()
	      (set! frozen? #t)
	      (set! changed-frozen? #t)
	      'done)
	    (unfreeze ()
	      (set! frozen? #f)
	      (set! changed-frozen? #t)
	      'done)
	    (clamp (new-value)
	      (monitor-slipnode-activation-change self activation new-value)
	      (set! activation new-value)
	      (set! activation-buffer 0)
	      (set! frozen? #t)
	      (set! changed-frozen? #t)
	      'done)
	    (set-activation (new-value)
	      (if* (not frozen?)
		(set! activation new-value)
		(set! activation-buffer 0))
	      'done)
	    (update-activation (new-value)
	      (if* (not frozen?)
		(monitor-slipnode-activation-change self activation new-value)
		(set! activation new-value)
		(set! activation-buffer 0))
	      'done)
	    (increment-activation-buffer (delta)
	      (if* (not frozen?)
		(set! activation-buffer (+ activation-buffer delta)))
	      'done)
	    (decrement-activation-buffer (delta)
	      (if* (not frozen?)
		(set! activation-buffer (- activation-buffer delta)))
	      'done)
	    (flush-activation-buffer ()
	      (let ((new-value (min %max-activation% (+ activation activation-buffer))))
		(monitor-slipnode-activation-change self activation new-value)
		(set! activation new-value)
		(set! activation-buffer 0))
	      'done)
	    (activate-from-workspace ()
	      (tell self 'increment-activation-buffer %workspace-activation%)
	      'done)
	    (decay-activation ()
	      (let ((decay-amount (round (* rate-of-decay activation))))
		(tell self 'decrement-activation-buffer decay-amount))
	      'done)
	    (spread-activation ()
	      (for* each link in (tell self 'get-outgoing-links) do
		(let* ((to-node (tell link 'get-to-node))
		       (association (tell link 'get-intrinsic-degree-of-assoc))
		       (spread-amount
			 (round (* (/ %update-cycle-length% 15)
				   (% association)
				   activation))))
		  (tell to-node 'increment-activation-buffer spread-amount)))
	      'done)

	    (set-intrinsic-link-length (new-value)
	      (set! intrinsic-link-length new-value)
	      (set! shrunk-link-length (round (40% new-value)))
	      'done)

	    (define-descriptor-predicate (new-procedure)
	      (set! descriptor-predicate? new-procedure)
	      'done)

	    (possible-descriptor? (object)
	      (descriptor-predicate? object))

	    (description-possible? (object)
	      (not (null? (tell self 'get-possible-descriptors object))))

	    (get-possible-descriptors (object)
	      (filter-meth (tell-all instance-links 'get-to-node)
		'possible-descriptor? object))

	    (set-top-down-codelet-types codelet-types
	      (set! top-down-codelet-types codelet-types)
	      'done)

	    (attempt-to-post-top-down-codelets ()
	      (if* (above-threshold? self)
		(let ((urgency (* (% conceptual-depth) activation)))
		  (for* each codelet-type in top-down-codelet-types do
		    (stochastic-if* (post-codelet-probability codelet-type)
		      (repeat* (num-of-codelets-to-post codelet-type) times
			;; Scope for top-down codelets posted by
			;; active slipnodes is entire workspace:
			(tell *coderack* 'add-deferred-codelet
			  (tell codelet-type 'make-codelet
			    urgency self *workspace*)))))))
	      'done)

	    (add-to-incoming-links (new-link)
	      (set! incoming-links (cons new-link incoming-links))
	      'done)

	    (add-to-outgoing-links (link-type new-link)
	      (case link-type
		(category (set! category-links (cons new-link category-links)))
		(instance (set! instance-links (cons new-link instance-links)))
		(property (set! property-links (cons new-link property-links)))
		(lateral (set! lateral-links (cons new-link lateral-links)))
		(lateral-sliplink
		  (set! lateral-sliplinks (cons new-link lateral-sliplinks))))
	      'done)

	    (add-to-links-labeled-by-node (link)
	      (set! links-labeled-by-node (cons link links-labeled-by-node))
	      'done)

	    ;; slippage <desc1>==<label>==><desc2> is applicable to <node> iff:
	    ;;
	    ;; (1) <desc1> = <node>
	    ;;   In this case the slipped node is <desc2>
	    ;;
	    ;; (2) <node> has a sliplink with label
	    ;;   In this case the slipped node is the node related to <node> by <label>
	    ;;   (probability is a function of sliplink's current degree of association)
	    ;; Example:   a => b
	    ;;            |
	    ;;            z => ?
	    ;; first=(opp)=>last slippage applied to <successor> node
	    ;; should sometimes cause a slippage to <predecessor>.

	    (apply-slippages (slippages sliplog)
	      (cond
		((null? slippages) self)
		((eq? (tell (1st slippages) 'get-descriptor1) self)
		 (tell sliplog 'applied (1st slippages))
		 (tell (1st slippages) 'get-descriptor2))
		(else
		  ;; See if a coattail slippage can be made:
		  (let ((label (tell (1st slippages) 'get-label)))
		    (if (or (not (exists? label))
			    (eq? (tell (1st slippages) 'get-CM-type)
			         (tell self 'get-category)))
		      (tell self 'apply-slippages (rest slippages) sliplog)
		      (let ((sliplink (select-meth lateral-sliplinks 'labeled? label)))
			(if (and (exists? sliplink)
			         (prob? (coattail-slippage-probability
					  (1st slippages) label self sliplink)))
			  (let ((node2 (tell self 'get-related-node label)))
			    (tell sliplog 'coattail self label node2 (1st slippages))
			    node2)
			  (tell self 'apply-slippages (rest slippages) sliplog))))))))

	    (else (delegate msg base-object))))))))


(define coattail-slippage-probability
  (lambda (inducing-slippage inducing-slippage-label node sliplink)
    (% (tell sliplink 'get-degree-of-assoc))))


(define get-label
  (lambda (from-node to-node)
    (if (eq? from-node to-node)
      plato-identity
      (let ((link (select
		    (lambda (link) (eq? (tell link 'get-from-node) from-node))
		    (tell to-node 'get-incoming-links))))
	(if (exists? link)
	  (tell link 'get-label-node)
	  #f)))))


(define relationship-between
  (lambda (nodes)
    (if (all-exist? nodes)
      (let ((relations (adjacency-map get-label nodes)))
	(if (and (all-exist? relations) (all-same? relations))
	  (1st relations)
	  #f))
      #f)))


(define make-slipnet-link
  (lambda (from-node to-node link-type)
    (let ((label-node #f)
	  (fixed-length? #f)
	  (link-length 0)
	  (print-name
	    (format "~a-->~a"
	      (tell from-node 'get-lowercase-name)
	      (tell to-node 'get-lowercase-name))))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'slipnet-link)
	    (print-name () print-name)
	    (print ()
	      (printf "Sliplink ~a~n" print-name))
	    (get-link-type () link-type)
	    (get-from-node () from-node)
	    (get-to-node () to-node)
	    (get-label-node () label-node)
	    (get-link-length () link-length)
	    (get-intrinsic-degree-of-assoc ()
	      (if fixed-length?
		(100- link-length)
		(100- (tell label-node 'get-intrinsic-link-length))))
	    (get-degree-of-assoc ()
	      (if fixed-length?
		(100- link-length)
		(if (fully-active? label-node)
		  (100- (tell label-node 'get-shrunk-link-length))
		  (100- (tell label-node 'get-intrinsic-link-length)))))
	    (labeled? (node) (eq? label-node node))
	    (set-label-node (node)
	      (set! label-node node)
	      (tell label-node 'add-to-links-labeled-by-node self)
	      'done)
	    (set-link-length (new-length)
	      (set! link-length new-length)
	      (set! fixed-length? #t)
	      'done)
	    (else (delegate msg base-object))))))))


(define related?
  (lambda (node1 node2)
    (or (eq? node1 node2) (linked? node1 node2))))


(define linked?
  (lambda (node1 node2)
    (member? node2 (tell-all (tell node1 'get-outgoing-links) 'get-to-node))))


(define slip-linked?
  (lambda (node1 node2)
    (member? node2
      (tell-all (tell node1 'get-lateral-sliplinks) 'get-to-node))))


(define establish-link
  (lambda (top-level-name from-node to-node link-type)
    (let ((new-link (make-slipnet-link from-node to-node link-type)))
      (define-top-level-value top-level-name new-link)
      (tell from-node 'add-to-outgoing-links link-type new-link)
      (tell to-node 'add-to-incoming-links new-link)
      new-link)))


(define update-slipnet-activations
  (lambda ()
    (for* each theme in (tell *themespace* 'get-all-active-themes) do
      (tell theme 'spread-activation-to-slipnet))
    (for* each node in *slipnet-nodes* do
      (tell node 'decay-activation))
    (for* each node in (filter fully-active? *slipnet-nodes*) do
      (tell node 'spread-activation))
    (for* each node in *slipnet-nodes* do
      (tell node 'flush-activation-buffer))
    (for* each node in (filter partially-active? *slipnet-nodes*) do
      (stochastic-if* (^3 (% (tell node 'get-activation)))
	(tell node 'update-activation %max-activation%)))))


(define fully-active?
  (lambda (node)
    (= (tell node 'get-activation) %max-activation%)))


(define above-threshold?
  (lambda (node)
    (>= (tell node 'get-activation) %full-activation-threshold%)))


(define partially-active?
  (lambda (node)
    (and (above-threshold? node) (not (fully-active? node)))))


(define *slipnet-nodes*
  (slipnet-node-list*
   (plato-a "a" conceptual-depth: 10)
   (plato-b "b" conceptual-depth: 10)
   (plato-c "c" conceptual-depth: 10)
   (plato-d "d" conceptual-depth: 10)
   (plato-e "e" conceptual-depth: 10)
   (plato-f "f" conceptual-depth: 10)
   (plato-g "g" conceptual-depth: 10)
   (plato-h "h" conceptual-depth: 10)
   (plato-i "i" conceptual-depth: 10)
   (plato-j "j" conceptual-depth: 10)
   (plato-k "k" conceptual-depth: 10)
   (plato-l "l" conceptual-depth: 10)
   (plato-m "m" conceptual-depth: 10)
   (plato-n "n" conceptual-depth: 10)
   (plato-o "o" conceptual-depth: 10)
   (plato-p "p" conceptual-depth: 10)
   (plato-q "q" conceptual-depth: 10)
   (plato-r "r" conceptual-depth: 10)
   (plato-s "s" conceptual-depth: 10)
   (plato-t "t" conceptual-depth: 10)
   (plato-u "u" conceptual-depth: 10)
   (plato-v "v" conceptual-depth: 10)
   (plato-w "w" conceptual-depth: 10)
   (plato-x "x" conceptual-depth: 10)
   (plato-y "y" conceptual-depth: 10)
   (plato-z "z" conceptual-depth: 10)
   (plato-one "one" conceptual-depth: 30)
   (plato-two "two" conceptual-depth: 30)
   (plato-three "three" conceptual-depth: 30)
   (plato-four "four" conceptual-depth: 30)
   (plato-five "five" conceptual-depth: 30)
   (plato-leftmost "lmost" conceptual-depth: 40)
   (plato-rightmost "rmost" conceptual-depth: 40)
   (plato-middle "middle" conceptual-depth: 40)
   (plato-single "single" conceptual-depth: 40)
   (plato-whole "whole" conceptual-depth: 40)
   (plato-alphabetic-first "first" conceptual-depth: 60)
   (plato-alphabetic-last "last" conceptual-depth: 60)
   (plato-left "left" conceptual-depth: 40)
   (plato-right "right" conceptual-depth: 40)
   (plato-predecessor "pred" conceptual-depth: 50)
   (plato-successor "succ" conceptual-depth: 50)
   (plato-sameness "same" conceptual-depth: 80)
   (plato-predgrp "predgrp" conceptual-depth: 50)
   (plato-succgrp "succgrp" conceptual-depth: 50)
   (plato-samegrp "samegrp" conceptual-depth: 80)
   (plato-identity "Identity" conceptual-depth: 90)
   (plato-opposite "Opposite" conceptual-depth: 90)
   (plato-letter "letter" conceptual-depth: 20)
   (plato-group "group" conceptual-depth: 80)
   (plato-letter-category "LetterCtgy" conceptual-depth: 30)
   (plato-string-position-category "StringPos" conceptual-depth: 70)
   (plato-alphabetic-position-category "AlphaPos" conceptual-depth: 80)
   (plato-direction-category "Direction" conceptual-depth: 70)
   (plato-bond-category "BondCtgy" conceptual-depth: 80)
   (plato-group-category "GroupCtgy" conceptual-depth: 80)
   (plato-length "Length" conceptual-depth: 60)
   (plato-object-category "ObjectCtgy" conceptual-depth: 90)
   (plato-bond-facet "BondFacet" conceptual-depth: 90)))


(define *slipnet-letters*
  (list plato-a plato-b plato-c plato-d plato-e plato-f plato-g plato-h plato-i
	plato-j plato-k plato-l plato-m plato-n plato-o plato-p plato-q plato-r
	plato-s plato-t plato-u plato-v plato-w plato-x plato-y plato-z))

(define *slipnet-numbers*	
  (list plato-one plato-two plato-three plato-four plato-five))

(define *top-down-slipnodes*
  (list plato-left plato-right plato-predecessor plato-successor
	plato-sameness plato-predgrp plato-succgrp plato-samegrp
	plato-string-position-category plato-alphabetic-position-category
	plato-length))

(define *initially-clamped-slipnodes*
  (list plato-letter-category plato-string-position-category))

(define platonic-letter?
  (lambda (node)
    (member? node *slipnet-letters*)))

(define platonic-number?
  (lambda (node)
    (member? node *slipnet-numbers*)))

(define number->platonic-number
  (lambda (n)
    (if (> n (length *slipnet-numbers*))
      #f
      (nth (- n 1) *slipnet-numbers*))))

(define platonic-number->number
  (lambda (node)
    (add1 (list-index *slipnet-numbers* node))))

(define platonic-relation?
  (lambda (node)
    (or (eq? node plato-identity)
	(eq? node plato-opposite)
	(eq? node plato-predecessor)
	(eq? node plato-successor))))

(define platonic-literal?
  (compose not platonic-relation?))

(define inverse
  (lambda (node)
    (cond
     ((eq? node plato-identity) plato-identity)
     ((exists? node) (tell node 'get-related-node plato-opposite))
     (else #f))))


;; Attach top-down codelet-types to top-down slipnodes.  This can only
;; be done after *codelet-types* has been defined.

(for* each node in (list plato-left plato-right) do
  (tell node 'set-top-down-codelet-types
    top-down-bond-scout:direction
    top-down-group-scout:direction))

(for* each node in (list plato-predecessor plato-successor plato-sameness) do
  (tell node 'set-top-down-codelet-types
    top-down-bond-scout:category))

(for* each node in (list plato-predgrp plato-succgrp plato-samegrp) do
  (tell node 'set-top-down-codelet-types
    top-down-group-scout:category))

(for* each node in (list plato-string-position-category
		         plato-alphabetic-position-category
			 plato-length) do
  (tell node 'set-top-down-codelet-types
    top-down-description-scout))

;; Set intrinsic-link-length values for the slipnodes
;; that serve as label nodes for certain slipnet links.

(tell plato-predecessor 'set-intrinsic-link-length 60)
(tell plato-successor 'set-intrinsic-link-length 60)
(tell plato-sameness 'set-intrinsic-link-length 0)
(tell plato-identity 'set-intrinsic-link-length 0)
(tell plato-opposite 'set-intrinsic-link-length 80)

;; Define possible-descriptor? predicate for slipnodes
;; that can be used as descriptors for objects.

(tell plato-one 'define-descriptor-predicate
  (lambda (object)
    (and (group? object) (= (tell object 'get-group-length) 1))))

(tell plato-two 'define-descriptor-predicate
  (lambda (object)
    (and (group? object) (= (tell object 'get-group-length) 2))))

(tell plato-three 'define-descriptor-predicate
  (lambda (object)
    (and (group? object) (= (tell object 'get-group-length) 3))))

(tell plato-four 'define-descriptor-predicate
  (lambda (object)
    (and (group? object) (= (tell object 'get-group-length) 4))))

(tell plato-five 'define-descriptor-predicate
  (lambda (object)
    (and (group? object) (= (tell object 'get-group-length) 5))))

(tell plato-leftmost 'define-descriptor-predicate
  (lambda (object)
    (and (not (tell object 'string-spanning-group?))
         (tell object 'leftmost-in-string?))))

(tell plato-rightmost 'define-descriptor-predicate
  (lambda (object)
    (and (not (tell object 'string-spanning-group?))
         (tell object 'rightmost-in-string?))))

(tell plato-middle 'define-descriptor-predicate
  (lambda (object)
    (tell object 'middle-in-string?)))

(tell plato-single 'define-descriptor-predicate
  (lambda (object)
    (and (letter? object) (tell object 'spans-whole-string?))))

(tell plato-whole 'define-descriptor-predicate
  (lambda (object)
    (tell object 'string-spanning-group?)))

(tell plato-alphabetic-first 'define-descriptor-predicate
  (lambda (object)
    (eq? (tell object 'get-descriptor-for plato-letter-category) plato-a)))

(tell plato-alphabetic-last 'define-descriptor-predicate
  (lambda (object)
    (eq? (tell object 'get-descriptor-for plato-letter-category) plato-z)))

(tell plato-letter 'define-descriptor-predicate letter?)

(tell plato-group 'define-descriptor-predicate group?)
			  


;;  SUCCESSOR and PREDECESSOR links

(lateral-link* a --> b label: successor)
(lateral-link* b --> c label: successor)
(lateral-link* c --> d label: successor)
(lateral-link* d --> e label: successor)
(lateral-link* e --> f label: successor)
(lateral-link* f --> g label: successor)
(lateral-link* g --> h label: successor)
(lateral-link* h --> i label: successor)
(lateral-link* i --> j label: successor)
(lateral-link* j --> k label: successor)
(lateral-link* k --> l label: successor)
(lateral-link* l --> m label: successor)
(lateral-link* m --> n label: successor)
(lateral-link* n --> o label: successor)
(lateral-link* o --> p label: successor)
(lateral-link* p --> q label: successor)
(lateral-link* q --> r label: successor)
(lateral-link* r --> s label: successor)
(lateral-link* s --> t label: successor)
(lateral-link* t --> u label: successor)
(lateral-link* u --> v label: successor)
(lateral-link* v --> w label: successor)
(lateral-link* w --> x label: successor)
(lateral-link* x --> y label: successor)
(lateral-link* y --> z label: successor)

(lateral-link* z --> y label: predecessor)
(lateral-link* y --> x label: predecessor)
(lateral-link* x --> w label: predecessor)
(lateral-link* w --> v label: predecessor)
(lateral-link* v --> u label: predecessor)
(lateral-link* u --> t label: predecessor)
(lateral-link* t --> s label: predecessor)
(lateral-link* s --> r label: predecessor)
(lateral-link* r --> q label: predecessor)
(lateral-link* q --> p label: predecessor)
(lateral-link* p --> o label: predecessor)
(lateral-link* o --> n label: predecessor)
(lateral-link* n --> m label: predecessor)
(lateral-link* m --> l label: predecessor)
(lateral-link* l --> k label: predecessor)
(lateral-link* k --> j label: predecessor)
(lateral-link* j --> i label: predecessor)
(lateral-link* i --> h label: predecessor)
(lateral-link* h --> g label: predecessor)
(lateral-link* g --> f label: predecessor)
(lateral-link* f --> e label: predecessor)
(lateral-link* e --> d label: predecessor)
(lateral-link* d --> c label: predecessor)
(lateral-link* c --> b label: predecessor)
(lateral-link* b --> a label: predecessor)

(lateral-link* one --> two label: successor)
(lateral-link* two --> three label: successor)
(lateral-link* three --> four label: successor)
(lateral-link* four --> five label: successor)

(lateral-link* five --> four label: predecessor)
(lateral-link* four --> three label: predecessor)
(lateral-link* three --> two label: predecessor)
(lateral-link* two --> one label: predecessor)



;;  LETTER-CATEGORY links

(instance-link*
   letter-category --> (a b c d e f g h i j k l m n o p q r s t u v w x y z)
   all-lengths: 97)

(category-link*
   (a b c d e f g h i j k l m n o p q r s t u v w x y z) --> letter-category
   all-lengths: 0)

(let ((letter-category-depth (cd plato-letter-category)))
  (for* each letter in *slipnet-letters* do
    (tell (1st (tell letter 'get-category-links))
      'set-link-length (- letter-category-depth (cd letter)))))

(lateral-link* samegrp --> letter-category length: 50)



;;  LENGTH links

(instance-link* length --> (one two three four five) all-lengths: 100)
(category-link* (one two three four five) --> length all-lengths: 0)

(let ((length-depth (cd plato-length)))
  (for* each number in *slipnet-numbers* do
    (tell (1st (tell number 'get-category-links))
      'set-link-length (- length-depth (cd number)))))

(lateral-link* predgrp --> length length: 95)
(lateral-link* succgrp --> length length: 95)
(lateral-link* samegrp --> length length: 95)



;;  OPPOSITE links

(lateral-sliplink* alphabetic-first <--> alphabetic-last label: opposite)
(lateral-sliplink* leftmost <--> rightmost label: opposite)
(lateral-sliplink* left <--> right label: opposite)
(lateral-sliplink* successor <--> predecessor label: opposite)
(lateral-sliplink* predgrp <--> succgrp label: opposite)



;;  PROPERTY links

(property-link* a --> alphabetic-first length: 75)
(property-link* z --> alphabetic-last length: 75)



;;  OBJECT-CATEGORY links

(instance-link* object-category --> letter length: 100)
(category-link* letter --> object-category
   length: (- (cd plato-object-category) (cd plato-letter)))

(instance-link* object-category --> group length: 100)
(category-link* group --> object-category
   length: (- (cd plato-object-category) (cd plato-group)))



;;  STRING-POSITION-CATEGORY links

(instance-link* string-position-category --> leftmost length: 100)
(category-link* leftmost --> string-position-category
   length: (- (cd plato-string-position-category)
	      (cd plato-leftmost)))

(instance-link* string-position-category --> rightmost length: 100)
(category-link* rightmost --> string-position-category
   length: (- (cd plato-string-position-category)
	      (cd plato-rightmost)))

(instance-link* string-position-category --> middle length: 100)
(category-link* middle --> string-position-category
   length: (- (cd plato-string-position-category)
	      (cd plato-middle)))

(instance-link* string-position-category --> single length: 100)
(category-link* single --> string-position-category
   length: (- (cd plato-string-position-category)
	      (cd plato-single)))

(instance-link* string-position-category --> whole length: 100)
(category-link* whole --> string-position-category
   length: (- (cd plato-string-position-category)
	      (cd plato-whole)))



;;  ALPHABETIC-POSITION-CATEGORY links

(instance-link* alphabetic-position-category --> alphabetic-first length: 100)
(category-link* alphabetic-first --> alphabetic-position-category
   length: (- (cd plato-alphabetic-position-category)
	      (cd plato-alphabetic-first)))

(instance-link* alphabetic-position-category --> alphabetic-last length: 100)
(category-link* alphabetic-last --> alphabetic-position-category
   length: (- (cd plato-alphabetic-position-category)
	      (cd plato-alphabetic-last)))



;;  DIRECTION-CATEGORY links

(instance-link* direction-category --> left length: 100)
(category-link* left --> direction-category
   length: (- (cd plato-direction-category)
	      (cd plato-left)))

(instance-link* direction-category --> right length: 100)
(category-link* right --> direction-category
   length: (- (cd plato-direction-category)
	      (cd plato-right)))



;;  BOND-CATEGORY links

(instance-link* bond-category --> predecessor length: 100)
(category-link* predecessor --> bond-category
   length: (- (cd plato-bond-category)
	      (cd plato-predecessor)))

(instance-link* bond-category --> successor length: 100)
(category-link* successor --> bond-category
   length: (- (cd plato-bond-category)
	      (cd plato-successor)))

(instance-link* bond-category --> sameness length: 100)
(category-link* sameness --> bond-category
   length: (- (cd plato-bond-category)
	      (cd plato-sameness)))



;;  GROUP-CATEGORY links

(instance-link* group-category --> predgrp length: 100)
(category-link* predgrp --> group-category
   length: (- (cd plato-group-category)
	      (cd plato-predgrp)))

(instance-link* group-category --> succgrp length: 100)
(category-link* succgrp --> group-category
   length: (- (cd plato-group-category)
	      (cd plato-succgrp)))

(instance-link* group-category --> samegrp length: 100)
(category-link* samegrp --> group-category
   length: (- (cd plato-group-category)
	      (cd plato-samegrp)))



;;  ASSOCIATED GROUP links

(lateral-link* sameness --> samegrp length: 30 label: group-category)
(lateral-link* successor --> succgrp length: 60 label: group-category)
(lateral-link* predecessor --> predgrp length: 60 label: group-category)



;;  ASSOCIATED BOND-CATEGORY links

(lateral-link* samegrp --> sameness length: 90 label: bond-category)
(lateral-link* succgrp --> successor length: 90 label: bond-category)
(lateral-link* predgrp --> predecessor length: 90 label: bond-category)



;;  BOND-FACET links

(instance-link* bond-facet --> letter-category length: 100)
(category-link* letter-category --> bond-facet
   length: (- (cd plato-bond-facet) (cd plato-letter-category)))

(instance-link* bond-facet --> length length: 100)
(category-link* length --> bond-facet
   length: (- (cd plato-bond-facet) (cd plato-length)))



;;  LETTER-CATEGORY-LENGTH links

(lateral-sliplink* letter-category <--> length length: 95)



;;  LETTER-GROUP links

(lateral-sliplink* letter <--> group length: 90)



;;  DIRECTION-POSITION, DIRECTION-NEIGHBOR, and POSITION-NEIGHBOR links

;; Copycat conceptual problem:  For abc -> ...; cba -> ?, if the a's in the
;; initial and target strings get described as "first" ("alphabetic-first", that is),
;; then a first=>first mapping is almost certain for any bridge between them.
;; However, this makes an a-->a bridge incompatible with a symmetric c-->c
;; bridge because the CMs {rmost=>lmost, first=>first} are incompatible.
;; This helps out in the xyz problem, but it is a big hindrence here.  Really these
;; CMs should be simply "non-supporting", rather than "incompatible".  We want
;; {rmost=>lmost, right=>right} to be incompatible, {rmost=>lmost, first=>first}
;; to be non-supporting but compatible, and {rmost=>lmost, first=>last} or
;; {rmost=>lmost, left=>right} to be supporting and compatible.  Unfortunately,
;; given the way links between these nodes are originally defined in the Slipnet,
;; this isn't possible.  The following is designed to remedy this.  This may change
;; the behavior of the program significantly, at least on problems like xyz.
;; Now {first=>first, lmost=>rmost} will be neither supporting-CMs nor
;; incompatible CMs.  However, {first=>last, lmost=>rmost} will still be supporting,
;; and {lmost=>rmost, left=>left} will still be incompatible, as they should be:

(lateral-link* leftmost <--> left length: 90 label: identity)
(lateral-link* leftmost <--> right length: 100 label: opposite)
(lateral-link* rightmost <--> left length: 100 label: opposite)
(lateral-link* rightmost <--> right length: 90 label: identity)
(lateral-link* alphabetic-first <--> leftmost length: 100)
(lateral-link* alphabetic-first <--> rightmost length: 100)
(lateral-link* alphabetic-last <--> leftmost length: 100)
(lateral-link* alphabetic-last <--> rightmost length: 100)



;;  OTHER LINKS

(lateral-sliplink* single <--> whole length: 90)
