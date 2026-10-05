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

(define %max-theme-activation% 100)
(define %dominant-theme-margin% 90)
(define %theme-spread-amount% 20)
(define %theme-boost-amount% 7)
(define %theme-decay-amount% 25)

;; Intra-cluster theme weights (< 0 = inhibitory, > 0 = excitatory):
(define negative->negative-weight 0)
(define negative->positive-weight +25)
(define positive->negative-weight -75)
(define positive->positive-weight -2)
(define self->self-weight +10)


(define clip-positive
  (clip-function 0 %max-theme-activation%))

(define clip-negative
  (clip-function (- %max-theme-activation%) 0))


(define make-themespace
  (lambda ()
    (let* ((dimensions (filter-meth *slipnet-nodes* 'category?))
	   (top-clusters
	     (map (make-theme-cluster 'top-bridge) dimensions))
	   (bottom-clusters
	     (map (make-theme-cluster 'bottom-bridge) dimensions))
	   (vertical-clusters
	     (map (make-theme-cluster 'vertical-bridge) dimensions))
	   (all-clusters
	     (append top-clusters bottom-clusters vertical-clusters))
	   (all-themes '())
	   ;; active-theme-types are those currently exerting thematic pressure:
	   (active-theme-types '())
	   (stored-current-state #f))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'themespace)
	    (print ()
	      (printf "TOP BRIDGE THEMES:~%")
	      (print top-clusters)
	      (printf "BOTTOM BRIDGE THEMES:~%")
	      (print bottom-clusters)
	      (printf "VERTICAL BRIDGE THEMES:~%")
	      (print vertical-clusters))
	    (current-state-displayed? () (not (exists? stored-current-state)))
	    (save-current-state ()
	      (set! stored-current-state (tell self 'get-complete-state))
	      'done)
	    (restore-current-state ()
	      (if* (exists? stored-current-state)
		(tell self 'restore-state stored-current-state)
		(set! stored-current-state #f))
	      'done)
	    (get-complete-state ()
	      (list
		'(top-bridge bottom-bridge vertical-bridge)
		active-theme-types
		(map (lambda (theme)
		       (list
			 (tell theme 'get-theme-type)
			 (tell theme 'get-dimension)
			 (tell theme 'get-relation)
			 (tell theme 'get-activation)
			 (tell theme 'individually-frozen?)))
		  all-themes)
		(map (lambda (cluster)
		       (list
			 (tell cluster 'get-theme-type)
			 (tell cluster 'get-dimension)
			 (tell cluster 'frozen?)))
		  all-clusters)))
	    (get-partial-state (theme-type)
	      (let ((complete-state (tell self 'get-complete-state))
		    (relevant-info? (lambda (info) (eq? (1st info) theme-type))))
		(list
		  (list theme-type)
		  (intersect (list theme-type) (2nd complete-state))
		  (filter relevant-info? (3rd complete-state))
		  (filter relevant-info? (4th complete-state)))))
	    (restore-state (state)
	      (let ((all-theme-types (1st state))
		    (active-theme-types (2nd state))
		    (theme-info (3rd state))
		    (cluster-info (4th state)))
		(for* each theme-type in all-theme-types do
		  (tell self 'delete-theme-type theme-type)
		  (tell self 'unfreeze-theme-type theme-type)
		  (tell self 'thematic-pressure-off theme-type))
		(for* each theme-type in active-theme-types do
		  (tell self 'thematic-pressure-on theme-type))
		(for* each entry in theme-info do
		  (let ((type (1st entry))
			(dim (2nd entry))
			(rel (3rd entry))
			(act (4th entry))
			(frozen? (5th entry)))
		    (tell self 'set-theme-activation type dim rel act)
		    (if* frozen? (tell self 'freeze-theme type dim rel))))
		(for* each entry in cluster-info do
		  (let ((type (1st entry))
			(dim (2nd entry))
			(frozen? (3rd entry)))
		    (if* frozen? (tell self 'freeze-theme-cluster type dim)))))
	      'done)
	    (initialize ()
	      (set! stored-current-state #f)
	      (tell self 'delete-everything)
	      (tell self 'unfreeze-everything)
	      (tell self 'thematic-pressure-off))
	    (get-possible-theme-types ()
	      (if %justify-mode%
		'(top-bridge bottom-bridge vertical-bridge)
		'(top-bridge vertical-bridge)))
	    (get-active-theme-types () active-theme-types)
	    (get-active-bridge-theme-types ()
	      (intersect
		'(top-bridge bottom-bridge vertical-bridge)
		(tell self 'get-active-theme-types)))
	    (thematic-pressure? types
	      (if (null? types)
		(not (null? active-theme-types))
		(andmap
		  (lambda (type) (member? type active-theme-types))
		  types)))
	    (thematic-pressure-on types
	      (if (null? types)
		(set! active-theme-types (tell self 'get-possible-theme-types))
		(for* each type in types do
		  (tell self 'set-thematic-pressure type #t)))
	      (tell *themespace-window* 'update-thematic-pressure)
	      'done)
	    (thematic-pressure-off types
	      (if (null? types)
		(set! active-theme-types '())
		(for* each type in types do
		  (tell self 'set-thematic-pressure type #f)))
	      (tell *themespace-window* 'update-thematic-pressure)
	      'done)
	    (set-thematic-pressure (type switch)
	      (if* (not (eq? switch (tell self 'thematic-pressure? type)))
		(set! active-theme-types
		  ((if switch cons remq) type active-theme-types)))
	      'done)
	    (supported-by-active-theme? (concept-mapping bridge)
	      (let* ((theme-type (tell bridge 'get-theme-type))
		     (theme (tell self 'get-theme theme-type
			      (tell concept-mapping 'get-CM-type)
			      (tell concept-mapping 'get-label))))
		(and (exists? theme)
		     (tell self 'thematic-pressure? theme-type)
		     (tell theme 'dominant?))))
	    (get-all-themes () all-themes)
	    ;; theme-type/s can be either a symbol or a list:
	    (get-themes (theme-type/s)
	      (filter-meth all-themes 'theme-type? theme-type/s))
	    (get-all-active-themes ()
	      (tell self 'get-themes active-theme-types))
	    (get-active-themes (theme-type/s)
	      (cond
		((pair? theme-type/s)
		 (tell self 'get-themes (intersect theme-type/s active-theme-types)))
		((member? theme-type/s active-theme-types)
		 (tell self 'get-themes theme-type/s))
		(else '())))
	    (get-theme (theme-type dimension relation)
	      (tell (tell self 'get-cluster theme-type dimension)
		'get-theme relation))
	    (get-clusters (theme-type)
	      (case theme-type
		(top-bridge top-clusters)
		(bottom-bridge bottom-clusters)
		(vertical-bridge vertical-clusters)))
	    (get-cluster (theme-type dimension)
	      (select-meth (tell self 'get-clusters theme-type) 'dimension? dimension))
	    (get-dimensions () dimensions)
	    (get-relations (theme-type dimension)
	      (tell (tell self 'get-cluster theme-type dimension) 'get-relations))
	    (get-complete-theme-pattern (theme-type)
	      (cons theme-type
		(flatmap
		  (lambda (cluster)
		    (map (lambda (theme)
			   (list
			     (tell theme 'get-dimension)
			     (tell theme 'get-relation)
			     (tell theme 'get-activation)))
		      (tell cluster 'get-themes)))
		  (tell self 'get-clusters theme-type))))
	    (get-dominant-theme-pattern (theme-type)
	      (cons theme-type
		(map-compress
		  (lambda (cluster)
		    (let ((dominant-theme (tell cluster 'get-dominant-theme)))
		      (if (exists? dominant-theme)
			(list
			  (tell dominant-theme 'get-dimension)
			  (tell dominant-theme 'get-relation))
			#f)))
		  (tell self 'get-clusters theme-type))))
	    ;; ignores all themes with activation 0
	    (get-nonzero-theme-pattern (theme-type)
	      (cons theme-type
		(filter-out
		  (lambda (entry) (and (= (length entry) 3) (= 0 (3rd entry))))
		  (entries (tell self 'get-complete-theme-pattern theme-type)))))
	    (get-all-complete-theme-patterns ()
	      (map
		(lambda (theme-type)
		  (tell self 'get-complete-theme-pattern theme-type))
		(tell self 'get-possible-theme-types)))
	    (get-all-dominant-theme-patterns ()
	      (map
		(lambda (theme-type)
		  (tell self 'get-dominant-theme-pattern theme-type))
		(tell self 'get-possible-theme-types)))
	    (get-percentage-of-dominant-themes ()
	      (let ((clusters
		      (if %justify-mode%
			all-clusters
			(append top-clusters vertical-clusters))))
		(/ (count-meth clusters 'dominant-theme?) (length clusters))))
	    (theme-present? (theme)
	      (exists? (tell self 'get-equivalent-theme theme)))
	    (get-equivalent-theme (theme)
	      (select-meth all-themes 'equal? theme))
	    (get-max-positive-theme-activation (theme-type/s)
	      (maximum (tell-all (tell self 'get-themes theme-type/s)
			 'get-positive-activation)))
	    ;;--------------------------------------------------------------------
	    ;; set-theme-activation adds a new theme if one doesn't exist or
	    ;; changes the activation of an existing theme, without affecting the
	    ;; frozen/unfrozen status of the theme or the theme's cluster.
	    (set-theme-activation (theme-type dimension relation activation)
	      (let ((theme (tell self 'add-theme-unconditionally
			     theme-type dimension relation)))
		(if* (exists? theme)
		  (tell theme 'set-activation activation)
		  (tell (tell theme 'get-cluster) 'update-dominant-theme)
		  (tell *themespace-window* 'update-graphics)))
	      'done)
	    (set-theme-cluster-activations (theme-type dimension activation)
	      (let ((cluster (tell self 'get-cluster theme-type dimension)))
		(for* each relation in (tell cluster 'get-relations) do
		  (tell self 'set-theme-activation
		    theme-type dimension relation activation)))
	      'done)
	    (set-theme-type-activations (theme-type activation)
	      (for* each cluster in (tell self 'get-clusters theme-type) do
		(tell self 'set-theme-cluster-activations
		  theme-type (tell cluster 'get-dimension) activation))
	      'done)
	    (set-all-theme-activations (activation)
	      (tell self 'set-theme-type-activations 'top-bridge activation)
	      (tell self 'set-theme-type-activations 'bottom-bridge activation)
	      (tell self 'set-theme-type-activations 'vertical-bridge activation))
	    ;;--------------------------------------------------------------------
	    (theme-frozen? (theme-type dimension relation)
	      (let ((theme (tell self 'get-theme theme-type dimension relation)))
		(and (exists? theme) (tell theme 'frozen?))))
	    (cluster-frozen? (theme-type dimension)
	      (tell (tell self 'get-cluster theme-type dimension) 'frozen?))
	    (theme-type-frozen? (theme-type)
	      (andmap-meth (tell self 'get-clusters theme-type) 'frozen?))
	    (everything-frozen? ()
	      (andmap
		(lambda (type) (tell self 'theme-type-frozen? type))
		(tell self 'get-possible-theme-types)))
	    ;;--------------------------------------------------------------------
	    (freeze-theme (theme-type dimension relation)
	      (let ((theme (tell self 'get-theme theme-type dimension relation)))
		(if* (exists? theme)
		  (tell theme 'freeze)))
	      'done)
	    ;; freeze-theme-cluster freezes an entire cluster.  As long as a
	    ;; cluster remains frozen, no new themes can be added to it.
	    (freeze-theme-cluster (theme-type dimension)
	      (tell (tell self 'get-cluster theme-type dimension) 'freeze))
	    (freeze-theme-type (theme-type)
	      (for* each cluster in (tell self 'get-clusters theme-type) do
		(tell cluster 'freeze))
	      'done)
	    (freeze-everything ()
	      (for* each cluster in all-clusters do
		(tell cluster 'freeze))
	      'done)
	    ;;--------------------------------------------------------------------
	    ;; unfreeze-theme has no effect on a theme in a frozen cluster.
	    (unfreeze-theme (theme-type dimension relation)
	      (let ((theme (tell self 'get-theme theme-type dimension relation)))
		(if* (exists? theme)
		  (tell theme 'unfreeze)))
	      'done)
	    ;; unfreeze-theme-cluster unfreezes a cluster and all of the
	    ;; individual themes in the cluster.
	    (unfreeze-theme-cluster (theme-type dimension)
	      (tell (tell self 'get-cluster theme-type dimension) 'unfreeze))
	    (unfreeze-theme-type (theme-type)
	      (for* each cluster in (tell self 'get-clusters theme-type) do
		(tell cluster 'unfreeze))
	      'done)
	    (unfreeze-everything ()
	      (for* each cluster in all-clusters do
		(tell cluster 'unfreeze))
	      'done)
	    ;;--------------------------------------------------------------------
	    ;; These methods delete themes from a cluster without changing the
	    ;; frozen/unfrozen status of the cluster.
	    (delete-theme (theme-type dimension relation)
	      (let ((theme (tell self 'get-theme theme-type dimension relation)))
		(if* (exists? theme)
		  (tell (tell theme 'get-cluster) 'delete-theme theme)
		  (set! all-themes (remq theme all-themes))
		  (tell *themespace-window* 'erase-theme theme)))
	      'done)
	    (delete-theme-cluster (theme-type dimension)
	      (let ((cluster (tell self 'get-cluster theme-type dimension)))
		(for* each relation in (tell cluster 'get-relations) do
		  (tell self 'delete-theme theme-type dimension relation)))
	      'done)
	    (delete-theme-type (theme-type)
	      (for* each cluster in (tell self 'get-clusters theme-type) do
		(tell cluster 'delete-themes))
	      (set! all-themes (filter-out-meth all-themes 'theme-type? theme-type))
	      (tell *themespace-window* 'erase-all-themes theme-type)
	      'done)
	    (delete-everything ()
	      (for* each cluster in all-clusters do
		(tell cluster 'delete-themes))
	      (set! all-themes '())
	      (tell *themespace-window* 'erase-all-themes)
	      'done)
	    ;;--------------------------------------------------------------------
	    (spread-activation ()
	      (for* each cluster in all-clusters do
		(tell cluster 'spread-activation)
		(tell cluster 'update-dominant-theme))
	      (tell *themespace-window* 'update-graphics)
	      'done)
	    (update-dominant-themes theme-types
	      (if (null? theme-types)
		(for* each cluster in all-clusters do
		  (tell cluster 'update-dominant-theme))
		(for* each theme-type in theme-types do
		  (for* each cluster in (tell self 'get-clusters theme-type) do
		    (tell cluster 'update-dominant-theme))))
	      'done)
	    (add-theme-if-possible (theme-type dimension relation)
	      (tell self 'add-theme theme-type dimension relation #t))
	    (add-theme-unconditionally (theme-type dimension relation)
	      (tell self 'add-theme theme-type dimension relation #f))
	    (add-theme (theme-type dimension relation check-if-frozen?)
	      (if (and (not %self-watching-enabled%) (not *display-mode?*))
		#f
		(let ((theme (tell self 'get-theme theme-type dimension relation)))
		  (if (exists? theme)
		    theme
		    (let ((cluster (tell self 'get-cluster theme-type dimension)))
		      (if (and check-if-frozen? (tell cluster 'frozen?))
			#f
			(let ((new-theme (tell cluster 'add-theme relation)))
			  (if* (exists? new-theme)
			    (set! all-themes (cons new-theme all-themes))
			    (tell *themespace-window*
			      'set-theme-graphics-parameters-and-draw new-theme))
			  new-theme)))))))
	    (set-propagation-function (func)
	      (for* each cluster in all-clusters do
		(tell cluster 'set-propagation-function func))
	      'done)
	    (set-activation-function (func)
	      (for* each cluster in all-clusters do
		(tell cluster 'set-activation-function func))
	      'done)
	    (set-sensitivity (value)
	      (for* each cluster in all-clusters do
		(tell cluster 'set-sensitivity value))
	      'done)
	    (else (delegate msg base-object))))))))


(define get-possible-relations
  (lambda (theme-type dimension)
    (let ((instance-nodes (tell dimension 'get-instance-nodes)))
      (remq-duplicates
	(cross-product-map get-label instance-nodes instance-nodes)))))


(define make-theme-cluster
  (lambda (theme-type)
    (lambda (dimension)
      (let* ((relations (get-possible-relations theme-type dimension))
	     (num-relations (length relations))
	     ;; This function determines the amount of inhibitory (-) or excitatory (+)
	     ;; activation that flows across a link from a theme with activation a1 to
	     ;; a theme with activation a2.  A positive value represents an excitatory
	     ;; effect, negative represents an inhibitory effect.
	     (propagation-function
	       (lambda (a1 a2)
		 (* (abs a1)
		    (% (cond
			 ((and (< a1 0) (< a2 0)) negative->negative-weight)
			 ((< a1 0) negative->positive-weight)
			 ((< a2 0) positive->negative-weight)
			 (else positive->positive-weight))))))
	     (sensitivity 1.0)
	     ;; This function determines the amount of self-excitation for a theme
	     ;; with activation a:
	     (self-excitation-function
	       (lambda (a)
		 (if (> a 0)
		   (* a (% self->self-weight))
		   0)))
	     ;; This function takes the raw net inhibitory (-) or excitatory (+)
	     ;; activation received by a theme from all themes in its cluster,
	     ;; including itself, and scales it into the range (-N,+N), where N is the
	     ;; maximum possible activation change allowed in one step.  The amount of
	     ;; scaling depends on the number of possible themes in the cluster.  The
	     ;; sensitivity parameter controls the slope of the sigmoid.  0.5 or less
	     ;; makes themes relatively insensitive to impinging excitatory/inhibitory
	     ;; activation, while above 2.0 or so makes them more sensitive:
	     (net-effect
	       (let ((alpha (* sensitivity 1/50 (/ 1 num-relations))))
		 (lambda (net-input)
		   (round (* %theme-spread-amount% (tanh (* alpha net-input)))))))
	     ;; This function determines the new activation of a theme based on the
	     ;; net inhibitory or excitatory input it receives from other themes in
	     ;; its cluster.  net-input < 0 causes an inhibitory effect, net-input > 0
	     ;; causes an excitatory effect.  Themes themselves can be either positively
	     ;; or negatively activated.  Inhibiting a negative theme pulls its
	     ;; activation toward zero, while exciting it pushes its activation toward
	     ;; -100.  Inhibiting a positive theme pulls its activation toward zero,
	     ;; while exciting it pushes its activation toward +100:
	     (activation-function
	       (lambda (net-input activation)
		 (if (< activation 0)
		   (clip-negative (- activation (net-effect net-input)))
		   (clip-positive (+ activation (net-effect net-input))))))
	     (dominant-theme #f)
	     (themes '())
	     (frozen? #f))
	(lambda msg
	  (let ((self (1st msg)))
	    (record-case (rest msg)
	      (object-type () 'theme-cluster)
	      (print ()
		(printf "~a ~a themes:"
		  theme-type
		  (if (exists? dimension) (tell dimension 'get-short-name) ""))
		(if (null? themes)
		  (printf " None~%")
		  (begin
		    (for* each theme in themes do
		      (printf "~%   ~a (~a)"
			(tell theme 'ascii-name)
			(tell theme 'get-activation))
		      (if* (eq? theme dominant-theme)
			(printf " - DOMINANT")))
		    (printf "~%"))))
	      (get-theme-type () theme-type)
	      (get-theme (relation) (select-meth themes 'relation? relation))
	      (get-themes () themes)
	      (get-dominant-theme () dominant-theme)
	      (dominant-theme? () (exists? dominant-theme))
	      (get-dimension () dimension)
	      (get-relations () relations)
	      (dimension? (dim) (eq? dim dimension))
	      (get-max-positive-theme-activation ()
		(maximum (tell-all themes 'get-positive-activation)))
	      (pick-positive-theme ()
		(stochastic-pick-by-method themes 'get-positive-activation))
	      (frozen? () frozen?)
	      (freeze ()
		;; This automatically freezes all themes in cluster:
		(set! frozen? #t)
		'done)
	      (unfreeze ()
		(set! frozen? #f)
		(for* each theme in themes do
		  (tell theme 'unfreeze))
		'done)
	      (update-dominant-theme ()
		(set! dominant-theme
		  (if (null? themes)
		    #f
		    (let ((ranked-themes
			    (sort-by-method 'get-absolute-activation > themes)))
		      (if (and (positive? (tell (1st ranked-themes) 'get-activation))
			       (> (- (tell (1st ranked-themes)
				       'get-absolute-activation)
				     (if (null? (rest ranked-themes))
				       0
				       (tell (2nd ranked-themes)
					 'get-absolute-activation)))
				  %dominant-theme-margin%))
			(1st ranked-themes)
			#f))))
		'done)
	      (spread-activation ()
		(for* each theme in themes do
		  (tell theme 'clear-net-input-buffer))
		(for* each theme in themes do
		  (tell theme 'spread-activation))
		(for* each theme in themes do
		  (tell theme 'update-activation))
		'done)
	      (add-theme (relation)
		(if (not (member? relation relations))
		  #f
		  (let ((theme (make-bridge-theme theme-type dimension relation)))
		    (for* each other-theme in themes do
		      (tell other-theme 'add-outgoing-link theme)
		      (tell theme 'add-outgoing-link other-theme))
		    (tell theme 'set-cluster self)
		    (tell theme 'set-propagation-function propagation-function)
		    (tell theme 'set-self-excitation-function self-excitation-function)
		    (tell theme 'set-activation-function activation-function)
		    (set! themes (cons theme themes))
		    theme)))
	      (delete-theme (theme)
		(set! themes (remq theme themes))
		(for* each other-theme in themes do
		  (tell other-theme 'delete-outgoing-link theme))
		(tell self 'update-dominant-theme)
		'done)
	      (delete-themes ()
		(set! themes '())
		(set! dominant-theme #f)
		'done)
	      (set-propagation-function (func)
		(for* each theme in themes do
		  (tell theme 'set-propagation-function func))
		(set! propagation-function func)
		'done)
	      (set-self-excitation-function (func)
		(for* each theme in themes do
		  (tell theme 'set-self-excitation-function func))
		(set! self-excitation-function func)
		'done)
	      (set-activation-function (func)
		(for* each theme in themes do
		  (tell theme 'set-activation-function func))
		(set! activation-function func)
		'done)
	      (set-sensitivity (value)
		(set! sensitivity value)
		'done)
	      (else (delegate msg base-object)))))))))


(define make-generic-theme
  (lambda (theme-type)
    (let* ((activation 0)
	   (net-input-buffer 0)
	   (propagation-function #f)
	   (self-excitation-function #f)
	   (activation-function #f)
	   (theme-cluster #f)
	   (outgoing-theme-links '())
	   (graphics-window-panel #f)
	   (graphics-coord #f)
	   (text-coord #f)
	   (normal-graphics-pexp #f)
	   (highlight-graphics-pexp #f)
	   (graphics-activation 0)
	   (frozen? #f))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'generic-theme)
	    (get-theme-type () theme-type)
	    ;; type/s may be either a symbol or a list:
	    (theme-type? (type/s)
	      ((if (symbol? type/s) eq? member?) theme-type type/s))
	    (set-graphics-parameters (panel center text norm-pexp hl-pexp)
	      (set! graphics-window-panel panel)
	      (set! graphics-coord center)
	      (set! text-coord text)
	      (set! normal-graphics-pexp norm-pexp)
	      (set! highlight-graphics-pexp hl-pexp)
	      (set! graphics-activation 0)
	      'done)
	    (get-normal-pexp () normal-graphics-pexp)
	    (get-highlight-pexp () highlight-graphics-pexp)
	    (get-graphics-panel () graphics-window-panel)
	    (clicked? (x y)
	      (if (exists? graphics-window-panel)
		(let ((delta (* 1/2 (tell graphics-window-panel 'get-activation-diameter))))
		  (or (and (<= (abs (- x (1st graphics-coord))) delta)
			   (<= (abs (- y (2nd graphics-coord))) delta))
		      (and (<= (abs (- x (1st text-coord))) delta)
			   (<= (abs (- y (2nd text-coord))) delta))))
		#f))
	    (draw-activation-graphics ()
	      (tell graphics-window-panel 'draw-absolute-activation
		graphics-coord activation)
	      (set! graphics-activation activation)
	      'done)
	    (erase-activation-graphics ()
	      (tell graphics-window-panel 'erase-activation graphics-coord)
	      (set! graphics-activation 0)
	      'done)
	    (update-activation-graphics ()
	      (if* (not (= activation graphics-activation))
		(if (= (sgn activation) (sgn graphics-activation))
		  (if (> (abs activation) (abs graphics-activation))
		    (tell graphics-window-panel 'draw-absolute-activation
		      graphics-coord activation)
		    (tell graphics-window-panel 'decrease-absolute-activation
		      graphics-coord activation))
		  (begin
		    (tell graphics-window-panel 'erase-activation graphics-coord)
		    (tell graphics-window-panel 'draw-absolute-activation
		      graphics-coord activation)))
		(set! graphics-activation activation))
	      'done)
	    (get-activation () activation)
	    (get-absolute-activation () (abs activation))
	    (get-positive-activation () (max 0 activation))
	    (negative? () (< activation 0))
	    (get-cluster () theme-cluster)
	    (set-cluster (c) (set! theme-cluster c) 'done)
	    (dominant? () (eq? self (tell theme-cluster 'get-dominant-theme)))
	    (frozen? () (or frozen? (tell theme-cluster 'frozen?)))
	    (individually-frozen? () frozen?)
	    (freeze () (set! frozen? #t) 'done)
	    (unfreeze () (set! frozen? #f) 'done)
	    (add-outgoing-link (theme)
	      (set! outgoing-theme-links (cons theme outgoing-theme-links))
	      'done)
	    (delete-outgoing-link (theme)
	      (set! outgoing-theme-links (remq theme outgoing-theme-links))
	      'done)
	    (clear-net-input-buffer ()
	      (set! net-input-buffer 0)
	      'done)
	    (increment-net-input-buffer (delta)
	      (set! net-input-buffer (+ net-input-buffer delta))
	      'done)
	    (spread-activation ()
	      (for* each theme in outgoing-theme-links do
		(tell theme 'increment-net-input-buffer
		  (propagation-function activation (tell theme 'get-activation))))
	      (tell self 'increment-net-input-buffer
		(self-excitation-function activation))
	      (tell self 'increment-net-input-buffer (- %theme-decay-amount%))
	      'done)
	    (update-activation ()
	      (if* (not (tell self 'frozen?))
		(set! activation (activation-function net-input-buffer activation)))
	      (set! net-input-buffer 0)
	      'done)
	    (boost-activation (factor)
	      (if* (not (tell self 'frozen?))
		(set! activation
		  (clip-positive
		    (round (+ activation (* (% factor) %theme-boost-amount%))))))
	      'done)
	    (set-activation (n)
	      (set! activation n)
	      'done)
	    (set-propagation-function (func)
	      (set! propagation-function func)
	      'done)
	    (set-self-excitation-function (func)
	      (set! self-excitation-function func)
	      'done)
	    (set-activation-function (func)
	      (set! activation-function func)
	      'done)
	    (else (delegate msg base-object))))))))


(define make-bridge-theme
  (lambda (theme-type dimension relation)
    (let ((generic-theme (make-generic-theme theme-type)))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'bridge-theme)
	    (ascii-name ()
	      (format "~a:~a"
		(tell dimension 'get-short-name)
		(if (exists? relation)
		  (tell relation 'get-lowercase-name)
		  "different")))
	    (print ()
	      (printf "~a ~a theme  (~a)~%"
		theme-type
		(tell self 'ascii-name)
		(tell self 'get-activation)))
	    (get-dimension () dimension)
	    (get-relation () relation)
	    (difference-theme? () (not (exists? relation)))
	    (identity-theme? () (eq? relation plato-identity))
	    (opposite-theme? () (eq? relation plato-opposite))
	    (dimension? (dim) (eq? dim dimension))
	    (relation? (rel) (eq? rel relation))
	    ;; ALL themes (regardless of type) should have an 'equal? method:
	    (equal? (other-theme)
	      (and (tell other-theme 'theme-type? theme-type)
		   (tell other-theme 'dimension? dimension)
		   (tell other-theme 'relation? relation)))
	    (spread-activation-to-slipnet ()
	      (stochastic-if* (^3 (% (tell self 'get-absolute-activation)))
		(tell dimension 'activate-from-workspace))
	      (if* (exists? relation)
		(stochastic-if* (^3 (% (tell self 'get-activation)))
		  (tell relation 'activate-from-workspace)))
	      'done)
	    (else (delegate msg generic-theme))))))))


(define theme-type->bridge-type
  (lambda (theme-type)
    (case theme-type
      (top-bridge 'top)
      (bottom-bridge 'bottom)
      (vertical-bridge 'vertical))))

(define bridge-type->theme-type
  (lambda (theme-type)
    (case theme-type
      (top 'top-bridge)
      (bottom 'bottom-bridge)
      (vertical 'vertical-bridge))))


(define-codelet-procedure* thematic-bridge-scout
  (lambda ()
    (if* (not %self-watching-enabled%)
      (say "Self-watching disabled. Fizzling.")
      (fizzle))
    (let ((active-bridge-theme-types
	    (tell *themespace* 'get-active-bridge-theme-types)))
      (if* (null? active-bridge-theme-types)
	(say "No active bridge theme types. Fizzling.")
	(fizzle))
      (let* ((theme-type-weights
	       (map (lambda (theme-type)
		      (* (tell *themespace*
			   'get-max-positive-theme-activation theme-type)
			 (100- (tell *workspace* 'get-mapping-strength
				 (theme-type->bridge-type theme-type)))))
		 active-bridge-theme-types))
	     (theme-type
	       (stochastic-pick active-bridge-theme-types theme-type-weights))
	     (bridge-type (theme-type->bridge-type theme-type))
	     (bridge-orientation (bridge-type->orientation bridge-type))
	     ;; Only positive themes can exert active pressure in the form of
	     ;; thematic codelets.  Negative themes can only influence
	     ;; structure strengths.  Otherwise, too many spurious bridges get
	     ;; created by thematic codelets looking for structures that are
	     ;; "not LettCtgy:identity" or whatever:
	     (clusters
	       (filter
		 (lambda (cluster)
		   (prob? (^2 (% (tell cluster 'get-max-positive-theme-activation)))))
		 (tell *themespace* 'get-clusters theme-type)))
	     (themes (tell-all clusters 'pick-positive-theme)))
	(say "Currently active bridge-theme types, weights:")
	(say active-bridge-theme-types)
	(say theme-type-weights)
	(say "Scouting for a " bridge-type " bridge...")
	(say "Focusing on dimensions "
	  (tell-all (tell-all clusters 'get-dimension) 'get-short-name))
	(if* (null? themes)
	  (say "Couldn't choose any " theme-type " themes. Fizzling.")
	  (fizzle))
	(say "Looking for a way to build a " bridge-type
	  " bridge that supports themes:")
	(vprint themes)
	(let* ((objects (tell *workspace* 'get-possible-bridge-objects bridge-type))
	       (chosen-object (stochastic-pick-by-method objects
				'get-inter-string-salience bridge-orientation))
	       (applicable-themes
		 (filter
		   (lambda (theme)
		     (tell chosen-object 'description-type-present?
		       (tell theme 'get-dimension)))
		   themes))
	       (non-applicable-themes
		 (remq-elements applicable-themes themes))
	       (chosen-string (tell chosen-object 'get-string)))
	  (say bridge-type " objects are:")
	  (say (tell-all objects 'ascii-name))
	  (say "Weights are:")
	  (say (tell-all objects 'get-inter-string-salience bridge-orientation))
	  (say "Chose " (tell chosen-object 'ascii-name) " in "
	    (tell chosen-string 'generic-name))
	  (say "Applicable themes: " (tell-all applicable-themes 'ascii-name))
	  (say "Non-applicable themes: " (tell-all non-applicable-themes 'ascii-name))
	  (if* (not (null? non-applicable-themes))
	    (say "Object lacks the following description types:")
	    (for* each theme in non-applicable-themes do
	      (say (tell theme 'get-dimension)))
	    (say "Proposing descriptions if possible...")
	    (for* each theme in non-applicable-themes do
	      ;; Special case: if we're focusing on StringPos, but a StringPos
	      ;; description is impossible for the chosen object, don't propose a
	      ;; bridge with the object.  Example: iijjkk->aabbdd with StringPos:iden
	      ;; and ObjCtgy:iden themes clamped.  Without this test, spurious i--b
	      ;; bridges get proposed:
	      (if* (and (tell theme 'dimension? plato-string-position-category)
		        (not (tell plato-string-position-category
			       'description-possible? chosen-object)))
		(say "StringPos description impossible for chosen object. Fizzling.")
		(fizzle))
	      (propose-description-based-on-theme chosen-object theme)))
	  (let* ((other-string
		   (tell *workspace* 'get-other-string
		     chosen-string bridge-orientation))
		 (other-object-candidates
		   (filter-meth (tell other-string 'get-objects)
		     'all-description-types-present?
		     (tell-all applicable-themes 'get-dimension))))
	    (say "Looking for " (tell other-string 'generic-name)
	      " object with " (tell-all (tell-all applicable-themes 'get-dimension)
				'get-short-name) " descriptions...")
	    (if* (null? other-object-candidates)
	      (say "Couldn't find " (tell other-string 'generic-name)
		" object with necessary descriptions. Fizzling.")
	      (fizzle))
	    (say "These " (tell other-string 'generic-name)
	      " objects have the necessary descriptions:")
	    (say (tell-all other-object-candidates 'ascii-name))
	    (let* ((from-object?
		     (or (tell chosen-string 'string-type? 'initial)
		         (and (tell chosen-string 'string-type? 'target)
			      (eq? bridge-orientation 'horizontal))))
		   (get-conditions
		     (conditions-for-bridge
		       chosen-object from-object? applicable-themes))
		   (other-object-selection-list
		     (map-compress
		       (lambda (obj)
			 (let ((conditions (get-conditions obj)))
			   (if (exists? conditions)
			     (let ((selection-weight
				     (tell obj 'get-inter-string-salience
				       bridge-orientation)))
			       (list selection-weight obj conditions))
			     #f)))
		       other-object-candidates)))
	      (if* (null? other-object-selection-list)
		(say "No way to make a bridge that supports the themes. Fizzling.")
		(fizzle))
	      (let* ((chosen-element (stochastic-select other-object-selection-list))
		     (other-object (2nd chosen-element))
		     (conditions (3rd chosen-element))
		     (object1 (if from-object? chosen-object other-object))
		     (object2 (if from-object? other-object chosen-object))
		     (flip1? (member? object1 conditions))
		     (flip2? (member? object2 conditions)))
		(say "Chose " (tell other-object 'ascii-name) ".")
		(let* ((proposed-bridge
			(propose-bridge
			  bridge-orientation object1 flip1? object2 flip2?))
		      (max-theme-activation
			(apply max (tell-all applicable-themes 'get-activation)))
		      (urgency
			(round (* (% max-theme-activation)
				  (if (tell chosen-object 'string-spanning-group?)
				    %extremely-high-urgency%
				    %very-high-urgency%)))))
		  (look-for-auxiliary-slippages proposed-bridge)
		  (post-codelet*
		    urgency: urgency
		    bridge-evaluator proposed-bridge))))))))))


(define look-for-auxiliary-slippages
  (lambda (proposed-bridge)
    (vprintf "Looking for auxiliary slippages to make...~n")
    (for* each slippage in (tell proposed-bridge 'get-slippages) do
      (let* ((object1 (tell slippage 'get-object1))
	     (object2 (tell slippage 'get-object2))
	     (CM-type (tell slippage 'get-CM-type))
	     (descriptor1 (tell slippage 'get-descriptor1))
	     (descriptor2 (tell slippage 'get-descriptor2))
	     (label (tell slippage 'get-label))
	     (linked-instance-nodes
	       (filter
		 (lambda (node)
		   (and (tell node 'instance?)
		        (not (eq? (tell node 'get-category) CM-type))))
		 (tell-all (tell descriptor1 'get-outgoing-links) 'get-to-node))))
	(for* each node in linked-instance-nodes do
	  (let ((new-CM-type (tell node 'get-category))
		(related-node (tell node 'get-related-node label)))
	    (if* (and (not (tell proposed-bridge 'CM-type-present? new-CM-type))
		      (exists? related-node)
		      (tell node 'possible-descriptor? object1)
		      (tell related-node 'possible-descriptor? object2))
	      (vprintf "Auxiliary slippage ~a=>~a possible based on ~a...~n"
		(tell node 'get-short-name)
		(tell related-node 'get-short-name)
		(tell slippage 'print-name))
	      (vprintf "~a's degree-of-assoc is ~a~n"
		(tell label 'get-lowercase-name)
		(tell label 'get-degree-of-assoc))
	      (let* ((slippage-probability (% (tell label 'get-degree-of-assoc)))
		     (make-slippage? (prob? slippage-probability)))
		(if make-slippage?
		  (vprintf "Making slippage...~n")
		  (vprintf "Failed. Not making slippage.~n"))
		(if* make-slippage?
		  (let ((new-slippage
			  (make-concept-mapping
			    object1 new-CM-type node
			    object2 new-CM-type related-node)))
		    (if* (not (tell object1 'description-type-present? new-CM-type))
		      (let ((new-description
			      (make-description object1 new-CM-type node)))
			(build-description new-description)
			(vprintf "Added description to ~a:~n" (tell object1 'ascii-name))
			(vprint new-description)
			(vprintf "Fizzling.~n")
			(fizzle)))
		    (if* (not (tell object2 'description-type-present? new-CM-type))
		      (let ((new-description
			      (make-description object2 new-CM-type related-node)))
			(build-description new-description)
			(vprintf "Added description to ~a:~n" (tell object2 'ascii-name))
			(vprint new-description)
			(vprintf "Fizzling.~n")
			(fizzle)))
		    (tell proposed-bridge 'add-concept-mapping new-slippage)
		    (vprintf "Added new ~a slippage to bridge:~n"
		      (tell new-slippage 'print-name))
		    (vprint proposed-bridge)))))))))))


(define propose-description-based-on-theme
  (lambda (object theme)
    (let* ((dimension (tell theme 'get-dimension))
	   (possible-descriptors (tell dimension 'get-possible-descriptors object)))
      (if (null? possible-descriptors)
	(say "   No " dimension " description possible.")
	(let* ((chosen-descriptor
		 (stochastic-pick-by-method possible-descriptors 'get-activation))
	       (proposed-description
		 (make-description object dimension chosen-descriptor)))
	  (tell proposed-description 'update-proposal-level %proposed%)
	  (tell chosen-descriptor 'activate-from-workspace)
	  (say "   Proposing " (tell proposed-description 'print-name) " description.")
	  (post-codelet*
	    urgency: (tell theme 'get-absolute-activation)
	    description-evaluator proposed-description))))))


(define conditions-for-bridge
  (lambda (chosen-object from-object? themes)
    (lambda (other-object)
      (let ((object1 (if from-object? chosen-object other-object))
	    (object2 (if from-object? other-object chosen-object))
	    (themes-support? (theme-support-tester themes)))
	(cond
	  ((lone-spanning-object? object1 object2) #f)
	  ((not (both-spanning-groups? object1 object2))
	   (if (themes-support? object1 object2) '() #f))
	  ((themes-support? object1 object2) '())
	  (else
	    ;; Don't always try to flip object1 first:
	    (let* ((num1 (tell object1 'get-num-of-spanning-bridges))
		   (num2 (tell object2 'get-num-of-spanning-bridges))
		   (object1-bias
		     (cond
		       ((> num1 num2) (% 20))
		       ((< num1 num2) (% 80))
		       (else (% 50)))))
	      (if (prob? object1-bias)
		(cond
		  ;; Try to flip object1 first
		  ((themes-support? (flipped object1) object2)
		   (list object1))
		  ((themes-support? object1 (flipped object2))
		   (list object2))
		  ((themes-support? (flipped object1) (flipped object2))
		   (list object1 object2))
		  (else #f))
		(cond
		  ;; Try to flip object2 first
		  ((themes-support? object1 (flipped object2))
		   (list object2))
		  ((themes-support? (flipped object1) object2)
		   (list object1))
		  ((themes-support? (flipped object1) (flipped object2))
		   (list object1 object2))
		  (else #f))))))))))


(define flipped
  (lambda (object) (tell object 'make-flipped-version)))


;; theme-support-tester assumes positively-activated themes:

(define theme-support-tester
  (lambda (themes)
    (lambda (object1 object2)
      (let ((conflicts?
	      (lambda (theme)
		(check-descriptions object1 object2 conflicts-with-theme? theme)))
	    (supports?
	      (lambda (theme)
		(check-descriptions object1 object2 supported-by-theme? theme))))
	(and (not (ormap conflicts? themes))
	     (ormap supports? themes))))))


(define check-descriptions
  (lambda (object1 object2 pred? theme)
    (cross-product-ormap
      (lambda (d1 d2)
	(and (tell d1 'description-type? (tell d2 'get-description-type))
	     (pred? d1 d2 theme)))
      (tell object1 'get-descriptions)
      (tell object2 'get-descriptions))))


;; conflicts-with-theme? and supported-by-theme? assume that descriptions
;; d1 and d2 have the same description-type.  The three special cases are
;; mutually exclusive.

(define conflicts-with-theme?
  (lambda (d1 d2 theme)
    (and (or (tell d1 'description-type? (tell theme 'get-dimension))
	     (special-direction-case? d1 d2 theme))
         (not (special-spanning-bridge-case? d1 d2 theme))
	 (not (special-middle-middle-case? d1 d2 theme))
	 (not (relation-consistent-with-theme? d1 d2 theme)))))

(define supported-by-theme?
  (lambda (d1 d2 theme)
    (and (or (tell d1 'description-type? (tell theme 'get-dimension))
	     (special-direction-case? d1 d2 theme))
         (not (special-spanning-bridge-case? d1 d2 theme))
	 (or (special-middle-middle-case? d1 d2 theme)
	     (relation-consistent-with-theme? d1 d2 theme)))))

(define special-direction-case?
  (lambda (d1 d2 theme)
    (and (tell d1 'description-type? plato-direction-category)
         (both-spanning-groups? (tell d1 'get-object) (tell d2 'get-object))
         (tell theme 'dimension? plato-string-position-category))))

(define special-spanning-bridge-case?
  (lambda (d1 d2 theme)
    (or (and (tell d1 'description-type? plato-object-category)
	     (both-spanning-groups? (tell d1 'get-object) (tell d2 'get-object))
             (tell theme 'dimension? plato-object-category))
        (and (tell d1 'description-type? plato-string-position-category)
	     (both-spanning-objects? (tell d1 'get-object) (tell d2 'get-object))
             (tell theme 'dimension? plato-string-position-category)))))

(define special-middle-middle-case?
  (lambda (d1 d2 theme)
    (and (eq? (tell d1 'get-descriptor) plato-middle)
         (eq? (tell d2 'get-descriptor) plato-middle)
         (tell theme 'dimension? plato-string-position-category)
         (tell theme 'relation? plato-opposite))))

(define relation-consistent-with-theme?
  (lambda (d1 d2 theme)
    (let ((label (get-label (tell d1 'get-descriptor) (tell d2 'get-descriptor))))
      (if (tell theme 'difference-theme?)
	(not (eq? label plato-identity))
	(eq? label (tell theme 'get-relation))))))


(define descriptions-affect-themespace?
  (lambda (d1 d2)
    (and (tell d1 'description-type? (tell d2 'get-description-type))
         (not (ignore-descriptions? d1 d2)))))

(define ignore-descriptions?
  (lambda (d1 d2)
    (or (not (tell d1 'relevant?))
        (not (tell d2 'relevant?))
	(and (tell d1 'description-type? plato-object-category)
	     (both-spanning-groups? (tell d1 'get-object) (tell d2 'get-object)))
        (and (tell d1 'description-type? plato-string-position-category)
	     (both-spanning-objects? (tell d1 'get-object) (tell d2 'get-object)))
	(and (eq? (tell d1 'get-descriptor) plato-middle)
	     (eq? (tell d2 'get-descriptor) plato-middle)))))


;; This function "sharpens" a bridge's theme-compatibility rating. It is
;; a squashing function from -1..+1 to -1..+1.

(define beta 4)

(define bridge-theme-compatibility-sigmoid
  (lambda (x) (sub1 (/ 2 (add1 (exp (* -2 beta x)))))))


;; *slipnet-nodes* must already be defined:
(define *themespace* (make-themespace))


;;---------------------------- Manual theme clamping ------------------------------

;; abbreviations
(define top 'top-bridge)
(define bot 'bottom-bridge)
(define ver 'vertical-bridge)
(define lcat plato-letter-category)
(define len plato-length)
(define dir plato-direction-category)
(define spos plato-string-position-category)
(define apos plato-alphabetic-position-category)
(define otype plato-object-category)
(define gtype plato-group-category)
(define btype plato-bond-category)
(define facet plato-bond-facet)
(define iden plato-identity)
(define succ plato-successor)
(define pred plato-predecessor)
(define opp plato-opposite)
(define diff #f)

(define ?
  (lambda ()
    (printf "Theme Types:  top bot ver~n")
    (printf "Dimensions:   lcat len dir spos apos otype gtype btype facet~n")
    (printf "Relations:    same succ pred opp diff~n")
    (printf "---------------------------------------------------------------~n")
    (printf "Set activation:         (set-themes [top] [lcat] [same] 100)~n")
    (printf "Freeze:                 (freeze-themes [top] [lcat] [same])~n")
    (printf "Unfreeze:               (unfreeze-themes [top] [lcat] [same])~n")
    (printf "Unfreeze and clear:     (clear-themes [top] [lcat] [same])~n")
    (printf "Ignore:                 (ignore-themes [top] [lcat] [same])~n")
    (printf "Delete from Themespace: (delete-themes [top] [lcat] [same])~n")))

(define set-themes
  (lambda args
    (case (length args)
      (1 (tell *themespace* 'set-all-theme-activations (1st args)))
      (2 (tell *themespace* 'set-theme-type-activations (1st args) (2nd args)))
      (3 (tell *themespace* 'set-theme-cluster-activations
	   (1st args) (2nd args) (3rd args)))
      (4 (tell *themespace* 'set-theme-activation
	   (1st args) (2nd args) (3rd args) (4th args))))))

(define freeze-themes
  (lambda args
    (case (length args)
      (0 (tell *themespace* 'freeze-everything))
      (1 (tell *themespace* 'freeze-theme-type (1st args)))
      (2 (tell *themespace* 'freeze-theme-cluster (1st args) (2nd args)))
      (3 (tell *themespace* 'freeze-theme (1st args) (2nd args) (3rd args))))))

(define unfreeze-themes
  (lambda args
    (case (length args)
      (0 (tell *themespace* 'unfreeze-everything))
      (1 (tell *themespace* 'unfreeze-theme-type (1st args)))
      (2 (tell *themespace* 'unfreeze-theme-cluster (1st args) (2nd args)))
      (3 (tell *themespace* 'unfreeze-theme (1st args) (2nd args) (3rd args))))))

(define clear-themes
  (lambda args
    (apply unfreeze-themes args)
    (apply delete-themes args)))

(define ignore-themes
  (lambda args
    (apply clear-themes args)
    (apply freeze-themes args)))

;; delete-themes by itself doesn't change the frozen/unfrozen status of clusters:
(define delete-themes
  (lambda args
    (case (length args)
      (0 (tell *themespace* 'delete-everything))
      (1 (tell *themespace* 'delete-theme-type (1st args)))
      (2 (tell *themespace* 'delete-theme-cluster (1st args) (2nd args)))
      (3 (tell *themespace* 'delete-theme (1st args) (2nd args) (3rd args))))))

;;;; temporary
;;(define showhi
;;  (lambda ()
;;    (delete-themes)
;;    (set-themes top lcat succ 100)
;;    (set-themes top spos opp 100)
;;    (set-themes top dir opp 100)
;;    (set-themes top otype diff 100)
;;    (set-themes top len succ 100)
;;    (set-themes top gtype opp 100)
;;    (set-themes top apos opp 100)
;;    (set-themes top facet diff 100)
;;    (set-themes ver lcat succ 100)
;;    (set-themes ver spos opp 100)
;;    (set-themes ver dir opp 100)
;;    (set-themes ver otype diff 100)
;;    (set-themes ver len succ 100)
;;    (set-themes ver gtype opp 100)
;;    (set-themes ver apos opp 100)
;;    (set-themes ver facet diff 100)
;;    (set-themes bot lcat succ 100)
;;    (set-themes bot spos opp 100)
;;    (set-themes bot dir opp 100)
;;    (set-themes bot otype diff 100)
;;    (set-themes bot len succ 100)
;;    (set-themes bot gtype opp 100)
;;    (set-themes bot apos opp 100)
;;    (set-themes bot facet diff 100)))

;;;; temporary
;;(define showreg
;;  (lambda ()
;;    (delete-themes)
;;    (set-themes 100)))
