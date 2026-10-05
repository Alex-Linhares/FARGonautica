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

(define %max-coderack-size% 100)
(define %num-of-coderack-bins% 7)

;; Codelet urgencies

(define %extremely-low-urgency% 7)
(define %very-low-urgency% 21)
(define %low-urgency% 35)
(define %medium-urgency% 49)
(define %high-urgency% 63)
(define %very-high-urgency% 77)
(define %extremely-high-urgency% 91)

(define urgency-name
  (lambda (urgency-value)
    (cond
      ((<= urgency-value %extremely-low-urgency%) 'extremely-low-urgency)
      ((<= urgency-value %very-low-urgency%) 'very-low-urgency)
      ((<= urgency-value %low-urgency%) 'low-urgency)
      ((<= urgency-value %medium-urgency%) 'medium-urgency)
      ((<= urgency-value %high-urgency%) 'high-urgency)
      ((<= urgency-value %very-high-urgency%) 'very-high-urgency)
      (else 'extremely-high-urgency))))

(define urgency-color
  (lambda (urgency-value)
    (cond
      ((<= urgency-value %extremely-low-urgency%) %extremely-low-urgency-color%)
      ((<= urgency-value %very-low-urgency%) %very-low-urgency-color%)
      ((<= urgency-value %low-urgency%) %low-urgency-color%)
      ((<= urgency-value %medium-urgency%) %medium-urgency-color%)
      ((<= urgency-value %high-urgency%) %high-urgency-color%)
      ((<= urgency-value %very-high-urgency%) %very-high-urgency-color%)
      (else %extremely-high-urgency-color%))))

(define %urgency-value-table%
  (let ((table (make-table %num-of-coderack-bins% 101)))
    (for-each-table-element* (table coderack-bin-number temperature) do
      (table-set! table coderack-bin-number temperature
	(round (expt (add1 coderack-bin-number)
		     (/ (+ (100- temperature) 10) 15.0)))))
    table))


(define make-codelet-type
  (lambda (codelet-type-name graphics-labels)
    (let ((codelet-proc #f)
	  (codelet-count 0)
	  (selection-probability 0)
	  (urgency-clamped? #f)
	  (clamped-relative-urgency #f)
	  (slot-pexp #f)
	  (get-urgency-pexp #f)
	  (get-highlight-pexp #f)
	  (bar-left #f)
	  (bar-bottom #f)
	  (bar-top #f)
	  (codelet-count-coord #f)
	  (compute-bar-width #f)
	  (previous-bar-width 0)
	  (codelet-pattern-value #f)
	  (coderack-window #f))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'codelet-type)
	    (print ()
	      (printf "Codelet type: ~a~a~n" codelet-type-name
		(if urgency-clamped?
		  (format " (clamped at urgency ~a)" clamped-relative-urgency)
		  "")))
	    (get-slot-pexp () slot-pexp)
	    (get-codelet-type-name () codelet-type-name)
	    (clamped? () urgency-clamped?)
	    (get-clamped-urgency () clamped-relative-urgency)
	    (clamp (urgency)
	      (if* (or (not urgency-clamped?)
		       (not (= urgency clamped-relative-urgency)))
		(set! urgency-clamped? #t)
		(set! clamped-relative-urgency urgency)
		(tell *coderack* 'set-urgencies self urgency)
		(if* %coderack-graphics%
		  (tell coderack-window 'draw (get-urgency-pexp urgency) 'urgency)
		  (if* %codelet-count-graphics%
		    (tell self 'draw-codelet-count))))
	      'done)
	    (unclamp ()
	      (if* urgency-clamped? 
		(set! urgency-clamped? #f)
		(set! clamped-relative-urgency #f)
		(tell *coderack* 'reset-urgencies self))
	      'done)
	    (get-graphics-labels ()
              (cond
		((not %codelet-count-graphics%) graphics-labels)
		((= (length graphics-labels) 1)
		 (list (format "~as" (1st graphics-labels))))
		(else
		  (list (1st graphics-labels) (format "~as" (2nd graphics-labels))))))
	    (highlight (color)
	      (tell coderack-window 'draw (get-highlight-pexp color) 'highlight)
	      (if* %codelet-count-graphics%
		(tell coderack-window 'draw
		  `(let-sgl ((font ,%coderack-codelet-count-font%)
			     (background-color ,color)
			     (text-justification right)
			     (text-mode image))
		     (text ,codelet-count-coord ,(format "     ~a" codelet-count)))
		  'highlight))
	      'done)
	    (unhighlight ()
	      (tell coderack-window 'delete 'highlight))
	    (set-graphics-parameters (g pexp proc1 proc2 coord x1 y1 y2 bar-proc)
	      (set! coderack-window g)
	      (set! slot-pexp pexp)
	      (set! get-urgency-pexp proc1)
	      (set! get-highlight-pexp proc2)
	      (set! codelet-count-coord coord)
	      (set! bar-left x1)
	      (set! bar-bottom y1)
	      (set! bar-top y2)
	      (set! compute-bar-width bar-proc)
	      'done)
	    (reset-values-to-zero ()
              (set! codelet-count 0)
	      (set! selection-probability 0)
	      'done)
	    ;; This must be separate from reset-values-to-zero method:
	    (reset-bar-width ()
	      (set! previous-bar-width 0)
	      'done)
	    (draw-graphics ()
	      (if* urgency-clamped?
		(tell coderack-window 'draw
		  (get-urgency-pexp clamped-relative-urgency) 'urgency))
	      (let ((bar-width (compute-bar-width selection-probability)))
 		(tell coderack-window 'draw
 		  (solid-box bar-left bar-bottom (+ bar-left bar-width) bar-top) 'bar)
		(set! previous-bar-width bar-width)
		(if* %codelet-count-graphics%
		  (tell self 'draw-codelet-count)))
	      'done)
	    (display-urgency (urgency)
	      (tell coderack-window 'draw (get-urgency-pexp urgency) 'urgency)
	      (set! codelet-pattern-value urgency)
	      'done)
	    (draw-pattern-value ()
	      (if* (exists? codelet-pattern-value)
		(tell coderack-window 'draw
		  (get-urgency-pexp codelet-pattern-value) 'urgency))
	      'done)
	    (clear-pattern-value ()
	      (set! codelet-pattern-value #f)
	      'done)
	    (update-bar-graphics ()
              (let ((bar-width (compute-bar-width selection-probability)))
		(tell coderack-window 'draw
		  (solid-box bar-left bar-bottom (+ bar-left bar-width) bar-top) 'bar)
		(set! previous-bar-width bar-width))
	      (if* %codelet-count-graphics%
		(tell self 'draw-codelet-count))
	      'done)
	    (draw-codelet-count ()
	      (tell coderack-window 'draw-codelet-count
		codelet-count-coord codelet-count
		(if urgency-clamped?
		  (urgency-color clamped-relative-urgency)
		  %coderack-background-color%)))
	    (set-codelet-procedure (proc) (set! codelet-proc proc) 'done)
	    (make-codelet args
	      ;; First element of args is always the codelet urgency.  If codelet has
	      ;; a proposed structure, it will always be the second element of args.
	      (let* ((codelet-type self)
		     (original-urgency (1st args))
		     (relative-urgency
		       (if urgency-clamped?
			 clamped-relative-urgency
			 original-urgency))
		     (coderack-bin (tell *coderack* 'get-coderack-bin relative-urgency))
		     (index-in-bin #f)
		     (codelet-arguments (rest args))
		     ;; Don't need to worry about description arguments since
		     ;; descriptions aren't structures stored in the Workspace:
		     (proposed-structure-argument?
		       (and (not (null? codelet-arguments))
			    (member? (tell (1st codelet-arguments) 'object-type)
			      '(bond group bridge))))
		     (time-stamp #f))
		(lambda msg
		  (let ((self (1st msg)))
		    (record-case (rest msg)
		      (object-type () 'codelet)
		      (print ()
			(printf "~a codelet of urgency ~a (time-stamp ~a)~%"
			  codelet-type-name (round relative-urgency) time-stamp)
			(if* (not (null? codelet-arguments))
			  (printf "Codelet argument:~%")
			  (print (1st codelet-arguments))
			  (if* (= (length codelet-arguments) 2)
			    (printf "(scope is ~a).~%"
			      (tell (2nd codelet-arguments) 'object-type)))))
		      (run ()
			(tell coderack-window 'set-last-codelet-type codelet-type)
			(if* (and %coderack-graphics% *step-mode?* (= %step-cycles% 1))
			  (tell codelet-type 'highlight %current-codelet-color%))
			(apply codelet-proc codelet-arguments)
			(if* (and %coderack-graphics% *step-mode?* (= %step-cycles% 1))
			  (pause %codelet-highlight-pause%)
			  (tell codelet-type 'unhighlight)))
		      (get-relative-urgency () relative-urgency)
		      (codelet-type? (type) (eq? type codelet-type))
		      (get-codelet-type-name () codelet-type-name)
		      (increment-selection-probability (delta)
			(set! codelet-count (add1 codelet-count))
			(set! selection-probability (+ delta selection-probability))
			'done)
		      (proposed-structure-argument? () proposed-structure-argument?)
		      (get-removal-weight ()
			(* (- *codelet-count* time-stamp)
			   (add1 (- (tell *coderack* 'get-highest-bin-urgency)
				    (tell coderack-bin 'get-urgency)))))
		      (get-proposed-structure () (1st codelet-arguments))
		      (get-argument (n) (nth n codelet-arguments))
		      (get-coderack-bin () coderack-bin)
		      (get-index-in-bin () index-in-bin)
		      (get-time-stamp () time-stamp)
		      (set-index-in-bin (index) (set! index-in-bin index) 'done)
		      (set-time-stamp () (set! time-stamp *codelet-count*) 'done)
		      (set-urgency (new-value)
			(let ((new-bin (tell *coderack* 'get-coderack-bin new-value)))
			  (if* (not (eq? new-bin coderack-bin))
			    (tell coderack-bin 'remove-codelet self)
			    (tell new-bin 'add-codelet self)
			    (set! coderack-bin new-bin))
			  (set! relative-urgency new-value))
			'done)
		      (reset-urgency ()
			(tell self 'set-urgency original-urgency))
		      (adjust-urgency (delta)
			(let* ((new-value (+ relative-urgency delta))
			       (new-bin (tell *coderack* 'get-coderack-bin new-value)))
			  (if* (not (eq? new-bin coderack-bin))
			    (tell coderack-bin 'remove-codelet self)
			    (tell new-bin 'add-codelet self)
			    (set! coderack-bin new-bin))
			  (set! relative-urgency (min 100 (max 0 new-value))))
			'done)
		      (else (delegate msg base-object)))))))
	    (else (delegate msg base-object))))))))


(define make-coderack-bin
  (lambda (bin-number)
    (let ((codelet-vector (make-vector %max-coderack-size%))
	  (current-index 0)
	  (codelet-list '()))
      (lambda msg
	(record-case (rest msg)
	  (object-type () 'coderack-bin)
	  (print ()
	    (printf "  Coderack bin #~a (~a codelets, bin urgency = ~a)~%"
	      bin-number current-index
	      (table-ref %urgency-value-table% bin-number *temperature*)))
	  (update-selection-probabilities (total-urgency-sum)
            (if* (not (zero? current-index))
	      (let* ((bin-urgency-sum
		       (* current-index
			  (table-ref %urgency-value-table% bin-number *temperature*)))
		     (bin-relative-urgency (/ bin-urgency-sum total-urgency-sum))
		     (codelet-probability (/ bin-relative-urgency current-index)))
		(for* each codelet in codelet-list do
		  (tell codelet 'increment-selection-probability codelet-probability))))
	    'done)
	  (get-codelets () codelet-list)
	  (get-num-of-codelets () current-index)
	  (get-urgency ()
	    (table-ref %urgency-value-table% bin-number *temperature*))
	  (get-urgency-sum ()
	    (* current-index
	       (table-ref %urgency-value-table% bin-number *temperature*)))
	  (add-codelet (codelet)
	    (vector-set! codelet-vector current-index codelet)
	    (tell codelet 'set-index-in-bin current-index)
	    (set! current-index (add1 current-index))
	    (set! codelet-list (cons codelet codelet-list))
	    (tell codelet 'set-time-stamp)
	    'done)
	  (choose-random-codelet ()
	    (vector-ref codelet-vector (random current-index)))
	  (remove-codelet (codelet)
	    (let ((index (tell codelet 'get-index-in-bin)))
	      (set! current-index (sub1 current-index))
	      (if* (not (= index current-index))
		(let ((swap-codelet (vector-ref codelet-vector current-index)))
		  (vector-set! codelet-vector index swap-codelet)
		  (tell swap-codelet 'set-index-in-bin index)))
	      (tell codelet 'set-index-in-bin #f))
	    (set! codelet-list (remq codelet codelet-list))
	    'done)
	  (clear-codelets ()
	    (set! current-index 0)
	    (set! codelet-list '())
	    'done)
	  (else (delegate msg base-object)))))))


(define make-coderack
  (lambda ()
    (let* ((bins (map make-coderack-bin (ascending-index-list %num-of-coderack-bins%)))
	   (highest-urgency-bin (last bins))
	   (codelet-list '())
	   (current-num 0)
	   ;; Deferred codelets are added only during calls to (add-top-down-codelets)
	   ;; or (add-bottom-up-codelets). They get posted immediately afterwards, so
	   ;; deferred-codelets should effectively always be '().
	   (deferred-codelets '()))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'coderack)
	    (print ()
	      (printf "Coderack:~%")
	      (for* each bin in bins do
		(print bin)))
	    (show-bin (n)
	      (printf "Codelets in coderack bin #~a:~%" n)
	      (for* each codelet in (tell (nth n bins) 'get-codelets) do
		(print codelet)))
	    (empty? () (null? codelet-list))
	    (initialize ()
              (for* each bin in bins do
		(tell bin 'clear-codelets))
	      (set! codelet-list '())
	      (set! current-num 0)
	      (set! deferred-codelets '())
	      (for* each codelet-type in *codelet-types* do
		(tell codelet-type 'reset-values-to-zero)
		(tell codelet-type 'reset-bar-width)
		(tell codelet-type 'clear-pattern-value)
		(tell codelet-type 'unclamp)))
	    ;; this method only affects the graphics, not the actual selection
	    ;; probabilities or numbers of codelets on the coderack:
	    (update-all-selection-probabilities ()
              (for* each codelet-type in *codelet-types* do
		(tell codelet-type 'reset-values-to-zero))
              (let ((total-urgency-sum (sum (tell-all bins 'get-urgency-sum))))
		(if* (not (zero? total-urgency-sum))
		  (for* each bin in bins do
		    (tell bin 'update-selection-probabilities total-urgency-sum))))
	      'done)
	    (get-num-of-codelets () current-num)
	    (get-all-codelets () codelet-list)
	    (get-codelets-of-type (type)
	      (filter-meth codelet-list 'codelet-type? type))
	    (get-all-bins () bins)
	    (get-total-urgency-sum () (sum (tell-all bins 'get-urgency-sum)))
	    (get-highest-bin-urgency () (tell highest-urgency-bin 'get-urgency))
	    (get-coderack-bin (urgency)
	      (let ((i (cond
			 ((>= urgency 100) (sub1 %num-of-coderack-bins%))
			 ((<= urgency 0) 0)
			 (else (floor (* (% urgency) %num-of-coderack-bins%))))))
		(nth i bins)))
	    (add-deferred-codelet (c)
	      (set! deferred-codelets (cons c deferred-codelets))
	      'done)
	    ;; deferred-codelets are either bottom-up or top-down scout codelets,
	    ;; and thus never have proposed-structures as arguments, so they can
	    ;; be freely deleted without regard to workspace graphics:
	    (post-deferred-codelets ()
	      (let ((total-deferred (length deferred-codelets)))
		(if (>= total-deferred %max-coderack-size%)
		  (let ((excess-deferred (- total-deferred %max-coderack-size%)))
		    (if* (> excess-deferred 0)
		      (repeat* excess-deferred times
			(let ((c (random-pick deferred-codelets)))
			  (set! deferred-codelets (remq c deferred-codelets)))))
		    (tell self 'delete-all-codelets))
		  (let ((num-to-delete
			  (- (+ current-num total-deferred) %max-coderack-size%)))
		    (if* (> num-to-delete 0)
		      (tell self 'delete-codelets num-to-delete))))
		(for* each codelet in deferred-codelets do
		  (let ((bin (tell codelet 'get-coderack-bin)))
		    (tell bin 'add-codelet codelet))
		  (set! codelet-list (cons codelet codelet-list))
		  (set! current-num (add1 current-num)))
		(set! deferred-codelets '()))
	      'done)
	    (post (codelet)
	      (if* (= current-num %max-coderack-size%)
		(tell self 'delete-codelets 1))
	      (let ((bin (tell codelet 'get-coderack-bin)))
		(tell bin 'add-codelet codelet)
		(set! codelet-list (cons codelet codelet-list))
		(set! current-num (add1 current-num)))
	      'done)
	    (choose-codelet ()
	      ;; Coderack should never be empty here:
	      (let* ((bin (stochastic-pick-by-method bins 'get-urgency-sum))
		     (codelet (tell bin 'choose-random-codelet)))
		(tell bin 'remove-codelet codelet)
		(set! codelet-list (remq codelet codelet-list))
		(set! current-num (sub1 current-num))
		codelet))
	    (delete-all-codelets ()
	      (for* each codelet in codelet-list do
		(if* (tell codelet 'proposed-structure-argument?)
		  (tell *workspace* 'delete-proposed-structure
		    (tell codelet 'get-proposed-structure))))
	      (for* each bin in bins do
		(tell bin 'clear-codelets))
	      (set! codelet-list '())
	      (set! current-num 0)
	      'done)
	    (delete-codelets (num-to-delete)
	      (repeat* num-to-delete times
		(let* ((codelet
			 (stochastic-pick-by-method codelet-list 'get-removal-weight))
		       (bin (tell codelet 'get-coderack-bin)))
		  (if* (tell codelet 'proposed-structure-argument?)
		    (tell *workspace* 'delete-proposed-structure
		      (tell codelet 'get-proposed-structure)))
		  (tell bin 'remove-codelet codelet)
		  (set! codelet-list (remq codelet codelet-list))))
	      (set! current-num (- current-num num-to-delete))
	      'done)
	    (set-urgencies (codelet-type new-value)
	      (for* each codelet in codelet-list do
		(if* (tell codelet 'codelet-type? codelet-type)
		  (tell codelet 'set-urgency new-value)))
	      'done)
	    (reset-urgencies (codelet-type)
	      (for* each codelet in codelet-list do
		(if* (tell codelet 'codelet-type? codelet-type)
		  (tell codelet 'reset-urgency)))
	      'done)
	    (adjust-urgencies (codelet-type delta)
	      (for* each codelet in codelet-list do
		(if* (tell codelet 'codelet-type? codelet-type)
		  (tell codelet 'adjust-urgency delta)))
	      'done)
	    (else (delegate msg base-object))))))))


(define post-codelet-probability
  (lambda (codelet-type)
    (if (or (and %justify-mode% (eq? codelet-type answer-finder))
	    (and (not %justify-mode%) (eq? codelet-type answer-justifier))
	    (and (not %self-watching-enabled%)
		 (member? codelet-type *self-watching-codelet-types*)))
      0
      (if (tell codelet-type 'clamped?)
	(% (tell codelet-type 'get-clamped-urgency))
	(case (tell codelet-type 'get-codelet-type-name)
	  ((bottom-up-bond-scout
	     top-down-bond-scout:category
	     top-down-bond-scout:direction
	     top-down-group-scout:category
	     top-down-group-scout:direction
	     group-scout:whole-string)
	   (% (tell *workspace* 'get-average-intra-string-unhappiness)))
	  ((bottom-up-bridge-scout
	     important-object-bridge-scout)
	   (% (100- (tell *workspace* 'get-min-mapping-strength))))
	  ((bottom-up-description-scout
	     top-down-description-scout)
	   (% (tell *workspace* 'get-average-unhappiness)))
	  (rule-scout
	    (if (null? (tell *workspace* 'get-possible-rule-types))
	      0.5
	      1))
	  (answer-finder
	    (if (tell *workspace* 'supported-rule-exists? 'top)
	      (% (100- *temperature*))
	      0))
	  (answer-justifier
	    (if (or (tell *workspace* 'supported-rule-exists? 'top)
		    (tell *workspace* 'supported-rule-exists? 'bottom))
	      (% (100- *temperature*))
	      0))
	  (breaker (% *temperature*))
	  (progress-watcher
	    (if (tell *themespace* 'thematic-pressure?) 1 0.25))
	  (jootser
	    (if (or (tell *trace* 'within-snag-period?)
		    (tell *trace* 'within-clamp-period?))
	      0.4
	      0.1))
	  (thematic-bridge-scout
	    (let ((active-bridge-theme-types
		    (tell *themespace* 'get-active-bridge-theme-types)))
	      (if (null? active-bridge-theme-types)
		0
		(% (tell *themespace* 'get-max-positive-theme-activation
		     active-bridge-theme-types))))))))))


(define num-of-codelets-to-post
  (lambda (codelet-type)
    (if (or (and %justify-mode% (eq? codelet-type answer-finder))
	    (and (not %justify-mode%) (eq? codelet-type answer-justifier)))
      0
      (case (tell codelet-type 'get-codelet-type-name)
	((bottom-up-bond-scout
	   top-down-bond-scout:category
	   top-down-bond-scout:direction)
	 (case (tell *workspace* 'get-rough-num-of-unrelated-objects)
	   (few 2) (some 4) (many 6)))
	((top-down-group-scout:category
	   top-down-group-scout:direction
	   group-scout:whole-string)
	 (if (null? (tell *workspace* 'get-bonds))
	   0
	   (case (tell *workspace* 'get-rough-num-of-ungrouped-objects)
	     (few 1) (some 2) (many 3))))
	((bottom-up-bridge-scout
	   important-object-bridge-scout)
	 (case (tell *workspace* 'get-rough-num-of-unmapped-objects)
	   (few 2) (some 5) (many 6)))
	((bottom-up-description-scout
	   top-down-description-scout) 2)
	(rule-scout
	  (max 1 (* 2 (length (tell *workspace* 'get-possible-rule-types)))))
	(answer-finder 1)
	(answer-justifier 1)
	(breaker 1)
	(thematic-bridge-scout
	  (round (* 10 (% (tell *workspace* 'get-max-inter-string-unhappiness)))))
	(progress-watcher 2)
	(jootser (if %justify-mode% 1 2))))))


(define add-top-down-codelets
  (lambda ()
    (for* each node in *top-down-slipnodes* do
      (tell node 'attempt-to-post-top-down-codelets))
    (for* each codelet-type in *thematic-codelet-types* do
      (stochastic-if* (post-codelet-probability codelet-type)
	(let ((urgency (thematic-codelet-urgency codelet-type)))
	  (repeat* (num-of-codelets-to-post codelet-type) times
	    (tell *coderack* 'add-deferred-codelet
	      (tell codelet-type 'make-codelet urgency))))))))


(define add-bottom-up-codelets
  (lambda ()
    (for* each codelet-type in *bottom-up-codelet-types* do
      (stochastic-if* (post-codelet-probability codelet-type)
	(let ((urgency (bottom-up-urgency codelet-type)))
	  (repeat* (num-of-codelets-to-post codelet-type) times
	    (tell *coderack* 'add-deferred-codelet
	      (tell codelet-type 'make-codelet urgency))))))))


(define bottom-up-urgency
  (lambda (codelet-type)
    (case (tell codelet-type 'get-codelet-type-name)
      (answer-finder (100- *temperature*))
      (answer-justifier (100- *temperature*))
      (breaker %extremely-low-urgency%)
      (progress-watcher %medium-urgency%)
      (jootser %medium-urgency%)
      (else %low-urgency%))))


(define thematic-codelet-urgency
  (lambda (codelet-type)
    (case (tell codelet-type 'get-codelet-type-name)
      (thematic-bridge-scout
	(tell *themespace* 'get-max-positive-theme-activation
	  (tell *themespace* 'get-active-bridge-theme-types))))))


;; *codelet-types* must be defined before the coderack is defined,
;; before any codelet-type-procedures are defined, and before any
;; top-down codelet-types are attached to slipnodes.

(define *codelet-types*
  (codelet-type-list*
    (bottom-up-bond-scout "Bottom-up" "bond scout")
    (top-down-bond-scout:category "Top-down bond" "(category) scout")
    (top-down-bond-scout:direction "Top-down bond" "(direction) scout")
    (bond-evaluator "Bond evaluator")
    (bond-builder "Bond builder")
    (top-down-group-scout:category "Top-down group" "(category) scout")
    (top-down-group-scout:direction "Top-down group" "(direction) scout")
    (group-scout:whole-string "Whole-string" "group scout")
    (group-evaluator "Group evaluator")
    (group-builder "Group builder")
    (bottom-up-bridge-scout "Bottom-up" "bridge scout")
    (important-object-bridge-scout "Important-object" "bridge scout")
    (bridge-evaluator "Bridge" "evaluator")
    (bridge-builder "Bridge" "builder")
    (bottom-up-description-scout "Bottom-up" "descrip. scout")
    (top-down-description-scout "Top-down" "descrip. scout")
    (description-evaluator "Description" "evaluator")
    (description-builder "Description" "builder")
    (rule-scout "Rule scout")
    (rule-evaluator "Rule evaluator")
    (rule-builder "Rule builder")
    (answer-finder "Answer finder")
    (answer-justifier "Answer justifier")
    (thematic-bridge-scout "Thematic" "bridge scout")
    (progress-watcher "Progress watcher")
    (jootser "Jootser")
    (breaker "Breaker")))


(define *thematic-codelet-types*
  (list thematic-bridge-scout))


(define *bottom-up-codelet-types*
  (list
    bottom-up-bond-scout
    group-scout:whole-string
    bottom-up-bridge-scout
    important-object-bridge-scout
    bottom-up-description-scout
    rule-scout
    answer-finder
    answer-justifier
    progress-watcher
    jootser
    breaker))


(define *self-watching-codelet-types*
  (list
    thematic-bridge-scout
    progress-watcher
    jootser))


(define *coderack* (make-coderack))
