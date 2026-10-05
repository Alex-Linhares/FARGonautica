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

;; coderack.ss must be loaded before this file because codelet patterns are
;; defined here using the globally-defined codelet-types in *codelet-types*

(define make-temporal-trace
  (lambda ()
    (let ((event-list '())
	  (next-event-number 1)
	  (last-clamp-time #f)
	  (last-unclamp-time #f)
	  (within-clamp-period? #f)
	  (within-snag-period? #f))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'temporal-trace)
	    (print ()
	      (printf "~nTEMPORAL TRACE:~n")
	      (for* each event in (reverse event-list) do
		(printf "-----------------------------------~n")
		(print event))
	      (printf "~n"))
	    (get-mouse-selected-event (x y)
	      (select
		(lambda (event) (tell event 'within-bounding-box? x y))
		event-list))
	    (initialize ()
	      (set! event-list '())
	      (set! next-event-number 1)
	      (set! last-clamp-time #f)
	      (set! last-unclamp-time #f)
	      (set! within-clamp-period? #f)
	      (set! within-snag-period? #f)
	      (tell *trace-window* 'initialize)
	      'done)
	    (unhighlight-all-events ()
	      (for* each event in event-list do
		(tell event 'unhighlight))
	      'done)
	    (unhighlight-all-events-except (event)
	      (for* each e in event-list do
		(if* (not (eq? e event))
		  (tell e 'unhighlight))))
	    (get-event (n)
	      (select
		(lambda (event) (= (tell event 'get-event-number) n))
		event-list))
	    (get-all-events () event-list)
	    (get-highlighted-event ()
	      (select-meth event-list 'highlighted?))
	    (get-events (event-type)
	      (filter-meth event-list 'type? event-type))
	    (get-num-of-events (event-type)
	      (length (filter-meth event-list 'type? event-type)))
	    (get-num-of-clamps (clamp-type)
	      (length
		(filter
		  (lambda (event)
		    (and (tell event 'type? 'clamp)
			 (tell event 'clamp-type? clamp-type)))
		  event-list)))
	    ;; If event-type/s is a list, the most recent event that matches
	    ;; any of the types in event-type/s is returned:
	    (get-last-event (event-type/s)
	      (if (symbol? event-type/s)
		(select-meth event-list 'type? event-type/s)
		(select
		  (lambda (event)
		    (member? (tell event 'get-type) event-type/s))
		  event-list)))
	    (get-new-events-since-last (event-type/s)
	      (let ((last-event (tell self 'get-last-event event-type/s)))
		(if (not (exists? last-event))
		  event-list
		  (get-first
		    (- (length event-list) (tell last-event 'get-event-number))
		    event-list))))
	    (get-new-structures-since-last (event-type/s)
	      (let ((last-event (tell self 'get-last-event event-type/s))
		    (current-workspace-structures (tell *workspace* 'get-structures)))
		(if (not (exists? last-event))
		  current-workspace-structures
		  (remq-elements
		    (tell last-event 'get-structures)
		    current-workspace-structures))))
	    (get-elapsed-time (event-type)
	      (let ((last-event (tell self 'get-last-event event-type)))
		(if (exists? last-event)
		  (tell last-event 'get-age)
		  *codelet-count*)))
	    (get-last-clamp-time () last-clamp-time)
	    (get-last-unclamp-time () last-unclamp-time)
	    (within-clamp-period? () within-clamp-period?)
	    (within-grace-period? ()
	      (and (not within-clamp-period?)
		   (exists? last-unclamp-time)
		   (< *codelet-count* (+ last-unclamp-time %grace-period%))))
	    (permission-to-clamp? ()
	      (and %self-watching-enabled%
		   (not within-clamp-period?)
		   (not (tell self 'within-grace-period?))))
	    (clamp-period-expired? ()
	      (and within-clamp-period?
		   (> *codelet-count* (+ last-clamp-time %max-clamp-period%))))
	    (progress-since-last-clamp ()
	      (let* ((last-clamp (tell self 'get-last-event 'clamp))
		     (new-events-since-last-clamp
		       (tell self 'get-new-events-since-last 'clamp))
		     (progress-evaluator (tell last-clamp 'get-progress-evaluator)))
		(maximum (map progress-evaluator new-events-since-last-clamp))))
	    (undo-last-clamp ()
	      (if* within-clamp-period?
		(vprintf "**** UNDOING CLAMP (time ~a) ****~n" *codelet-count*)
		(let ((last-clamp (tell self 'get-last-event 'clamp))
		      (progress-achieved (tell self 'progress-since-last-clamp)))
		  (set! within-clamp-period? #f)
		  (set! last-unclamp-time *codelet-count*)
		  (tell last-clamp 'update-progress-achieved progress-achieved)
		  (tell *comment-window* 'add-comment
		    (case (tell last-clamp 'get-clamp-type)
		      (rule-codelet-clamp
			(list
			  "Well, my latest effort to think up new rules resulted in"
			  (format " ~a progress."
			    (clamp-progress-amount-phrase progress-achieved))
			  (format "  Guess it was ~a effort, in retrospect."
			    (clamp-progress-adjective-phrase progress-achieved))))
		      (snag-response-clamp
			(list
			  (format "Looks like I made ~a headway in coming up with"
			    (clamp-progress-amount-phrase progress-achieved))
			  " new ideas."))
		      (justify-clamp
			(list
			  "Looks like that last brilliant idea I had resulted in "
			  (format "~a progress."
			    (clamp-progress-amount-phrase progress-achieved))
			  (format "  Guess it was ~a idea, in retrospect."
			    (clamp-progress-adjective-phrase progress-achieved))))
		      (manual-clamp
			(list
			  "That last suggestion of yours resulted in "
			  (format "~a progress."
			    (clamp-progress-amount-phrase progress-achieved))
			  (format "  Guess it was ~a idea, in retrospect."
			    (clamp-progress-adjective-phrase progress-achieved)))))
		    (list ;; non-eliza-mode
		      (format "Unclamping patterns.  Progress achieved by ~a clamp = ~a."
			(case (tell last-clamp 'get-clamp-type)
			  (rule-codelet-clamp "rule-codelet")
			  (snag-response-clamp "snag-response")
			  (justify-clamp "justify")
			  (manual-clamp "manual"))
			progress-achieved)))
		  (tell last-clamp 'deactivate)))
	      'done)
	    (current-answer? ()
	      (and (exists? (tell self 'get-last-event 'answer))
		   (zero? (tell self 'get-elapsed-time 'answer))))
	    (within-snag-period? () within-snag-period?)
	    (immediate-snag-condition? ()
	      (and within-snag-period?
		   (zero? (tell self 'get-elapsed-time 'snag))))
	    (progress-since-last-snag ()
	      (let* ((last-snag (tell self 'get-last-event 'snag))
		     (new-structures-since-last-snag
		       (tell self 'get-new-structures-since-last 'snag))
		     (progress-evaluator (tell last-snag 'get-progress-evaluator)))
		(maximum (map progress-evaluator new-structures-since-last-snag))))
	    (undo-snag-condition ()
	      (if* within-snag-period?
		(vprintf "**** UNDOING SNAG (time ~a) ****~n" *codelet-count*)
		(let ((last-snag (tell self 'get-last-event 'snag)))
		  (set! within-snag-period? #f)
		  (tell last-snag 'update-progress-achieved
		    (tell self 'progress-since-last-snag))
		  (set! *temperature-clamped?* #f)
		  (tell last-snag 'deactivate)))
	      'done)
	    (add-event (new-event)
	      (tell new-event 'set-event-number next-event-number)
	      (set! next-event-number (+ next-event-number 1))
	      (set! event-list (cons new-event event-list))
	      (if* (tell new-event 'type? 'clamp)
		(set! within-clamp-period? #t)
		(set! last-clamp-time *codelet-count*))
	      (if* (tell new-event 'type? 'snag)
		(set! within-snag-period? #t))
	      (tell *trace-window* 'add-event new-event)
	      'done)
	    (else (delegate msg base-object))))))))


(define clamp-progress-amount-phrase
  (lambda (progress-achieved)
    (cond
      ((= progress-achieved 0) "zero")
      ((< progress-achieved 50) "very little")
      ((< progress-achieved 80) "some")
      (else "a lot of"))))

(define clamp-progress-adjective-phrase
  (lambda (progress-achieved)
    (cond
      ((= progress-achieved 0) "a pretty useless")
      ((< progress-achieved 50) "not such a great")
      ((< progress-achieved 80) "an okay")
      (else "a pretty good"))))

;; event-type is one of:
;; snag, answer, clamp, concept-activation, concept-mapping, rule, group

(define make-generic-event
  (lambda (event-type)
    (let ((event-number #f)
	  (time-of-occurrence *codelet-count*)
	  (temperature *temperature*)
	  (workspace-structures (tell *workspace* 'get-structures))
	  (clamped-rules (tell *workspace* 'get-clamped-rules))
	  (active-theme-types (tell *themespace* 'get-active-theme-types))
	  (complete-themespace-patterns
	    (tell *themespace* 'get-all-complete-theme-patterns))
	  (dominant-themespace-patterns
	    (tell *themespace* 'get-all-dominant-theme-patterns))
	  (normal-graphics-pexp #f)
	  (highlight-graphics-pexp #f)
	  (highlighted? #f)
	  (bounding-box-x1 #f)
	  (bounding-box-y1 #f)
	  (bounding-box-x2 #f)
	  (bounding-box-y2 #f))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'generic-event)
	    (print-name ()
	      (format "Event #~a  Type: ~a  Time: ~a  Temperature: ~a"
		event-number event-type time-of-occurrence temperature))
	    (print ()
	      (printf "~a~n" (tell self 'print-name)))
	    (print-themespace-patterns ()
	      (printf "~nComplete Themespace patterns:~n~n")
	      (for* each pattern in complete-themespace-patterns do
		(print-pattern pattern))
	      (printf "~nDominant Themespace patterns:~n~n")
	      (for* each pattern in dominant-themespace-patterns do
		(print-pattern pattern)))
	    (type? (type) (or (eq? type 'any) (eq? type event-type)))
	    (set-graphics-pexps (normal-pexp highlight-pexp)
	      (set! normal-graphics-pexp normal-pexp)
	      (set! highlight-graphics-pexp highlight-pexp)
	      'done)
	    (set-bounding-box (x1 y1 x2 y2)
	      (set! bounding-box-x1 x1)
	      (set! bounding-box-y1 y1)
	      (set! bounding-box-x2 x2)
	      (set! bounding-box-y2 y2)
	      'done)
	    (within-bounding-box? (x y)
	      (and (>= x bounding-box-x1) (<= x bounding-box-x2)
		   (>= y bounding-box-y1) (<= y bounding-box-y2)))
	    (get-normal-graphics-pexp () normal-graphics-pexp)
	    (get-highlight-graphics-pexp () highlight-graphics-pexp)
	    (highlighted? () highlighted?)
	    (highlight ()
	      (if* (not highlighted?)
		(set! highlighted? #t)
		(tell *trace-window* 'draw highlight-graphics-pexp))
	      'done)
	    (unhighlight ()
	      (if* highlighted?
		(set! highlighted? #f)
		(tell *trace-window* 'draw normal-graphics-pexp))
	      'done)
	    (toggle-highlight ()
	      (tell self (if highlighted? 'unhighlight 'highlight)))
	    (set-event-number (n)
	      (set! event-number n)
	      'done)
	    (get-event-number () event-number)
	    (get-type () event-type)
	    (get-age () (- *codelet-count* time-of-occurrence))
	    (get-time () time-of-occurrence)
	    (get-temperature () temperature)
	    (get-structures () workspace-structures)
	    (get-active-theme-types () active-theme-types)
	    (get-complete-themespace-patterns () complete-themespace-patterns)
	    (get-dominant-themespace-patterns () dominant-themespace-patterns)
	    (get-complete-themespace-pattern (theme-type)
	      (assq theme-type complete-themespace-patterns))
	    (get-dominant-themespace-pattern (theme-type)
	      (assq theme-type dominant-themespace-patterns))
	    (display-workspace-state ()
	      (tell *workspace-window* 'draw-codelet-count time-of-occurrence)
	      (set! *fg-color* %faded-workspace-structure-color%)
	      (tell *workspace-window* 'draw-all-letters)
	      (tell *workspace-window* 'workspace-arrow 'top)
	      (tell *workspace-window* 'workspace-arrow 'bottom)
	      (for* each g in (filter group? workspace-structures) do
		(tell *workspace-window* 'display-group g))
	      (let ((prev-color %bridge-label-background-color%))
		(set! %bridge-label-background-color% %faded-bridge-label-background-color%)
		(for* each b in (filter bridge? workspace-structures) do
		  (tell *workspace-window* 'display-bridge b))
		(set! %bridge-label-background-color% prev-color))
	      (for* each r in clamped-rules do
		(tell *workspace-window* 'draw (tell r 'get-clamped-graphics-pexp)))
	      (set! *fg-color* %default-fg-color%)
	      'done)
	    (display args
	      (error #f "event has no display method: ~a" (tell self 'print-name)))
	    (else (delegate msg base-object))))))))


(define make-answer-event
  (lambda (initial-string modified-string target-string answer-string
	    top-rule bottom-rule supporting-vertical-bridges supporting-groups
	    top-rule-ref-objects bottom-rule-ref-objects slippage-log
	    unjustified-slippages)
    (let ((generic-event (make-generic-event 'answer))
	  (answer-description #f))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'answer-event)
	    (print-name () (format "[Answer ~a]" (tell answer-string 'print-name)))
	    (problem-print-name ()
	      (format "~a => ~a; ~a => ?"
		(tell initial-string 'print-name)
		(tell modified-string 'print-name)
		(tell target-string 'print-name)))
	    (problem-answer-print-name ()
	      (format "~a => ~a; ~a => ~a"
		(tell initial-string 'print-name)
		(tell modified-string 'print-name)
		(tell target-string 'print-name)
		(tell answer-string 'print-name)))
	    (print ()
	      (print generic-event)
	      (printf "Answer \"~a\"  (Quality = ~a)~n"
		(tell self 'problem-answer-print-name)
		(tell self 'get-quality)))
	    (print-slippages ()
	      (print slippage-log))
	    (get-initial-string () initial-string)
	    (get-modified-string () modified-string)
	    (get-target-string () target-string)
	    (get-answer-string () answer-string)
	    (get-initial-letters () (tell initial-string 'get-letter-categories))
	    (get-modified-letters () (tell modified-string 'get-letter-categories))
	    (get-target-letters () (tell target-string 'get-letter-categories))
	    (get-answer-letters () (tell answer-string 'get-letter-categories))
	    (get-rule (rule-type)
	      (case rule-type
		(top top-rule)
		(bottom bottom-rule)))
	    (get-supporting-bridges (type)
	      (case type
		(top (tell top-rule 'get-supporting-horizontal-bridges))
		(vertical supporting-vertical-bridges)
		(bottom (tell bottom-rule 'get-supporting-horizontal-bridges))))
	    (get-slippage-log () slippage-log)
	    (get-supporting-groups () supporting-groups)
	    (get-rule-ref-objects (rule-type)
	      (case rule-type
		(top top-rule-ref-objects)
		(bottom bottom-rule-ref-objects)))
	    (unjustified? () (not (null? unjustified-slippages)))
	    (get-unjustified-slippages () unjustified-slippages)
	    (get-answer-description () answer-description)
	    (set-answer-description (answer)
	      (set! answer-description answer)
	      'done)
	    (get-absolute-quality ()
	      (round (weighted-average
		       (list (tell top-rule 'get-quality)
			     (100- (tell self 'get-temperature)))
		       (list 60 40))))
	    (get-relative-quality ()
	      (round (weighted-average
		       (list (tell top-rule 'get-relative-quality)
			     (100- (tell self 'get-temperature)))
		       (list 60 40))))
	    (get-quality () (tell self 'get-absolute-quality))
	    (get-strength () (tell self 'get-quality))
	    (equal? (event) (tell event 'type? 'answer))
	    (display ()
	      (set! *display-mode?* #t)
	      (tell *workspace-window* 'clear)
	      (tell self 'display-workspace)
	      ;; Display themes:
 	      (if* (tell *themespace* 'current-state-displayed?)
 		(tell *themespace* 'save-current-state))
 	      (tell *themespace* 'delete-everything)
 	      (tell *themespace* 'thematic-pressure-off)
 	      ;; Display answer-description's theme patterns:
 	      (impose-theme-pattern
 		(tell answer-description 'get-vertical-theme-pattern))
 	      (impose-theme-pattern
 		(tell answer-description 'get-top-theme-pattern))
 	      (impose-theme-pattern
 		(tell answer-description 'get-bottom-theme-pattern))
	      ;; Blank slipnet window:
	      (tell *slipnet-window* 'blank-window)
	      ;; Blank coderack window:
	      (tell *coderack-window* 'blank-window "Coderack")
	      ;; Display answer temperature:
	      (tell *temperature-window* 'update-graphics (tell self 'get-temperature))
	      (if* (and %verbose% (tell self 'unjustified?))
		(printf "Unjustified slippages:~n")
		(for* each s in unjustified-slippages do
		  (print s))))
	    (display-workspace ()
	      (tell *workspace-window* 'draw-event-header
		(cond
		  ((not %justify-mode%) "Found new answer")
		  ((tell self 'unjustified?) "Settled for unjustified answer")
		  (else "Justified answer"))
		self)
	      ;; Restore Workspace appearance:
	      (tell self 'display-workspace-state)
	      (tell *workspace-window* 'draw-all-letters)
	      (if* (not %justify-mode%)
		(tell *workspace-window* 'draw-string-letters answer-string 'answer)
		(set! *fg-color* %faded-workspace-structure-color%)
		(for* each g in (tell answer-string 'get-groups) do
		  (tell *workspace-window* 'display-group g))
		(set! *fg-color* %default-fg-color%))
	      ;; Highlight vertical mapping:
	      (tell *workspace-window* 'highlight-bridges %vertical-bridge-color%
		(tell self 'get-supporting-bridges 'vertical))
	      ;; Highlight applied vertical slippages:
	      (tell *workspace-window* 'highlight-applied-slippages slippage-log)
	      (if* %verbose% (print slippage-log))
	      ;; Highlight top mapping:
	      (for* each obj in top-rule-ref-objects do
		(tell *workspace-window* 'display-in-color obj %top-bridge-color%))
	      (tell *workspace-window* 'highlight-bridges %top-bridge-color%
		(tell self 'get-supporting-bridges 'top))
	      ;; Highlight bottom mapping:
	      (for* each obj in bottom-rule-ref-objects do
		(tell *workspace-window* 'display-in-color obj %bottom-bridge-color%))
	      (tell *workspace-window* 'highlight-bridges %bottom-bridge-color%
		(tell self 'get-supporting-bridges 'bottom))
	      ;; Display rules:
	      (tell *workspace-window* 'display-rule top-rule %top-rule-color%)
	      (tell *workspace-window* 'display-rule bottom-rule %bottom-rule-color%))
	    (make-answer-description-pexp (theme-bridges theme-CMs)
	      (tell *workspace-window* 'caching-on)
	      (tell *workspace-window* 'clear)
	      (tell *workspace-window* 'draw-header
		(if (tell self 'unjustified?)
		  "Unjustified Answer Description"
		  "Answer Description"))
	      (tell *workspace-window* 'draw-string-letters initial-string 'initial)
	      (tell *workspace-window* 'workspace-arrow 'top)
	      (tell *workspace-window* 'draw-string-letters modified-string 'modified)
	      (tell *workspace-window* 'draw-string-letters target-string 'target)
	      (tell *workspace-window* 'workspace-arrow 'bottom)
	      (tell *workspace-window* 'draw-string-letters answer-string 'answer)
	      (for* each g in supporting-groups do
		(tell *workspace-window* 'display-group g))
	      (for* each b in theme-bridges do
		(let ((object1 (tell b 'get-object1))
		      (object2 (tell b 'get-object2)))
		  (if* (group? object1)
		    (tell *workspace-window* 'display-group object1))
		  (tell *workspace-window* 'display-bridge b)
		  (if* (group? object2)
		    (tell *workspace-window* 'display-group object2))))
	      ;; Highlight vertical mapping:
	      (tell *workspace-window* 'highlight-bridges %vertical-bridge-color%
		(tell self 'get-supporting-bridges 'vertical))
	      ;; Highlight the concept-mappings supporting the theme pattern and
	      ;; the slippages supporting rule translation:
	      (for* each cm in theme-CMs do
		(tell *workspace-window* 'display-in-color cm
		  %theme-supporting-concept-mapping-color%))
	      (for* each s in (tell slippage-log 'get-applied-slippages) do
		(let ((h (tell slippage-log 'get-slippage-to-highlight s)))
		  (tell *workspace-window* 'display-in-color h
		    %vertical-slippage-color%)))
	      ;; Display top mapping:
	      (for* each obj in top-rule-ref-objects do
		(tell *workspace-window* 'display-in-color obj %top-bridge-color%))
	      (tell *workspace-window* 'highlight-bridges %top-bridge-color%
		(tell self 'get-supporting-bridges 'top))
	      ;; Display bottom mapping:
	      (for* each obj in bottom-rule-ref-objects do
		(tell *workspace-window* 'display-in-color obj %bottom-bridge-color%))
	      (tell *workspace-window* 'highlight-bridges %bottom-bridge-color%
		(tell self 'get-supporting-bridges 'bottom))
	      ;; Display rules:
	      (tell *workspace-window* 'display-rule top-rule %top-rule-color%)
	      (tell *workspace-window* 'display-rule bottom-rule %bottom-rule-color%)
	      (let ((pexp (tell *workspace-window* 'get-cached-pexp)))
		(tell *workspace-window* 'clear-pending-flush)
		pexp))
	    (else (delegate msg generic-event))))))))


(define make-clamp-event
  (lambda (clamp-type patterns rules progress-focus)
    (let* ((generic-event (make-generic-event 'clamp))
	   (specified-theme-patterns (filter theme-pattern? patterns))
	   (specified-concept-patterns (filter concept-pattern? patterns))
	   (specified-codelet-patterns (filter codelet-pattern? patterns))
	   (theme-related-concept-patterns
	     (map get-associated-concept-pattern specified-theme-patterns))
	   (clamped-theme-patterns specified-theme-patterns)
	   (clamped-concept-patterns
	     (append specified-concept-patterns theme-related-concept-patterns))
	   (clamped-codelet-patterns
	     specified-codelet-patterns)
	   (unifying-slippages
	     (if (eq? clamp-type 'justify-clamp)
	       (get-unifying-slippages
		 (select-meth rules 'type? 'top)
		 (select-meth rules 'type? 'bottom))
	       #f))
	   (progress-evaluator
	     (lambda (event)
	       (if (tell event 'type? progress-focus)
		 (tell event 'get-strength)
		 0)))
	   (progress-achieved 0))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'clamp-event)
	    (print-name () "[Clamp]")
	    (print ()
	      (printf "~a (~a)~%" (tell generic-event 'print-name) clamp-type))
	    (print-patterns ()
	      (tell generic-event 'print-patterns)
	      (printf "~nClamped patterns:~n~n")
	      (for* each pattern in (tell self 'get-all-clamped-patterns) do
		(print-pattern pattern))
	      (if* (not (null? rules))
		(printf "~nClamped rules:~n~n")
		(for* each rule in rules do
		  (print rule))))
	    (get-clamped-theme-patterns () clamped-theme-patterns)
	    (get-clamped-concept-patterns () clamped-concept-patterns)
	    (get-clamped-codelet-patterns () clamped-codelet-patterns)
	    (get-theme-related-concept-patterns () theme-related-concept-patterns)
	    (get-complement-codelet-pattern () complement-codelet-pattern)
	    (get-all-clamped-patterns ()
	      (append
		clamped-theme-patterns
		clamped-concept-patterns
		clamped-codelet-patterns))
	    (get-rules () rules)
	    (get-rule (rule-type)
	      (case rule-type
		(top (select-meth rules 'type? 'top))
		(bottom (select-meth rules 'type? 'bottom))))
	    (get-unifying-slippages () unifying-slippages)
	    (get-clamp-type () clamp-type)
	    (clamp-type? (type) (eq? type clamp-type))
	    (get-progress-focus () progress-focus)
	    (get-progress-evaluator () progress-evaluator)
	    (get-progress-achieved () progress-achieved)
	    (update-progress-achieved (new-value)
	      (set! progress-achieved new-value)
	      'done)
	    (get-strength () 100)
	    (equal? (event)
	      (and (tell event 'type? 'clamp)
		   (tell event 'clamp-type? clamp-type)
		   (eq? progress-focus (tell event 'get-progress-focus))
		   (sets-equal-pred? rules-equal? rules (tell event 'get-rules))))
	    (activate ()
	      (tell *comment-window* 'add-comment
		(case clamp-type
		  (rule-codelet-clamp
		    (list "I'll just have to try a little harder..."))
		  (snag-response-clamp
		    (list
		      "All right, I've had enough of this!  "
		      "Let's try something different for a change..."))
		  (justify-clamp
		    (list
		      (format "Aha!  I have ~a idea..."
			(if (> (tell *trace* 'get-num-of-events 'clamp) 1)
			  "another"
			  "an"))))
		  (manual-clamp
		    (list
		      (format "Thank you for ~a interesting suggestion!  "
			(if (> (tell *trace* 'get-num-of-clamps 'manual-clamp) 1)
			  "another"
			  "that"))
		      "Let me think about it...")))
		(list ;; non-eliza-mode
		  (case clamp-type
		    (rule-codelet-clamp "Clamping rule-codelet pattern...")
		    (snag-response-clamp "Clamping negative theme pattern...")
		    (justify-clamp "Clamping theme patterns...")
		    (manual-clamp "Manually clamping patterns..."))))
	      (tell *trace* 'undo-snag-condition)
	      ;; Rules
	      (if* (not (null? rules))
		(for* each rule in rules do
		  (tell *workspace* 'clamp-rule rule)))
	      ;; Theme-patterns
	      (if* (not (null? clamped-theme-patterns))
		(for* each pattern in clamped-theme-patterns do
		  (clamp-theme-pattern pattern)))
	      ;; Concept-patterns
	      (if* (not (null? clamped-concept-patterns))
		(for* each pattern in clamped-concept-patterns do
		  (clamp-concept-pattern pattern))
		(if* %slipnet-graphics%
		  (tell *slipnet-window* 'update-graphics)))
	      ;; Codelet-patterns
	      (if* (not (null? clamped-codelet-patterns))
		(for* each pattern in clamped-codelet-patterns do
		  (clamp-codelet-pattern pattern))
		(if* %coderack-graphics%
		  (tell *coderack-window* 'update-graphics)))
	      'done)
	    (deactivate ()
	      ;; Rules
	      (if* (not (null? rules))
		(for* each rule in rules do
		  (tell *workspace* 'unclamp-rule rule)))
	      ;; Theme-patterns
	      (if* (not (null? clamped-theme-patterns))
		(for* each pattern in clamped-theme-patterns do
		  (unclamp-theme-pattern pattern)))
	      ;; Concept-patterns
	      (if* (not (null? clamped-concept-patterns))
		(for* each pattern in clamped-concept-patterns do
		  (unclamp-concept-pattern pattern))
		(if* %slipnet-graphics%
		  (tell *slipnet-window* 'update-graphics)))
	      ;; Codelet-patterns
	      (if* (not (null? clamped-codelet-patterns))
		(for* each pattern in clamped-codelet-patterns do
		  (unclamp-codelet-pattern pattern))
		(if* %coderack-graphics%
		  (tell *coderack-window* 'delete 'urgency)
		  (if* %codelet-count-graphics%
		    (tell *coderack-window* 'delete 'count))
		  (tell *coderack-window* 'update-graphics)))
	      'done)
	    (display ()
	      (vprintf "Progress achieved by clamp = ~a~n" progress-achieved)
	      (set! *display-mode?* #t)
	      (tell *workspace-window* 'clear)
	      (tell self 'display-workspace)
	      ;; Display clamped themes:
	      (if* (tell *themespace* 'current-state-displayed?)
		(tell *themespace* 'save-current-state))
	      (tell *themespace* 'delete-everything)
	      (tell *themespace* 'thematic-pressure-off)
	      (for* each pattern in clamped-theme-patterns do
		(impose-theme-pattern pattern)
		(tell *themespace* 'thematic-pressure-on (1st pattern)))
	      ;; Display concept-patterns:
	      (tell *slipnet-window* 'display-patterns
		clamped-concept-patterns
		%clamp-event-concept-pattern-color%
		"Concept Pattern")
	      ;; Display codelet patterns:
	      (tell *coderack-window* 'display-patterns
		clamped-codelet-patterns "Codelet Pattern")
	      ;; Display temperature:
	      (tell *temperature-window* 'update-graphics
		(tell self 'get-temperature)))
	    (display-workspace ()
	      (tell *workspace-window* 'draw-event-header
		(if (eq? clamp-type 'manual-clamp)
		    "Manually clamped patterns"
		    "Clamped patterns")
		self)
	      ;; Restore Workspace appearance:
	      (tell self 'display-workspace-state)
	      ;; Display clamped rules:
	      (for* each rule in rules do
		(tell *workspace-window* 'draw
		  (tell rule 'get-clamped-graphics-pexp))))
	    (else (delegate msg generic-event))))))))


(define make-concept-activation-event
  (lambda (slipnode)
    (let ((generic-event (make-generic-event 'concept-activation))
	  (print-name (format "(~a)" (tell slipnode 'get-short-name)))
	  (concept-pattern (list 'concepts (list slipnode %max-activation%))))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'concept-activation-event)
	    (print-name () print-name)
	    (print ()
	      (print generic-event)
	      (printf "Concept: ~a~n" (full-slipnode-name slipnode)))
	    (get-slipnode () slipnode)
	    (get-concept-pattern () concept-pattern)
	    (get-strength () (tell slipnode 'get-conceptual-depth))
	    (equal? (event)
	      (and (tell event 'type? 'concept-activation)
		   (eq? (tell event 'get-slipnode) slipnode)))
	    (display ()
	      (set! *display-mode?* #t)
	      (tell *workspace-window* 'clear)
	      (tell self 'display-workspace)
	      ;; Blank themespace window:
	      (if* (tell *themespace* 'current-state-displayed?)
		(tell *themespace* 'save-current-state))
	      (tell *themespace* 'delete-everything)
	      (tell *themespace* 'thematic-pressure-off)
	      ;; Display concept pattern:
	      (tell *slipnet-window* 'display-patterns
		(list concept-pattern)
		%concept-activation-event-concept-pattern-color%
		"Concept Activation")
	      ;; Blank coderack window:
	      (tell *coderack-window* 'blank-window "Coderack")
	      ;; Display temperature:
	      (tell *temperature-window* 'update-graphics
		(tell self 'get-temperature)))
	    (display-workspace ()
	      (tell *workspace-window* 'draw-event-header
		(format "Activation of ~a concept" (full-slipnode-name slipnode)) self)
	      ;; Restore Workspace appearance:
	      (tell self 'display-workspace-state))
	    (else (delegate msg generic-event))))))))


(define make-concept-mapping-event
  (lambda (cm bridge)
    (let* ((generic-event (make-generic-event 'concept-mapping))
	   (bridge-type (tell bridge 'get-bridge-type))
	   (print-name
	     (format "~a:~a"
	       (case bridge-type
		 (top "T")
		 (vertical "V")
		 (bottom "B"))
	       (tell cm 'print-name)))
	   (slippage? (tell cm 'slippage?))
	   (theme-pattern
	     (list
	       (bridge-type->theme-type (tell bridge 'get-bridge-type))
	       (list (tell cm 'get-CM-type) (tell cm 'get-label))))
	   (cm-strength (tell cm 'get-strength))
	   (concept-pattern (tell cm 'get-concept-pattern)))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'concept-mapping-event)
	    (print-name () print-name)
	    (print ()
	      (print generic-event)
	      (printf "~a ~a~n"
		(if slippage? "Slippage" "Concept-mapping")
		(tell cm 'print-name)))
	    (type? (type)
	      (or (eq? type 'workspace) (tell generic-event 'type? type)))
	    (CM-type? (type) (eq? type (tell cm 'get-CM-type)))
	    (bridge-type? (type) (eq? type bridge-type))
	    (relevant-for-answer-description? ()
	      (and (eq? bridge-type 'vertical)
		   (tell self 'currently-present?)))
	    (get-concept-mapping () cm)
	    (get-CM-type () (tell cm 'get-CM-type))
	    (get-bridge () bridge)
	    (currently-present? ()
	      (tell *workspace* 'bridge-present? bridge))
	    (get-equivalent-bridge ()
	      (tell *workspace* 'get-equivalent-bridge bridge))
	    (get-bridge-type () bridge-type)
	    (get-theme-pattern () theme-pattern)
	    (get-concept-pattern () concept-pattern)
	    (get-strength () cm-strength)
	    (slippage? () slippage?)
	    (equal? (event)
	      (and (tell event 'type? 'concept-mapping)
		   (CMs-equal? cm (tell event 'get-concept-mapping))))
	    (display ()
	      (set! *display-mode?* #t)
	      (tell *workspace-window* 'clear)
	      (tell self 'display-workspace)
	      (if* (tell *themespace* 'current-state-displayed?)
		(tell *themespace* 'save-current-state))
	      (tell *themespace* 'delete-everything)
	      (tell *themespace* 'thematic-pressure-off)
	      (impose-theme-pattern theme-pattern)
	      ;; Display concept-mapping concepts:
	      (tell *slipnet-window* 'display-patterns
		(list concept-pattern)
		%concept-mapping-event-concept-pattern-color%
		(format "~a Concepts" (if slippage? "Slippage" "Concept-mapping")))
	      ;; Blank coderack window:
	      (tell *coderack-window* 'blank-window "Coderack")
	      ;; Display temperature:
	      (tell *temperature-window* 'update-graphics
		(tell self 'get-temperature)))
	    (display-workspace ()
	      (tell *workspace-window* 'draw-event-header
		(format "Made ~a ~a"
		  (tell cm 'print-name)
		  (if slippage? "slippage" "concept-mapping"))
		self)
	      ;; Restore Workspace appearance:
	      (tell self 'display-workspace-state)
	      ;; Highlight bridge and concept-mapping (if possible):
	      (tell *workspace-window* 'highlight-bridges
		%workspace-event-structure-color% (list bridge))
	      (if* (tell bridge 'concept-mapping-graphics-active?)
		(tell *workspace-window* 'display-in-color
		  cm %workspace-event-structure-color%)))
	    (else (delegate msg generic-event))))))))


(define make-group-event
  (lambda (group flipped?)
    (let ((generic-event (make-generic-event 'group))
	  (print-name
	    (let ((text (group-event-pexp-text-string group))
		  (direction (tell group 'get-direction)))
	      (cond
		((eq? direction plato-right) (format ">~a>" text))
		((eq? direction plato-left) (format "<~a<" text))
		(else (format "[~a]" text)))))
	  (group-category (tell group 'get-group-category))
	  (direction (tell group 'get-direction))
	  (group-strength (tell group 'get-strength))
	  (concept-pattern (tell group 'get-concept-pattern)))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'group-event)
	    (print-name () print-name)
	    (print ()
	      (print generic-event)
	      (print group))
	    (type? (type)
	      (or (eq? type 'workspace) (tell generic-event 'type? type)))
	    (relevant-for-answer-description? ()
	      (and (tell self 'spanning?)
		   (tell self 'currently-present?)))
	    (spanning? () (tell group 'spans-whole-string?))
	    (string-type? (string-type) (eq? (tell group 'which-string) string-type))
	    (get-string-type () (tell group 'which-string))
	    (spans? (string-type)
	      (and (tell self 'spanning?)
		   (tell self 'string-type? string-type)))
	    (flipped? () flipped?)
	    (get-group () group)
	    (currently-present? ()
	      (tell (tell group 'get-string) 'group-present? group))
	    (get-equivalent-group ()
	      (tell (tell group 'get-string) 'get-equivalent-group group))
	    (get-string-type () (tell group 'which-string))
	    (get-group-category () group-category)
	    (get-direction () direction)
	    (get-concept-pattern () concept-pattern)
	    (get-strength () group-strength)
	    (equal? (event)
	      (and (tell event 'type? 'group)
		   (equivalent-workspace-objects? group (tell event 'get-group))))
	    (display ()
	      (set! *display-mode?* #t)
	      (tell *workspace-window* 'clear)
	      (tell self 'display-workspace)
	      ;; Blank themespace window:
	      (if* (tell *themespace* 'current-state-displayed?)
		(tell *themespace* 'save-current-state))
	      (tell *themespace* 'delete-everything)
	      (tell *themespace* 'thematic-pressure-off)
	      ;; Display concepts in group's descriptions:
	      (tell *slipnet-window* 'display-patterns
		(list concept-pattern)
		%group-event-concept-pattern-color%
		"Group Descriptors")
	      ;; Blank coderack window:
	      (tell *coderack-window* 'blank-window "Coderack")
	      ;; Display temperature:
	      (tell *temperature-window* 'update-graphics
		(tell self 'get-temperature)))
	    (display-workspace ()
	      (tell *workspace-window* 'draw-event-header
		(if flipped?
		  (format "Flipped ~a" (unflipped-group-name group))
		  (format "Built ~a" (full-workspace-object-name group)))
		self)
	      ;; Restore Workspace appearance:
	      (tell self 'display-workspace-state)
	      ;; Highlight group:
	      (tell *workspace-window* 'display-in-color group
		%workspace-event-structure-color%))
	    (else (delegate msg generic-event))))))))


(define equivalent-workspace-objects?
  (lambda (object1 object2)
    (and (eq? (tell object1 'object-type) (tell object2 'object-type))
         (eq? (tell object1 'which-string) (tell object2 'which-string))
	 (= (tell object1 'get-left-string-pos) (tell object2 'get-left-string-pos))
	 (= (tell object1 'get-right-string-pos) (tell object2 'get-right-string-pos))
	 (if (letter? object1)
	   (same-letter-category? object1 object2)
	   (and (same-group-category? object1 object2)
	        (same-group-direction? object1 object2)
		(= (tell object1 'get-group-length) (tell object2 'get-group-length))
		(andmap equivalent-workspace-objects?
		  (tell object1 'get-constituent-objects)
		  (tell object2 'get-constituent-objects)))))))


(define make-rule-event
  (lambda (rule)
    (let* ((generic-event (make-generic-event 'rule))
	   (rule-type (tell rule 'get-rule-type))
	   (print-name
	     (case rule-type
	       (top "[Top Rule]")
	       (bottom "[Bottom Rule]")))
	   (supporting-bridges (tell rule 'get-supporting-horizontal-bridges))
	   (reference-objects
	     (filter-out workspace-string?
	       (case rule-type
		 (top (tell *initial-string* 'get-all-reference-objects rule))
		 (bottom (tell *target-string* 'get-all-reference-objects rule)))))
	   (relative-quality (tell rule 'get-relative-quality))
	   (concept-pattern (tell rule 'get-concept-pattern)))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'rule-event)
	    (print-name () print-name)
	    (print ()
	      (print generic-event)
	      (print rule))
	    (type? (type)
	      (or (eq? type 'workspace) (tell generic-event 'type? type)))
	    (get-rule () rule)
	    (get-rule-type () rule-type)
	    (get-supporting-bridges () supporting-bridges)
	    (get-reference-objects () reference-objects)
	    (get-relative-quality () relative-quality)
	    (get-concept-pattern () concept-pattern)
	    (get-strength () relative-quality)
	    (equal? (event)
	      (and (tell event 'type? 'rule)
		   (tell rule 'equal? (tell event 'get-rule))))
	    (display ()
	      (set! *display-mode?* #t)
	      (tell *workspace-window* 'clear)
	      (tell self 'display-workspace)
	      ;; Display rule's theme-pattern:
	      (if* (tell *themespace* 'current-state-displayed?)
		(tell *themespace* 'save-current-state))
	      (tell *themespace* 'delete-everything)
	      (tell *themespace* 'thematic-pressure-off)
	      (impose-theme-pattern (tell rule 'get-theme-pattern))
	      ;; Display rule concepts:
	      (tell *slipnet-window* 'display-patterns
		(list concept-pattern)
		(case rule-type
		  (top %top-rule-event-concept-pattern-color%)
		  (bottom %bottom-rule-event-concept-pattern-color%))
		(case rule-type
		  (top "Top Rule Concepts")
		  (bottom "Bottom Rule Concepts")))
	      (if* %verbose%
		;; Display relative quality:
		(printf "~n----------------------------------------------------------~n")
		(print rule)
		(tell rule 'show)
		(printf "Quality = ~a~n" (tell rule 'get-quality))
		(printf "Relative quality at creation = ~a~n" relative-quality)
		(printf "Relative quality now = ~a" (tell rule 'get-relative-quality))
		(printf "~n----------------------------------------------------------~n"))
	      ;; Blank coderack window:
	      (tell *coderack-window* 'blank-window "Coderack")
	      ;; Display temperature:
	      (tell *temperature-window* 'update-graphics
		(tell self 'get-temperature)))
	    (display-workspace ()
	      (tell *workspace-window* 'draw-event-header
		(format "Built ~arule" (if (tell rule 'translated?) "translated " ""))
		self)
	      ;; Restore appropriate mapping:
	      (tell self 'display-workspace-state)
	      (if* (tell rule 'translated?)
		(tell *workspace-window* 'display-rule
		  (tell rule 'get-original-rule)
		  %faded-workspace-structure-color%))
	      ;; Highlight reference objects:
	      (for* each object in reference-objects do
		(tell *workspace-window* 'display-in-color object
		  (case rule-type
		    (top %top-bridge-color%)
		    (bottom %bottom-bridge-color%))))
              ;; Highlight supporting bridges:
              (tell *workspace-window* 'highlight-bridges
		(case rule-type
		  (top %top-bridge-color%)
		  (bottom %bottom-bridge-color%))
		supporting-bridges)
              (tell *workspace-window* 'display-rule rule
		(case rule-type
		  (top %top-rule-color%)
		  (bottom %bottom-rule-color%))))
	    (else (delegate msg generic-event))))))))


(define get-snag-theme-pattern
  (lambda (snag-concept-mappings)
    (cons 'vertical-bridge
      (let ((pattern-entries
	      (remove-duplicates
		(map (lambda (cm)
		       (list (tell cm 'get-CM-type) (tell cm 'get-label)))
		  snag-concept-mappings))))
	pattern-entries))))


(define get-snag-concept-pattern
  (lambda (snag-objects)
    (cons 'concepts
      (map (lambda (descriptor)
	     (list descriptor %max-activation%))
	(remq-duplicates
	  (apply append (tell-all snag-objects 'get-all-descriptors)))))))


(define make-snag-event
  (lambda (failure-result rule translated-rule supporting-vertical-bridges
	    slippage-log rule-ref-objects)
    (let* ((generic-event (make-generic-event 'snag))
	   (snag-type (1st failure-result))
	   (snag-objects
	     (record-case failure-result
	       (SWAP (objects dim) objects)
	       (CONFLICT (object1 dim1 object2 dim2) (list object1 object2))
	       (CHANGE (object transform) (list object))))
	   (snag-bridges
	     (compress (tell-all snag-objects 'get-bridge 'vertical)))
	   (snag-concept-mappings
	     (if (null? snag-bridges)
	       (tell *workspace* 'get-all-vertical-CMs)
	       (apply append (tell-all snag-bridges 'get-all-concept-mappings))))
	   (snag-theme-pattern (get-snag-theme-pattern snag-concept-mappings))
	   (snag-concept-pattern (get-snag-concept-pattern snag-objects))
	   (progress-evaluator
	     (lambda (workspace-structure)
	       (if (not (bond? workspace-structure))
		 (tell workspace-structure 'get-strength)
		 0)))
	   (progress-achieved 0))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'snag-event)
	    (print-name () "[Snag]")
	    (print ()
	      (print generic-event)
	      (printf "~a-snag involving object~a:~n"
		(case snag-type
		  (swap "Swap")
		  (conflict "Conflict")
		  (change "Change"))
		(if (eq? snag-type 'change) "" "s"))
	      (for* each object in snag-objects do
		(print object)))
	    (print-patterns ()
	      (printf "Snag theme pattern:~n")
	      (print-pattern snag-theme-pattern)
	      ;; Rule thematic pattern was recorded at the time of rule creation.
	      ;; translated-rule failed, so it has no theme-pattern:
	      (printf "Rule thematic pattern:~n")
	      (print-pattern (tell rule 'get-theme-pattern)))
	    (get-explanation ()
	      (record-case failure-result
		(SWAP (objects dimension)
		  (format "no ~a swap is possible between ~a in ~a"
		    (tell dimension 'get-lowercase-name)
		    (punctuate (map snag-object-phrase objects))
		    (tell (tell (1st objects) 'get-string) 'print-name)))
		(CONFLICT (object1 dimension1 object2 dimension2)
		  (format "changing ~a of ~a conflicts with changing ~a of ~a in ~a"
		    (format "the ~a" (tell dimension1 'get-lowercase-name))
		    (snag-object-phrase object1)
		    (format "the ~a" (tell dimension2 'get-lowercase-name))
		    (snag-object-phrase object2)
		    (tell (tell object1 'get-string) 'print-name)))
		(CHANGE (object transform)
		  (let ((string (tell object 'get-string)))
		    (if (eq? (1st transform) plato-group-category)
		      (format "reversing the starting and ending ~a of ~a ~a"
			(tell (3rd transform) 'get-lowercase-name)
			(snag-object-phrase object)
			(format "is not possible in ~a"
			  (tell string 'print-name)))
		      (format "changing the ~a of ~a to ~a is not possible in ~a"
			(tell (1st transform) 'get-lowercase-name)
			(snag-object-phrase object)
			(if (platonic-relation? (2nd transform))
			  (format "its ~a" (tell (2nd transform) 'get-lowercase-name))
			  (format "`~a'" (tell (2nd transform) 'get-lowercase-name)))
			(tell string 'print-name)))))))
	    (get-failure-result () failure-result)
	    (get-rule (rule-type)
	      (case rule-type
		(top rule)
		(bottom translated-rule)))
	    (get-supporting-bridges (bridge-type)
	      ;; Snags have no supporting bottom bridges since translated-rule failed
	      (case bridge-type
		(top (tell rule 'get-supporting-horizontal-bridges))
		(vertical supporting-vertical-bridges)))
	    (get-slippage-log () slippage-log)
	    (get-rule-ref-objects () rule-ref-objects)
	    (get-snag-type () snag-type)
	    (get-snag-objects () snag-objects)	      
	    (get-snag-bridges () snag-bridges)
	    (get-snag-concept-mappings () snag-concept-mappings)
	    (get-snag-theme-pattern () snag-theme-pattern)
	    (get-snag-concept-pattern () snag-concept-pattern)
	    (get-progress-evaluator () progress-evaluator)
	    (get-progress-achieved () progress-achieved)
	    (update-progress-achieved (new-value)
	      (set! progress-achieved new-value)
	      'done)
	    (get-strength () 100)
	    (equal? (event)
	      (and (tell event 'type? 'snag)
		   (tell translated-rule 'equal? (tell event 'get-rule 'bottom))
		   (sets-equal-pred? equivalent-workspace-objects?
		     snag-objects (tell event 'get-snag-objects))))
	    (activate ()
	      (tell *trace* 'undo-last-clamp)
	      (for* each object in snag-objects do
		(tell object 'clamp-salience))
	      (clamp-concept-pattern snag-concept-pattern)
	      (if* %slipnet-graphics%
		(tell *slipnet-window* 'update-graphics))
	      'done)
	    (deactivate ()
	      (for* each object in snag-objects do
		(tell object 'unclamp-salience))
	      (unclamp-concept-pattern snag-concept-pattern)
	      (if* %slipnet-graphics%
		(tell *slipnet-window* 'update-graphics))
	      'done)
	    (display ()
	      (vprintf "Progress achieved by snag = ~a~n" progress-achieved)
	      (set! *display-mode?* #t)
	      (tell *workspace-window* 'clear)
	      (tell self 'display-workspace)
	      ;; Display snag theme pattern:
	      (if* (tell *themespace* 'current-state-displayed?)
		(tell *themespace* 'save-current-state))
	      (tell *themespace* 'delete-everything)
	      (tell *themespace* 'thematic-pressure-off)
	      (impose-theme-pattern snag-theme-pattern)
	      ;; Display concept pattern:
	      (tell *slipnet-window* 'display-patterns
		(list snag-concept-pattern)
		%snag-event-concept-pattern-color%
		"Snag Object Descriptors")
	      ;; Blank coderack window:
	      (tell *coderack-window* 'blank-window "Coderack")
	      ;; Display maximum temperature:
	      (tell *temperature-window* 'update-graphics 100))
	    (display-workspace ()
	      (tell *workspace-window* 'draw-event-header "Snag" self)
	      ;; Restore Workspace appearance:
	      (tell self 'display-workspace-state)
	      ;; Highlight vertical mapping:
	      (tell *workspace-window* 'highlight-bridges %vertical-bridge-color%
		(tell self 'get-supporting-bridges 'vertical))
	      ;; Highlight applied vertical slippages:
	      (tell *workspace-window* 'highlight-applied-slippages slippage-log)
	      ;; Highlight top mapping:
	      (for* each obj in rule-ref-objects do
		(tell *workspace-window* 'display-in-color obj %top-bridge-color%))
	      (tell *workspace-window* 'highlight-bridges %top-bridge-color%
		(tell self 'get-supporting-bridges 'top))
	      ;; Highlight snag objects:
	      (for* each object in snag-objects do
		(if* (not (workspace-string? object))
		  (tell *workspace-window* 'display-in-color object %snag-color%)))
	      ;; Display rules:
	      (tell *workspace-window* 'display-rule rule %top-rule-color%)
	      (tell *workspace-window* 'display-rule translated-rule %snag-color%)
	      (tell *workspace-window* 'question-mark 'draw "???" %snag-color%))
	    (make-snag-description-pexp (theme-bridges theme-CMs)
	      (tell *workspace-window* 'caching-on)
	      (tell *workspace-window* 'clear)
	      (tell *workspace-window* 'draw-header "Snag Description")
	      (tell *workspace-window* 'draw-all-letters)
	      (tell *workspace-window* 'workspace-arrow 'top)
	      (tell *workspace-window* 'workspace-arrow 'bottom)
	      (for* each b in theme-bridges do
		(let ((object1 (tell b 'get-object1))
		      (object2 (tell b 'get-object2)))
		  (if* (group? object1)
		    (tell *workspace-window* 'display-group object1))
		  (tell *workspace-window* 'display-bridge b)
		  (if* (group? object2)
		    (tell *workspace-window* 'display-group object2))))
	      ;; Highlight vertical mapping:
	      (tell *workspace-window* 'highlight-bridges %vertical-bridge-color%
		(tell self 'get-supporting-bridges 'vertical))
	      ;; Highlight the concept-mappings supporting the theme pattern and
	      ;; the slippages supporting rule translation:
	      (for* each cm in theme-CMs do
		(tell *workspace-window* 'display-in-color cm
		  %theme-supporting-concept-mapping-color%))
	      (for* each s in (tell slippage-log 'get-applied-slippages) do
		(let ((h (tell slippage-log 'get-slippage-to-highlight s)))
		  (tell *workspace-window* 'display-in-color h
		    %vertical-slippage-color%)))
	      ;; Display top mapping:
	      (for* each obj in rule-ref-objects do
		(tell *workspace-window* 'display-in-color obj %top-bridge-color%))
	      (tell *workspace-window* 'highlight-bridges %top-bridge-color%
		(tell self 'get-supporting-bridges 'top))
	      ;; Highlight snag objects:
	      (for* each object in snag-objects do
		(if* (not (workspace-string? object))
		  (tell *workspace-window* 'display-in-color object %snag-color%)))
	      ;; Display rules:
	      (tell *workspace-window* 'display-rule rule %top-rule-color%)
	      (tell *workspace-window* 'display-rule translated-rule %snag-color%)
	      (tell *workspace-window* 'question-mark 'draw "???" %snag-color%)
	      (let ((pexp (tell *workspace-window* 'get-cached-pexp)))
		(tell *workspace-window* 'clear-pending-flush)
		pexp))
	    (else (delegate msg generic-event))))))))


(define snag-object-phrase
  (lambda (object)
    (cond
      ((workspace-string? object)
       (format "the string ~a" (quoted-string object)))
      ((letter? object)
       (format "the letter ~a" (tell object 'print-name)))
      ((group? object)
       (format "the ~a group"
	 (apply string-append
	   (tell-all (tell object 'get-letters) 'print-name)))))))


(define full-slipnode-name
  (lambda (slipnode)
    (cond
      ((eq? slipnode plato-alphabetic-position-category) "Alphabetic-Position")
      ((eq? slipnode plato-bond-facet) "Bond-Facet")
      ((eq? slipnode plato-object-category) "Object-Category")
      ((eq? slipnode plato-letter-category) "Letter-Category")
      ((eq? slipnode plato-length) "Length")
      ((eq? slipnode plato-bond-category) "Bond-Category")
      ((eq? slipnode plato-group-category) "Group-Category")
      ((eq? slipnode plato-direction-category) "Direction")
      ((eq? slipnode plato-string-position-category) "String-Position")
      ((eq? slipnode plato-opposite) "Opposite")
      ((eq? slipnode plato-identity) "Identity")
      ((eq? slipnode plato-predgrp) "predecessor-group")
      ((eq? slipnode plato-succgrp) "successor-group")
      ((eq? slipnode plato-samegrp) "sameness-group")
      (else (tell slipnode 'get-lowercase-name)))))


(define unflipped-group-name
  (lambda (group)
    (cond
      ((eq? (tell group 'get-group-category) plato-predgrp) "successor-group")
      ((eq? (tell group 'get-group-category) plato-succgrp) "predecessor-group"))))


(define full-workspace-object-name
  (lambda (object)
    (cond
      ((letter? object) "letter")
      ((eq? (tell object 'get-group-category) plato-samegrp) "sameness-group")
      ((eq? (tell object 'get-group-category) plato-predgrp) "predecessor-group")
      ((eq? (tell object 'get-group-category) plato-succgrp) "successor-group"))))


;;----------------------------------------------------------------------------
;; Workspace and Slipnet event monitoring

(define %concept-mapping-importance-threshold% 65)
(define %concept-activation-importance-threshold% 85)
(define %group-importance-threshold% 100)
(define %rule-importance-threshold% 67)

(define monitor-slipnode-activation-change
  (lambda (slipnode previous-activation new-activation)
    (let ((importance (concept-activation-importance
			slipnode previous-activation new-activation)))
      (if* (>= importance %concept-activation-importance-threshold%)
	(let ((concept-activation-event (make-concept-activation-event slipnode)))
	  (tell *trace* 'add-event concept-activation-event))))))

(define monitor-new-concept-mappings
  (lambda (concept-mappings bridge)
    (for* each cm in concept-mappings do
      (let ((importance (concept-mapping-importance cm bridge)))
	(if* (>= importance %concept-mapping-importance-threshold%)
	  (let ((concept-mapping-event (make-concept-mapping-event cm bridge)))
	    (tell *trace* 'add-event concept-mapping-event)))))))

(define monitor-new-groups
  (lambda (group flipped?)
    (let ((importance (group-importance group flipped?)))
      (if* (>= importance %group-importance-threshold%)
	(let ((group-event (make-group-event group flipped?)))
	  (tell *trace* 'add-event group-event))))))

(define monitor-new-rules
  (lambda (rule)
    (let ((importance (rule-importance rule)))
      (if* (>= importance %rule-importance-threshold%)
	(let ((rule-event (make-rule-event rule)))
	  (tell *trace* 'add-event rule-event))))))

(define concept-activation-importance
  (lambda (slipnode previous-activation new-activation)
    (let ((delta (- new-activation previous-activation)))
      (100* (* (% (abs delta)) (% (cd slipnode)))))))

(define group-importance
  (lambda (group flipped?)
    (if (or flipped?
	    (tell group 'spans-whole-string?)
	    (and (tell group 'singleton-group?)
	         (tell group 'description-type-present? plato-length)))
      100
      (tell group 'get-strength))))

(define rule-importance
  (lambda (rule)
    (if (= (tell rule 'get-uniformity) 100)
      100
      (tell rule 'get-relative-quality))))

(define concept-mapping-importance
  (lambda (cm bridge)
    (cond
      ((not (tell cm 'slippage?)) 0)
      ((tell *themespace* 'supported-by-active-theme? cm bridge) 100)
      (else (let ((CM-type (tell cm 'get-CM-type))
		  (descriptor1 (tell cm 'get-descriptor1))
		  (descriptor2 (tell cm 'get-descriptor2))
		  (label (tell cm 'get-label))
		  (object1 (tell bridge 'get-object1))
		  (object2 (tell bridge 'get-object2)))
	      (if (eq? CM-type plato-bond-category)
		0
		(round
		  (weighted-average
		    (list
		      (cd CM-type)
		      (if (tell bridge 'spanning-bridge?) 100 0)
		      (average (cd descriptor1) (cd descriptor2))
		      (if (exists? label) (cd label) 50))
		    (list 4 2 2 3)))))))))


;;--------------------------------- Patterns -----------------------------------
;;
;; NOTE: there is a slight discrepency between the description of theme-patterns
;; given on page 146 of my PhD thesis and the structure of theme-patterns in the
;; code here.  Vertical theme-patterns use the tag symbol 'vertical-bridge, not
;; 'vertical-themes as shown on page 146.  I used 'vertical-themes in the thesis
;; simply for clarity.
;;
;;
;;         <pattern> ::= <theme-pattern>
;;                     | <concept-pattern>
;;                     | <codelet-pattern>
;;
;;   <theme-pattern> ::= (<theme-type> <entry> ...)
;;      <theme-type> ::= top-bridge | vertical-bridge | bottom-bridge
;;           <entry> ::= (<dimension> <relation>)
;;                     | (<dimension> <relation> <activation>)
;;
;; <concept-pattern> ::= (concepts <entry> ...)
;;           <entry> ::= (<slipnode> <activation>)
;;
;; <codelet-pattern> ::= (codelets <entry> ...)
;;           <entry> ::= (<codelet-type> <urgency>)

(define theme-pattern?
  (lambda (pattern)
    (member? (1st pattern) '(top-bridge vertical-bridge bottom-bridge))))

(define concept-pattern?
  (lambda (pattern)
    (eq? (1st pattern) 'concepts)))

(define codelet-pattern?
  (lambda (pattern)
    (eq? (1st pattern) 'codelets)))

(define same-pattern-type?
  (lambda (pattern1 pattern2)
    (eq? (1st pattern1) (1st pattern2))))

(define pattern-type-present?
  (lambda (pattern patterns)
    (exists? (assq (1st pattern) patterns))))

(define negate-theme-pattern-entry
  (lambda (entry)
    (let ((negative-activation
	    (if (= (length entry) 3)
	      (- (3rd entry))
	      (- %max-theme-activation%))))
      (list (1st entry) (2nd entry) negative-activation))))

;; These functions ignore positive/negative theme activations (if any),
;; slipnode activations, and codelet urgencies:

(define patterns-equal?
  (lambda (pattern1 pattern2)
    (cond
      ((not (same-pattern-type? pattern1 pattern2)) #f)
      ((concept-pattern? pattern1) (concept-patterns-equal? pattern1 pattern2))
      ((codelet-pattern? pattern1) (codelet-patterns-equal? pattern1 pattern2))
      ((theme-pattern? pattern1) (theme-patterns-equal? pattern1 pattern2)))))

(define theme-patterns-equal?
  (lambda (pattern1 pattern2)
    (and (eq? (1st pattern1) (1st pattern2))
         (sets-equal-pred?
	   theme-pattern-entries-equal?
	   (entries pattern1) (entries pattern2)))))

(define theme-pattern-entries-equal?
  (lambda (entry1 entry2)
    (and (eq? (1st entry1) (1st entry2))
         (eq? (2nd entry1) (2nd entry2)))))

(define concept-patterns-equal?
  (lambda (pattern1 pattern2)
    (sets-equal?
      (map 1st (entries pattern1))
      (map 1st (entries pattern2)))))

(define codelet-patterns-equal?
  (lambda (pattern1 pattern2)
    (sets-equal?
      (map 1st (entries pattern1))
      (map 1st (entries pattern2)))))

(define entries rest)

(define print-pattern
  (lambda (pattern)
    (printf "(~a" (1st pattern))
    (case (1st pattern)
      (concepts
	(for* each entry in (entries pattern) do
	  (printf "~n  (~a ~a)"
	    (tell (1st entry) 'get-short-name)
	    (2nd entry))))
      ((top-bridge vertical-bridge bottom-bridge)
       (for* each entry in (entries pattern) do
	 (printf "~n  (~a ~a~a)"
	   (tell (1st entry) 'get-short-name)
	   (relation-name (2nd entry))
	   (if (= (length entry) 3) (format " ~a" (3rd entry)) ""))))
      (codelets
	(for* each entry in (entries pattern) do
	  (printf "~n  (~a ~a)"
	    (tell (1st entry) 'get-codelet-type-name)
	    (let ((name (urgency-name (2nd entry))))
	      (if (exists? name) name (2nd entry)))))))
    (printf ")~n")))
  
;;----------------------------------------------------------------------
;; Theme-patterns

(define get-associated-concept-pattern
  (lambda (theme-pattern)
    (cons 'concepts
      (remove-duplicates
	(flatmap
	  (lambda (entry)
	    (let ((dim (1st entry))
		  (rel (2nd entry))
		  (act (if (= (length entry) 3) (3rd entry) %max-theme-activation%)))
	      (cons (list dim %max-activation%)
		(if (eq? rel plato-opposite)
		  (list (list rel (if (> act 0) %max-activation% 0)))
		  '()))))
	  (entries theme-pattern))))))

;; impose-theme-pattern does not affect the frozen status of clusters
;; or the thematic-pressure status of the themespace.

(define impose-theme-pattern
  (lambda (theme-pattern)
    (let ((theme-type (1st theme-pattern)))
      (for* each entry in (entries theme-pattern) do
	(let ((dim (1st entry))
	      (rel (2nd entry))
	      (act (if (= (length entry) 3) (3rd entry) %max-theme-activation%)))
	  (tell *themespace* 'set-theme-activation theme-type dim rel act))))))

(define clamp-theme-pattern
  (lambda (theme-pattern)
    (let ((theme-type (1st theme-pattern)))
      (tell *themespace* 'delete-theme-type theme-type)
      (impose-theme-pattern theme-pattern)
      (tell *themespace* 'freeze-theme-type theme-type)
      (tell *themespace* 'thematic-pressure-on theme-type))))

(define unclamp-theme-pattern
  (lambda (theme-pattern)
    (let ((theme-type (1st theme-pattern)))
      (tell *themespace* 'unfreeze-theme-type theme-type)
      (tell *themespace* 'thematic-pressure-off theme-type))))

;;----------------------------------------------------------------------
;; Concept-patterns

(define clamp-concept-pattern
  (lambda (concept-pattern)
    (for* each entry in (entries concept-pattern) do
      (let ((node (1st entry))
	    (act (2nd entry)))
	(tell node 'clamp act)))))

(define unclamp-concept-pattern
  (lambda (concept-pattern)
    (for* each entry in (entries concept-pattern) do
      (tell (1st entry) 'unfreeze))))

;;----------------------------------------------------------------------
;; Codelet-patterns

(define get-complement-codelet-pattern
  (lambda (urgency codelet-patterns)
    (let* ((all-specified-pattern-entries (flatmap entries codelet-patterns))
	   (all-specified-codelet-types
	     (remq-duplicates (map 1st all-specified-pattern-entries)))
	   (unspecified-codelet-types
	     (remq-elements all-specified-codelet-types *codelet-types*))
	   (complement-pattern-entries
	     (map (lambda (type) (list type urgency))
	       unspecified-codelet-types)))
      (cons 'codelets complement-pattern-entries))))

(define against-background
  (lambda (urgency . codelet-patterns)
    (let ((specified-entries
	    (flatmap entries codelet-patterns))
	  (complement-entries
	    (entries (get-complement-codelet-pattern urgency codelet-patterns))))
      (cons 'codelets
	(append specified-entries complement-entries)))))

(define clamp-codelet-pattern
  (lambda (codelet-pattern)
    (for* each entry in (entries codelet-pattern) do
      (let ((codelet-type (1st entry))
	    (urgency (2nd entry)))
	(tell codelet-type 'clamp urgency)))))

(define unclamp-codelet-pattern
  (lambda (codelet-pattern)
    (for* each entry in (entries codelet-pattern) do
      (tell (1st entry) 'unclamp))))

;; not all of these codelet-patterns are used

(define %top-down-codelet-pattern%
  `(codelets
     (,top-down-bond-scout:direction ,%very-high-urgency%)
     (,top-down-group-scout:direction ,%very-high-urgency%)
     (,top-down-bond-scout:category ,%very-high-urgency%)
     (,top-down-group-scout:category ,%very-high-urgency%)
     (,top-down-description-scout ,%very-high-urgency%)
     (,bond-evaluator ,%extremely-high-urgency%)
     (,bond-builder ,%extremely-high-urgency%)
     (,group-evaluator ,%extremely-high-urgency%)
     (,group-builder ,%extremely-high-urgency%)
     (,description-evaluator ,%extremely-high-urgency%)
     (,description-builder ,%extremely-high-urgency%)))

(define %bottom-up-codelet-pattern%
  `(codelets
     (,bottom-up-bond-scout ,%very-high-urgency%)
     (,bond-evaluator ,%extremely-high-urgency%)
     (,bond-builder ,%extremely-high-urgency%)
     (,group-scout:whole-string ,%very-high-urgency%)
     (,group-evaluator ,%extremely-high-urgency%)
     (,group-builder ,%extremely-high-urgency%)
     (,bottom-up-bridge-scout ,%very-high-urgency%)
     (,important-object-bridge-scout ,%very-high-urgency%)
     (,bridge-evaluator ,%extremely-high-urgency%)
     (,bridge-builder ,%extremely-high-urgency%)
     (,bottom-up-description-scout ,%very-high-urgency%)
     (,description-evaluator ,%extremely-high-urgency%)
     (,description-builder ,%extremely-high-urgency%)
     (,rule-scout ,%very-high-urgency%)
     (,rule-evaluator ,%extremely-high-urgency%)
     (,rule-builder ,%extremely-high-urgency%)))

(define %thematic-codelet-pattern%
  `(codelets
     (,thematic-bridge-scout ,%extremely-high-urgency%)))

(define %rule-codelet-pattern%
  `(codelets
     (,rule-scout ,%very-high-urgency%)
     (,rule-evaluator ,%extremely-high-urgency%)
     (,rule-builder ,%extremely-high-urgency%)))

(define %bond-codelet-pattern%
  `(codelets
     (,bottom-up-bond-scout ,%very-high-urgency%)
     (,bond-evaluator ,%extremely-high-urgency%)
     (,bond-builder ,%extremely-high-urgency%)))

(define %group-codelet-pattern%
  `(codelets
     (,group-scout:whole-string ,%very-high-urgency%)
     (,group-evaluator ,%extremely-high-urgency%)
     (,group-builder ,%extremely-high-urgency%)))

(define %bridge-codelet-pattern%
  `(codelets
     (,bottom-up-bridge-scout ,%very-high-urgency%)
     (,important-object-bridge-scout ,%very-high-urgency%)
     (,bridge-evaluator ,%extremely-high-urgency%)
     (,bridge-builder ,%extremely-high-urgency%)))

(define %description-codelet-pattern%
  `(codelets
     (,bottom-up-description-scout ,%very-high-urgency%)
     (,description-evaluator ,%extremely-high-urgency%)
     (,description-builder ,%extremely-high-urgency%)))

(define %answer-codelet-pattern%
  `(codelets
     (,answer-finder ,%extremely-high-urgency%)
     (,answer-justifier ,%extremely-high-urgency%)))

;;----------------------------------------------------------------------

(define *trace* (make-temporal-trace))
