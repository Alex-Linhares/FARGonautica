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

(define make-memory
  (lambda ()
    (let ((answer-descriptions '())
	  (snag-descriptions '())
	  (all-descriptions '()))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'memory)
	    (print ()
	      (for* each answer in all-descriptions do
		(print answer)))
	    (clear ()
	      (set! answer-descriptions '())
	      (set! snag-descriptions '())
	      (set! all-descriptions '())
	      (tell *memory-window* 'initialize)
	      'done)
	    (clear-activations ()
	      (for* each answer in answer-descriptions do
		(tell answer 'update-activation 0))
	      'done)
	    (delete (answer)
	      (cond
		((member? answer answer-descriptions)
		 (set! answer-descriptions (remq answer answer-descriptions))
		 (set! all-descriptions (remq answer all-descriptions))
		 (tell *memory-window* 'erase-memory-icon answer)
		 'done)
		((member? answer snag-descriptions)
		 (set! snag-descriptions (remq answer snag-descriptions))
		 (set! all-descriptions (remq answer all-descriptions))
		 (tell *memory-window* 'erase-memory-icon answer)
		 'done)
		(else (printf "Not currently in memory.~n"))))
	    (get-answers () answer-descriptions)
	    (get-snags () snag-descriptions)
	    (get-all-descriptions () all-descriptions)
	    (get-highlighted-description ()
	      (select-meth all-descriptions 'highlighted?))
	    (unhighlight-all-answers ()
	      (for* each answer in all-descriptions do
		(tell answer 'unhighlight))
	      'done)
	    (unhighlight-all-answers-except (exception)
	      (for* each answer in all-descriptions do
		(if* (not (eq? answer exception))
		  (tell answer 'unhighlight))))
	    (get-mouse-selected-answer (x y)
	      (select
		(lambda (answer) (tell answer 'within-bounding-box? x y))
		all-descriptions))
	    (get-other-highlighted-answer (exception)
	      (select
		(lambda (answer)
		  (and (not (eq? answer exception))
		       (tell answer 'highlighted?)))
		answer-descriptions))
	    (snag-present? (snag-rule)
	      (ormap-meth snag-descriptions 'equal?
		(tell *initial-string* 'get-letter-categories)
		(tell *modified-string* 'get-letter-categories)
		(tell *target-string* 'get-letter-categories)
		(tell snag-rule 'get-rule-clauses)))
	    (get-equivalent-snag (answer)
	      (select-meth snag-descriptions 'equal?
		(tell answer 'get-initial-letters)
		(tell answer 'get-modified-letters)
		(tell answer 'get-target-letters)
		(tell answer 'get-top-rule-clauses)))
	    (answer-present? (answer-letters top-rule bottom-rule)
	      (ormap-meth answer-descriptions 'equal?
		(tell *initial-string* 'get-letter-categories)
		(tell *modified-string* 'get-letter-categories)
		(tell *target-string* 'get-letter-categories)
		answer-letters
		(tell top-rule 'get-rule-clauses)
		(tell bottom-rule 'get-rule-clauses)))
	    (add-answer-description (new-answer)
	      ;; This needs to be set before calling add-memory-icon:
	      (tell new-answer 'set-activation 100)
	      (tell *memory-window* 'add-memory-icon new-answer)
	      (for* each answer in answer-descriptions do
		(tell answer 'compare new-answer))
	      (set! answer-descriptions (cons new-answer answer-descriptions))
	      (set! all-descriptions (cons new-answer all-descriptions))
	      'done)
	    (add-snag-description (new-snag)
	      (set! snag-descriptions (cons new-snag snag-descriptions))
	      (set! all-descriptions (cons new-snag all-descriptions))
	      (tell *memory-window* 'add-memory-icon new-snag)
	      'done)
	    (else (delegate msg base-object))))))))


(define make-answer-description
  (lambda (initial-letters modified-letters target-letters answer-letters
	    top-rule-clauses bottom-rule-clauses
	    top-rule-phrases bottom-rule-phrases
	    top-rule-abstractness bottom-rule-abstractness
	    temperature quality vertical-theme-pattern top-theme-pattern
	    bottom-theme-pattern unjustified-theme-pattern unjustified-slippages)
    (let* ((initial-print-name
	     (apply string-append (tell-all initial-letters 'print-name)))
	   (modified-print-name
	     (apply string-append (tell-all modified-letters 'print-name)))
	   (target-print-name
	     (apply string-append (tell-all target-letters 'print-name)))
	   (answer-print-name
	     (apply string-append (tell-all answer-letters 'print-name)))
	   (activation 0)
	   (get-normal-icon-pexp #f)
	   (highlight-icon-pexp #f)
	   (highlighted? #f)
	   (bounding-box-x1 #f)
	   (bounding-box-y1 #f)
	   (bounding-box-x2 #f)
	   (bounding-box-y2 #f)
	   (answer-description-pexp #f)
	   (this-run *this-run*))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'answer-description)
	    (print-name ()
	      (format "~a -> ~a, ~a -> ~a"
		initial-print-name modified-print-name
		target-print-name answer-print-name))
	    (problem-print-name ()
	      (format "~a -> ~a, ~a -> ?"
		initial-print-name modified-print-name target-print-name))
	    (print ()
	      (printf "Answer Description \"~a\"  (Quality = ~a)~n"
		(tell self 'print-name)
		quality))
	    (get-initial-letters () initial-letters)
	    (get-modified-letters () modified-letters)
	    (get-target-letters () target-letters)
	    (get-answer-letters () answer-letters)
	    (get-initial-print-name () initial-print-name)
	    (get-modified-print-name () modified-print-name)
	    (get-target-print-name () target-print-name)
	    (get-answer-print-name () answer-print-name)
	    (get-top-rule-clauses () top-rule-clauses)
	    (get-bottom-rule-clauses () bottom-rule-clauses)
	    (get-top-rule-phrases () top-rule-phrases)
	    (get-bottom-rule-phrases () bottom-rule-phrases)
	    (get-top-rule-abstractness () top-rule-abstractness)
	    (get-bottom-rule-abstractness () bottom-rule-abstractness)
	    (get-temperature () temperature)
	    (get-quality () quality)
	    (get-vertical-theme-pattern () vertical-theme-pattern)
	    (get-top-theme-pattern () top-theme-pattern)
	    (get-bottom-theme-pattern () bottom-theme-pattern)
	    ;; Top and bottom theme patterns are stored in answer descriptions
	    ;; but are not used in comparing answers with each other:
	    (get-themes () (entries vertical-theme-pattern))
	    (get-unjustified-theme-pattern () unjustified-theme-pattern)
	    (get-unjustified-themes () (entries unjustified-theme-pattern))
	    (get-unjustified-slippages () unjustified-slippages)
	    (get-this-run () this-run)
	    (unjustified? () (not (null? unjustified-slippages)))
	    (answers-equal? (answer)
	      (tell self 'equal?
		(tell answer 'get-initial-letters)
		(tell answer 'get-modified-letters)
		(tell answer 'get-target-letters)
		(tell answer 'get-answer-letters)
		(tell answer 'get-top-rule-clauses)
		(tell answer 'get-bottom-rule-clauses)))
	    (equal? (i-letters m-letters t-letters a-letters top-clauses bot-clauses)
	      (and (equal? i-letters initial-letters)
		   (equal? m-letters modified-letters)
		   (equal? t-letters target-letters)
		   (equal? a-letters answer-letters)
		   (rule-clause-lists-equal? top-clauses top-rule-clauses)
		   (rule-clause-lists-equal? bot-clauses bottom-rule-clauses)))
	    (get-activation () activation)
	    (set-activation (value)
	      (set! activation value)
	      'done)
	    (update-activation (new-value)
	      (if* (not (= new-value activation))
		(tell *memory-window* 'draw
		  (get-normal-icon-pexp new-value)))
	      (set! activation new-value)
	      'done)
	    (get-answer-distance (other-answer)
	      (calculate-answer-distance self other-answer))
	    (compare (new-answer)
	      (let* ((distance (calculate-answer-distance self new-answer))
		     (new-activation
		       (100- (100* (min 1 (/ distance %distance-threshold%))))))
		(tell self 'update-activation new-activation)
		(if* (> new-activation 0)
		  (tell *comment-window* 'add-comment
		    (list
		      (format "This answer ~a of the answer ~a to ~a."
			(cond
			  ((> new-activation 70) "strongly reminds me")
			  ((> new-activation 30) "reminds me somewhat")
			  (else "vaguely reminds me"))
			answer-print-name
			(format "the problem \"~a\"" (tell self 'problem-print-name))))
		    (list
		      "This answer is reminiscent of the answer "
		      (format "~a to ~a.  Reminding strength = ~a."
			answer-print-name
			(format "the problem \"~a\"" (tell self 'problem-print-name))
			new-activation)))))
	      'done)
	    (get-answer-description-pexp () answer-description-pexp)
	    (get-normal-icon-pexp () (get-normal-icon-pexp activation))
	    (get-highlight-icon-pexp () highlight-icon-pexp)
	    (set-answer-description-pexp (pexp)
	      (set! answer-description-pexp pexp)
	      'done)
	    (set-graphics-info (proc highlight-icon)
	      (set! get-normal-icon-pexp proc)
	      (set! highlight-icon-pexp highlight-icon)
	      'done)
	    (get-bounding-box-x1 () bounding-box-x1)
	    (get-bounding-box-y1 () bounding-box-y1)
	    (get-bounding-box-x2 () bounding-box-x2)
	    (get-bounding-box-y2 () bounding-box-y2)
	    (set-bounding-box (x1 y1 x2 y2)
	      (set! bounding-box-x1 x1)
	      (set! bounding-box-y1 y1)
	      (set! bounding-box-x2 x2)
	      (set! bounding-box-y2 y2)
	      'done)
	    (within-bounding-box? (x y)
	      (and (>= x bounding-box-x1) (<= x bounding-box-x2)
		   (>= y bounding-box-y1) (<= y bounding-box-y2)))
	    (highlighted? () highlighted?)
	    (highlight ()
	      (if* (not highlighted?)
		(set! highlighted? #t)
		(tell *memory-window* 'draw highlight-icon-pexp))
	      'done)
	    (unhighlight ()
	      (if* highlighted?
		(set! highlighted? #f)
		(tell *memory-window* 'draw
		  (get-normal-icon-pexp activation)))
	      'done)
	    (toggle-highlight ()
	      (tell self (if highlighted? 'unhighlight 'highlight)))
	    (display ()
	      (set! *display-mode?* #t)
	      (tell *workspace-window* 'draw answer-description-pexp)
	      (if* (tell *themespace* 'current-state-displayed?)
		(tell *themespace* 'save-current-state))
	      (tell *themespace* 'delete-everything)
	      (tell *themespace* 'thematic-pressure-off)
	      (impose-theme-pattern vertical-theme-pattern)
	      (impose-theme-pattern top-theme-pattern)
	      (impose-theme-pattern bottom-theme-pattern)
	      ;; Blank slipnet window:
	      (tell *slipnet-window* 'blank-window)
	      ;; Blank coderack window:
	      (tell *coderack-window* 'blank-window "Coderack")
	      ;; Display answer temperature:
	      (tell *temperature-window* 'update-graphics temperature))
	    (display-workspace ()
	      (tell *workspace-window* 'draw answer-description-pexp))
	    (else (delegate msg base-object))))))))


(define make-snag-description
  (lambda (rule-clauses translated-rule-clauses rule-phrases translated-rule-phrases
	    snag-explanation theme-pattern)
    (let* ((initial-letters (tell *initial-string* 'get-letter-categories))
	   (modified-letters (tell *modified-string* 'get-letter-categories))
	   (target-letters (tell *target-string* 'get-letter-categories))
	   (initial-print-name (tell *initial-string* 'print-name))
	   (modified-print-name (tell *modified-string* 'print-name))
	   (target-print-name (tell *target-string* 'print-name))
	   (normal-icon-pexp #f)
	   (highlight-icon-pexp #f)
	   (highlighted? #f)
	   (bounding-box-x1 #f)
	   (bounding-box-y1 #f)
	   (bounding-box-x2 #f)
	   (bounding-box-y2 #f)
	   (snag-description-pexp #f)
	   (this-run *this-run*))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'snag-description)
	    (print-name ()
	      (format "~a -> ~a, ~a -> SNAG"
		initial-print-name
		modified-print-name
		target-print-name))
	    (problem-print-name ()
	      (format "~a -> ~a, ~a -> ?"
		initial-print-name
		modified-print-name
		target-print-name))
	    (print ()
	      (printf "Snag Description \"~a\"~n"
		(tell self 'print-name)))
	    (get-initial-letters () initial-letters)
	    (get-modified-letters () modified-letters)
	    (get-target-letters () target-letters)
	    (get-initial-print-name () initial-print-name)
	    (get-modified-print-name () modified-print-name)
	    (get-target-print-name () target-print-name)
	    (get-rule-clauses () rule-clauses)
	    (get-translated-rule-phrases () translated-rule-phrases)
	    (get-explanation () snag-explanation)
	    (get-theme-pattern () theme-pattern)
	    (get-themes () (entries theme-pattern))
	    (get-this-run () this-run)
	    (equal? (i-letters m-letters t-letters rc-list)
	      (and (equal? i-letters initial-letters)
		   (equal? m-letters modified-letters)
		   (equal? t-letters target-letters)
		   (rule-clause-lists-equal? rc-list rule-clauses)))
	    ;; This is used by the memory-window's add-memory-icon method:
	    (get-activation () 0)
	    (get-snag-description-pexp () snag-description-pexp)
	    (get-normal-icon-pexp () normal-icon-pexp)
	    (get-highlight-icon-pexp () highlight-icon-pexp)
	    (set-snag-description-pexp (pexp)
	      (set! snag-description-pexp pexp)
	      'done)
	    (set-graphics-info (proc highlight-icon)
	      ;; Snag icons are always the same color as the memory background:
	      (set! normal-icon-pexp (proc 0))
	      (set! highlight-icon-pexp highlight-icon)
	      'done)
	    (get-bounding-box-x1 () bounding-box-x1)
	    (get-bounding-box-y1 () bounding-box-y1)
	    (get-bounding-box-x2 () bounding-box-x2)
	    (get-bounding-box-y2 () bounding-box-y2)
	    (set-bounding-box (x1 y1 x2 y2)
	      (set! bounding-box-x1 x1)
	      (set! bounding-box-y1 y1)
	      (set! bounding-box-x2 x2)
	      (set! bounding-box-y2 y2)
	      'done)
	    (within-bounding-box? (x y)
	      (and (>= x bounding-box-x1) (<= x bounding-box-x2)
		   (>= y bounding-box-y1) (<= y bounding-box-y2)))
	    (highlighted? () highlighted?)
	    (highlight ()
	      (if* (not highlighted?)
		(set! highlighted? #t)
		(tell *memory-window* 'draw highlight-icon-pexp))
	      'done)
	    (unhighlight ()
	      (if* highlighted?
		(set! highlighted? #f)
		(tell *memory-window* 'draw normal-icon-pexp))
	      'done)
	    (toggle-highlight ()
	      (tell self (if highlighted? 'unhighlight 'highlight)))
	    (display ()
	      (set! *display-mode?* #t)
	      (tell *workspace-window* 'draw snag-description-pexp)
	      (if* (tell *themespace* 'current-state-displayed?)
		(tell *themespace* 'save-current-state))
	      (tell *themespace* 'delete-everything)
	      (tell *themespace* 'thematic-pressure-off)
	      (impose-theme-pattern theme-pattern)
	      ;; Blank slipnet window:
	      (tell *slipnet-window* 'blank-window)
	      ;; Blank coderack window:
	      (tell *coderack-window* 'blank-window "Coderack")
	      ;; Display answer temperature:
	      (tell *temperature-window* 'update-graphics 100))
	    (display-workspace ()
	      (tell *workspace-window* 'draw snag-description-pexp))
	    (else (delegate msg base-object))))))))


(define abstract-answer-description
  (lambda (answer-event)
    (let* ((answer-description-vertical-theme-pattern
	     (abstract-answer-description-theme-pattern
	       (most-recent-group-and-concept-mapping-events)))
	   (unjustified-theme-pattern
	     (get-unjustified-theme-pattern
	       (tell answer-event 'get-unjustified-slippages)))
	   (top-rule (tell answer-event 'get-rule 'top))
	   (bottom-rule (tell answer-event 'get-rule 'bottom))
	   (theme-supporting-bridges
	     (filter-meth (tell *workspace* 'get-bridges 'vertical)
	       'supports-theme-pattern? answer-description-vertical-theme-pattern))
	   (theme-supporting-concept-mappings
	     (get-theme-supporting-concept-mappings
	       answer-description-vertical-theme-pattern
	       theme-supporting-bridges))
	   (new-answer
	     (make-answer-description
	       (tell answer-event 'get-initial-letters)
	       (tell answer-event 'get-modified-letters)
	       (tell answer-event 'get-target-letters)
	       (tell answer-event 'get-answer-letters)
	       (tell top-rule 'get-rule-clauses)
	       (tell bottom-rule 'get-rule-clauses)
	       (tell top-rule 'get-english-transcription)
	       (tell bottom-rule 'get-english-transcription)
	       (tell top-rule 'get-abstractness)
	       (tell bottom-rule 'get-abstractness)
	       (tell answer-event 'get-temperature)
	       ;; This is relative quality:
	       (tell answer-event 'get-quality)
	       answer-description-vertical-theme-pattern
	       (tell top-rule 'get-theme-pattern)
	       (tell bottom-rule 'get-theme-pattern)
	       unjustified-theme-pattern
	       (tell answer-event 'get-unjustified-slippages))))
      (if* %workspace-graphics%
	(tell new-answer 'set-answer-description-pexp
	  (tell answer-event 'make-answer-description-pexp
	    theme-supporting-bridges
	    theme-supporting-concept-mappings)))
      (tell answer-event 'set-answer-description new-answer)
      (tell *memory* 'add-answer-description new-answer))))


(define abstract-snag-description
  (lambda (snag-event)
    (let* ((snag-theme-pattern
	     ;; Retain only the dominant themes:
	     (cons 'vertical-bridge
	       (map 1st (filter-out
			  (lambda (cluster) (> (length cluster) 1))
			  (partition
			    (lambda (entry1 entry2) (eq? (1st entry1) (1st entry2)))
			    (entries (tell snag-event 'get-snag-theme-pattern)))))))
	   (rule (tell snag-event 'get-rule 'top))
	   (translated-rule (tell snag-event 'get-rule 'bottom))
	   (theme-supporting-bridges
	     (let ((snag-bridges (tell snag-event 'get-snag-bridges)))
	       (if (not (null? snag-bridges))
		 snag-bridges
		 (filter-meth (tell *workspace* 'get-bridges 'vertical)
		   'supports-theme-pattern? snag-theme-pattern))))
	   (theme-supporting-concept-mappings
	     (get-theme-supporting-concept-mappings
	       snag-theme-pattern
	       theme-supporting-bridges))
	   (new-snag
	     (make-snag-description
	       (tell rule 'get-rule-clauses)
	       (tell translated-rule 'get-rule-clauses)
	       (tell rule 'get-english-transcription)
	       (tell translated-rule 'get-english-transcription)
	       (tell snag-event 'get-explanation)
	       snag-theme-pattern)))
      (if* %workspace-graphics%
	(tell new-snag 'set-snag-description-pexp
	  (tell snag-event 'make-snag-description-pexp
	    theme-supporting-bridges
	    theme-supporting-concept-mappings)))
      (tell *memory* 'add-snag-description new-snag))))


;; If the calculated distance between two answers is equal to
;; or greater than %distance-threshold%, reminding will not
;; occur.  This value should never be set to zero.  Setting
;; it to one effectively turns off reminding.

(define %distance-threshold% 5)

;; The distance between two identical answers (all strings
;; and both rules exactly equal) is zero, and the distance
;; between two non-identical answers is always at least one.

(define calculate-answer-distance
  (lambda (answer1 answer2)
    (if (tell answer1 'answers-equal? answer2)
      0
      (let* ((all-themes1
	       (append
		 (tell answer1 'get-themes)
		 (tell answer1 'get-unjustified-themes)))
	     (all-themes2
	       (append
		 (tell answer2 'get-themes)
		 (tell answer2 'get-unjustified-themes)))
	     (unjustified-themes1
	       (remove-elements
		 (get-snag-justified-themes answer1)
		 (tell answer1 'get-unjustified-themes)))
	     (unjustified-themes2
	       (remove-elements
		 (get-snag-justified-themes answer2)
		 (tell answer2 'get-unjustified-themes)))
	     (common-themes
	       (intersect-themes
		 all-themes1 all-themes2))
	     (common-dimensions
	       (map 1st common-themes))
	     (differing-dimensions
	       (remq-elements
		 common-dimensions
		 (intersect
		   (map 1st all-themes1)
		   (map 1st all-themes2))))
	     (answer1-only-themes
	       (remove-elements common-themes all-themes1))
	     (answer2-only-themes
	       (remove-elements common-themes all-themes2))
	     (differing-themes1
	       (filter
		 (lambda (theme) (member? (1st theme) differing-dimensions))
		 all-themes1))
	     (differing-themes2
	       (filter
		 (lambda (theme) (member? (1st theme) differing-dimensions))
		 all-themes2))
	     (unique-themes1
	       (remove-elements differing-themes1 answer1-only-themes))
	     (unique-themes2
	       (remove-elements differing-themes2 answer2-only-themes))
	     (common-unjustified-themes
	       (intersect-themes
		 unjustified-themes1 unjustified-themes2))
	     (answer1-only-unjustified-themes
	       (remove-elements common-unjustified-themes unjustified-themes1))
	     (answer2-only-unjustified-themes
	       (remove-elements common-unjustified-themes unjustified-themes2))
	     (common-answer1-only-unjustified-themes
	       (remove-elements differing-themes1 answer1-only-unjustified-themes))
	     (common-answer2-only-unjustified-themes
	       (remove-elements differing-themes2 answer2-only-unjustified-themes))
	     (theme-distance
	     (+ (length differing-dimensions)
	        (* 2 (length unique-themes1))
		(* 2 (length unique-themes2))))
	     (rule-differences
	       (compare-rule-clause-lists
		 (tell answer1 'get-top-rule-clauses)
		 (tell answer2 'get-top-rule-clauses)))
	     (num-rule-differences
	       (if (exists? rule-differences)
		 (length (filter-out
			   (lambda (nodes) (= (cd (1st nodes)) (cd (2nd nodes))))
			   rule-differences))
		 -1))
	     (rule1-abstractness (tell answer1 'get-top-rule-abstractness))
	     (rule2-abstractness (tell answer2 'get-top-rule-abstractness))
	     (rule-distance
	       (if (= num-rule-differences -1)
		 (round (/ (abs (- rule1-abstractness rule2-abstractness)) 10))
		 (* 2 num-rule-differences)))
	     (justification-distance
	       (+ (length common-answer1-only-unjustified-themes)
		  (length common-answer2-only-unjustified-themes)))
	     (incoherence-distance
	       (if (eq? (answer-incoherent? answer1) (answer-incoherent? answer2))
		 0
		 1))
	     (total-distance
	       (+ 1 theme-distance rule-distance
		  justification-distance incoherence-distance)))

      total-distance))))


(define *memory* (make-memory))
