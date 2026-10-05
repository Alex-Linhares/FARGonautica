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

(define %update-cycle-length% 15)
(define %initial-slipnode-clamp-cycles% 50)
(define %garbage-collect-cycles% 100)

(define *this-run* #f)
(define *running?* #f)
(define *interrupt?* #f)
(define *breakpoint-continuation* #f)
(define *break-time* #f)
(define *step-mode?* #f)
(define %step-cycles% 1)
(define *display-mode?* #f)

(define ss
  (lambda args
    (if* (not (null? args))
      (let ((n (1st args)))
	(cond
	  ((> n 0) (set! %step-cycles% n) (step-mode-on))
	  (else (step-mode-off)))))
    (printf "step mode ~a~a~%"
      (if *step-mode?* 'on 'off)
      (if *step-mode?* (format ", step size ~a" %step-cycles%) ""))))

(define step-mode-on
  (lambda ()
    (set! *step-mode?* #t)
    (tell *control-panel* 'set-verbose-step-mode (not (> %step-cycles% 10)))))

(define step-mode-off
  (lambda ()
    (set! *step-mode?* #f)
    (tell *control-panel* 'set-verbose-step-mode #f)))

(define runtil
  (lambda args
    (if (null? args)
      (if *break-time*
	`(breaktime set at ,*break-time*)
	'(no breaktime set))
      (let ((breaktime (1st args)))
	(if (= breaktime 0)
	  (begin
	    (set! *break-time* #f)
	    (tell *control-panel* 'clear-breakpoint-message)
	    '(breaktime cleared))
	  (begin
	    (set! *break-time* breaktime)
	    (tell *control-panel* 'display-breakpoint-message)
	    `(breaktime set at ,*break-time*)))))))

(define clear-breakpoint
  (lambda ()
    (set! *breakpoint-continuation* #f)
    'done))

;;---------------------------------------------------------------------------
;; prompt and no-prompt provide an ugly (and not entirely correct) workaround
;; for an annoying problem in SWL 0.9u in which >'s gradually fill up the
;; bottom line of the REPL window with each new call to (break).  However,
;; this workaround seems to cause other bad things to happen in 0.9x, so we
;; only use it with 0.9u.

(define prompt (waiter-prompt-and-read))

(define no-prompt
  (lambda (n)
    (unless (and (integer? n) (>= n 0))
      (error 'no-prompt "~s is not a nonnegative exact integer" n))
    (let ([x (read (console-input-port))])
      (when (eof-object? x)
        (newline (console-output-port))
        (flush-output-port (console-output-port)))
      (waiter-prompt-and-read prompt)
      x)))

;;---------------------------------------------------------------------------

(define break
  (lambda ()
    (continuation-point* breakpoint
      (set! *breakpoint-continuation* breakpoint)
      (swl:sync-display)
      (printf "stopped~%")
      ;; see above
      (if* (equal? swl:version "0.9u")
	(printf "> ") ;; fake a prompt
	(waiter-prompt-and-read no-prompt))
      (set! *running?* #f)
      (tell *control-panel* 'switch-to-input-mode)
      (reset))))

(define quiet-break
  (lambda ()
    (continuation-point* breakpoint
      (set! *breakpoint-continuation* breakpoint)
      (swl:sync-display)
      ;; see above
      (if* (equal? swl:version "0.9u")
	(waiter-prompt-and-read no-prompt))
      (set! *running?* #f)
      (tell *control-panel* 'switch-to-input-mode)
      (reset))))

(define go
  (lambda ()
    (if (not (exists? *breakpoint-continuation*))
      (printf "No previous break.~%")
      (begin
	(tell *control-panel* 'switch-to-run-mode)
	(set! *interrupt?* #f)
	(set! *running?* #t)
	(if* *display-mode?* (restore-current-state))
	(*breakpoint-continuation* 'ignore)))))

(define suspend
  (lambda ()
    (printf "Type (go) or click on the Workspace to continue...~%")
    (break)))

(define rerun
  (lambda ()
    (if (not (exists? *this-run*))
      (printf "No current problem.~%")
      (tell *control-panel* 'run-new-problem *this-run*))))

(define run-mcat
  (lambda ()
    (continuation-point* terminate
      (repeat* forever
	(step-mcat)
	(if* (= *codelet-count* *initial-slipnode-unclamp-time*)
	  (say "Unclamping initially-clamped slipnodes...")
	  (for* each node in *initially-clamped-slipnodes* do
	    (tell node 'unfreeze)))
	(if* (tell *coderack* 'empty?)
	  (post-initial-codelets)
	  (clamp-initial-slipnodes))
	(if* (= 0 (modulo *codelet-count* %update-cycle-length%))
	  (update-everything))
	(if* (or *interrupt?*
	         (and *step-mode?* (= 0 (modulo *codelet-count* %step-cycles%)))
	         (and *break-time* (= *break-time* *codelet-count*)))
	  (update-all-graphics)
	  (printf "Codelets run: ~a~n" *codelet-count*)
	  (break))
	(if* (= 0 (modulo *codelet-count* %garbage-collect-cycles%))
	  (tell *themespace-window* 'garbage-collect)
	  (tell *workspace-window* 'garbage-collect))))))

(define step-mcat
  (lambda ()
    (let ((codelet (tell *coderack* 'choose-codelet)))
      (tell codelet 'run)
      (set! *codelet-count* (+ 1 *codelet-count*))
      'done)))

(define init-mcat
  (lambda (initial-sym modified-sym target-sym answer-sym seed)
    (set! *breakpoint-continuation* #f)
    (set! *interrupt?* #f)
    (set! *running?* #t)
    (set! *display-mode?* #f)
    (step-mode-off)
    (random-seed seed)
    (set! *this-run*
      (if %justify-mode%
	(list initial-sym modified-sym target-sym answer-sym seed)
	(list initial-sym modified-sym target-sym seed)))
    (tell *coderack* 'initialize)
    (set! *codelet-count* 0)
    (set! *initial-slipnode-unclamp-time* 0)
    (set! *temperature* 100)
    (set! *temperature-clamped?* #f)
    (for* each node in *slipnet-nodes* do
      (tell node 'reset))
    (set! *fg-color* %default-fg-color%)
    (tell *temperature-window* 'initialize)
    (tell *temperature-window* 'update-graphics 100)
    (tell *EEG-window* 'initialize)
    (tell *slipnet-window* 'clear)
    (tell *coderack-window* 'clear)
    (tell *comment-window* 'clear)
    (tell *trace* 'initialize)
    (tell *memory* 'clear-activations)
    (tell *memory* 'unhighlight-all-answers)
    (tell *themespace* 'initialize)
    (init-workspace initial-sym modified-sym target-sym answer-sym)
    (add-string-position-descriptions-to-letters *initial-string*)
    (add-string-position-descriptions-to-letters *modified-string*)
    (add-string-position-descriptions-to-letters *target-string*)
    (if* %justify-mode%
      (add-string-position-descriptions-to-letters *answer-string*))
    (if* (or (= (tell *initial-string* 'get-length) 1)
	     (= (tell *modified-string* 'get-length) 1)
	     (= (tell *target-string* 'get-length) 1)
	     (and %justify-mode% (= (tell *answer-string* 'get-length) 1)))
      ;; changed 'update-activation to 'set-activation to avoid the
      ;; possibility of creating trace events before a run actually begins
      (tell plato-object-category 'set-activation %max-activation%))
    (for* each obj in (tell *workspace* 'get-objects) do
      (for* each descriptor in (tell-all (tell obj 'get-descriptions) 'get-descriptor)
	;; changed 'update-activation to 'set-activation to avoid the
	;; possibility of creating trace events before a run actually begins
	do (tell descriptor 'set-activation %max-activation%)))
    (update-workspace-values)
    (clamp-initial-slipnodes)
    (if* %slipnet-graphics% (tell *slipnet-window* 'update-graphics))
    (post-initial-codelets)
    (if* %coderack-graphics% (tell *coderack-window* 'update-graphics))
    (collect 4)))

(define init-workspace
  (lambda (initial-sym modified-sym target-sym answer-sym)
    (let ((initial-string (make-workspace-string 'initial initial-sym))
	  (modified-string (make-workspace-string 'modified modified-sym))
	  (target-string (make-workspace-string 'target target-sym))
	  (answer-string (if %justify-mode%
			   (make-workspace-string 'answer answer-sym)
			   #f)))
      (tell *workspace* 'initialize
	initial-string modified-string target-string answer-string)
      (set! *initial-string* initial-string)
      (set! *modified-string* modified-string)
      (set! *target-string* target-string)
      (set! *answer-string* answer-string)
      ;; Order of strings is important:
      (set! *top-strings* (list *initial-string* *modified-string*))
      (set! *bottom-strings* (list *target-string* *answer-string*))
      (set! *vertical-strings* (list *initial-string* *target-string*))
      (set! *non-answer-strings*
	(list *initial-string* *modified-string* *target-string*))
      (set! *all-strings*
	(list *initial-string* *modified-string* *target-string* *answer-string*))
      (if* %workspace-graphics%
	(tell *workspace-window* 'draw-problem
	  initial-string modified-string target-string answer-string))
      (tell *comment-window* 'new-problem
	initial-sym modified-sym target-sym answer-sym))))

(define clamp-initial-slipnodes
  (lambda ()
    (for* each node in *initially-clamped-slipnodes* do
      (tell node 'clamp %max-activation%))
    (set! *initial-slipnode-unclamp-time*
      (+ *codelet-count*
	 (* %initial-slipnode-clamp-cycles% %update-cycle-length%)))))

(define post-initial-codelets
  (lambda ()
    (repeat* (* 2 (length (tell *workspace* 'get-objects))) times
      (tell *coderack* 'add-deferred-codelet
	(tell bottom-up-bond-scout 'make-codelet %very-low-urgency%))
      (tell *coderack* 'add-deferred-codelet
	(tell bottom-up-bridge-scout 'make-codelet %very-low-urgency%)))
    (tell *coderack* 'post-deferred-codelets)))

(define add-string-position-descriptions-to-letters
  (lambda (string)
    (let ((string-length (tell string 'get-length))
	  (leftmost-letter (tell string 'get-letter 0)))
      (if (= string-length 1)
	(tell leftmost-letter 'new-description
	  plato-string-position-category plato-single)
	(let ((rightmost-letter (tell string 'get-letter (sub1 string-length))))
	  (tell leftmost-letter 'new-description
	    plato-string-position-category plato-leftmost)
	  (tell rightmost-letter 'new-description
	    plato-string-position-category plato-rightmost)
	  (if* (odd? string-length)
	    (let ((middle-letter
		    (tell string 'get-letter (truncate (/ string-length 2)))))
	      (tell middle-letter 'new-description
		plato-string-position-category plato-middle))))))))

(define update-everything
  (lambda ()
    (tell *workspace* 'check-if-rules-possible)
    (update-workspace-values)
    (if* (tell *trace* 'within-snag-period?)
      (let ((progress-achieved (tell *trace* 'progress-since-last-snag)))
	(stochastic-if* (% progress-achieved)
	  (tell *trace* 'undo-snag-condition))))
    (if* (tell *trace* 'clamp-period-expired?)
      (tell *trace* 'undo-last-clamp))
    (tell *workspace* 'spread-activation-to-themespace)
    (tell *themespace* 'spread-activation)
    (update-slipnet-activations)
    (update-temperature)
    (add-bottom-up-codelets)
    (add-top-down-codelets)
    (tell *coderack* 'post-deferred-codelets)
    (if* %workspace-graphics%
      (tell *EEG* 'record-current-values)
      (tell *EEG-window* 'plot-current-values))
    (update-all-graphics)))

(define update-workspace-values
  (lambda ()
    (for* each structure in (tell *workspace* 'get-structures) do
      (tell structure 'update-strength))
    (let ((objects (tell *workspace* 'get-objects)))
      (for* each object in objects do
	(tell object 'update-raw-importance))
      (tell *initial-string* 'update-all-relative-importances)
      (tell *modified-string* 'update-all-relative-importances)
      (tell *target-string* 'update-all-relative-importances)
      (if* %justify-mode%
	(tell *answer-string* 'update-all-relative-importances))
      (for* each object in objects do
	(tell object 'update-object-values)))
    (tell *initial-string* 'update-average-intra-string-unhappiness)
    (tell *modified-string* 'update-average-intra-string-unhappiness)
    (tell *target-string* 'update-average-intra-string-unhappiness)
    (if* %justify-mode%
      (tell *answer-string* 'update-average-intra-string-unhappiness))
    (tell *workspace* 'update-average-unhappiness-values)))

(define update-all-graphics
  (lambda ()
    (if* %workspace-graphics%
      (tell *workspace-window* 'update-graphics)
      (tell *temperature-window* 'update-graphics *temperature*))
    (if* %slipnet-graphics%
      (tell *slipnet-window* 'update-graphics))
    (if* %coderack-graphics%
      (tell *coderack-window* 'update-graphics))))
