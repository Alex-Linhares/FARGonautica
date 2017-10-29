;=============================================================================
; recog.ss : grid letter recognizer  |  main program and help functions
;=============================================================================

; switch for graphics vs ACSII display
(define *graphics* #f)    ;;;; <----- for recog.ss this should ALWAYS be #t
                          ;;;; use stat-recog.ss for a non-graphical version

(set! *all-codelets* 0)
(set! *exam-verbose* #f)

(set! *display-roles* #f)

; load the smart loader code (check for object files and load them first)
; (load "loader.so")

(set! *codelet-pause* #f)       ; pause after every codelet

; this will always be false for the interactive program
; this flag is used in (collect-stats ...) to write data to a file or not
(set! *write-stats* #f)

; track how many codelets run here
(set! *codelets-run* 0)

(if *graphics*
    ; sxm windows for each of these which can be turned off
    ; they MUST be on initially in order for the windows to set
    ; themselves up
    (begin
      (set! workTrove '())
      (set! tempTrove '())
      (set! roleTrove '())
      (set! rackTrove '())
;      (set! *display-roles* #t)
      (set! *display-temp* #t)
      (set! *display-coderack* #t)
      (set! *draw-codelets* #t)
      ; draw activated roles in the workspace
      (set! *draw-roles* #t))     ; <-- overridden by display-roles
    (begin
      ; these are always false since they require *graphics*
      (set! workTrove '())
      (set! tempTrove '())
      (set! roleTrove '())
      (set! rackTrove '())
      (set! *display-roles* #f)
      (set! *display-temp* #f)
      (set! *display-coderack* #f)
      (set! *draw-roles* #f)))

(load "glom.ss")
(load "bond.ss")
(set! *rolesNwholes* (append *roles* *wholes*))


(randomize)  ; get a REALLY random seed (Jim's randomizer is in tools.ss)

(define statmsg
  (ifprint-proto *write-stats*))

(define recognize
  (lambda optional-args
    (set! *codelets-run* 0)
    (if (not (null? optional-args))
	(begin
	  (set! *seed* (car optional-args))
	  (random-seed (car optional-args)))
	(set! *seed* (random-seed)))
    (clear-acts)
    (get-data)
    (glom)
    (set! *spark-threshold* 50)
    (set! *punish* -25)  (set! *punish-hard* -40)
    (g-start)
    (label-parts)
    (get-highest-whole *wholes* '($ 0))))

(define run-examiner
  (lambda optional-args
    (if (null? *quanta-list*)
	(begin
	  (set! *workspace* '())
	  '(none 0))
	(begin
	  (set! *codelets-run* 0)
	  (if (not (null? optional-args))
	      (begin
		(set! *seed* (car optional-args))
		(random-seed (car optional-args)))
	      (set! *seed* (random-seed)))
	  (clear-acts)
	  (do-glom)
	  (set! *spark-threshold* 50)
	  (set! *punish* -25)  (set! *punish-hard* -40)
	  (g-start)
	  (label-parts)
	  (set! *all-codelets* (+ *all-codelets* *codelets-run*))
	  (get-highest-whole *wholes* '($ 0))))))

; function created merely to intercept error that occurs when
; no two quanta touch each other

(define do-glom
  (lambda ()
    (if (not (null?
	      (apply append (map pick-neighbor-to-glom *quanta-list*))))
	(glom)
	(smart-parse))))
    
(define clean
  (lambda ()
    (set! *temperature* 100)
    (set! *coderack* '())
    (set! *solved* #f)
    (if *graphics*
	(begin
	  (if *display-roles* (draw-role-activations))
	  (if *display-temp* (draw-thermometer))
	  (if *display-coderack* (draw-coderack *coderack*))
	  (set! *ant-obj* '())
	  (draw-grid-back workGP)
	  (draw-grid workGP)
	  (erase-info workGP)
	  (draw-info-bar "information bar")))
    (collect 4)))

;===========================================================================
; EXAMINER CLEANUP -- making everything good for the Adjudicator
;===========================================================================

(define better-flipped?
  (lambda (part role)
    (let*
	([type (lookup 'topology role)]
	 [all-features (get-features part)]
	 [topos (not-atoms all-features)]
	 [flip-topos (topo-tip-flip topos)]
	 [norms (get-norms role)]
	 [normal-score (topology-tally type topos norms)]
	 [flip-score (topology-tally type flip-topos norms)]
	 [choice (if (> normal-score flip-score) 'normal 'flipped)])
      (> flip-score normal-score))))

; if the quanta are in the reverse order of where tip1 and tip2
; should be, reverse them

(define ordered-quanta
  (lambda (part rolename)
    (let
	([pre-order-quanta (get-part-quanta part)])
      (if (better-flipped? part (eval rolename))
	  (reverse pre-order-quanta)
	  pre-order-quanta))))

; eventually, you'll want to compute a goodness rating

(define examiner-cleanup
  (lambda ()
    (let*
	([winner (car (get-highest-whole *wholes* '($ 0)))]
	 [roles (covers winner)])
      (if (null? roles)
	  (begin
	    (set! *answer* 'quit)
	    (set! *fillers* '()))
	  (let
	      ([ordered (map ordered-quanta *workspace* roles)])
	    (set! *answer* winner)
	    (set! *fillers* (map list roles ordered)))))))

(define parts-score
  (lambda ()
    (if (eq? *codelets-run* *max-codelets*)
	0
	(let*
	    ([role-names (covers *answer*)]
	     [part-happiness
	      (lambda (role part)
		(lookup role (spark-all part)))])
	  (average (map part-happiness role-names *workspace*))))))

(define examiner-score
  (lambda ()
    (let*
	([winner (car (get-highest-whole *wholes* '($ 0)))]
	 [roles (covers winner)]
	 [ordered (map ordered-quanta *workspace* roles)])
      (set! *answer* winner)
      (set! *fillers* (map list roles ordered)))))
