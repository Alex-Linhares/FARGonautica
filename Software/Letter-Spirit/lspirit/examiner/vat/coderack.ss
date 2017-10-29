;=============================================================================
; coderack.ss : parallel terraced scan for labeling parts
;=============================================================================

; coderack printer
(define rackprint
  (ifpprint-proto *noisy-coderack*))

; A simple codelet caller without any temperature
; pick a codelet with the roulette wheel, run it, remove it and pause

(define run-codelets
  (lambda ()
    (codemsg "[~s] " *codelets-run*)
    (set! *codelets-run* (add1 *codelets-run*))
    (if (and *graphics*
	     (eq? 0 (mod *codelets-run* grafStep)))(graf-flush))
    (cond
      [(< 99 (cadr (get-highest-whole *wholes* '($ 0))))
       (codemsg "***** Forcing a guess~%")
       (let*
	   ([contestants (over-threshold 99 (list-active-nodes *wholes*))]
	    [tie-breaker (map sum-role-acts (map car contestants))]
	    [ans (find-max tie-breaker)])
	 (begin
	   (handle-winner (list ans (get-activation ans)))
	   (statmsg "run terminated at ~s seed ~a~%" *max-codelets*
		    *seed*)
	   (list '$ 0 *max-codelets* 100 *seed*)))]
      [(>= *codelets-run* *max-codelets*)
       (codemsg "***** Forcing a guess~%")
       (let ([ltr (guess-winner)])
         (if ltr
             (let* ([act 0]
                    [ans (list ltr act)])
               (handle-winner ans))
             (begin
               (statmsg "run terminated at ~s seed ~a~%" *max-codelets*
                        *seed*)
               (list '$ 0 *max-codelets* 100 *seed*))))]
      [(= (mod (+ *codelets-run* 2) 500) 0)
       (set! *spark-threshold* (* 0.9 *spark-threshold*))
       (set! *punish* (* 0.9 *punish*))
       (set! *punish-hard* (* 0.9 *punish-hard*))
       (run-codelets)]
      [*solved* (let ([ans (get-highest-whole *wholes* '($ 0))])
		  (if (not (equal? ans '($ 0)))
		      (handle-winner ans)
		      (begin
			(codemsg "  solver must >>>Fizzle<<<~%")
			(set! *solved* #f)
			(run-codelets))))]
      ; Uh oh, out of codelets!
      ; two alternatives.  1) Add more lookers if **whiny stuff.
      ;                    2) Break everything up and "start over"
      [(null? *coderack*)
         (codemsg "Coderack empty...~%")
	 (if (or (> *temperature* 90)
		 (member*? '**whine *workspace*))
	     ; a big shakeup!
	     (begin
	       (codemsg "Temperature still high (adding more lookers)~%")
	       (letrec ((wl (* 2 (length *workspace*)))
			(lp (lambda (ctr)
			      (cond
				((= ctr 0) (run-codelets))
				(else (add-to-coderack
					looker-codelet 'looker
					*medium-urgency* 1)
				      (lp (sub1 ctr)))))))
		 (lp wl)))
	     (begin
	       (codemsg "  PANIC.  Breaking up all existing parts.~%")
	       (codemsg "  reglomming quanta.~%")
	       (if *graphics*
		   (begin
		     (set! *graphics* #f)
		     (set! *gfl* #t))
		   (set! *gfl* #f))
	       (glom)
	       (if *gfl* (set! *graphics* #t))
	       (label-parts)))]
      ; Try to solve things when the temperature is low
      [(< *temperature* 30)
       (codemsg "  Very low temperature.  Looking for winner.~%")
       (let ([ans (get-highest-whole *wholes* '($ 0))])
	 (if (not (equal? (car ans) '$))
	     (begin
	       (codemsg "  Checking r-roles~%")
	       (r-role-check-wholes *wholes*)
	       (compute-temperature)
	       (if (and (< *temperature* 20)
			(not (equal? (car ans) '$)))
		   (begin
		     (codemsg "  Still very low.  Picking winner.~%")
		     (let ([ans (get-highest-whole *wholes* '($ 0))])
		       (handle-winner ans)))
		   (run-codelets)))
	     (begin
	       (compute-temperature)
	       (if (and (< *temperature* 20)
			(not (equal? (car ans) '$)))
		   (begin
		     (codemsg "  Still very low.  Picking winner.~%")
		     (let ([ans (get-highest-whole *wholes* '($ 0))])
		       (handle-winner ans)))
		   (run-codelets)))))]
      ; Run a codelet
      [else (rackprint *workspace*)
	    (if (and *graphics* (> *codelets-run* 10))
		(begin (draw-workspace *workspace*)
		       (if *display-coderack*
			   (draw-coderack *coderack*))
		       (draw-codelet-count *codelets-run*))) 
	    (rackprint *coderack*)
	    (run-codelet)
	    ;graphics flush added by JAR, 2/16/97
	    (compute-temperature)
	    (if (and *graphics*
		     (eq? 0 (mod *codelets-run* grafStep)))(graf-flush))
	    (if *codelet-pause* (pause))
	    (run-codelets)])))

; Once a winner has been found.  Do various bookkeeping tasks.
(define handle-winner
  (lambda (ans)
    (if (string=? (substring (symbol->string (car ans)) 0 1)
		  "?")
	(begin
	  (codemsg "  Guess was made (using syntax).  Answer ~s~%" (car ans))
	  (statmsg "~a [ ~a ] in ~a tmp ~a  seed ~a~%"
		   (car ans) (cadr ans)
		   *codelets-run* (round *temperature*) *seed*)
	  (list (car ans) (cadr ans)
		*codelets-run* (round *temperature*) *seed*))
	(begin
	  (codemsg "  Winner is ~a [~a]~%" (car ans) (cadr ans))
	  (statmsg "~a [ ~a ] in ~a tmp ~a  seed ~a~%"
		   (car ans) (cadr ans)
		   *codelets-run* (round *temperature*) *seed*)
	  (list (car ans) (cadr ans)
		*codelets-run* (round *temperature*) *seed*)
	  (if *graphics* (graf-flush))))))
  
; shorthand call
(define rc
  (lambda ()
    (run-codelets)))

; use a weighted roulette wheel to pick the codelet to reference
; uses urgency directly.  Note that temperature plays no role in
; codelet picking!  This might be fun to play with.
; returns a pair (<procedure> . <generation>)
; *** may need temperature later ***
(define roulette-pick
  (lambda (codelets urgencies generations)
    (let* ((sum (apply + urgencies))
	   (roll (n-sided-die sum)))
      (letrec ((get-ref (lambda (u-ls counter ref)
			  (cond 
			    ((<= roll counter) ref)
			    (else (get-ref (cdr u-ls) (+ (car u-ls)
							 counter)
				    (add1 ref)))))))
	(let ((ref (get-ref urgencies 0 -1)))
	  (cons (list-ref codelets ref)
		(list-ref generations ref)))))))
			    
; pick a codelet off the coderack (probabalistically) and run it.
(define run-codelet
  (lambda ()
    (let ((codelets (map car *coderack*))
	  (urgencies (map caddr *coderack*))
	  (generations (map cadddr *coderack*)))
      (let* ((codelet-pr (roulette-pick codelets urgencies generations))
	     (codelet (car codelet-pr))
	     (generation (cdr codelet-pr)))
	(remove-from-coderack codelet)
	(codelet generation)))))

; Remove a codelet from the coderack
(define remove-from-coderack
  (lambda (codelet)
    (set! *coderack* (remq (select codelet *coderack*) *coderack*))))

; Add a codelet to the coderack
(define add-to-coderack
  (lambda (codelet name urgency generation)
    (let* ((divisor (expt *generation-gap-constant* generation))
	   (adj-urgency (round (/ urgency divisor))))
    (if (not (zero? urgency))
	(set! *coderack* (cons (list codelet name adj-urgency generation)
			       *coderack*))))))

; Add N copies of a codelet to the coderack all at once
(define add-n-to-coderack
  (lambda (n codelet name urgency generation)
    (cond      
      ((= n 1) (add-to-coderack codelet name urgency generation))
      (else (add-to-coderack codelet name urgency generation)
	    (add-n-to-coderack (sub1 n) codelet name urgency generation)))))

;-------------------------[ temperature ]--------------------------------
; This code is general enough to handle a more momentum-based temperature
; right now with the *ws-effect* variable set to 1 it goes straight to
; the target temperature

(set! *temperature* 100)
(set! *ws-effect* 1.0)	    ; at 1 effect is large at 0 it is nill
(set! *label-total* 16)     ; number of labels attached to "finished" part
(set! *label-value* (/ 100 *label-total*)) ; "value" of label for workspace

; numbers over which activation "counts" for temperature adjustment
; see memory-goodness below
(set! *role-threshold* 51)
(set! *whole-threshold* 51)

; compute the current temperature. this actually amounts to checking the
; goodness of parts in the workspace (which takes role activation into
; account)
(define compute-temperature
  (lambda ()
    (let* ([ws (workspace-goodness *workspace*)]        ; good [100+ 0] bad
	   [mem (memory-goodness)]                      ; good [100 0] bad
	   [ct *temperature*]			        ; good [0..100] bad
	   [target-t  (abs (- 100 (+ (/ ws 2)
				     (/ mem 2))))]      ; g [0-..100] b
	   [diff (abs (- ct target-t))]
	   [sdiff (* *ws-effect* diff)]
	   [nt (if (> target-t ct)
		   (+ ct sdiff)
		   (- ct sdiff))])
      (set! *temperature* nt)
      (if (and *graphics* *display-temp*)
	  (draw-temp nt)))))

; Goodness computations:
;--------------------------[ workspace ]-------------------------------------
; Assume that there will be around *label-total* labels resulting in a score
; between 0 and 80-100 for the workspace.  Also check for whiners.  If there
; are whiners in the workspace, lower the overall score.
(define workspace-goodness
  (lambda (w)
    (let ([l (length w)])
      (letrec ([check-each-part (lambda (w)
				  (cond
				    ((null? w) '())
				    (else (cons (part-goodness (caar w))
						(check-each-part (cdr w))))))])
	(let* ([list-of-ratings (check-each-part w)]
	       [any_whining? (member*? '**whine w)]
	       ; average the parts' values together
	       [raw-score (/ (apply + list-of-ratings) l)]
	       [chopper (if (> raw-score 100.0)
			    100.0
			    raw-score)])
	  (if any_whining?
	      (if (> chopper 20) (- chopper 20) chopper)
	      chopper))))))

; Goodness of a part is computed by the number of labels a part has
; averaged with the highest activation of the role(s) that it is bound
; to (if any)
(define part-goodness
  (lambda (jls)
    (if (part-exists? (list jls))
	(let* ([part (get-updated-part jls)]
	       [label-count (if (member*? '**whine part)
				0
				(length (get-features part)))]
	       [top-act (part-top-activation part)])
	  (min 100
	       (/ (+ (* *label-value* label-count)
		     top-act) 2)))
	#f)))


;-----------------------[ conceptual memory ]---------------------------------
; *** very rough probably needs more thought
; if wholes are active, there should only be one (ignore roles at that point)
; Note that roles' "happiness" is taken care of in the workspace goodness
; calculation.
(define memory-goodness
  (lambda ()
    (let ([wholes (count-active-wholes *wholes* *whole-threshold*)])
      (if (zero? wholes)
	  0               ; no longer consider roles (* roles 10.0)
	  (/ 100.0 wholes)))))
    
; Chalk up wholes with activations over some number (*whole-threshold*)
(define count-active-wholes
  (lambda (rls num)
    (cond
     [(null? rls) 0]
     [(< (get-activation (car rls)) num)
      (count-active-wholes (cdr rls) num)]
     [else (add1 (count-active-wholes (cdr rls) num))])))

;----------------------------------------------------------------------------
; Other random functions

; Used in picking a winner.
(define get-highest-whole
  (lambda (wls a-ls)
    (cond
     [(null? wls) a-ls]
     [else
        (let* ([act (round-3 (get-activation (car wls)))])
	  (if (> act (cadr a-ls))
	      (get-highest-whole (cdr wls) (list (car wls) act))
	      (get-highest-whole (cdr wls) a-ls)))])))

(define list-active-nodes
  (lambda (wls)
    (cond
     [(null? wls) '()]
     [else
        (let* ([act (round-3 (get-activation (car wls)))])
	  (if (> act 0)
	      (cons (list (car wls) act) (list-active-nodes (cdr wls)))
	      (list-active-nodes (cdr wls))))])))

(define sum-role-acts
  (lambda (whole)
    (list whole (apply + (map get-activation (whole-roles whole))))))

