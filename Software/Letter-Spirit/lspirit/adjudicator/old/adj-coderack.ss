;=============================================================================
; coderack.ss : parallel terraced scan for labeling parts
;=============================================================================

; A simple codelet caller without any temperature
; pick a codelet with the roulette wheel, run it, remove it and pause
(define adj-run
  (lambda ()
    (codemsg "[~s] " *codelets-run*)
    (set! *codelets-run* (add1 *codelets-run*))
    (cond
     [(and (null? *coderack*) (eq? *phase* 1))
      (begin
	(printf "Going to phase 2~%")
	(post-bridge-outs 200)
	(post-bridge-backs 200)
	(set! *phase* 2)
	(adj-run))]
     [(and (null? *coderack*) (eq? *phase* 2))
      (begin
	(printf "Going to phase 3~%")
	(post-promoters 150)
	(set! *phase* 3)
	(adj-run))]
     [(and (eq? *phase* 3) (null? *coderack*) (null? *bridges*))
      (begin
	(printf "Phase 3, no codelets, no bridges~%")
	(codemsg "The Adjudicator is done "))]
     ; Run a codelet
     [else (rackprint *workspace*)
	   (if *graphics* (begin (draw-workspace *workspace*)
				 (if *display-coderack*
				     (draw-coderack *coderack*))
				 (draw-codelet-count *codelets-run*))) 
	   (rackprint *coderack*)
	   (run-codelet)
	   (adj-run)])))
