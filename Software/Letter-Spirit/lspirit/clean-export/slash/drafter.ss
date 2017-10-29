;=============================================================================
; drafter.ss : Drafter codelets
;=============================================================================

; [motif-rewarder]---------------------------------------------------------

(define motif-rewarder-codelet
  (lambda (sp-type motif levelname)
    (lambda (gen)
      (if *graphics* (draw-codelet "Motif-rewarder" gen))
      (codemsg "Motif-rewarder from generation ~s~%" gen)
      (let* ([step-to-motif
	      (lambda (step)
		(quanta-to-motif sp-type (append *own-stuff* (list step))))]
	     [already (quanta-to-motif sp-type *own-stuff*)]
	     [the-sublists
	      (find-motif-sublists sp-type motif)]
	     [no-overlap-score
	      (length
	       (sublists-include
		the-sublists
		already))]
	     [step-match
	      (lambda (step)
		(sublists-include
		 the-sublists
		 (step-to-motif step)))]
	     [motif-score
	      (lambda (candidate)
		(let*
		    ([step (cadr candidate)]
		     [match (step-match step)]
		     [match-size (length match)])
		  (if (and
		       (> match-size no-overlap-score))
		      (let
			  ([match-score
			    (* 100 (draft-level-score sp-type levelname match)
			       (* match-size (motif-bonus sp-type)
				   (- match-size no-overlap-score)))])
;			(if (eq? step 5)
;			    (printf "~s ~s ~s ~%" sp-type levelname match))
			match-score)
		      0)))]
	     [subtotal
	      (lambda (candidate)
		(add-choice candidate (motif-score candidate) 7))]
	     [process-row
	      (lambda (row)
		(list (car row)
		      (round-3 (+ (motif-score (car row)) (cadr row)))))])
;	     [new-row (map process-row *workspace*)])
	(codemsg "  ~%")
	(if *graphics*
	    (draw-codelet-message
	      (format " ~%")))
	(begin
	  (map subtotal *candidates*)
;	  (set! *workspace* new-row)
	  (if #f
	      (begin
		(add-to-coderack
		 (motif-rewarder-codelet operands)
		 'motif-rewarder-codelet
		 *medium-urgency*
		 (add1 gen))
		(codemsg " motif-rewarder-codelet spun.~%"))))))))

; [breadcrumbs]---------------------------------------------------------

; lay out the path to follow, based on curve and tips

(define breadcrumb-codelet
  (lambda (gen)
    (if *graphics* (draw-codelet "breadcrumb" gen))
    (codemsg "breadcrumb from generation ~s~%" gen)
    (let* ([subtotal
	    (lambda (candidate)
	      (add-choice candidate (candidate-breadcrumbs candidate) 1))]
	   [process-row
	    (lambda (row)
	      (list (car row)
		    (round-3
		     (+ (candidate-breadcrumbs (car row)) (cadr row)))))])
;	     [new-row (map process-row *workspace*)])
      (codemsg "  ~%")
      (if *graphics*
	  (draw-codelet-message
	   (format " ~%")))
      (begin
	(map subtotal *candidates*)
;	(set! *workspace* new-row)
	(if #f
	    (begin
	      (add-to-coderack
	       breadcrumb-codelet
	       'breadcrumb-codelet
	       *medium-urgency*
	       (add1 gen))
	      (codemsg " breadcrumb-codelet spun.~%")))))))

(define breadcrumb-codelet
  (lambda (gen)
    (if *graphics* (draw-codelet "breadcrumb" gen))
    (codemsg "breadcrumb from generation ~s~%" gen)
    (let* ([subtotal
	    (lambda (candidate)
	      (add-choice candidate
			  (heading-score *current-point* (car candidate))
			  1))]
	   [process-row
	    (lambda (row)
	      (list (car row)
		    (round-3
		     (+ (candidate-breadcrumbs (car row)) (cadr row)))))])
;	     [new-row (map process-row *workspace*)])
      (codemsg "  ~%")
      (if *graphics*
	  (draw-codelet-message
	   (format " ~%")))
      (begin
	(map subtotal *candidates*)
;	(set! *workspace* new-row)
	(if #f
	    (begin
	      (add-to-coderack
	       breadcrumb-codelet
	       'breadcrumb-codelet
	       *medium-urgency*
	       (add1 gen))
	      (codemsg " breadcrumb-codelet spun.~%")))))))

; [homing]---------------------------------------------------------

; don't get turned around, probably

(define homing-codelet
  (lambda (gen)
    (if *graphics* (draw-codelet "homing" gen))
    (codemsg "homing from generation ~s~%" gen)
    (let* ([subtotal
	    (lambda (candidate)
	      (if (eq? *last-point* 'none)
		  0
		  (add-choice candidate (round-3
					 (candidate-homing candidate)) 3)))]
	   [process-row
	    (lambda (row)
	      (list (car row)
		    (round-3
		     (+ (candidate-homing (car row)) (cadr row)))))])
;	     [new-row (map process-row *workspace*)])
      (codemsg "  ~%")
      (if *graphics*
	  (draw-codelet-message
	   (format " ~%")))
      (begin
	(map subtotal *candidates*)
;	(set! *workspace* new-row)
	(if #f
	    (begin
	      (add-to-coderack
	       homing-codelet
	       'homing-codelet
	       *medium-urgency*
	       (add1 gen))
	      (codemsg " homing-codelet spun.~%")))))))

; [rule-enforcer]---------------------------------------------------------

(define rule-enforcer-codelet
  (lambda (rule-info)
    (lambda (gen)
      (if *graphics* (draw-codelet "rule-enforcer" gen))
      (codemsg "rule-enforcer from generation ~s~%" gen)
      (let* ([subtotal
	      (lambda (candidate)
		(add-choice candidate
			    (* 800 (rule-step-score rule-info candidate)) 6))]
	     [process-row
	      (lambda (row)
		(list (car row)
		      (round-3
		       (+ (rule-step-score rule-info row) (cadr row)))))])
;	     [new-row (map process-row *workspace*)])
	(codemsg "  ~%")
	(if *graphics*
	    (draw-codelet-message
	      (format " ~%")))
	(begin
	  (map subtotal *candidates*)
;	  (set! *workspace* new-row)
	  (if #f
	      (begin
		(add-to-coderack
		 (rule-enforcer-codelet operands)
		 'rule-enforcer-codelet
		 *medium-urgency*
		 (add1 gen))
		(codemsg " rule-enforcer-codelet spun.~%"))))))))

; [trait-rewarder]---------------------------------------------------------

(define trait-progress-reward-codelet
  (lambda (dimension)
    (lambda (gen)
      (if *graphics* (draw-codelet "trait-progress-reward" gen))
      (codemsg "trait-progress-reward from generation ~s~%" gen)
      (let* ([trait-score
	      (lambda (candidate)
		(+
		 (* 20 (trait-ahead-score
			dimension
			(cons (cadr candidate) *own-stuff*)))
		 (* 15 (trait-looking-ahead-score
			dimension
			(cons (cadr candidate) *own-stuff*)
			*tip-1* *tip-2*))))]
	     [subtotal
	      (lambda (candidate)
		(add-choice candidate
			    (trait-score candidate) 5))]
	     [process-row
	      (lambda (row)
		(list (car row)
		      (round-3
		       (+ (trait-score (car row)) (cadr row)))))])
;	     [new-row (map process-row *workspace*)])
	(codemsg "  ~%")
	(if *graphics*
	    (draw-codelet-message
	      (format " ~%")))
	(begin
	  (map subtotal *candidates*)
;	  (set! *workspace* new-row)
	  (if #f
	      (begin
		(add-to-coderack
		 (trait-progress-reward-codelet operands)
		 'trait-progress-reward-codelet
		 *medium-urgency*
		 (add1 gen))
		(codemsg " trait-progress-reward-codelet spun.~%"))))))))

(define trait-met-reward-codelet
  (lambda (dimension)
    (lambda (gen)
      (if *graphics* (draw-codelet "trait-met-reward" gen))
      (codemsg "trait-met-reward from generation ~s~%" gen)
      (let* ([trait-score
	      (lambda (candidate)
		(* 35 (trait-met-score
		       dimension
		       (cons (cadr candidate) *own-stuff*))))]
	     [subtotal
	      (lambda (candidate)
		(add-choice candidate
			    (trait-score candidate) 4))]
	     [process-row
	      (lambda (row)
		(list (car row)
		      (round-3
		       (+ (trait-score (car row)) (cadr row)))))])
;	     [new-row (map process-row *workspace*)])
	(codemsg "  ~%")
	(if *graphics*
	    (draw-codelet-message
	      (format " ~%")))
	(begin
	  (map subtotal *candidates*)
;	  (set! *workspace* new-row)
	  (if #f
	      (begin
		(add-to-coderack
		 (trait-met-reward-codelet operands)
		 'trait-met-reward-codelet
		 *medium-urgency*
		 (add1 gen))
		(codemsg " trait-met-reward-codelet spun.~%"))))))))

; [touching]---------------------------------------------------------

(define tip-2-score
  (lambda (pt)
    (lookup-score pt *tip-2s*)))

(define finisher-codelet
  (lambda (gen)
    (if *graphics* (draw-codelet "finisher" gen))
    (codemsg "finisher from generation ~s~%" gen)
    ; will have to generalize if you want to be able to draw the
    ; other direction
    (let* ([candidate-tip-value
	    (lambda (candidate)
	      (let*
		  ([the-point (car candidate)]
		   [points-beyond
		    (remove-item *last-point*
				 (points-from-point the-point))]
		   ; consider how well you might do after the next step
		   [score-after
		    (if (null? points-beyond)
			0
			(* 0.25 (apply max (map tip-2-score points-beyond))))]
		   [score
		    (- (tip-2-score the-point) score-after)])
		(if (> score 0) score -200)))]
	   [subtotal
	    (lambda (candidate)
	      (add-choice candidate
			  (* 0.25 (candidate-tip-value candidate))
			  2))]
	   [process-row
	    (lambda (row)
	      (list (car row)
		    (round-3
		     (+ (candidate-tip-value (car row)) (cadr row)))))])
;	   [new-row (map process-row *workspace*)])
      (codemsg "  ~%")
      (if *graphics*
	  (draw-codelet-message
	   (format " ~%")))
      (begin
	(map subtotal *candidates*)
;	(set! *workspace* new-row)
	(if #f
	    (begin
	      (add-to-coderack
	       finisher-codelet
	       'finisher-codelet
	       *medium-urgency*
	       (add1 gen))
	      (codemsg " finisher-codelet spun.~%")))))))


; [touching]---------------------------------------------------------

(define touching-codelet
  (lambda (gen)
    (if *graphics* (draw-codelet "touching" gen))
    (codemsg "touching from generation ~s~%" gen)
    (let* ([subtotal
	    (lambda (candidate)
	      (add-choice candidate (candidate-touching candidate) 8))]
	   [quit-subtotal
	    (lambda (candidate)
	      (add-choice candidate (candidate-quit-touching candidate) 9))]
	   [process-row
	    (lambda (row)
	      (list (car row)
		    (round-3
		     (+ (candidate-touching (car row)) (cadr row)))))])
;	     [new-row (map process-row *workspace*)])
      (codemsg "  ~%")
      (if *graphics*
	  (draw-codelet-message
	   (format " ~%")))
      (begin
	(map subtotal *candidates*)
	(map quit-subtotal *candidates*)
;	(set! *workspace* new-row)
	(if #f
	    (begin
	      (add-to-coderack
	       touching-codelet
	       'touching-codelet
	       *medium-urgency*
	       (add1 gen))
	      (codemsg " touching-codelet spun.~%")))))))
