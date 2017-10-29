;=============================================================================
; adjudicator.ss : Adjudicator codelets
;=============================================================================

; [comparer]-----------------------------------------------------------------
; Adjudicator codelet, JAR 3/5/98
;
; Adjudicator codelets progressing with difficulty, 4/21/98
;
(define comparer-codelet
  (lambda (part nvtype)
    (lambda (gen)
      (if *graphics* (draw-codelet "Comparer-codelet" gen))
      (codemsg "Comparer-codelet from generation ~s~%" gen)
      (codemsg "  looking for norm violation ~s~%" nvtype)
      (if *graphics*
	  (draw-codelet-message
	   (format "looking for norm violation ~s~%"
		   nvtype)))
      (let*
	  ([partname (car part)]
	   [type (lookup 'topology (eval partname))]
	   [partprops (lookup partname *relative-NVs*)]
	   [oldprops (find partname *relative-NVs*)]
	   [comparison (compare-nv-type part nvtype)]
	   [already (subtract (mapcar car (cadr oldprops)) '(tips))]
	   [tiptypes (not-atoms (map cadr (cadr oldprops)))])
	(if
	    (not (or
		  (member? nvtype already)
		  (eq? comparison 'same)
		  (and (eq? 'tips nvtype)
		       (or
			(eq? type 'loop)
			(eq? type 'dot)
			(member? comparison tiptypes)
			(eq? (cadr comparison) '*no-move*)))))
	    (set! *relative-NVs*
		  (condcons
		   (list partname
			 (cons
			  (list nvtype comparison)
			  partprops))
	       (remove oldprops *relative-NVs*))))
	(if (< (n-sided-die 2) 2)
	    (begin
	      (add-to-coderack
	       (comparer-codelet (roulette *fillers*) (roulette *nv-types*))
	       'comparer-codelet
	       *medium-urgency*
	       (add1 gen))
	      (codemsg " comparer-codelet spun.~%")))))))

; [stenographer]------------------------------------------------------------
; notes the difference between norms and observed features, but literally
(define stenographer-codelet
  (lambda (part nvtype)
    (lambda (gen)
      (if *graphics* (draw-codelet "Stenographer-codelet" gen))
      (codemsg "Stenographer-codelet from generation ~s~%" gen)
      (codemsg "  looking for norm violation ~s~%" nvtype)
      (if *graphics*
	  (draw-codelet-message
	   (format "looking for norm violation ~s~%"
		   nvtype)))
      (let*
	  ([partname (car part)]
	   [type (lookup 'topology (eval partname))]
	   [partprops (lookup partname *map-NVs*)]
	   [oldprops (find partname *map-NVs*)]
	   [comparison (map-nv-type part nvtype)]
	   [already (subtract (mapcar car (cadr oldprops)) '(tips))]
	   [tiptypes (map car (not-atoms (map cadr (cadr oldprops))))])
	(if
	    (not (or
		  (member? nvtype already)
		  (eq? comparison 'none)
		  (eq? (car comparison) (cadr comparison))
		  (and (eq? 'tips nvtype)
		       (or
			(eq? type 'loop)
			(eq? type 'dot)
			(member? (car comparison) tiptypes)
			(eq? (caadr comparison) (cadadr comparison))))))
	    (set! *map-NVs*
		  (condcons
		   (list partname
			 (cons
			  (list nvtype comparison)
			  partprops))
	       (remove oldprops *map-NVs*))))
	(if (< (n-sided-die 2) 2)
	    (begin
	      (add-to-coderack
	       (stenographer-codelet
		(roulette *fillers*) (roulette *nv-types*))
	       'stenographer-codelet
	       *medium-urgency*
	       (add1 gen))
	      (codemsg " stenographer-codelet spun.~%")))))))

; [constable]---------------------------------------------------------------

(define constable-codelet
  (lambda (artype)
    (lambda (gen)
      (if *graphics* (draw-codelet "Constable-codelet" gen))
      (codemsg "Constable-codelet from generation ~s~%" gen)
      (let* ([enforced? (rule-type artype)]
	     [care? (member? artype *rules-to-notice*)])
	(codemsg "  looking for rule violation ~s~%" artype)
	(if *graphics*
	    (draw-codelet-message
	      (format "looking for rule violation ~s~%"
		artype)))
	(begin
	  (if (and enforced? care?)
	      (set! *abstract-rules* (condcons artype *abstract-rules*)))
	  (if (< (n-sided-die 2) 2)
	      (begin
		(add-to-coderack
		 (constable-codelet (roulette *rule-types*))
		 'constable-codelet
		 *medium-urgency*
		 (add1 gen))
		(codemsg " constable-codelet spun.~%"))))))))


; just keep all the motifs of a type together
(define worm-codelet
  (lambda (mtftype)
    (lambda (gen)
      (begin
	(if *graphics* (draw-codelet "Worm-codelet" gen))
	(codemsg "Worm-codelet from generation ~s~%" gen)
	(codemsg "  looking for motif ~s~%" mtftype)
	(if *graphics*
	    (draw-codelet-message
	     (format "looking for motif ~s~%"
		     mtftype)))
	(case mtftype
	  [literal (set! *literal-motifs*
			 (condcons (worm-type mtftype) *literal-motifs*))]
	  [translate (set! *translate-motifs*
			   (condcons (worm-type mtftype) *translate-motifs*))]
	  [turn-180 (set! *turn-180-motifs*
			  (condcons (worm-type mtftype) *turn-180-motifs*))]
	  [turn-90 (set! *turn-90-motifs*
			  (condcons (worm-type mtftype) *turn-90-motifs*))]
	  [turn-45 (set! *turn-45-motifs*
			  (condcons (worm-type mtftype) *turn-45-motifs*))])
	(if (< (n-sided-die 2) 2)
	    (begin
	      (add-to-coderack
	       (worm-codelet (roulette *motif-types*))
	       'worm-codelet
	       *medium-urgency*
	       (add1 gen))
	      (codemsg " worm-codelet spun.~%")))))))

; far end is in Thematic Focus
; near end is in the current letter's SPs (the *workspace*)
; bridges stored in *bridges*

(set! *sp-types*
      '(*relative-NVs* *map-NVs* *abstract-rules* *literal-motifs*
		       *translate-motifs* *turn-180-motifs*
		       *turn-90-motifs* *turn-45-motifs*))

(set! *favored-sp-types*
      '((*relative-NVs* 10) (*map-NVs* 10) (*abstract-rules* 10)
	(*literal-motifs* 3) (*translate-motifs* 2) (*turn-180-motifs* 1)
	(*turn-90-motifs* 1) (*turn-45-motifs* 1)))

(set! *favored-motif-types*
      '((*literal-motifs* 10) (*translate-motifs* 5) (*turn-180-motifs* 3)
	(*turn-90-motifs* 2) (*turn-45-motifs* 2)))


(set! *favored-tf-levels*
      '((*letter-rows* 2)
	(*two-time-sps* 3)
	(*occasional-sps* 3)
	(*common-sps* 4)
	(*frequent-sps* 6)
	(*universal-sps* 10)))

(define bridge-back-codelet
  (lambda (levelname)
    (lambda (gen)
      (codemsg "Bridge-back codelet from generation ~s~%" gen)
      (codemsg " bridge from ~s to current letter~%" levelname)
      (let* ([sp-type (weighted-roulette *favored-sp-types*)]
	     [far-end (bridge-far-start-what sp-type levelname)])
	; fizzle if we find no SP there at all, or if we find
	; an abstract-rule that doesn't apply to this letter
	(if (or
	     (null? far-end)
	     (and
	      (eq? sp-type '*abstract-rules*)
	      (not (member? far-end (whole-rule-norms *answer*)))))
	    (codemsg " Bridge-back fizzled on level ~s -- no relevant SP~%"
		     levelname)
	    (let*
		([near-where (bridge-near-where sp-type)]
		 [near-choices (bridge-near-choices near-where)]
		 [test-near-end
		  (lambda (choice)
		    (sp-match sp-type choice far-end))]
		 [valid-near-ends
		  (remove-item '() (map test-near-end near-choices))])
	      ; (printf "Bridge-back~%")
	      (if (null? valid-near-ends)
		  (let
		      ([new-bridge
			(list 'demote sp-type levelname far-end)])
		    ; was condcons
		    (set! *bridges* (condcons new-bridge *bridges*))
		    (set! temp-jolts
			  (condcons
			   (list 'back-not-found sp-type levelname
				 far-end)
			   temp-jolts))
		    (thermostat 100
				(* 2 (importance sp-type levelname far-end)))
		    (codemsg " Letter lacks SP from Thematic Focus~%"))
		  (let*
		      ([match (length-roulette valid-near-ends)]
		       [new-bridge
			(list 'promote sp-type levelname far-end
			      match)])
		    (set! temp-jolts
			  (condcons
			   (list 'back-found sp-type levelname match)
			   temp-jolts))
		    (thermostat 0 (importance sp-type levelname match))
		    (set! *bridges* (condcons new-bridge *bridges*))))))))))

(define bridge-ahead-codelet
  (lambda (gen)
      (codemsg "bridge-ahead codelet from generation ~s~%" gen)
      (codemsg " bridge from current letter~%")
      (let* ([sp-type (weighted-roulette *favored-sp-types*)]
	     [near-where (bridge-near-where sp-type)]
	     [near-choices (bridge-near-choices near-where)])
	(if (null? near-choices)
	    (codemsg " There are no SPs of type ~s~%" sp-type)
	    (let*
		([near-end (roulette near-choices)]
		 [far-match
		      (if (motif-type? sp-type)
			  (motif-match sp-type near-end)
			  (bridge-highest-far-match
			   sp-type near-end
			   (reverse *thematic-focus*)))])
	      (if (null? far-match)
		  (let
		      ([new-bridge
			(list 'add sp-type near-end)])
		    (set! *bridges* (condcons new-bridge *bridges*))
		    (set! temp-jolts
			  (condcons
			   (list 'ahead-not-found sp-type near-end)
			   temp-jolts))
		    (thermostat (* 16.667 (full-levels)) 2.0)
		    (codemsg " SP not in Thematic Focus yet ~%"))
		  (let*
		      ([far-level (car far-match)]
		       [far-sp (cadr far-match)]
		       [match (caddr far-match)]
		       [new-bridge
			(list 'promote sp-type far-level far-sp match)])
		    (thermostat 0 (importance sp-type far-level match))
		    (set! temp-jolts
			  (condcons
			   (list 'ahead-found sp-type far-level far-sp)
			   temp-jolts))
		    (set! *bridges* (condcons new-bridge *bridges*)))))))))

; just determines the order of the bridges for eventual TF update (if any)

(define promoter-codelet
  (lambda (gen)
    (codemsg "Promoter codelet from generation ~s~%" gen)
    (codemsg " Promote SP in Thematic Focus~%")
    (if (null? *bridges*)
	(begin
	  (codemsg "No bridges left!~%")
	  (codemsg "  >>>Fizzle<<<~%"))
	(let
	    ([bridge (roulette *bridges*)])
	  (set! *bridges* (remove-item bridge *bridges*))
	  (set! *promotions* (cons bridge *promotions*))))))

(define update-tf
  (lambda ()
    (begin
      (set! *library*  ; must also yank old entry, if any
	    (condcons (library-entry) *library*))
      (handle-promotion))))
      
; deterministic promotion and probabilistic demotion
; items in promote bridge are
; add '*letter-rows* sp-type workspace-sp
; promote sp-type levelname tf-sp workspace-sp
; demote sp-type levelname tf-sp

(define handle-promotion
  (lambda ()
    (if (not (null? *promotions*))
	(let ([bridge (car *promotions*)])
	  (set! *promotions* (cdr *promotions*))
	  (case (car bridge)
	    [add
	     (add-to-tf '*letter-rows* (cadr bridge) (caddr bridge))]
	    [promote
	     (if (prob-decision? (promote-prob (caddr bridge) (cadr bridge)))
		 (promote-in-tf (cadr bridge) (caddr bridge)
				(nth 4 bridge) (nth 5 bridge)))]
	    [demote
	     (if (prob-decision? (demote-prob (caddr bridge) (cadr bridge)))
		 (demote-in-tf (cadr bridge) (caddr bridge)
			       (nth 4 bridge)))])
	  (handle-promotion)))))

(define motif-cleaning
  (lambda ()
    (begin
      (set! *literal-motifs*
	    (pruned-motifs  '*literal-motifs* *literal-motifs*))
      (set! *translate-motifs*
	    (pruned-motifs  '*translate-motifs* *translate-motifs*))
      (set! *turn-180-motifs*
	    (pruned-motifs  '*turn-180-motifs* *turn-180-motifs*))
      (set! *turn-90-motifs*
	    (pruned-motifs  '*turn-90-motifs* *turn-90-motifs*))
      (set! *turn-45-motifs*
	    (pruned-motifs  '*turn-45-motifs* *turn-45-motifs*)))))
