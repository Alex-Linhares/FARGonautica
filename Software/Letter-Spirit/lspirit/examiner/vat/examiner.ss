;=============================================================================
; examiner.ss : a set of codelets which can be added to the coderack
; AKA the file formerly known as codelets.ss (JAR, 7/28/97)
;=============================================================================
; There are two types of codelets defined below.  Those that get "frozen"
; on the coderack list WITH particular values, and those that are "frozen"
; without other arguments.  Those with values are curried functions.
; A codelet is simply a function that can be "thawed" by evaluating it if
; it is picked off the coderack.
;-----------------------------------------------------------------------------
; (sload 'labels)

; This constant makes urgencies of later generations scaleable according
; to Bob's formula.  Set at 1 you have a vanilla coderack.  The higher it
; is, the bigger the gap.  I have never experimented with this variable.
(set! *generation-gap-constant* 1)

; default urgency settings (many are calculated with these as a base)
(set! *very-high-urgency* 100)
(set! *high-urgency* 50)
(set! *medium-urgency* 25)
(set! *low-urgency* 10)

; if whine is over *whine-threshold* then make pacifier higher urgency
(set! *whine-threshold* 50)

; this constant allows occassional whiner<-->non-whiner gloms
; was 92, now 80 (1/11/95)
(set! *non-whine-glom* 80)

; set the scale of label-checker-codelet spin probability
; the probability of spin is set by # of labels times this constant
; for example 5 labels * 5 = 25% chance of label-checker
(set! *label-checker-constant* 5)

; Throughout the following code, a "part" is a structure of the form
; ((<quanta pairs>) <labels>)
; A stupid ancient convention results in a weirdness here.  If there is
; only one quanta pair, it is represented as a single list instead of
; a list of one pair.  The code is written as to reflect this dumb idea.
; Too late to change...
; Example.  Part: ((4 5) (**whine 15)) vs. Part: (((4 5)(5 22)) (**whine 15))
;                  ^^should be a list of joints...isn't

;===========================[ unbound codelets ]=============================
; A codelet is simply a function that gets frozen into a list (the coderack).
; If picked, it is thawed out and run (evaluated).  Some codelets are frozen
; with particular arguments in curried form.  Others (like the ones directly
; below) have no values frozen in.  All codelets have the form of a function
; of one argument...generation.

; [looker]--------------------------------------------------------------------
; This codelet picks a part at random and figures out if it's worth
; labeling or not.  If so spin some bound codelets else increase whine of part.
(define looker-codelet
  (lambda (gen)
    (codemsg "Looker-codelet from generation ~s~%" gen)
    (if *graphics* (draw-codelet "Looker-codelet" gen))
    (let* ((nm-parts (length *workspace*))	         ; how many parts??
	   (roll (sub1 (n-sided-die nm-parts)))          ; pick one at random
	   (part (list-ref *workspace* roll))	         ; get part
	   (part-jls (car part))		         ; get joint list
	   (quanta (collapse part-jls))	                 ; get quanta list
	   (nm-quanta (length quanta))	                 ; number of quanta
	   (nm-all-quanta (count-bits *quanta*))         ; global num of quanta
	   (weird? (weird-check-part part gen))    ; check tips (2 only)
	   (passes? (inspect-part part gen))       ; check size
	   (labeled? (member*? 'quanta part)))
      (codemsg "  Looking at part ~s~%" (car part))
      (if (or (and passes? (not weird?))
	      labeled?)
	  (begin
	    (codemsg "	Part passed inspection..")
	    ; add some labeler codelets (bound to the part) to the coderack
	    ; add the part's quanta list as a label since we know it!
	    (if (member*? '**whine (get-updated-part part-jls))
		(begin
		  (if *graphics*
		      (draw-codelet-message
			(format "Part ~s passed inspection adding labelers"
			  (car part))))
		  (codemsg "adding bound labelers~%")
		  (remove-whine part)
		  ; linearized by JAR, 9/98
		  (add-label part-jls (list 'quanta (linearize quanta)))
		  (add-n-to-coderack
		    *labelers*
		    (labeler-codelet (get-updated-part part-jls))
		    'labeler *medium-urgency*
		    (add1 gen)))
		(begin
		  (if *graphics*
		      (draw-codelet-message
			(format "Part ~s is already labeled...>>>>Fizzle<<<<"
			  (car part))))
		  (codemsg "but it's already labeled~%")
		  (codemsg "  >>>Fizzle<<<~%"))))
	  ; make an overlooked part a bit more whiny
	  (begin
	    (if *graphics*
		(draw-codelet-message
		  (format
		   "Bad part ~s...abusing it and posting pacifier"
		    (car part))))
	    (codemsg "	Bad part...abusing it and posting pacifier~%")
	    (increase-whine part)
	    (let ([whine-value (lookup-score '**whine
					     (get-features
					      (get-updated-part part-jls)))])
	      (add-to-coderack pacifier-codelet 'pacifier whine-value
			       (add1 gen))))))))

; [pacifier]-------------------------------------------------------------------
; { this is the second shot at a pacifier }
; More logical structure to the pacifier now.  Probablistically pick the
; greatest whiner and handle it completely.  Only in the case of a weird
; glom proposal will nothing be done. 

(define pacifier-codelet
  (lambda (gen)
    (codemsg "Pacifier-codelet from generation ~s~%" gen)
    (if *graphics* (draw-codelet "Pacifier-codelet" gen))
    (let* ([whiners (find-whiners *workspace*)]
	   [num (length whiners)]
	   [one? (>= num 1)]
	   [none? (zero? num)]
	   ; crybaby is probabalistic whiniest {squeaky wheel gets the grease!}
	   [crybaby (if one? (roulette-pick-whiner whiners))]
	   [cryquanta (if one? (collapse (car crybaby)))]
	   [nhbs? (if one? (not (null? (find-neighbors crybaby)))
		      #f)]
	   [neighbors (if one? (find-smallest-neighbor crybaby))]
	   [small-neighbor (if one? (car neighbors))]
	   [rand-neighbor (if (and one? nhbs?)
			      (roulette (find-neighbors crybaby)))]
	   [neighbor (if (> (length *workspace*) 1)
			 (roulette (list small-neighbor rand-neighbor))
			 small-neighbor)]
	   [others (if one? (remove neighbor (cadr neighbors)))]
	   [weird? (if none?
		       #f
		       (weird-check-part crybaby gen))]
	   [passes? (if none?
			#f
			(inspect-part crybaby gen))])
      (cond
       [none? (if *graphics*
		  (draw-codelet-message
		   "There are no whiners...>>>Fizzle<<<"))
	      (codemsg "  there are no whiners!  >>>Fizzle<<<~%")]
       [weird? (if *graphics*
		  (draw-codelet-message
		   "Whiny part weird...>>>Fizzle<<<"))
	      (codemsg "  whiny part weird!  >>>Fizzle<<<~%")]
       [(or passes?
	    (not neighbor))
	    (codemsg "  part ~s passed inspection.~%" (car crybaby))
	    (codemsg "  Label part...")
	    ; add some labeler codelets (bound to the part)
	    ; to the coderack
	    ; add the label's quanta list
	    ; since we know it!
	    (if *graphics*
		(draw-codelet-message
		 (format
		  "~s passed adding labelers" cryquanta)))
	    (codemsg "adding bound labelers~%")
	    (remove-whine crybaby)
	    (let* ((cry-jls (car crybaby))
		   (quanta (collapse cry-jls))
		   (nm-quanta (length quanta)))
	      (add-label (car crybaby) (list 'quanta (linearize quanta)))
	      (add-n-to-coderack
	       *labelers*
	       (labeler-codelet
		(get-updated-part
		 (car crybaby)))
	       'labeler *medium-urgency*
		   (add1 gen)))]
       [(and one? nhbs? neighbor)
	(codemsg "  checking smallest neighboring part for ~s..."
			  (car crybaby))
		 ; glomming
		 (if (hypothetical-combine-parts-and-test crybaby
							  neighbor others)
		     (begin
		       (add-to-coderack looker-codelet
					'looker *medium-urgency*
					(add1 gen))
		       (add-to-coderack looker-codelet
					'looker *medium-urgency*
					(add1 gen))
		       (dampen)
		       (if *graphics*
			   (begin
			     (draw-codelet-message
			      (format
			       "glomming ~s to smallest neighbor ~s"
			       cryquanta (car neighbor)))
			     (draw-codelet-message2
			      "adding looker codelets")))
		       (codemsg
			"	adding more looker-codelets~%"))
		     ; Whack the offending part and spin pacifier
		     (begin
		       ;***** should LABEL it now
		       (codemsg "  passing part ~s.~%" (car crybaby))
		       (codemsg "  Label part...")
		       ; add some labeler codelets (bound to the part)
		       ; label part with its quanta list
		       (if *graphics*
			   (draw-codelet-message
			    (format
			     "~s passed adding labelers" cryquanta)))
		       (codemsg "adding bound labelers~%")
		       (remove-whine crybaby)
		       (let* ((cry-jls (car crybaby))
			      (quanta (collapse cry-jls))
			      (nm-quanta (length quanta)))
			 (add-label cry-jls (list 'quanta (linearize quanta)))
			 (add-n-to-coderack
			  *labelers*
			  (labeler-codelet
			   (get-updated-part
			    cry-jls))
			  'labeler *medium-urgency*
			  (add1 gen)))))]
    [else (codemsg "ERROR!! pacifier fell through~%")]))))
       
; [label-checker]-------------------------------------------------------------
; Check a random part in the workspace to see if it can be bound to a
; relation-sparking codelet.
; This type of codelet is spun probabalistically based on the number of
; labels a part has when it is going through the labeler.
(define label-checker-codelet
  (lambda (gen)
    (codemsg "Label-checker-codelet from generation ~s~%" gen)
    (if *graphics* (draw-codelet "Label-checker-codelet" gen))
    (let* ([part (rand-part)]	    ; get part
	   [labels (length part)]
	   [enough-labels (label-threshold part)]
	   [urgency (+ 20 (* 5 (- labels enough-labels -3)))])
      (codemsg "  Checking labels of part ~s~%" (car part))
      (if *graphics*
	  (draw-codelet-message
	   (format "  Checking part ~s" (car part))))
      (if (> labels enough-labels)
	  (begin
	    (if *graphics*
		(draw-codelet-message2
		  (format "Part ~s passed inspection...adding sparker."
		    (car part))))
	    (codemsg "  part passed inspection.~%")
	    (codemsg "  sparking codelet spun.~%")
	    (add-to-coderack
	      (sparker-codelet part)
	      'sparker urgency
	      (add1 gen)))
	  (begin
	    (if *graphics*
		(draw-codelet-message2 "Not enough labels...>>>Fizzle<<<"))
	    (codemsg "  not enough labels...>>>Fizzle<<<~%"))))))

; [activation-spreader]-------------------------------------------------------
; Key thing overhauled, 1998
; Spread activation unless some parts are whining out there.  (Note that
; the test of an activator that waited for all parts to be BOUND was a
; failure.  This is a good compromise.)  The work is done by memory.ss
; Whining condition stricken by JAR, 7/9/96
; R-role checking taken from supercdlet by JAR, 9/25/96

(define activation-spreader-codelet
  (lambda (gen)
    (if *graphics* (draw-codelet "Activation spreader" gen))
    (if (and *all-sparked* (eq? 3 (n-sided-die 3)))
	(do-something (rand-part) gen))
    (codemsg "Activation spreader codelet from generation ~s~%" gen)
    (examiner-spread-acts)
    (set! *all-sparked* (eq? (length *workspace*)
			     (length (uniquify (mapcar caadr *bindings*)))))
    (r-role-check-wholes *wholes*)))

; [gestalt-codelet]-----------------------------------------------------------
; The code that this front-end calls lives in topdown.ss. Basically, pick a
; gestalt type and check it out.
(define gestalt-codelet
  (lambda (gen)
    (if *graphics* (draw-codelet "Gestalt codelet" gen))
    (codemsg "Gestalt codelet from generation ~s~%" gen)
    (gestalt-activate gen)))

;========================[ bound codelets ]===================================
; codelet makers - return codelet procedures
; These codelet-makers are curried since they are bound to specific parts
; or have other specific arguments frozen in them when posted.

; [top-glommer-codelet]-------------------------------------------------------
; This codelet gets spun by the activator (see memory.ss).  It is spun
; probabilistically if the activation is low (the lower, the more possible)
; and tha part in question is "small".
(define top-glommer-codelet
  (lambda (part)
    (lambda (gen)
      (if *graphics* (draw-codelet "Top glommer codelet" gen))
      (codemsg "Top glommer codelet from generation ~s~%" gen)
      (codemsg "   attempt to glom ~s to " (car part))
      (if (part-exists? part)
	  (let* ([pt (get-updated-part (car part))]
		 [nhbs (find-neighbors pt)]
		 [rand-neighbor (if (not (null? nhbs))
				    (roulette (find-neighbors pt)))]
		 [worst (pick-worst-neighbor pt)]
		 [nb (roulette (list rand-neighbor worst))])
	    (if (and (not (null? nb)) (not (null? nhbs))
		     (hypothetical-combine-parts-and-test pt nb '()))
		(begin
		  (add-to-coderack looker-codelet
				   'looker *medium-urgency*
				   (add1 gen))
		  (add-to-coderack looker-codelet
				   'looker *medium-urgency*
				   (add1 gen))
		  (dampen)
		  (if *graphics*
		      (begin
			(draw-codelet-message
			 (format
			  "glomming ~s to neighbor ~s" (car pt) (car nb)))
			(draw-codelet-message2
			 "adding looker codelets")))
		  (codemsg "	adding more looker-codelets~%"))
		(codemsg "  Nobody to glom to...>>>FIZZLE<<<~%"))))
      (codemsg "  Part no longer around...>>>FIZZLE<<<~%"))))

; [top-breaker-codelet]-------------------------------------------------------
; This is a similar codelet (spun by the activator) but this time the part
; in question is "big".
(define top-breaker-codelet
  (lambda (part)
    (lambda (gen)
      (if *graphics* (draw-codelet "Top breaker codelet" gen))
      (codemsg "Top breaker codelet from generation ~s~%" gen)
      ((breaker-codelet part) gen))))
      ; run the old regular breaker NOW

; [sparker]-------------------------------------------------------------------
; This codelet checks a part, then activates roles that the part matches
; and posts an activation-spreading codelet.
; If no roles are activated, then the part is slated for breaking
; unless it has too few labels in which case labeler codelets are spun.
; Now gets an updated part before it runs...
(define sparker-codelet
  (lambda (part)
    (lambda (gen)
      (if *graphics* (draw-codelet "Sparker-codelet" gen))
      (codemsg "Sparker-codelet from generation ~s~%" gen)
      (if (part-exists? part)
	  (begin
	    (if *graphics*
		(draw-codelet-message
		 (format "  activating roles using part ~s" (car part))))
	    (codemsg "  activating roles using part ~s~%" (car part))
	    (let* ([up-part (get-updated-part (car part))]
		   [enough-labels (label-threshold part)]
		   [size (length (collapse (car part)))]
		   [whine? (member*? '**whine up-part)])
	      (if (not (activate-roles-with-part up-part gen))
		  (if (< (sub1 (length up-part)) enough-labels)
		      (begin
			(if *graphics*
			    (draw-codelet-message2
			     "  no roles activated (few labels), adding labelers"))
			(codemsg
			 "  no roles activated, but not many labels...>>>Fizzle<<<~%")
			(codemsg "  Adding bound labelers~%")
			(let ([prt
			       (if (not whine?)
				   up-part
				   (begin
				     (remove-whine up-part)
				     (add-label
				      (car up-part)
				      (list
				       'quanta
				       (linearize (collapse (car up-part)))))
				     (get-updated-part (car up-part))))])
			    (add-n-to-coderack
			     3
			     (labeler-codelet prt)
			     'labeler *medium-urgency*
			     (add1 gen))))
		      (do-something part gen))
		  (begin
		    ; decides to break/glom or do nothing
		    (let* ([top-act (max 0 (part-top-activation part))]
			   [probability (- 55 top-act)]
			   [roll (n-sided-die 100)])
		      (if (and (> *codelets-run* 25000)
			       (<= (n-sided-die 100) 30))
			  (smart-parse)
			  (if (and (not (negative? probability))
				   (<= roll probability))
			      (begin
				(if *graphics*
				    (draw-codelet-message2
				     "  spinning top-down glommer or breaker"))
				(codemsg "   Spinning top-down codelet [~s/~s]"
					 roll probability)
				(cond
				 ; was based on size: <=2: glom, >=3: break
				 [(<= size (add1 (n-sided-die 5)))
				  (codemsg "top-glommer~%")
				  (add-to-coderack
				   (top-glommer-codelet up-part)
				   'top-glommer (+ *high-urgency* probability)
				   (add1 gen))]
				 [else
				  (codemsg "top-breaker~%")
				  (add-to-coderack
				   (top-breaker-codelet up-part)
				   'top-breaker (+ *high-urgency* probability)
				   (add1 gen))]))))
			(if *graphics*
			    (draw-codelet-message2
			     "  spinning activation-spreader"))
			(codemsg "  Activation-spreader codelet spun.~%")
			(add-to-coderack
			 activation-spreader-codelet
			 'activation-spreader *high-urgency* (add1 gen)))))))
	    (begin
	      (codemsg "  part no longer around...>>>Fizzle<<<~%")
	      (if *graphics*
		  (draw-codelet-message "  part gone...>>>Fizzle<<<")))))))

; [breaker]-------------------------------------------------------------------
; This codelet is bound to a part with too many tips (more than two)
; or perhaps to a part that does not activate any roles.
; It breaks the part up in a mostly logical fashion...bottom up.
; 
; rerun the glom-erasing and glom-drawing graphics
(define breaker-codelet
  (lambda (part)
    (lambda (gen)
      (if *graphics* (draw-codelet "Breaker-codelet" gen))
      (codemsg "Breaker-codelet from generation ~s~%" gen)
      (if (part-exists? part)
	  (begin
	    (if *graphics*
		(draw-codelet-message
		 (format "  breaking part ~s" (car part))))
	    (codemsg "  breaking part ~s~%" (car part))
	    (let ([parts (reglue-sort-and-crack (car part))]
		  [leftover-workspace (remq (get-updated-part (car part))
					    *workspace*)])
	      (if *graphics* (erase-subgloms
			      (make-display-list
			       (list (get-updated-part (car part))))))
	      (dampen)
	      (set! *workspace*
		    (append (map (lambda (part)
				   (snoc '(**whine 25)
					 (list part))) parts)
			    leftover-workspace))
	      (if *graphics* (draw-subgloms (make-display-list *workspace*)))
	      (add-n-to-coderack (* 2 (length parts))
				 looker-codelet
				 'looker *medium-urgency* (add1 gen)))
	    (if *graphics*
		(draw-codelet-message2 "  workspace re-set and lookers spun"))
	    (codemsg "  workspace re-set and lookers spun~%"))
	  (begin
	    (if *graphics*
		(draw-codelet-message
		 "  part no longer around..>>>>Fizzle<<<<"))
	    (codemsg "  part no longer around..>>>>Fizzle<<<<~%"))))))

; [labeler]-------------------------------------------------------------------
; This is a very general codelet which attempts to attach one of several
; different labels to its part (part is bound in by looker).  Pick one of
; the label types probablistically.  If the picked label is already attached
; then the codelet fizzles.  The label is attached probabalistically.
;
; A label-checker-codelet gets spun probablistically (based on the number
; of labels attached to a part) if the codelet proceeds.
;
; Spins label-checkers after the labeling is done (or not)


(define labeler-codelet
  (lambda (part)
    (lambda (gen)
      (if *graphics* (draw-codelet "Labeler-codelet" gen))
      (codemsg "Labeler-codelet from generation ~s~%" gen)
      (codemsg "  labeling part ~s~%" (car part))
      (rand-add-label part gen)
      (rand-add-label part gen)
      (rand-add-label part gen)
      (rand-add-label part gen)
      (rand-add-label part gen)
      (rand-add-label part gen)
      (rand-add-label part gen)
      (rand-add-label part gen)
      (rand-add-label part gen)
      (add-to-coderack label-checker-codelet 'label-checker
		       *medium-urgency*
		       (add1 gen))
      (codemsg "  label-checker codelet spun.~%"))))

;========================[ helper functions ]==============================
; for labeling stuff, see labels.ss

(define rand-part
  (lambda ()
    (let*
	([nm-parts (length *workspace*)]	    ; how many parts??
	 [roll (sub1 (n-sided-die nm-parts))])      ; pick one at random
      (list-ref *workspace* roll))))    	    ; get part

;---------------( helper for sparker )------------------------------------
; called when we seem to be at a dead end

(define do-something
  (lambda (part gen)
    (let
	([choice (roulette '(break glom smartparse))])
      (case choice
	[break
	 (if *graphics*
	     (draw-codelet-message2
	      "  no roles activated, Breaker spun."))
	 (codemsg "  no roles activated.  Breaker spun.~%")
	 (add-to-coderack
	  (breaker-codelet part)
	  'breaker *high-urgency* (add1 gen))]
	[glom
	 (if *graphics*
	     (draw-codelet-message2
	      "  no roles activated, Glommer spun."))
	 (codemsg "  no roles activated.  Glommer spun.~%")
	 (add-to-coderack
	  (top-glommer-codelet part)
	  'top-glommer *high-urgency*
	  (add1 gen))]
	[else
	 (if *graphics*
	     (draw-codelet-message2
	      "  no roles activated, Reparsing."))
	 (codemsg "  no roles activated. Re-parsing.~%")
	 (smart-parse)]))))

;--------------( helpers for label-angles curve finder )-------------------
; Compute curves given a list of quanta.  See find-angles (defined below)
; as a staring point to understanding what's going on here.
; In the end, return an angles list or a closure pair.


; Use the *neighbors* data to pick a neighboring quantum to consider
; Don't consider quanta not in the qls
(define find-neighbors
  (lambda (q qls)
    (let ((n-list (lookup q *neighbors*)))
      (letrec ((keep-on
		 (lambda (ls)
		   (cond
		     ((null? ls) '())
		     ((equal? (car ls) '-) (cons (car ls)
						 (keep-on (cdr ls))))
		     ((member? (car ls) qls) (cons (car ls)
						   (keep-on (cdr ls))))
		     (else (cons '- (keep-on (cdr ls))))))))
	(keep-on n-list)))))

; Choose a neighbor, compute the angle and return a list (quantum angle).
; Note that qls must include only those quanta not yet processed!
(define pick-neighbor
  (lambda (q qls)
    (let ((lst (find-neighbors q qls)))
      (letrec ((collapse (lambda (ls)
			   (cond
			     ((null? ls) '())
			     ((equal? (caar ls) '-) (collapse (cdr ls)))
			     (else (cons (car ls)
					 (collapse (cdr ls))))))))
	(car (collapse
	       (list (list (nth 4 lst) 0) (list (nth 11 lst) 0)
		 (list (nth 3 lst) -45) (list (nth 5 lst) 45)
		 (list (nth 10 lst) 45) (list (nth 12 lst) -45)
		 (list (nth 2 lst) -90) (list (nth 6 lst) 90)
		 (list (nth 9 lst) 90) (list (nth 13 lst) -90)
		 (list (nth 1 lst) -135) (list (nth 7 lst) 135)
		 (list (nth 8 lst) 135) (list (nth 14 lst) -135))))))))

; Traverse a part (quantum list), starting at one tip and moving along the
; part.  Calculate an angle for each joint.
(define traverse-part
  (lambda (qls quantum angle-list-register)
    (let* ((neighbor-pair (pick-neighbor quantum qls))
	   (new-q (car neighbor-pair))
	   (angle (cadr neighbor-pair)))
      (cond
	[(null? (cddr qls)) (cons angle angle-list-register)]
	[else (traverse-part (remove quantum qls)
		new-q (cons angle angle-list-register))]))))


;------------------( helpers for pacifier codelet )------------------------
; Find all parts in the workspace with **whine as a label.
; This function also gets called by things other than the pacifier.
(define find-whiners
  (lambda (w)
    (letrec ((loop (lambda (w)
		     (cond
		       ((null? w) '())
		       ((member*? '**whine (car w))
			  (cons (car w) (loop (cdr w))))
		       (else (loop (cdr w))))))
	     (whiner-key (lambda (ls1 ls2)
			   (if (> (lookup '**whine ls1)
				  (lookup '**whine ls2))
			       #t
			       #f))))
      (sort whiner-key (loop w)))))

; Pick a whiner probablistically based on its whininess.  The pick is
; done roulette-wheel style.
(define roulette-pick-whiner
  (lambda (wls)
    (letrec ((whines (lambda (w)
		       (cond
			 ((null? w) '())
			 (else (cons (lookup '**whine (car w))
				     (whines (cdr w))))))))
      (let* ((wns (whines wls))
	     (sum (apply + wns))
	     (roll (n-sided-die sum)))
;      (printf "whine list ~s~%" wns)
;      (printf "sum is ~s.  roll is ~s~%" sum roll)	 
      (letrec ((get-ref (lambda (whines counter ref)
			  (cond
			    ((<= roll counter) ref)
			    (else (get-ref (cdr whines) (+ (car whines)
							   counter)
				    (add1 ref)))))))
	(let ((ref (get-ref wns 0 -1)))
	  (list-ref wls ref)))))))

(define find-neighbors
  (lambda (part)
    (remove-non-neighbors (remq part *workspace*)
			  (collapse (car part)))))

; Find a whiner's smallest neighbor.
; Return a list of the form (<smallest> <rest>) so that procesing
; can continue if smallest neighbor doesn't pan out...
(define find-smallest-neighbor
  (lambda (crybaby)
    (let* ((ws (remq crybaby *workspace*))
	   (nws (remove-non-neighbors ws (collapse (car crybaby)))))
      (letrec ([sz (lambda (wls ref)
		     (cond
		       ((null? wls) '())
		       (else (cons (list ref (length (collapse (caar wls))))
				   (sz (cdr wls) (add1 ref))))))])
	(let* ([sizes (sort (lambda (x y)
			      (< (cadr x) (cadr y))) (sz nws 0))]
	       [smallest	
		 (if (>= (length sizes) 2)
		     (if (= (cadar sizes)
			    (cadadr sizes))
			 (cond
			  [(member*? '**whine
				     (list-ref nws (caar sizes)))
			   (caar sizes)]
			  [(member*? '**whine
				     (list-ref nws (caadr sizes)))
			   (caadr sizes)]
			  [else (let ([g1 (part-goodness
					   (car
					    (list-ref nws (caar sizes))))]
				      [g2 (part-goodness
					   (car
					    (list-ref nws (caadr sizes))))])
				  (if (= g1 g2)
				      (if (= (n-sided-die 2) 1)
					  ; flip to choose
					  (caadr sizes)
					  (caar sizes))
				      (if (< g1 g2)  ; pick smallest goodness
					  (caar sizes)
					  (caadr sizes))))])       
			 (caar sizes))                         
		     (if (= (length sizes) 1)
			 0
			 -1))]
	       [neighbor (if (= -1 smallest)
			     #f
			     (list-ref nws smallest))])
;	      (pretty-print nws)
;	      (pretty-print sizes)
;	  (pretty-print smallest) 
	(list neighbor nws))))))

; Remove stuff from the local copy of the workspace that does not touch
; the quanta-list stuff somewhere.
(define remove-non-neighbors
  (lambda (ws qls)
    (letrec ((outer2 (lambda (ws qls)
;		       (printf "out2: ~s ~s~%" ws qls)
		       (cond
			 ((null? ws) '())
			 (else (let ((result (outer1 (collapse (caar ws))
					       qls)))
				 (if result
				     (cons (car ws) (outer2 (cdr ws) qls))
				     (outer2 (cdr ws) qls)))))))
	     (outer1 (lambda (carwsqnta qls)
;		       (printf "out1: ~s ~s~%" carwsqnta qls)
		       (cond
			 ((null? qls) #f)
			 (else (let ((result (inner (car qls) carwsqnta)))
				 (if result
				     result
				     (outer1 carwsqnta (cdr qls))))))))
	     (inner (lambda (quanta qnls)
;		      (printf "in: ~s ~s~%" quanta qnls)
		      (let ((nbs (get-on-neighbors-list quanta)))
				(cond
				  ((null? qnls) #f)
				  ((member? (car qnls) nbs) #t)
				  (else (inner quanta (cdr qnls))))))))
      (outer2 ws qls))))

; Remove a whiner from the whine list in order to pick another
(define remove-whiner
  (lambda (whiner wls)
    (cond
     [(null? wls) '()]
     [(equal? whiner (car wls)) (cdr wls)]
     [else (cons (car wls)
		 (remove-whiner whiner (cdr wls)))])))

; Used by the pacifier: check out a glom before doing it to make sure
; a weird part is not created.
; Also used by top-down-globber (taking care of extra stuff)
(define hypothetical-combine-parts-and-test
  (lambda (cb ob others)
    (letrec ([outer (lambda (qls1 qls2)
		      (cond
		       [(null? qls1) #f]
		       [else
			(let ([result (inner (car qls1) qls2)])
			  (if result
			      result
			      (outer (cdr qls1) qls2)))]))]
	     [inner (lambda (quanta qls2)
		      (let ([nbs (get-on-neighbors-list quanta)])
			(cond
			 [(null? qls2) #f]
			 [(member? (car qls2) nbs)
			  (list (car qls2) quanta)]
			 [else (inner quanta
				      (cdr qls2))])))])
      (let ([ans (sort < (outer (collapse (car cb))
				(collapse (car ob))))])
	(codemsg "~s~%" (car ob))
	(let* ([j1 (car (get-updated-part (car cb)))]
	       [j2 (car (get-updated-part (car ob)))]
	       [label-pair '(**whine 15)]
	       [newjoints (pair-list (pair-list j1 j2) ans)]
	       [newpart (snoc label-pair (list newjoints))]
	       [pre-qls (car newpart)]
	       [qls (if (list? (car pre-qls))
			pre-qls
			(list pre-qls))]
	       [nm-quanta (length (collapse qls))]
	       [nm-tips (length (quanta-real-tips (collapse qls)))])
	  (cond
	   [(> nm-tips 2) (codemsg "  new part would be weird...next ")
	                  (if (null? others)
			      (begin
				(codemsg "  no more neighbors! Skip it.~%")
				#f)
			      (hypothetical-combine-parts-and-test
			       cb (car others) (cdr others)))]
	   [else (begin
		   (combine-parts cb ob ans)
		   #t)]))))))

;------------------( helpers for looker-codelet )----------------------------
; Take a joint list (car of a part) and collapse it to
; quanta info (lose joint information).
; This funtion gets called all over the place.
(define collapse
  (lambda (part)
    (if (null? part)
	'()
	(if (list? (car part))
	    (let ((part-ln (length part)))
	      (letrec ((unlist (lambda (partls n)
				 (cond
				  ((zero? n) '())
				  (else (append (car partls)
						(unlist (cdr partls) (sub1 n))))))))
		(letrec ((ls (unlist part part-ln)))
		  (letrec ((loop (lambda (ls)
				   (cond
				    ((null? ls) '())
				    ((member? (car ls) (cdr ls)) (loop (cdr ls)))
				    (else (cons (car ls)
						(loop (cdr ls))))))))
		    (loop ls)))))
	    part))))

; Count number of on-bits (1) in a list of bits
(define count-bits
  (lambda (ls)
    (cond
      ((null? ls) 0)
      ((= (car ls) 1) (add1 (count-bits (cdr ls))))
      (else (count-bits (cdr ls))))))

; Pass all parts of size > 2 unless they have 3 tips
; Pass whiny size 2 parts depending on the total number of quanta in the
; mystery letter.  (length of part) / (all quanta)
; Parts with 3 tips need to be broken...spin breaker codelet (bound)
;   code now passes small but labeled parts (which are alone on the grid)
;   (the pacifier codelet does this actually)
; return a boolean #t if passes #f otherwise
(define inspect-part
  (lambda (part gen)
    (cond
      ((null? part) #f)
      (else
	(let* ((qls (car part))
	       (whine-factor (if (member*? '**whine part)
				 (lookup '**whine part)
				 0))
	       (nm-quanta (length (collapse qls))))  ; number of quanta
	  (if (or (> nm-quanta 2)
		  (> (length part) 2))
	      #t
	      (if (< (n-sided-die 100)
		     (* 450 (/ (length part)
			       (length *quanta-list*))))
		  #t
		  #f)))))))

; Check a part's tips to see if it is weird or not.  Weird parts spin a
; breaker and return #t.  Parts are only allowed to have 2 tips at this
; point.  This will not work for some style-filled letters!
; JAR tosses parts with closure and two tips

(define weird-check-part
  (lambda (part gen)
    (cond
     [(null? part) #f]
     [else (let* ([pre-qls (car part)]
		  [jls (if (list? (car pre-qls)) ; handle the stupid
			   pre-qls               ; joints problem
			   (list pre-qls))]
		  [qls (collapse jls)]
		  [nm-quanta (length qls)]
		  [closure (closure-in-shape? qls)]
		  [nm-tips (length (quanta-real-tips qls))])
	     (cond
	      [(or (> nm-tips 2)
		   (not (linearizable? qls)))
	       (codemsg "   Strangely shaped...posting breaker.~%")
	       (add-to-coderack (breaker-codelet part)
				'breaker *high-urgency* (add1 gen))
	       #t]
	      [else #f]))])))

; --------------- helpers for the top-glommer-codelet ---------------------
; Glom a part probabilistically to its worst neighbor
; Probabilistically pick a neighboring part to glom to based on
; all neighboring parts' goodness values.  Glom most of the time to
; the worst part.
(define pick-worst-neighbor
  (lambda (part)
    (let* ([qls (collapse (car part))]
	   [wrk (remq part *workspace*)]
	   [nws (remove-non-neighbors wrk qls)]
	   [pls (invert-probabilities-value-list
		    (part-goodness-list nws))])
      (if (not (null? pls))
	  (probability-pick-value-list pls)
	  pls))))

(define invert-probabilities-value-list
  (lambda (ls)
    (cond
     [(null? ls) '()]
     [else (cons (list (caar ls)
		       (- 100 (cadar ls)))
		 (invert-probabilities-value-list (cdr ls)))])))

; Pick probabalistically from a list of the form
;((thing VAL)(thing VAL)...)
; pick highest VAL most in roulette-wheel style (similar to codelet pick)
(define probability-pick-value-list
  (lambda (ls)
    (letrec ([remove-empties (lambda (ls)
			       (cond
				[(null? ls) '()]
				[(equal? (car ls) '(() 110))
				 (remove-empties (cdr ls))]
				[else (cons (car ls)
					    (remove-empties (cdr ls)))]))]
	     [pick-one (lambda (sum roll ls)
			 (cond
			  [(null? ls) (error 'probability-pick-value-list
					     "nothing picked~%")]
			  [(>= (+ sum (cadar ls))
			      roll) (caar ls)]
			  [else (pick-one (+ sum (cadar ls))
					  roll (cdr ls))]))])
      (let* ([nls (remove-empties ls)]
	     [sum (apply + (map (lambda (x) (cadr x)) nls))]
	     [roll (if (not (zero? sum))
		       (n-sided-die sum))])
	(if (zero? sum)
	    (caar ls)
	    (pick-one 0 roll nls))))))

; Return a list of ((<jls> <goodness>) ...) pairs
; Goodness functions are in coderack.ss
(define part-goodness-list
  (lambda (w)
    (cond
     [(null? w) '()]
     [else (cons
	    (list (car w) (part-goodness (caar w)))
	    (part-goodness-list (cdr w)))])))

;----------------( helpers for the breaker codelet )------------------------
; Create a joints-list from the old *joints* data (plus 0 in case no glue)
; sort the list so the smallest are first --- cdr down, breaking those
; until two parts are left.  Return the parts (unmodified with **whine).
; (break all joints that have no glue)
;
; problem with a single joint list (i.e. (8 9)) fixed
(define reglue-sort-and-crack
  (lambda (part-ls)
    ; delete all non-joints from the part list
    (let* ([joints (if (not (list? (car part-ls)))
		       (remove-singles (list part-ls))
		       (remove-singles part-ls))]
	   [pre-jls (map (lambda (joint)
			   (if (member*? joint *joints*)
			       (list joint (lookup joint *joints*))
			       (list joint (n-sided-die 100))))
			 joints)]
	   [jls (if (= (length pre-jls) 1)
		    pre-jls
		    (sort (lambda (i1 i2)
			    (< (cadr i1)
			       (cadr i2))) pre-jls))])
      (letrec ([break-loop (lambda (new-jls)
			     (let* ([value (if (not (null? new-jls))
					       (cadar new-jls)
					       1)]
				    [parts (check-singles-after-break
					    (make-part-parts
					     (erase-glue new-jls))
					    (collapse part-ls))]
				    [n-parts (length parts)])
			       (if (and (not (zero? value))
					(>= n-parts 2))
				   parts
				   (break-loop (cdr new-jls)))))])
	(if (null? jls)
	    (list part-ls)
	    (break-loop (cdr jls)))))))

; Get rid of "joints" that live alone
(define remove-singles
  (lambda (jls)
    (cond
     [(null? jls) '()]
     [(= 1 (length (car jls))) (remove-singles (cdr jls))]
     [else (cons (car jls)
		 (remove-singles (cdr jls)))])))

; Make lists of joints which preserve connectivity info.
(define make-part-parts
  (lambda (u-j-list)
    (let ((j-list (sort q-listsortkey u-j-list)))
      (merge-parts j-list))))

; Make sure that single quanta all alone are counted as parts after a break.
(define check-singles-after-break
  (lambda (pts qls)
    (letrec ((loop
	      (lambda (pts qls)
		(cond
		 ((null? qls) '())
		 ((not (member*? (car qls) pts))
		  (cons (list (car qls))
			(loop pts (cdr qls))))
		 (else (loop pts (cdr qls)))))))
      (append (loop pts qls) pts))))	  

(set! *noisy-coderack* #f)

; print codelet messages to scheme only if the flag is up! (see tools.ss)
(define codemsg
  (ifprint-proto *noisy-coderack*))
