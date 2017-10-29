;===========================================================================
; labeler.ss : labeling module for recognition section
;===========================================================================
; globals: *parts* list of parts in quanta-pair form (unlabeled)
;---------------------------------------------------------------------------
; Here is where a coderack is first used.  The *workspace* consists of
; the *parts* all labeled with some whinyness.  Codelets run around
; attempting to label parts (see codelets.ss) and maybe joining especially
; whiny parts.

; number of labeler codelets to spin (by looker)
(set! *labelers* 10)

; The workspace will have the form of a list of joints followed by
; a list of labels.  In the beginning the label (**whine 20) is attached
; to each of the parts.  A part whines until it has been looked over
; and "passed" by a looker-codelet.

(define set-up-workspace
  (lambda ()
    (let ((pls *parts*))
      (letrec ((attach-whine (lambda (part)
			       (snoc '(**whine 20)
				     (list part))))
	       (loop (lambda (pls)
		       (cond
			 ((null? pls) '())
			 (else (cons (attach-whine (car pls))
				     (loop (cdr pls))))))))
	(set! *workspace* (loop pls))))))

; two lookers for each item in the workspace
; *gestalt-codelet* gestalt-codelets at low activation
(define initialize-coderack
  (lambda ()
    (add-n-to-coderack *gestalt-codelets*
		       gestalt-codelet
		       'gestalt
		       *very-high-urgency*
		       1)
    (let ((ctr (length *workspace*)))
      (letrec ((loop (lambda (n)
		       (cond
			 ((= n 0) #t)
			 (else 
			   (add-to-coderack
			    looker-codelet 'looker *medium-urgency* 3)
			   (loop (sub1 n)))))))
	(loop (* 2 ctr))))))

(define label-parts
  (lambda ()
    (if *graphics* (draw-info-bar "recognizing..."))
    (set-up-workspace)
    ; sun graphics
    (if *graphics*
	(begin (draw-subgloms (make-display-list *workspace*))
	       (if (not *draw-codelets*)
		   (draw-no-codelets)
		   (draw-codelet-message-back))))
    (set! *coderack* '())
    (initialize-coderack)
    (smart-parse) ; dangerous stuff!!! Added 12/9/98
    ; JAR added new bonding to help with parsing (helps?)
    (set! *joints* '())
    (set! *redund-ants* 0)
    (bond-quanta)
    (run-codelets)))

;----------------------( labeling functions )--------------------------------

; add some to the whine label of a part - direct side-effect to *workspace*
(set! *whine-constant* 10)

; Add whine-constant whiny points to a given part
(define increase-whine
  (lambda (part)
    (let
	((new-whine (+ (lookup-score '**whine part) *whine-constant*)))
      (begin
	(remove-whine part)
	(add-label (car part) (list '**whine new-whine))))))

; Remove the whine label from a part
(define remove-whine
  (lambda (part)
    (set! *workspace* (cons (without-whine part)
			    (remq part *workspace*)))))

(define without-whine
  (lambda (ls)
    (cond
     ((null? ls) '())
     ((and (not (atom? (car ls)))
	   (eq? (caar ls) '**whine)) (without-whine (cdr ls)))
     (else (cons (car ls) (without-whine (cdr ls)))))))

; Add a label to a part (may be a list of the form (<label> <value>) too)
(define add-label
  (lambda (part-jls label)
    (let* ((part (get-updated-part part-jls))
	   (newpart (snoc label part))
	   (there? (member*? label part)))
      (if (not there?)
	  (begin
	    (set! *workspace* (cons newpart
				    (remq part *workspace*)))
	    (codemsg "  label ~s attached.~%" label))
	  (codemsg "  label ~s exists! >>>Fizzle<<<~%" label)))))

; Given a part's joint-list, get an updated version of the part from the
; workspace.
(define get-updated-part
  (lambda (part-jls)
    (letrec ((loop (lambda (w)
		     (cond
		       ((equal? part-jls (caar w)) (car w))
		       (else (loop (cdr w)))))))
      (loop *workspace*))))
   
; not only combine parts but update output window
(define combine-parts
  (lambda (cb ob ans)
    (let* ((j1 (car (get-updated-part (car cb))))
	   (j2 (car (get-updated-part (car ob))))
	   (label-pair '(**whine 15))
	   (newjoints (pair-list (pair-list j1 j2) ans))
	   (newpart (snoc label-pair (list newjoints))))
      (set! *workspace*
	(cons newpart
	      (remq (get-updated-part (car ob))
		    (remq (get-updated-part (car cb)) *workspace*))))
      ; sun graphics call
      (if *graphics*
	  (draw-subgloms (make-display-list *workspace*)))
      newpart)))

(define rand-label-type
  (lambda ()
    (let ([lnth (length *label-types*)])
      (list-ref *label-types* (sub1 (n-sided-die lnth))))))

(define rand-add-label
  (lambda (part gen)
    (let ([ltype (rand-label-type)])
      	(codemsg "  attempting to attach labels of type ~s~%" ltype)
	(if *graphics*
	    (draw-codelet-message
	      (format "trying to label part ~s with type ~s"
		(car part) ltype)))
	(if (and (part-exists? part)
		 (not (member*? '**whine (get-updated-part (car part)))))
	    (label-type part ltype)
	    (begin
	      (if *graphics*
		  (draw-codelet-message2
		   (format "part ~s no longer around...>>>Fizzle<<<"
			   (car part))))
	      (codemsg "  part no longer around >>>Fizzle<<<~%"))))))
