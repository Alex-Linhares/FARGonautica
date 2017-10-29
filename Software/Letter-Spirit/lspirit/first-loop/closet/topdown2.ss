;=============================================================================
; topdown.ss : top down code (gestalt, o-glommer, meta-roles and guessers)
;=============================================================================

; number of gestalt codelets to post initially
; picked at random by JAR
(set! *gestalt-codelets* 1)

;[ gestalt code ]------------------------------------------------------------
; JAR version, 9/13/96
; 3 gestalts: quanta location, stuff per square, and tip location,
; are combined, and a closure constant is added in
; an index of gestalt per letter category is created
; index is made when letterform is chosen
; this code gives each whole the appropriate gestalt activation

(define set-gestalt
  (lambda (whole)
    (let*
	([new-activation
	  (min 100 (max -100
			(+ (get-activation whole)
			   (lookup-score (get-category whole)
					 *gestalt-index*))))])
      (set-activation whole new-activation))))

(define set-gestalts
  (lambda ()
    (mapcar set-gestalt *wholes*)))

(define gestalt-activate
  (lambda (gen)
    (begin
      (set-gestalts)
      (if *graphics*
	  (draw-codelet-message2
	   "  spinning activation-spreader"))
      (codemsg "  Activation-spreader codelet spun.~%")
      (add-to-coderack
       activation-spreader-codelet
       'activation-spreader *high-urgency* (add1 gen)))))
      
;----------------------------------------------------------------------------
; Gestalt Routines
; JAR, 9/13/96
; 4 kinds of gestalts combined into one measure
; approximately (quanta-closeness * square-closeness * tip-closeness)
;  + closure-agreement
; on 11/5/96, JAR adds ascender and descender to the mix, and
; gestalt is a straight sum of the six, no weighting

; JAR's knob to twiddle, circa 10/17/96
; play around with; trials before 10/17 used 20.0
; 30.0 didn't work so well for hard letters, although better for
; easy ones; simulated annealing???

; make sure the CORRECT closure routine is used!!!
(set! *gestalt-perk* 20.0)

; return x's place on a 1-to-10 scale
(define scale-to-ten
  (lambda (x top bottom)
    (* 10.0 (/ (- x bottom) (- top bottom)))))

; scale a list to ten
(define list-helper
  (lambda (ls top bottom)
    (cond
     ((null? ls) nil)
     (else (cons
	    (scale-to-ten (car ls) top bottom)
	    (list-helper (cdr ls) top bottom))))))

; for LS gestalts, low numbers (distances) are high goodness
(define list-to-ten
  (lambda (ls)
    (list-helper ls (apply min ls) (apply max ls))))

;take off the tags from lists like ((?a 0.1) ... )
(define peel-list
  (lambda (ls)
    (cond
     ((null? ls) nil)
     (else (cons
	    (cadar ls)
	    (peel-list (cdr ls)))))))

(define property-check
  (lambda (ls prop)
    (cond
     ((null? ls) nil)
     (else
      (cons
       (cond
	(prop (* 10.0 (cadar ls)))
	(else (* 10.0 (- 1 (cadar ls)))))
       (property-check (cdr ls) prop))))))

(define convert-six
  (lambda (a b c d e f)
    (cond
     ((null? a) nil)
     (else
      (cons
       ; scales to allow values up to 20 or down to -55
	    (round-3 (* (/ *gestalt-perk* 16.0) (- (+
			(car a) (car b) (car c) (car d) (car e) (car f)) 44)))
	    (convert-six (cdr a) (cdr b) (cdr c) (cdr d) (cdr e) (cdr f)))))))

; addAll and somebodyPositive added by JAR, 4/15/97
; these make sure that at least ONE gestalt comes up above zero
; otherwise, we'll never get anything approved
(define addAll
 (lambda (ls num)
   (cond
     ((null? ls) nil)
     (else
      (cons (+ num (car ls)) (addAll (cdr ls) num))))))

(define somebodyPositive
  (lambda (ls)
    (cond
     ((< (apply max ls) 0) (addAll ls (+ 1(* -1 (apply max ls)))))
     (else ls))))
    
(define gestalt-init
  (lambda ()
    (let*
	([quanta-raw
	  (list-to-ten
	   (peel-list
	    (make-square-value-list *quanta* *blurred-prototypes*)))]
	 [square-raw
	  (list-to-ten
	   (peel-list
	    (make-square-value-list *squares* *square-prototypes*)))]
	 [tip-raw
	  (list-to-ten
	   (peel-list
	    (make-square-value-list *tip-squares* *tip-prototypes*)))]
	 [closure-raw
	  (property-check *closure-prototypes*
			  (gestalt-closure *quanta-list*))]
	 [ascender-raw
	  (property-check *ascend-prototypes*
			  (ascender-in-shape? *quanta-list*))]
	 [descender-raw
	  (property-check *descend-prototypes*
			  (descender-in-shape? *quanta-list*))])
      (set! *gestalts*
	    (convert-six
	     quanta-raw
	     square-raw
	     tip-raw
	     closure-raw
	     ascender-raw
	     descender-raw))
      (set! *gestalt-index*
	    (map list
		 alphabet
		 (n-copies 10 26))))))

(define gestalt-closure
  (lambda (qls)
    (search (make-graph qls '()))))
      
; based on stuff comparison and tips comparison
; 'g' for Gestalt
; to start things during runs

(define g-start
    (lambda ()
      (begin
	(q-to-squares)
	(tips-to-squares)
	(gestalt-init))))

; for debugging
(define g-check
    (lambda ()
      (begin
	(get-mystery)
	(q-to-squares)
	(tips-to-squares)
	(gestalt-init))))

