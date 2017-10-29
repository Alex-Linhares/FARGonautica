;===========================================================================
; labels.ss : code for computing labels
; new by JAR 9/14/98
;===========================================================================
; type of labels (different code for each type.  types chosen by codelet.

(set! *label-types* '(Contact Neighbors Tips Curve Height Weight
			      Shape Ends Width Vertical Horizontal))

; topology issues

; two tips
; one tip and closure
; two tips and closure
; no tips and closure

; called by the labeler codelet which picks at random one of the types from
; *label-types*
; need to pass neighboring stuff, and not just the part
(define label-type
  (lambda (part ltype)
    (let* ([labels (cdr part)]
	   [quanta (lookup 'Quanta labels)] ; the part's quanta
	   ; these are linearized at creation in examiner.ss
	   [otherstuff (subtract *quanta-list* quanta)])
      (case ltype
	[Contact (label-contact quanta (car part))]
	[Neighbors (label-neighbors quanta otherstuff (car part))]
	[Tips (label-tips quanta (car part))]
	[Ends (label-ends quanta (car part))]
	[Curve (label-curve quanta (car part))]
	[Height (label-height quanta (car part))]
	[Weight (label-weight quanta (car part))]
	[Width (label-width quanta (car part))]
	[Shape (label-shape quanta (car part))]
	[Horizontal (edge-label-horizontal quanta (car part))]
	[Vertical (label-vertical quanta (car part))]
	[else (error 'label-type "no such label type ~s~%" ltype)]))))

;------------------( helpers for labeler-codelets )-------------------------
; see if the part (to be used) is still around.  It may have disappeared.
(define part-exists?
  (lambda (part)
    (or (memq? part *workspace*)
	(member? (car part) (map car *workspace*)))))
	
;HEIGHT
; calculate the height of a part by adding up sector counts.  There are 6
; sectors to the grid so 6 is max height.
(define height
  (lambda (qls)
    (letrec ((make-sct (lambda (n1 n2)
			 (list n1 (add1 n1) (+ 2 n1)
			   n2 (add1 n2) (+ 12 n2) (+ 13 n2))))
	     (check-one (lambda (qls sct)
			  (if (overlap*? qls sct)
			      1
			      0))))
      (letrec ((loop (lambda (num1 num2 count)
		       (let* ((sct (make-sct num1 num2))
			      (ans (check-one qls sct)))
			 (cond
			   ((= num1 32) count)
			   (else (loop (+ num1 3) (+ num2 2)
				   (+ ans count))))))))
	(loop 14 32 0)))))

;WIDTH
; calculate width in the same manner as height.	 This time there are only
; two sectors so a quick check works better.
; possible labels in label-type are: skinny, half_wide, wide
(define width
  (lambda (qls)
    (if (overlap*? qls '(0 2 4 6 8 10 12 32 34 36 38 40 42
			  44 46 48 50 52 54))
	(if (overlap*? qls '(1 3 5 7 9 11 13 33 35 37 39 41 43
			      45 47 49 51 53 55))
	    2 1)
	(if (overlap*? qls '(1 3 5 7 9 11 13 33 35 37 39 41 43
			      45 47 49 51 53 55))
	    1 0))))


(define label-neighbors
  (lambda (qls other cpart)
    (add-label cpart
	       (list
		'neighborhood (neighborhood-watch qls other)))))

(define label-height
  (lambda (qls cpart)
    (add-label cpart (height-label (height qls)))))

(define label-width
  (lambda (qls cpart)
    (add-label cpart (width-label (width qls)))))

;WEIGHT
(define label-weight
  (lambda (qls cpart)
    (add-label cpart (weight-label (length qls)))))

;HORIZONTAL

(define horiz-touches
  (lambda (qls)
    (list
     (overlap? qls *left-quanta*)
     (overlap? qls *middle-quanta*)
     (overlap? qls *right-quanta*))))

(define edge-label-horizontal
  (lambda (qls cpart)
    (let
	([touches-across (horiz-touches qls)])
      (begin
	(add-label cpart (apply right-edge-label touches-across))
	(add-label cpart (apply left-edge-label touches-across))))))

;VERTICAL
; add labels ascender, x-zone, descender, on-baseline, on-xheight
; top, bottom

; (apply max (quanta-verts qls)) (apply min (quanta-verts qls))

(define label-vertical
  (lambda (qls cpart)
    (let
	([roof-level (apply max (quanta-verts qls))]
	 [floor-level (apply min (quanta-verts qls))])
      (begin
	(case roof-level
	  (0 (add-label cpart 'roof-bottom))
	  (1 (add-label cpart 'roof-middown))
	  (2 (add-label cpart 'roof-baseline))
	  (3 (add-label cpart 'roof-midline))
	  (4 (add-label cpart 'roof-x-height))
	  (5 (add-label cpart 'roof-t-height))
	  (6 (add-label cpart 'roof-top)))
	(case floor-level
	  (0 (add-label cpart 'floor-bottom))
	  (1 (add-label cpart 'floor-middown))
	  (2 (add-label cpart 'floor-baseline))
	  (3 (add-label cpart 'floor-midline))
	  (4 (add-label cpart 'floor-x-height))
	  (5 (add-label cpart 'floor-t-height))
	  (6 (add-label cpart 'floor-top)))))))

(define find-tips
  (lambda (qls)
    (let
	([tips (quanta-get-tips qls)]
	 [orientation1 (tail (quanta-to-compass (reverse qls)))]
	 [orientation2 (tail (quanta-to-compass qls))])
      (list
       (list (car tips) orientation1)
       (list (cadr tips) orientation2)))))

(define label-tips
  (lambda (qls cpart)
    (if (has-tips? qls)
	(add-label cpart
		   (list 'tips (find-tips qls))))))

(define find-ends
  (lambda (qls)
    (let
	([orientation1 (tail (quanta-to-compass (reverse qls)))]
	 [orientation2 (tail (quanta-to-compass qls))])
      (list
       (list (car qls) orientation1)
       (list (car (reverse qls)) orientation2)))))

(define label-ends
  (lambda (qls cpart)
    (if (has-tips? qls)
	(add-label cpart (list 'ends (find-ends qls))))))

(define label-curve
  (lambda (qls cpart)
    (begin
      (add-label
       cpart (list
	      'curve (curve-label qls)))
      (add-label
       cpart (list
	      'curve1 (curve-label (first-half qls))))
      (add-label
       cpart (list
	      'curve2 (curve-label (second-half qls)))))))

(define label-shape
  (lambda (qls cpart)
    (add-label cpart
	       (list
		'shape (shape-label qls)))))

; contact
; only go into detail if there are tips - otherwise, just touch/no-touch

(define label-contact
  (lambda (qls cpart)
    (add-label cpart
	       (list 'contact (contact-label qls)))))

; MUST BE IN ORDER!
(define contact-label
  (lambda (qls)
    (if (has-tips? qls)
	(let*
	    ([other (subtract *quanta-list* qls)]
	     [touching (touching-pattern qls other)]
	     [tip1? (car touching)]
	     [middle (cadr touching)]
	     [tip2? (caddr touching)])
	  (if tip1?
	      (if (> middle 0)
		  (if tip2?
		      'all-over      ; both tips and middle
		      't1m)          ; tip1 and middle
		  (if tip2?
		      '2ts           ; both tips, not middle
		      't1))          ; tip1 only
	      (if tip2?
		  (if (> middle 0)
		      'mt2           ; middle and tip2
		      't2)           ; tip2 only
		  (case middle
		    (0 'nc)          ; nowhere
		    (1 'm)           ; middle, just once
		    (else '2ms)))))  ; middle, more than once
	(let*
	    ([my-points (quanta-to-points qls)]
	     [their-quanta (subtract *quanta-list* qls)]
	     [their-points (quanta-to-points their-quanta)]
	     [touching-points (intersect my-points their-points)])
	  (if (null? touching-points)
	      'nc ; no contact
	      (if (> (car touching-points) 14)
		  'c-rt ; contact on the right
		  'c-lf)))))) ; contact on the left

; ---------------------------------------------------------------------

; threshold for how many labels needed to spark

; max labels segment: 16 (so length of part is 17)
; loop has 2 fewer (no tips, no ends)

(define label-threshold
  (lambda (part)
    (let
	([tough-standard (if (part-tips? part) 16 14)])
      (if (< *codelets-run* *exam-phase*)
	  tough-standard
	  (- tough-standard 1)))))
