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
; possible labels in label-type are: very_short, short, medium,
; tall, very_tall
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
    (case (height qls)
      [0 (add-label cpart 'no-height)]
      [1 (add-label cpart 'very-short)]
      [2 (add-label cpart 'short)]
      [3 (add-label cpart 'medium)]
      [4 (add-label cpart 'tall)]
      [else (add-label cpart 'very-tall)])))

(define label-width
  (lambda (qls cpart)
    (case (width qls)
      [0 (add-label cpart 'skinny)]
      [1 (add-label cpart 'half-wide)]
      [else (add-label cpart 'wide)])))

;WEIGHT
; JAR's error-checking is sloppy to non-existant
(define label-weight
  (lambda (qls cpart)
    (case (length qls)
      [(1 2) (add-label cpart 'light)]
      [(3 4 5) (add-label cpart 'normal-wt)]
      [(6 7 8) (add-label cpart 'heavy)]
      [else (add-label cpart 'huge)])))

;HORIZONTAL
; add labels left, right, and middle if they exist
(define label-horizontal
  (lambda (qls cpart)
    (begin
      (if (overlap? qls *left-quanta*) (add-label cpart `left))
      (if (overlap? qls *middle-quanta*) (add-label cpart `middle))
      (if (overlap? qls *right-quanta*) (add-label cpart `right)))))

(define edge-label-horizontal
  (lambda (qls cpart)
    (let
	((lf? (overlap? qls *left-quanta*))
	 (md? (overlap? qls *middle-quanta*))
	 (rt? (overlap? qls *right-quanta*)))
      (begin
	(add-label cpart (right-edge-label lf? md? rt?))
	(add-label cpart (left-edge-label lf? md? rt?))))))

;VERTICAL
; add labels ascender, x-zone, descender, on-baseline, on-xheight
; top, bottom

; first three taken from norm-viols.ss -  REMOVE THEM FROM THERE!
(set! *vert-quanta*
      '((0 (12 13 29 42 54 30 43 55 31))
	(1 (10 11 29 42 54 30 43 55 31 26 40 52 27 41 53 28))
	(2 (8 9 26 40 52 27 41 53 28 23 38 50 24 39 51 25))
	(3 (6 7 23 38 50 24 39 51 25 20 36 48 21 37 49 22))
	(4 (4 5 20 36 48 21 37 49 22 17 34 46 18 35 47 19))
	(5 (2 3 17 34 46 18 35 47 19 14 32 44 15 33 45 16))
	(6 (0 1 14 32 44 15 33 45 16))))

(define touches-vert
 (lambda (i qls)
   (if (> (length (intersect qls (lookup i *vert-quanta*))) 0)
       (list i) nil)))

; gives the vertical values touched by the quanta list
(define quanta-verts
  (lambda (qls)
    (append (touches-vert 0 qls) (touches-vert 1 qls)
	  (touches-vert 2 qls) (touches-vert 3 qls)
	  (touches-vert 4 qls) (touches-vert 5 qls)
	  (touches-vert 6 qls))))

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
		    (0 'nt)          ; nowhere
		    (1 'm)           ; middle, just once
		    (else '2ms)))))  ; middle, more than once
	(if (parts-touch? qls (subtract *quanta-list* qls))
	    't        ; touch
	    'nt))))  ; no touch

; ---------------------------------------------------------------------

; threshold for how many labels needed to spark

; max labels segment: 16 (so length of part is 17)
; loop has 2 fewer (no tips, no ends)

(define label-threshold
  (lambda (part)
    (if (part-tips? part) 16 14)))
