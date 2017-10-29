; test rules for being broken, norms + NVs for being violated

; --------------------------------------------------------------------
; GENERAL CODE
; --------------------------------------------------------------------

; remember, motifs should be chosen by similarity to what's been seen so far
; influence of SP should depend on level, and for motifs, the match so far

(define pick-sp
  (lambda (sp-type)
    (let*
	([pick-level (weighted-roulette *favored-tf-levels*)]
	 [level-sps (car (lookup-list sp-type (eval pick-level)))])
      (if (null? level-sps)
	  '()
	  (list pick-level
		(if (motif-type? sp-type)
		    (length-roulette level-sps)
		    (roulette level-sps)))))))

(define sp-sample
  (lambda (sp-type)
    (uniquify
     (remove-item
      '()
      (repeat
       (case sp-type
	 [*relative-nvs* '(pick-sp '*relative-nvs*)]
	 [*val-to-val-nvs* '(pick-sp '*val-to-val-nvs*)]
	 [*abstract-rules* '(pick-sp '*abstract-rules*)]
	 [*literal-motifs* '(pick-sp '*literal-motifs*)]
	 [*translate-motifs* '(pick-sp '*translate-motifs*)]
	 [*turn-180-motifs* '(pick-sp '*turn-180-motifs*)]
	 [*turn-90-motifs* '(pick-sp '*turn-90-motifs*)]
	 [*turn-45-motifs* '(pick-sp '*turn-45-motifs*)])
       200)))))

; --------------------------------------------------------------------
; RULES
; --------------------------------------------------------------------

(define breaks-rule?
  (lambda (quantum quanta-so-far rtype)
    (let*
	([draft-rtype
	  (if (eq? rtype 'at-least-2)
	      'draft-at-least-2
	      rtype)]
	 [original-quanta *quanta-list*]
	 [dummy (set! *quanta-list* quanta-so-far)]
	 [pre-test (rule-type draft-rtype)])
      (begin
	(set! *quanta-list* (condcons quantum quanta-so-far))
	(let
	    ([post-test (rule-type draft-rtype)])
	  (set! *quanta-list* original-quanta)
	  (and pre-test (not post-test)))))))

(define rule-step-score
  (lambda (rule-info candidate)
    (if
	(breaks-rule? (cadr candidate)
		      (append *own-stuff* *other-stuff*)
		      (cadr rule-info))
	(* -1.0 (draft-level-score '*abstract-rules* (car rule-info) 'a))
	0.0)))


(define rule-victims
  (lambda (rtype)
    (case rtype
	[ban-topedge *topedge-zone*]
	[ban-topbox *topbox-zone*]
	[ban-ascender *ascender-zone*]
	[ban-descender *descender-zone*]
	[ban-bottombox *bottombox-zone*]
	[ban-bottomedge *bottomedge-zone*]
	[ban-rightedge *rightedge-zone*]
	[ban-leftedge *leftedge-zone*]
	[ban-vertedges *vertedges-zone*]
	[ban-vertboxes *vertboxes-zone*]
	[ban-topleft *topleft-zone*]
	[ban-topright *topright-zone*]
	[ban-bottomleft *bottomleft-zone*]
	[ban-bottomright *bottomright-zone*]
	[ban-corners *corners-zone*]
	[ban-horiz-quanta *horizontals*]
	[ban-vert-quanta *verticals*]
	[ban-foreslash-quanta *foreslashes*]
	[ban-backslash-quanta *backslashes*]
	[ban-rectilinear-quanta *rectilinears*]
	[ban-diagonal-quanta *diagonals*]
	[else '()])))

; --------------------------------------------------------------------
; NORM VIOLATIONS
; --------------------------------------------------------------------

; this quantum will lead to what, vis-a-vis this norm?
(define hypo-norm-violation
  (lambda (quantum quanta-so-far nv-type)
    (compare-nv-type
     (list
      *the-role*
      (condcons quantum quanta-so-far))
     nv-type)))

(define lookup-val
  (lambda (item ls)
    (cond
      [(null? ls) '()]
      [(equal?  (car (cadadr (car ls))) item)
       (cons (car ls) (lookup-val item (cdr ls)))]
     [else (lookup-val item (cdr ls))])))

(define lookup-rel
  (lambda (item ls)
    (cond
      [(null? ls) '()]
      [(equal? (caadr (car ls)) item)
       (cons (car ls) (lookup-rel item (cdr ls)))]
     [else (lookup-rel item (cdr ls))])))

; very embryonic version of finished product
; wanted weighted vector of norms+NVs
(define val-to-val-nvs
  (lambda (norm)
    (let*
	([vtv-nvs (lookup-val norm *val-to-val-nvs*)]
	 [violate
	  (lambda (nv)
	    (list (cadr (cadadr nv))
		  (draft-level-score '*val-to-val-nvs*
				     (car nv)
				     'a)))])
      (map violate vtv-nvs))))

(define relative-nvs
  (lambda (norm)
    (let*
	([r-nvs (lookup-rel (feature-dimension norm) *relative-nvs*)]
	 [violate
	  (lambda (nv)
	    (list (violate-norm norm (cadadr nv))
		 (draft-level-score '*relative-nvs*
				     (car nv)
				     'a)))])
      (map violate r-nvs))))

; vtvs should count more, maybe?
(define all-norm-violations
  (lambda ()
    (uniquify
     (append
      (apply append (map relative-nvs (size-traits)))
      (apply append (map val-to-val-nvs (size-traits)))
      (apply append (map relative-nvs (border-traits)))
      (apply append (map val-to-val-nvs (border-traits)))))))

; anything that comes up a lot
(define normal-norms
  (lambda (rolename)
    (over-threshold
     8
     (remove-keys '(curve tips ends shape neighborhood contact
			  curve1 curve2 midpoint first-point last-point)
		  (get-norms (eval rolename))))))

(define role-traits
  (lambda ()
    (let*
	([all-norms (map car (normal-norms *the-role*))]
	 [all-violations (all-norm-violations)]
	 [just-violations (uniquify (map car all-violations))]
	 [violation-value
	  (lambda (viol)
	    (list viol (round-3
			(apply max (lookup-many viol all-violations)))))]
	 [norm-value
	  (lambda (norm)
	    (list norm 5))])
      (append
       (map norm-value all-norms)
       (map violation-value just-violations)))))

; of course, you may want the norm violated

; handles relative NV
; must weigh this against the norm with the level
; violation should take form "more" or "less"
; curves can screw this up with *closure*
(define violate-norm
  (lambda (norm violation)
    (let*
	([dimension (eval (feature-dimension-ls norm))]
	 [norm-order (order norm dimension)]
	 [violation-order
	  (if (eq? 'less violation)
	      (max 1 (- norm-order 1))
	      (min (length dimension) (+ norm-order 1)))])
      (nth violation-order dimension))))

(define norm-score
  (lambda (feature)
    (dimension-score feature *norms*)))

(define dimension-norm
  (lambda (dimension)
    (find-max (map
	       list
	       dimension
	       (map norm-score dimension)))))

(define dimension-label
  (lambda (dimension quanta)
    (case dimension
      [height (height-label (height quanta))]
      [width (width-label (width quanta))]
      [weight (weight-label (weight-qls quanta))]
      [l-edge (apply left-edge-label (horiz-touches quanta))]
      [r-edge (apply right-edge-label (horiz-touches quanta))]
      [floor (floor-label (floor-val quanta))]
      [roof (roof-label (roof-val quanta))])))

; count the tips in there, too
(define dimension-label-anticipating
  (lambda (dimension quanta tip1 tip2)
    (case dimension
      [height (height-label (height-with-tips quanta tip1 tip2))]
      [width (width-label (width-with-tips quanta tip1 tip2))]
      [weight (weight-label (+ 1 (weight-qls quanta)))]
      [l-edge (apply left-edge-label (horiz-touches-with-tips
				      quanta tip1 tip2))]
      [r-edge (apply right-edge-label (horiz-touches-with-tips
				       quanta tip1 tip2))]
      [floor (floor-label (floor-with-tips quanta tip1 tip2))]
      [roof (roof-label (roof-with-tips quanta tip1 tip2))])))

(define border-dimension?
  (lambda (dimension)
    (member? dimension '(l-edge r-edge floor roof))))

; score for best norm value still acheivable with these quanta
; may include adding future quanta, or, not

; now gives huge reward for meeting norm, and smaller component for
; not exceeding it

; made these two based on dimension-score, which punishes bigger
; discrepancies more
; this eliminated subtle distinction of punishing border-norm-nv
; violations more than size norm-nv violations

(define trait-ahead-score
  (lambda (dimension quanta)
    (let*
	([label (dimension-label dimension quanta)]
	 [now-and-future
	  (eat-up-to-item label (eval (feature-dimension-ls label)))]
	 [label-val
	  (lambda (l)
	    (if (border-dimension? dimension)
		(dimension-score-hard l *role-traits*)
		(dimension-score l *role-traits*)))])
      (if (null? now-and-future)
	  0
	  (let
	      ([label-vals (map label-val now-and-future)])
	    (apply max label-vals))))))

(define trait-looking-ahead-score
  (lambda (dimension quanta tip1 tip2)
    (let*
	([label (dimension-label-anticipating dimension quanta tip1 tip2)]
	 [now-and-future
	  (eat-up-to-item label (eval (feature-dimension-ls label)))]
	 [label-val
	  (lambda (l)
	    (if (border-dimension? dimension)
		(dimension-score-hard l *role-traits*)
		(dimension-score l *role-traits*)))])
      (if (null? now-and-future)
	  0
	  (let
	      ([label-vals (map label-val now-and-future)])
	    (apply max label-vals))))))

(define trait-met-score
  (lambda (dimension quanta)
    (let*
	([label (dimension-label dimension quanta)]
	 [stored
	  (if (border-dimension? dimension)
	      (dimension-score-hard label *role-traits*)
	      (dimension-score label *role-traits*))])
      (if (eq? stored 0) -1 stored))))

; for norms, oughta keep "rarer" values around, because not only
; the norm and NV are possibilities

; left, right, bottom, top
(define border-traits
  (lambda ()
    (list
     (dimension-norm *left-edge-list*)
     (dimension-norm *right-edge-list*)
     (dimension-norm *floor-list*)
     (dimension-norm *roof-list*))))

(define size-traits
  (lambda ()
    (list
     (dimension-norm *heights-list*)
     (dimension-norm *widths-list*)
     (dimension-norm *weights-list*))))

(define contact-norms
  (lambda ()
    (lookup-list *whole* (lookup-list 'contact *norms*))))

; --------------------------------------------------------------------
; MOTIFS
; --------------------------------------------------------------------

(define quanta-to-motif
  (lambda (sp-type q-ls)
    (if (< (length q-ls)
	   (case sp-type
	     [*turn-45-motifs* 2]
	     [else 1]))
	'()
	(case sp-type
	  [*literal-motifs* q-ls]
	  [*turn-45-motifs* (quanta-to-angles q-ls)]
	  [else (quanta-to-compass q-ls)]))))

(define motif-bonus
  (lambda (mtf-type)
    (case mtf-type
      [*literal-motifs* 1.5]
      [*translate-motifs* 0.38]
      [*turn-180-motifs* 0.24]
      [*turn-90-motifs* 0.18]
      [else 0.13])))

; --------------------------------------------------------------------
; GENERAL sugar-dropping code
; currently keeps contributions to total separate
; code would run faster if everything totalled in two bins (continue
; and quit scores) when debugging is done
; --------------------------------------------------------------------

(define add-nth
  (lambda (add n ls)
    (let
	([old (nth n ls)]
	 [len (length ls)])
      (append (sublist 0 (- n 2) ls)
	      (list (round-3 (+ old add)))
	      (sublist n len ls)))))

(define add-nulls
  (lambda (item)
    (list item 0 0 0 0 0 0 0 0 0)))

(define init-choices
  (lambda ()
    (set! *choice-weights*
      (map add-nulls (map cadr *candidates*)))))

(define add-choice
  (lambda (candidate add n)
    (set! *choice-weights*
	  (cons (cons (cadr candidate)
		      (add-nth add n
			       (lookup-list
				(cadr candidate) *choice-weights*)))
		(remove-key (cadr candidate) *choice-weights*)))))
