; --------------------------------------------------------------------
; GLOBAL VARIABLES, DRAFTING CALLS
; --------------------------------------------------------------------

(set! *busy-draft* #f)

(set! *draft-dimensions*
      '(height width weight l-edge r-edge floor roof))

(set! *of-each* 15)

(set! *style-knob* 1.0)

(set! *tip-randomness* 2.0) ; 6.25 was the old value, lower is more random

(define draft-init
  (lambda ()
    (let
	([type (lookup 'topology (eval *the-role*))])
      (case type
	[segment (segment-draft-init)]
	[bisegment (bisegment-draft-init)]
	[else (loop-dot-draft-init)]))))

(define general-draft-init
  (lambda ()
    (let
	([type (lookup 'topology (eval *the-role*))])
      (if *busy-draft*
	  (token-place (get-category *the-whole*) *other-stuff* "MistyRose"))
      (set! *punish* -5000) ; was 17
      (set! *punish-hard* -6000)
      (set! *quit-consider* 0)
      (set! *quit-drawing* #f)
      (set! *norms* (get-norms (eval *the-role*)))
      (set! *abstract-rules* (sp-sample '*abstract-rules*))
      (set! *literal-motifs* (sp-sample '*literal-motifs*))
      (set! *translate-motifs* (sp-sample '*translate-motifs*))
      (set! *turn-180-motifs* (sp-sample '*turn-180-motifs*))
      (set! *turn-90-motifs* (sp-sample '*turn-90-motifs*))
      (set! *turn-45-motifs* (sp-sample '*turn-45-motifs*))
      (set! *val-to-val-nvs* (sp-sample '*val-to-val-nvs*))
      (set! *relative-nvs* (sp-sample '*relative-nvs*))
      (set! *role-traits* (role-traits))
      (clear-sublist-cache)
      (set! *draft-mode*
	    (case type
	      [(segment bisegment) 'normal]
	      [loop 'loop-begin]
	      [dot 'dot]))
      ; touching -- handle the cases
      (set! *touch-norms* (touch-norms *the-whole* *the-role*))
      (set! *touch-norm* (find-max (touch-norms *the-whole* *the-role*)))
      (set! *other-touch*
	    (find-max (remove-item *touch-norm* *touch-norms*)))
      (set! *touch-norm-scores* (touch-norm-scores *touch-norm*))
      (set! *other-touch-scores*
	    (touch-norm-scores *other-touch*))
      (case *draft-mode*
	[normal
	 (begin
	   (set! *mid-touch* (eq? 1 (cadr *touch-norm-scores*)))
	   (set! *t2-touch* (eq? 1 (caddr *touch-norm-scores*)))
	   (set! *alt-mid-touch* (eq? 1 (cadr *other-touch-scores*)))
	   (set! *alt-t2-touch* (eq? 1 (caddr *other-touch-scores*))))]
	[loop-begin
	 (begin
	   (set! *mid-touch* (eq? 1 (cadr *touch-norm-scores*)))
	   (set! *t2-touch* (eq? 1 (caddr *touch-norm-scores*)))
	   (set! *alt-mid-touch* (eq? 1 (cadr *other-touch-scores*)))
	   (set! *alt-t2-touch* (eq? 1 (caddr *other-touch-scores*))))]
	; here's the rub: circle can require touch on either side
	[dot
	 (begin
	   (set! *mid-touch* #f)
	   (set! *t2-touch* #f)
	   (set! *alt-mid-touch* #f)
	   (set! *alt-t2-touch* #f))]))))

(define segment-draft-init
  (lambda ()
    (general-draft-init)
    (set! *flip-flag* (cointoss))
    (set! *tip-1* (weight-to-n-roulette (role-tips-1) *tip-randomness*))
    (set! *tip-2s* (remove-key *tip-1* (role-tips-2)))
    (set! *tip-2* (weight-to-n-roulette *tip-2s* *tip-randomness*))
    (if (eq? 'segment (lookup 'topology (eval *the-role*)))
	(set! *role-curve* (role-curve)))
    (set! *norm-curve* (norm-curve))
    (if *flip-flag* (segment-flip))
    (after-tip-draft-init)))

(define segment-flip
  (lambda ()
    (let
	([old-tip-1 *tip-1*])
      (set! *tip-1* (weight-to-n-roulette (role-tips-2) *tip-randomness*))
      (set! *tip-2s* (remove-key *tip-1* (role-tips-1)))
      (set! *tip-2* old-tip-1)
      (set! *norm-curve* (car (tip-flip (list *norm-curve*)))))))

(define take-step
  (lambda ()
    (step-init)
    (draft-run)
    (set! *all-codelets* (+ *all-codelets* *codelets-run*))))

(define draft-step
  (lambda ()
    (if (not *quit-drawing*); (printf "~%")
	(begin
	  (take-step)
	  (draft-step)))))

(define bisegment-draft-step
  (lambda ()
    (if (not *quit-drawing*); (printf "~%")
	(begin
	  (take-step)
	  (if (or
	       (<= (points-dist *current-point* *actual-tip-2*)
		   (points-dist *current-point* *tip-1*))
	       (eq? *current-point* *midpoint*))
	      (begin
		(set! *quit-drawing* #f)
		(set! *tip-1* *tip-2*)
		(set! *tip-2* *actual-tip-2*)
		(if *flip-flag*
		    (set! *norm-curve* (car (tip-flip (norm-curve))))
		    (set! *norm-curve* (cadr (norm-curve))))
		(draft-step))
	      (bisegment-draft-step))))))

(define bisegment-draft-init
  (lambda ()
    (segment-draft-init)
    (set! *midpoint* (lookup 'midpoint *norms*))
    (set! *midpoints* (halo-points *midpoint*))
    (set! *actual-tip-2* *tip-2*) ; pretend midpoint is tip-2 for now
    (set! *tip-2* *midpoint*)
    (if *flip-flag*
	(set! *norm-curve* (cadr (tip-flip (norm-curve))))
	(set! *norm-curve* (car (norm-curve))))))

(define bisegment-draft
  (lambda ()
    (bisegment-draft-init)
    (bisegment-draft-step)))

(define do-draft
  (lambda ()
    (segment-draft-init)
    (draft-step)))

(define after-tip-draft-init
  (lambda ()
    (set! *quanta-list* '())
    (set! *last-point* 'none)
    (set! *current-point* *tip-1*)
    (set! *own-stuff* '())))

(define step-init
  (lambda ()
    (case *draft-mode*
      [loop-finish
	(set! *candidates*
	      (if (member? *tip-2* (point-neighbors *current-point*))
		  (candidates-from-points
		   *current-point*
		   (cons *tip-2* (points-from-point *current-point*)))
		  (candidates)))]
      [loop-begin
       (set! *candidates*
	     (candidates-from-points
	      *current-point*
	      (points-from-point *current-point*)))]       
      [dot
       (set! *candidates*
	      (if (member? *tip-1* (point-neighbors *current-point*))
		  (candidates-from-points
		   *current-point*
		   (cons *tip-1* (points-from-point *current-point*)))
		  (candidates)))]
      [else (set! *candidates* (candidates))])
    (map quantum-remove-candidate *other-stuff*)
    (map quantum-remove-candidate *own-stuff*)
    (init-choices)
    (set! *workspace* (map clear-scoreboard-item (candidates)))
    (set! *coderack* '())
    (set! *quanta-list* '())
    (set! *codelets-run* 0)
    (post-breadcrumbs *of-each*)
    (post-touchings *of-each*)
    (post-homings *of-each*)
    (post-motif-rewarders (* 6 *style-knob* *of-each*))
    (post-trait-progress-rewards *of-each*)
    (post-trait-met-rewards *of-each*)
    (post-finishers *of-each*)
    (post-rule-enforcers (* *style-knob* *of-each*))))

(define loop-dot-draft-init
  (lambda ()
    (general-draft-init)
    (if (eq? (lookup 'topology (eval *the-role*)) 'dot)
	(begin
	  (set! *norm-curve* '*straight*)
	  (set! *mid-touch* #f)
	  (set! *t2-touch* #f)
	  (set! *alt-mid-touch* #f)
	  (set! *alt-t2-touch* #f)
	  (set! *tip-1*
		(weight-to-n-roulette (dot-start-points) *tip-randomness*))
	  (set! *tip-2s* (remove-key *tip-1* (dot-last-points)))

	  (set! *tip-2* (weight-to-n-roulette *tip-2s* *tip-randomness*)))
	(begin
	  (set! *norm-curve* '*strong-left*)
	  (set! *mid-touch* #f)
	  (set! *t2-touch* #t)
	  (set! *alt-mid-touch* #f)
	  (set! *alt-t2-touch* #f)
	  (set! *tip-1*
		(weight-to-n-roulette (loop-start-points) *tip-randomness*))
	  (set! *tip-2s* (remove-key *tip-1* (loop-last-points)))
	  (set! *tip-2* (weight-to-n-roulette *tip-2s* *tip-randomness*))))
    (after-tip-draft-init)))

(define loop-draft-step
  (lambda ()
    (if (not (or *quit-drawing* (eq? *current-point* *tip-2*))); (printf "~%")
	(begin
	  (take-step)
	  (if (eq? *current-point* *tip-2*)
	      (begin
		(set! *quit-drawing* #f)
		(let
		    ([temp *tip-1*])
		  (set! *tip-2* *tip-1*)
		  (set! *tip-2s* (loop-start-points))
		  (set! *tip-1* *current-point*))
		(set! *touch-points* (cons (list *tip-2*) *touch-points*))
		(set! *draft-mode* 'loop-finish)
		(loop-finish-draft-step))
	      (loop-draft-step))))))

(define dot-draft-step
  (lambda ()
    (if (not (or *quit-drawing* (eq? *current-point* *tip-2*))); (printf "~%")
	(begin
	  (take-step)
	  (if (eq? *current-point* *tip-2*)
	      (begin
		(set! *quit-drawing* #f))
	      (dot-draft-step))))))

(define loop-finish-draft-step
  (lambda ()
    (if (not (or *quit-drawing* (eq? *current-point* *tip-2*))); (printf "~%")
	(begin
	  (take-step)
	  (if (not (eq? *current-point* *tip-2*))
	      (set! *quit-consider* 0))
	  (loop-finish-draft-step)))))

(define loop-draft
  (lambda ()
    (loop-dot-draft-init)
    (loop-draft-step)))

(define dot-draft
  (lambda ()
    (loop-dot-draft-init)
    (dot-draft-step)))

(define role-borrow-options
  (lambda (rname)
    (let
	([kin (lookup rname *role-borrow-list*)]
	 [kin-fillers
	  (lambda (cousin)
	    (lookup-many cousin (apply append (map cadddr *library*))))])
      (apply append (map kin-fillers kin)))))

(define borrow-or-draft
  (lambda (rname)
    (let
	([options
	  (role-borrow-options rname)])
      (set! *the-role* rname)
      (if (or
	   (null? options)
	   (n-percent 25))
	  (draft rname)
	  (begin
	    ; (printf "Borrowed role ")
	    (set! *own-stuff* (roulette options))
	    (record-touching-info)
	    (set! *other-stuff*
		  (append *own-stuff* *other-stuff*)) *own-stuff*)))))

(define draft
  (lambda (rname)
    (let
	([type (lookup 'topology (eval rname))])
      (set! *the-role* rname)
;      (printf "~s" rname)
      (case type
	[segment (do-draft)]
	[bisegment (bisegment-draft)]
	[loop (loop-draft)]
	[dot (dot-draft)])
      (if *flip-flag*
		(begin
		  (set! *flip-flag* #f)
		  (set! *own-stuff* (reverse *own-stuff*))))
      (record-touching-info)
      (set! *other-stuff* (append *own-stuff* *other-stuff*)) *own-stuff*)))

; candidate: of the form (point quantum compass angle)


(define points-from-point
  (lambda (pt)
    (let*
	([quanta-touching
	  (lookup pt *point-list*)]
	 [their-points
	  (apply append
		 (map quantum-get-points quanta-touching))])
    (uniquify
     (remove-item *current-point* 
		  (subtract their-points
			    (quanta-to-points *own-stuff*)))))))

; just for loop and dot
(define bias-points-scores
  (lambda (pt-scores)
    (let*
	([rule-and-motif-weights
	  (score-lists-multiply (rules-scores) (motifs-scores))]
	 [quantum-bias
	  (lambda (q)
	    (lookup-score q rule-and-motif-weights))]
	 [point-options
	  (lambda (pt)
	    (lookup pt *point-list*))]
	 [point-best-option
	  (lambda (pt)
	    (apply max (map quantum-bias (point-options pt))))]
	 [bias-index
	  (over-threshold
	   0.01
	   (map list *all-points* (map point-best-option *all-points*)))])
      (score-lists-multiply bias-index pt-scores))))

; point, quantum, compass, angle
(define candidates-from-points
  (lambda (pt pt-ls)
    (let
	([pt-to-quanta
	  (lambda (p)
	    (car (points-to-quanta (list pt p))))]
	 [pt-to-compass
	  (lambda (p)
	    (car (points-to-compass (list pt p))))]
	 [pt-to-angle
	  (lambda (p)
	    (if (eq? *last-point* 'none)
		'none
		(car (points-to-angles (list *last-point* pt p)))))])
      (map list
	   pt-ls
	   (map pt-to-quanta pt-ls)
	   (map pt-to-compass pt-ls)
	   (map pt-to-angle pt-ls)))))

; need to subtract points that take us over previous quanta
(define candidates
  (lambda ()
    (candidates-from-points
     *current-point*
     (points-from-point *current-point*))))

; --------------------------------------------------------------------
; CODELET POSTERS
; --------------------------------------------------------------------

(define post-touchings
  (lambda (n)
    (if (> n 0)
	(begin
	  (add-to-coderack
	   touching-codelet
	   'touching-codelet
	   *medium-urgency*
	   1)
	  (post-touchings (- n 1))))))

(define post-breadcrumbs
  (lambda (n)
    (if (> n 0)
	(begin
	  (add-to-coderack
	   breadcrumb-codelet
	   'breadcrumb-codelet
	   *medium-urgency*
	   1)
	  (post-breadcrumbs (- n 1))))))

(define post-finishers
  (lambda (n)
    (if (> n 0)
	(begin
	  (add-to-coderack
	   finisher-codelet
	   'finisher-codelet
	   *medium-urgency*
	   1)
	  (post-finishers (- n 1))))))

(define post-homings
  (lambda (n)
    (if (> n 0)
	(begin
	  (add-to-coderack
	   homing-codelet
	   'homing-codelet
	   *medium-urgency*
	   1)
	  (post-homings (- n 1))))))

(define post-rule-enforcers
  (lambda (n)
    (if (and
	 (> n 0)
	 (not (null? *abstract-rules*)))
	(begin
	  (add-to-coderack
	   (rule-enforcer-codelet (roulette *abstract-rules*))
	   'rule-enforcer-codelet
	   *medium-urgency*
	   1)
	  (post-rule-enforcers (- n 1))))))

(define post-trait-progress-rewards
  (lambda (n)
    (if (> n 0)
	(begin
	  (add-to-coderack
	   (trait-progress-reward-codelet (roulette *draft-dimensions*))
	   'trait-progress-reward-codelet
	   *medium-urgency*
	   1)
	  (post-trait-progress-rewards (- n 1))))))

(define post-trait-met-rewards
  (lambda (n)
    (if (> n 0)
	(begin
	  (add-to-coderack
	   (trait-met-reward-codelet (roulette *draft-dimensions*))
	   'trait-met-reward-codelet
	   *medium-urgency*
	   1)
	  (post-trait-met-rewards (- n 1))))))

(define post-motif-rewarders
  (lambda (n)
    (if (and
	 (> n 0)
	 (not (null? *motif-lists*)))
	(let
	    ([mtf-type (roulette *motif-lists*)])
	  (if (not (null? (eval mtf-type)))
	      (let*
		  ([mtf-info (roulette (eval mtf-type))]
		   [mtf (cadr mtf-info)]
		   [tf-level (car mtf-info)])
		(add-to-coderack
		 (motif-rewarder-codelet mtf-type mtf tf-level)
		 'motif-rewarder-codelet
		 *medium-urgency*
		 1)
		(post-motif-rewarders (- n 1))))))))

(define clear-scoreboard-item
  (lambda (item)
    (list item 0)))

; --------------------------------------------------------------------
; SCORE LIST PROCESSING
; --------------------------------------------------------------------

(define score-list-collapse
  (lambda (ls)
    (if (null? ls)
	'()
	(let*
	    ([heads (uniquify (map car ls))]
	     [head-total
	      (lambda (head)
		(apply + (lookup-many head ls)))])
	  (map list
	       heads
	       (map head-total heads))))))

(define score-list-max-collapse
  (lambda (ls)
    (if (null? ls)
	'()
	(let*
	    ([heads (uniquify (map car ls))]
	     [head-total
	      (lambda (head)
		(apply max (lookup-many head ls)))])
	  (map list
	       heads
	       (map head-total heads))))))

(define score-lists-multiply
  (lambda (ls1 ls2)
    (if (null? ls1)
	ls2
	(if (null? ls2)
	    ls1
	    (let*
		([all-heads (uniquify (append (map car ls1) (map car ls2)))]
		 [head-score
		  (lambda (head)
		    (round-3 (* (lookup-score head ls1)
				(lookup-score head ls2))))])
	      (map list
		   all-heads
		   (map head-score all-heads)))))))

(define score-lists-max
  (lambda (ls1 ls2)
    (if (null? ls1)
	'()
	(let*
	    ([all-heads (uniquify (append (map car ls1) (map car ls2)))]
	     [head-score
	      (lambda (head)
		(round-3 (max (lookup-score head ls1)
			      (lookup-score head ls2))))])
	  (map list
	       all-heads
	       (map head-score all-heads))))))

(define score-lists-add
  (lambda (ls1 ls2)
    (if (null? ls1)
	'()
	(let*
	    ([all-heads (uniquify (append (map car ls1) (map car ls2)))]
	     [head-score
	      (lambda (head)
		(+ (lookup-score head ls1) (lookup-score head ls2)))])
	  (map list
	       all-heads
	       (map head-score all-heads))))))

(define score-lists-maximum
  (lambda (ls-ls)
    (cond
     [(null? ls-ls) '()]
     [(eq? (length ls-ls) 1) (car ls-ls)]
     [(eq? (length ls-ls) 2)
      (score-lists-max (car ls-ls) (cadr ls-ls))]
     [else
      (score-lists-max (car ls-ls)
			    (score-lists-maximum (cdr ls-ls)))])))

(define score-lists-product
  (lambda (ls-ls)
    (cond
     [(null? ls-ls) '()]
     [(eq? (length ls-ls) 1) (car ls-ls)]
     [(eq? (length ls-ls) 2)
      (score-lists-multiply (car ls-ls) (cadr ls-ls))]
     [else
      (score-lists-multiply (car ls-ls)
			    (score-lists-product (cdr ls-ls)))])))

(define score-lists-sum
  (lambda (ls-ls)
    (cond
     [(null? ls-ls) '()]
     [(eq? (length ls-ls) 1) (car ls-ls)]
     [(eq? (length ls-ls) 2)
      (score-lists-add (car ls-ls) (cadr ls-ls))]
     [else
      (score-lists-add (car ls-ls)
		       (score-lists-sum (cdr ls-ls)))])))

; --------------------------------------------------------------------
; TIPS -- RULES
; --------------------------------------------------------------------

(set! *rule-penalty* -10)

(define rule-punishment
  (lambda (rule-enforcement)
    (let*
	([rtype (cadr rule-enforcement)]
	 [level (car rule-enforcement)]
	 [penalty
	  (lambda (q) (list q (* *rule-penalty*
				 (round-3 (/ (level-score level) 6)))))]
	 [victims (rule-victims rtype)])
      (if (null? victims)
	  '()
	  (map penalty victims)))))

(define rules-punishment
  (lambda (enforcement-list)
    (if (null? enforcement-list)
	'()
	(score-lists-add
	 (rule-punishment (car enforcement-list))
	 (rules-punishment (cdr enforcement-list))))))

(define rule-scores
  (lambda (rule-info)
    (let*
	([victims (rule-victims (cadr rule-info))]
	 [score (round-3
		 (* -0.01
		    (+
		     1
		     (draft-level-score
		      '*abstract-rules*
		      (car rule-info)
		      'a))))]
	 [scores (n-copies score (length victims))])
      (score-lists-add
       (map list victims scores)
       (map list *all-quanta* (n-copies 1 56))))))

(define rules-scores
  (lambda ()
    (let
	([ls-ls (remove-item '() (map rule-scores *abstract-rules*))])
      (score-lists-product ls-ls))))

; --------------------------------------------------------------------
; TIPS -- MOTIFS
; --------------------------------------------------------------------

(define motif-scores
  (lambda (motif-info)
    (let*
	([rewarded (cadr motif-info)]
	 [score (round-3 (* 40.0
			    (draft-level-score
			     '*literal-motifs* (car motif-info) '(a))))]
	 [scores (n-copies score (length rewarded))])
      (score-lists-add
       (map list rewarded scores)
       (map list *all-quanta* (n-copies 1 56))))))

(define motifs-scores
  (lambda ()
    (let
	([ls-ls (remove-item '() (map motif-scores *literal-motifs*))])
      (score-lists-sum ls-ls))))

; --------------------------------------------------------------------
; TIPS -- NVS
; --------------------------------------------------------------------

(define tip-nv
  (lambda (nv-info)
    (if (eq? (caadr nv-info) 'tips)
	(list (car nv-info) (cadadr nv-info))
	'())))

; set up canonical nv lists at beginning?
(define relative-tip-nvs
    (lambda ()
      (remove-item '() (map tip-nv *relative-nvs*))))

(define val-to-val-tip-nvs
    (lambda ()
      (remove-item '() (map tip-nv *val-to-val-nvs*))))

; types are the types of the role's tip
; relative for sp-type OK for when it's val-to-val?
(define tip-nv-match-score
  (lambda (nv-info types)
    (if (member? (caadr nv-info) types)
	(round-3 (* 6.0 (draft-level-score '*relative-nvs*
					   (car nv-info)
					   'a))))))

(define rel-tip-nvs
  (lambda (pt violation)
    (case violation
      [*no-move* (list pt)]
      [(*n* *ne* *e* *se* *s* *sw* *w* *nw*)
       (list (point-vector-point pt violation))]
      [*inward* (shift-inward pt)]
      [*outward* (shift-outward pt)]
      [else (list pt)]))) ; MUST handle inward and outward!!!

(define shift-inward
  (lambda (pt)
    (let*
	([vector (car (points-to-compass (list pt 11)))]
	 [new-vectors
	  (case vector
	    [*ne* '(*e* *ne* *n*)]
	    [*se* '(*e* *se* *s*)]
	    [*nw* '(*w* *nw* *n*)]
	    [*sw* '(*w* *sw* *s*)]
	    [else (list vector)])]
	 [shift
	  (lambda (v)
	    (point-vector-point pt v))])
	 (if (eq? pt 11)
	     '(11)
	     (map shift new-vectors)))))

(define shift-outward
  (lambda (pt)
    (let*
	([vector (car (points-to-compass (list pt 11)))]
	 [new-vectors
	  (case vector
	    [*ne* '(*e* *ne* *n*)]
	    [*se* '(*e* *se* *s*)]
	    [*nw* '(*w* *nw* *n*)]
	    [*sw* '(*w* *sw* *s*)]
	    [else (list vector)])]
	 [shift
	  (lambda (v)
	    (point-vector-point pt v))])
	 (if (eq? pt 11)
	     '(11)
	     (map shift new-vectors)))))

; nv-info of the form: (tf-levelname (type violation))
(define rel-nv-scores
  (lambda (norm types nv-info)
    (let
	([shifts (rel-tip-nvs norm (cadadr nv-info))])
      (map list
	   shifts
	   (n-copies (tip-nv-match-score nv-info types) (length shifts))))))

(define rel-nvs-scores
  (lambda (norm types)
    (let
	([nv-scores
	  (lambda (nv-info)
	    (if (member? (caadr nv-info) types)
		(rel-nv-scores norm types nv-info)
		'()))])
      (score-list-collapse
       (apply append
	      (map nv-scores
		   (remove-item '() (map tip-nv *relative-nvs*))))))))

; nv-info of the form: (tf-levelname (type violation))
(define vtv-nv-scores
  (lambda (norm types nv-info)
    (let*
	([val-to-val (cadadr nv-info)]
	 [shift (if (eq? (car val-to-val) norm)
		    (cadr val-to-val)
		    '())])
      (if (null? shift)
	  '()
	  (list (list (list shift (tip-nv-match-score nv-info types))))))))

(define vtv-nvs-scores
  (lambda (norm types)
    (let
	([nv-scores
	  (lambda (nv-info)
	    (if (member? (caadr nv-info) types)
		(vtv-nv-scores norm types nv-info)
		'()))])
      (score-list-collapse
       (apply append (apply append
	      (map nv-scores
		   (remove-item '()
				(map tip-nv *val-to-val-nvs*)))))))))

; --------------------------------------------------------------------
; TIPS -- GENERAL
; --------------------------------------------------------------------

(define end-to-tip
  (lambda (endinfo)
    (let*
	([end (caar endinfo)]
	 [compass (cadar endinfo)]
	 [points (quantum-get-points end)])
      (list
       (case compass
	 [(*n* *sw* *w* *nw*) (car points)]
	 [else (cadr points)])
       (cadr endinfo)))))

(define tip-candidates
  (lambda (tip-info end-info)
    (let*
	([end-tips (score-list-max-collapse
		    (map end-to-tip end-info))]
	 [raw-scored (score-lists-multiply tip-info end-tips)]
	 [best (max 0.0000000001 (apply max (map cadr raw-scored)))])
      (rescale raw-scored (/ 400.0 best)))))

; biases end data for abstract rules and motifs
(define end-info-bias
  (lambda (end-info)
    (let*
	([rule-and-motif-weights
	  (score-lists-multiply (rules-scores) (motifs-scores))]
	 [quantum-compass-score
	  (lambda (quantum-compass)
	    (* (lookup-score quantum-compass end-info)
	       (lookup-score (car quantum-compass) rule-and-motif-weights)))]
	 [quantum-compasses (map car end-info)]
	 [real-thing
	  (map list
	       quantum-compasses
	       (map quantum-compass-score quantum-compasses))]
	 [quanta-norms
	  (map list
	       (map caar end-info)
	       (map cadr end-info))]
	 [biased-norms
	  (score-lists-multiply
	   rule-and-motif-weights
	   quanta-norms)]
	 [new-item
	  (lambda (head)
	    (list (car head) (lookup (caar head) biased-norms)))])
      (remove-keys *other-stuff*
		   (remove-index-small real-thing 0.001)))))

; biases tip data for norm violations
(define tip-info-bias
  (lambda (tip-info which-tip)
    (let*
	([the-norm (find-max tip-info)]
	 [types (tip-types *the-role* which-tip)]
	 [nv-weights
	  (remove-key
	   'off-grid
	   (score-list-collapse
	    (append
	     (vtv-nvs-scores the-norm types)
	     (rel-nvs-scores the-norm types))))]
	 [style-weights
	  (score-lists-multiply (rules-scores) (motifs-scores))]
	 [biased-norms
	  (remove-index-small
	   (score-lists-multiply
	    style-weights
	    tip-info) 0.001)]
	 [new-item
	  (lambda (head)
	    (list head (lookup head biased-norms)))])
      (score-list-collapse
       (append nv-weights biased-norms)))))

(define role-tips-1
  (lambda ()
    (let*
	([tip-ls (score-lists-multiply
		  (cdaar (lookup-list 'tips *norms*))
		  (tip-touch-scores 1))]
	 [end-ls (end-info-bias (car (lookup-list 'ends *norms*)))])
      (tip-candidates tip-ls end-ls))))

(define role-tips-2
  (lambda ()
    (let*
	([tip-ls (score-lists-multiply
		  (cdaadr (lookup-list 'tips *norms*))
		  (tip-touch-scores 2))]
	 [end-ls (end-info-bias (cadr (lookup-list 'ends *norms*)))])
      (tip-candidates tip-ls end-ls))))

(define scale-to-best
  (lambda (index)
    (let
	([best (+ 0.01 (apply max (map cadr index)))])
      (rescale index (/ 400.0 best)))))

(define loop-start-points
  (lambda ()
    (scale-to-best
     (score-lists-multiply
      (bias-points-scores (lookup-list 'first-point *norms*))
      (tip-touch-scores 1)))))

(define loop-last-points
  (lambda ()
    (scale-to-best
     (score-lists-multiply
      (bias-points-scores (lookup-list 'last-point *norms*))
      (tip-touch-scores 2)))))

(define dot-start-points
  (lambda ()
    (scale-to-best
     (score-lists-multiply
      (lookup-list 'first-point *norms*)
      (tip-touch-scores 1)))))

(define dot-last-points
  (lambda ()
    (scale-to-best
     (score-lists-multiply
      (lookup-list 'last-point *norms*)
      (tip-touch-scores 2)))))
