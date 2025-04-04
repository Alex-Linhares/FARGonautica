; Adjudicator output of letter for storage in the Library
; This enables a letter to be removed later, and have its
; prior effect on the Thematic Focus to be minimized
(define library-entry
  (lambda ()
    (list
     (get-category *answer*)
     *fillers*
     (map list *workspace* (map eval *workspace*)))))

(define motif-type?
  (lambda (sp-type)
    (member?
     sp-type
     '(*literal-motifs*
       *translate-motifs* *turn-180-motifs*
       *turn-90-motifs* *turn-45-motifs*))))

(define nv-type?
  (lambda (sp-type)
    (member?
     sp-type
     '(*map-nvs* *compare-nvs*))))

(define sublists
  (lambda (ls)
    (if (null? ls)
	'()
	(uniquify
	 (remove-item '()
		      (append
		       (list ls)
		       (sublists (cdr ls))
		       (sublists (anticdr ls))))))))

(define proper-sublists
  (lambda (ls)
    (cdr (sublists ls))))

; will take a list of motifs and eliminate all which are
; sublists of bigger ones
(define clean-ls-ls
  (lambda (ls-ls)
    (let*
	([proper-sublists (apply append (map proper-sublists ls-ls))]
	 [all-sublists (append proper-sublists (map reverse proper-sublists))])
      (subtract ls-ls all-sublists))))

(define common-sublists
  (lambda (ls1 ls2)
    (intersect (sublists ls1) (sublists ls2))))

; only keep the lists of length greater than operand
(define as-big-as
  (lambda (n ls)
    (cond
     [(null? ls) '()]
     [(>= (length (car ls)) n) (cons (car ls)
				    (as-big-as n (cdr ls)))]
     [else (as-big-as n (cdr ls))])))

; only keep the lists of length greater than operand
(define head-as-big-as
  (lambda (n ls)
    (cond
     [(null? ls) '()]
     [(and
       (not (null? (car ls)))
       (>= (general-length (caar ls)) n))
      (cons (car ls) (head-as-big-as n (cdr ls)))]
     [else (head-as-big-as n (cdr ls))])))

(define as-many-as
  (lambda (n ls)
    (cond
     [(null? ls) '()]
     [(>= (cadar ls) n) (cons (car ls)
				    (as-many-as n (cdr ls)))]
     [else (as-many-as n (cdr ls))])))

(define show-tf
  (lambda ()
    (map eval *thematic-focus*)))

(define clear-tf
  (lambda ()
    (set! *library* '())
    (set! *workspace*
	  '(*relative-NVs*
	    *map-NVs*
	    *abstract-rules*
	    *literal-motifs*
	    *translate-motifs*
	    *turn-180-motifs*
	    *turn-90-motifs*
	    *turn-45-motifs*))
    (set! *letter-rows* '((*relative-NVs* ())
			  (*map-NVs* ())
			  (*abstract-rules* ())
			  (*literal-motifs* ())
			  (*translate-motifs* ())
			  (*turn-180-motifs* ())
			  (*turn-90-motifs* ())
			  (*turn-45-motifs* ())))
    (set! *two-time-SPs* *letter-rows*)
    (set! *occasional-SPs* *two-time-SPs*)
    (set! *common-SPs* *two-time-SPs*)
    (set! *frequent-SPs* *two-time-SPs*)
    (set! *universal-SPs* *two-time-SPs*)))

(define bridge-near-where
  (lambda (sp-type)
    (case sp-type
      [(*relative-NVs* *map-NVs*)
       (list sp-type (roulette (map car *fillers*)))]
      [else sp-type])))

(define bridge-near-choices
  (lambda (where)
    (if (atom? where)
	(eval where)
	(lookup (cadr where) (eval (car where))))))

(define bridge-far-start-what
  (lambda (sp-type levelname)
    (let
	([options (car (lookup-list sp-type (eval levelname)))])
      (if (null? options)
	  '()
	  (roulette options)))))

(define remove-null-cadr
  (lambda (ls)
    (cond
     [(null? ls) '()]
     [(or (null? (car ls))
	  (null? (cadar ls))) (remove-null-cadr (cdr ls))]
     [else (condcons (car ls) (remove-null-cadr (cdr ls)))])))

(define remove-index-small
  (lambda (ls thresh)
    (cond
     [(null? ls) '()]
     [(< (cadar ls) thresh) (remove-index-small (cdr ls) thresh)]
     [else (condcons (car ls) (remove-index-small (cdr ls) thresh))])))

(define bridge-level-search-what
  (lambda (levelname sp-type near-end)
    (let*
	([options (lookup sp-type (eval levelname))]
	 [match-far
	  (lambda (far-sp)
	    (sp-match sp-type near-end far-sp))]
	 [option-matches (map match-far options)]
	 [paired (map list options option-matches)]
	 [far-matches (remove-null-cadr paired)])
      (if (null? far-matches)
	  '()
	  (roulette far-matches)))))

; don't understand all the conditions, so figure that out!
(define bridge-highest-far-match
  (lambda (sp-type near-end all-levels-descending)
    (if (null? all-levels-descending)
	  '()
	  (let*
	      ([levelname (car all-levels-descending)]
	       [match (bridge-level-search-what levelname
						sp-type
						near-end)]
	       [this-match
		(if (null? match) '() (cons levelname match))])
	    (if (null? this-match)
		(bridge-highest-far-match
		 sp-type near-end (cdr all-levels-descending))
		this-match)))))

(define sp-match
  (lambda (type current-sp tf-sp)
    (case type
      [(*relative-nvs* *map-nvs*) (if (equal? current-sp tf-sp)
				      tf-sp
				      '())]
      [*abstract-rules* (if (equal? current-sp tf-sp)
			    tf-sp
			    '())]
      [else
       (sublists-include (motif-sublists type tf-sp)
			 current-sp)])))

(define motif-row
  (lambda (levelname sp-type)
    (cons levelname
	  (motif-expand sp-type
			(map list (lookup sp-type (eval levelname)))))))

(define motif-type-DB
  (lambda (sp-type)
    (let
	([one-row
	  (lambda (level)
	    (motif-row level sp-type))])
      (map one-row (reverse *thematic-focus*)))))

(define string-matches
  (lambda (ls1 ls2)
    (intersect (append (sublists ls1) (sublists (reverse ls1)))
	       (list (intersect ls1 ls2)))))

(define motif-size-level-search
  (lambda (level-info target-sublists size)
    (let*
 	([options (head-as-big-as size level-info)]
	 [common
	  (lambda (option)
	    (intersect target-sublists
		       (list (intersect-uniq (car target-sublists)
					(car option)))))]
	 [size-limit
	  (lambda (ls)
	    (as-big-as size ls))]
	 [success (map common options)]
	 [filtered (map list (map car options) (map size-limit success))])
     (remove-null-cadr filtered))))

(define motif-size-search
  (lambda (levels type-DB target-sublists size)
    (if (null? levels)
	'()
	(let*
	    ([top-level (car levels)]
	     [top-check (motif-size-level-search
			 (lookup-list top-level type-DB)
			 target-sublists
			 size)])
	  (if (null? top-check)
	      (motif-size-search (cdr levels) type-DB target-sublists size)
	      (cons top-level top-check))))))

(define motif-DB-match
  (lambda (type-DB target-sublists size)
    (if (<= size 0)
	'()
	(let*
	    ([all-levels (reverse *thematic-focus*)]
	     [biggest-check (motif-size-search
			     all-levels
			     type-DB
			     target-sublists
			     size)])
	  (if (null? biggest-check)
	      '()
	      biggest-check)))))

(define motif-matches
  (lambda (sp-type target)
    (motif-DB-match (motif-type-DB sp-type)
		    (motif-sublists sp-type target)
		    (length target))))

(define motif-match
  (lambda (sp-type target)
    (let
	([matches (motif-matches sp-type target)])
      (if (null? matches)
	  '()
	  (let*
	      ([where (car matches)]
	       [one-row (roulette (cdr matches))]
	       [one-match (roulette (cadr one-row))])
	    (list where (car one-row) one-match))))))

(define general-length
  (lambda (item)
    (if (atom? item) 1 (length item))))

; weight the roulette wheel by length
(define length-roulette
  (lambda (ls)
    (let
	([indexed-ls (map list ls (map general-length ls))])
      (weighted-roulette indexed-ls))))

(define sublists-include
  (lambda (sublists ls2)
    (let
	([common (intersect (car sublists) ls2)])
      (if (member? common sublists)
	  common
	  '()))))

; motif redundancy eliminated with these

(define motif-includes?
  (lambda (type m1 m2)
    (member? m2 (motif-sublists type m1))))

(define pruned-motifs
  (lambda (type mtf-ls)
    (if (null? mtf-ls)
	'()
	(let*
	    ([biggest (find-max (map list mtf-ls (map length mtf-ls)))]
	     [rest (remove-item biggest mtf-ls)]
	     [biggest-includes?
	      (lambda (little)
		(motif-includes? type biggest little))]
	     [keepers
	      (map cdr
		   (remove-key
		    #t
		    (map cons (map biggest-includes? rest) rest)))])
	  (cons biggest (pruned-motifs type keepers))))))
      

; low-level tf manipulation routines

(define remove-from-tf
  (lambda (levelname sp-type sp)
    (let*
	([level (eval levelname)]
	 [rest-of-level (remove-key sp-type level)]
	 [row-with-sp (car (lookup-list sp-type level))]
	 [row-minus-sp (remove-item sp row-with-sp)]
	 [new-row (list sp-type row-minus-sp)]
	 [new-level (cons new-row rest-of-level)])
      (set-top-level-value! levelname new-level))))

(define add-to-tf
  (lambda (levelname sp-type sp)
    (if (not (null? sp))
	(let*
	    ([level (eval levelname)]
	     [rest-of-level (remove-key sp-type level)]
	     [row-for-sp (car (lookup-list sp-type level))]
	     [row-data (condcons sp row-for-sp)]
	     [new-row (list sp-type row-data)]
	     [new-level (cons new-row rest-of-level)])
	  (set-top-level-value! levelname new-level)))))

(define level-motif-type-prune
  (lambda (levelname sp-type)
    (let*
	([level (eval levelname)]
	 [row-data (car (lookup-list sp-type level))])
      (if (not (null? row-data))
	  (let*
	      ([rest-of-level (remove-key sp-type level)]
	       [row-pruned (if (motif-type? sp-type)
			       (pruned-motifs sp-type row-data)
			       row-data)]
	       [new-row (list sp-type row-pruned)]
	       [new-level (cons new-row rest-of-level)])
	    (set-top-level-value! levelname new-level))))))

(define level-motif-prune
  (lambda (levelname)
    (let
	([type-prune
	  (lambda (type)
	    (level-motif-type-prune levelname type))])
      (map type-prune
	   '(*literal-motifs* *translate-motifs* *turn-180-motifs*
			      *turn-90-motifs* *turn-45-motifs*)))))

(define tf-motif-prune
  (lambda ()
    (let
	([level-prune
	  (lambda (levelname)
	    (level-motif-prune levelname))])
      (map level-prune *thematic-focus*))))

(define level-successor
  (lambda (levelname)
    (if (eq? levelname '*universal-sps*)
	levelname
	(cadr (eat-up-to-item levelname *thematic-focus*)))))

(define level-predecessor
  (lambda (levelname)
    (if (or (eq? levelname '*letter-rows*)
	    (eq? levelname '*two-time-SPs*))
	levelname
	(cadr (eat-up-to-item levelname (reverse *thematic-focus*))))))

(define level-shift
  (lambda (levelname n)
    (cond
     [(eq? n 0) levelname]
     [(< n 0) (level-shift (level-predecessor levelname) (+ n 1))]
     [else (level-shift (level-successor levelname) (- n 1))])))

; promotion/demotion

(define promote-in-tf
  (lambda (sp-type old-level far-sp match)
    (let
	([new-level (level-successor old-level)])
      (if (not (eq? old-level new-level))
	  (begin
	    ; we won't remove old multi-quanta motifs from early levels
	    ; they may form the basis for multiple promotions
	    (if (or
		 (member? sp-type
			  '(*relative-nvs* *map-nvs* *abstract-rules*))
		 (eq? (length match) (length far-sp))
		 (< 2 (length far-sp)))
		(remove-from-tf old-level sp-type far-sp))
	    (add-to-tf new-level sp-type match))))))

(define demote-in-tf
  (lambda (sp-type old-level far-sp)
    (let
	([new-level (level-predecessor old-level)])
      (if (not (eq? old-level new-level))
	  (begin
	    (remove-from-tf old-level sp-type far-sp)
	    (add-to-tf new-level sp-type far-sp))))))

; Adjudicator temperature manipulation
; goes here, because bridge building success and failure is
; the sole determinant of temperature

; idea: GOODNESS will be based on temperature, but terminating a run
; will be based on emptying *bridges*

; helper adjusts temperature towards goal

(set! *temp-jump* 0.001)

(define thermostat
  (lambda (aim rate)
    (let*
      ([step (* rate *temp-jump*)]
       [new-temp (+ (* (- 1 step) *temperature*) (* step aim))])
      (set! *temperature* (min 100 (max new-temp 0))))))


; salience of an SP
; returns probability with which we would credit finding the SP
; need not be precisely true

; note -- an abstract rule that's irrelevant for the current letter
; do you need to deal with that specially?

(define salience
  (lambda (sp-type sp)
    (case sp-type
      [(*relative-nvs* *map-nvs* *abstract-rules*)
       0.5]
      [*literal-motifs*
       (if (eq? 1 (length sp))
	   0.25
	   (* 0.25 (salience '*literal-motifs* (cdr sp))))]
      [*translate-motifs*
       (if (eq? 1 (length sp))
	   0.75
	   (* 0.75 (salience '*translate-motifs* (cdr sp))))]
      [*rotate-motifs*
       (if (eq? 1 (length sp))
	   0.75
	   (* 0.75 (salience '*rotate-motifs* (cdr sp))))]
      [*reflect-motifs*
       (if (eq? 1 (length sp))
	   0.875
	   (* 0.75 (salience '*reflect-motifs* (cdr sp))))])))

(define importance
  (lambda (sp-type level sp)
    (let*
	([per-sp
	  (case sp-type
	    [*abstract-rules* 1.0]
	    [(*relative-nvs* *map-nvs*) 0.5]
	    [*literal-motifs* (* 0.6 (length sp) (length sp))]
	    [*translate-motifs* (* 0.3 (length sp) (length sp))]
	    [*turn-180-motifs* (* 0.2 (length sp) (length sp))]
	    [*turn-90-motifs* (* 0.1 (length sp) (length sp))]
	    [else (* 0.05 (length sp) (length sp))])]
	 [adjusted-level-score (lookup level *adjusted-level-scores*)]
	 [per-level (/ (power 4 adjusted-level-score) 128.0)])
      (* per-sp per-level))))

(define level-score
  (lambda (levelname)
    (order levelname *thematic-focus*)))

(define set-adjusted-level-scores
  (lambda ()
    (let
	([adjusted-level-score
	  (lambda (level)
	    (+ (level-score level) (- 6 (full-levels))))])
      (set! *adjusted-level-scores*
	    (map
	     list
	     *thematic-focus*
	     (map adjusted-level-score *thematic-focus*))))))

; probability that a failure to find an SP in levelname should result
; in demotion

(define demote-prob
  (lambda (levelname sp-type)
    (*
     (case sp-type
      [*abstract-rules* 10.0]
      [else 1.0])
     (max 0 (/ (- (level-score levelname) 2.0) 4)))))

(define promote-prob
  (lambda (levelname sp-type)
    (*
     (case sp-type
      [*literal-motifs* 3.0]
      [(*relative-nvs* *abstract-rules*) 1.4]
      [*translate-motifs* 1.0]
      [*map-nvs* 0.45]
      [else 0.3])
     (max 0 (/ (- 6.0 (level-score levelname)) 5)))))

(define prob-decision?
  (lambda (prob)
    (< (/ (n-sided-die 1000) 1000.0) prob)))

(define library-fillers
  (lambda ()
    (if (null? *library*)
	'()
	(map caadr *library*))))

(define filler-reward
  (lambda (filler)
    (let
	([priors (lookup-many (car filler) (library-fillers))])
      (if (member? (cadr filler) priors)
	  (thermostat 0 50)
	  (thermostat 50 10)))))

(define level-empty
  (lambda (levelname)
    (if (null? (apply append (map cadr (eval levelname))))
	0
	1)))

; an empty level has no interest!
(define tf-level-interest
  (lambda ()
    (map list
	 (map car *favored-tf-levels*)
	 (map * (map level-empty *thematic-focus*)
	      (map cadr *favored-tf-levels*)))))

; how many levels have some stuff in them?
; we'll use this to decide how to adjust temperature for the first
; letters in a gridfont, when SPs are unlikely to find a match
(define full-levels
  (lambda ()
    (apply + (map level-empty *thematic-focus*))))
