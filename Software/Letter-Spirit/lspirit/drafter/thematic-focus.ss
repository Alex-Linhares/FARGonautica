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
     '(*val-to-val-nvs* *compare-nvs*))))

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

; only gives the sublists that end with the final atom
(define tail-sublists
  (lambda (ls)
    (if (null? ls)
	'()
	(cons ls (tail-sublists (cdr ls))))))

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
	    *val-to-val-nvs*
	    *abstract-rules*
	    *literal-motifs*
	    *translate-motifs*
	    *turn-180-motifs*
	    *turn-90-motifs*
	    *turn-45-motifs*))
    (set! *letter-rows* '((*relative-NVs* ())
			  (*val-to-val-nvs* ())
			  (*abstract-rules* ())
			  (*literal-motifs* ())
			  (*translate-motifs* ())
			  (*turn-180-motifs* ())
			  (*turn-90-motifs* ())
			  (*turn-45-motifs* ())))
    (set! *two-time-sps* *letter-rows*)
    (set! *occasional-sps* *two-time-sps*)
    (set! *common-sps* *two-time-sps*)
    (set! *frequent-sps* *two-time-sps*)
    (set! *universal-sps* *two-time-sps*)))

(define bridge-near-where
  (lambda (sp-type)
    (case sp-type
      [(*relative-NVs* *val-to-val-nvs*)
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

(define match-far-list
  (lambda (levelname sp-type near-end mtf-ls best-so-far best-length)
    (if (null? mtf-ls)
	best-so-far
	(let*
	    ([next-match (sp-match levelname sp-type near-end (car mtf-ls))]
	     [next-match-length (general-length next-match)])
	  (if (> next-match-length best-length)
	      (match-far-list
	       levelname
	       sp-type
	       near-end 
	       (as-long-as (cdr mtf-ls) (+ 1 next-match-length))
	       next-match
	       next-match-length)
	      (match-far-list
	       levelname
	       sp-type
	       near-end 
	       (cdr mtf-ls)
	       best-so-far
	       best-length))))))

(define bridge-level-search-what
  (lambda (levelname sp-type near-end)
    (let
	([options (lookup sp-type (eval levelname))])
      (if (motif-type? sp-type)
	  (match-far-list levelname sp-type near-end options '() 0)
	  (let*
	      ([match-far
		(lambda (far-sp)
		  (sp-match levelname sp-type near-end far-sp))]
	       [option-matches (map match-far options)]
	       [paired (map list options option-matches)]
	       [far-matches (remove-null-cadr paired)])
	    (if (null? far-matches)
		'()
		(roulette far-matches)))))))


; of all motifs in the TF which match the near-end,
; returns the longest one at the highest level
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
  (lambda (levelname sp-type current-sp tf-sp)
    (if (motif-type? sp-type)
	(sublists-include (find-motif-sublists sp-type tf-sp)
			  current-sp)
	(if (equal? current-sp tf-sp)
	    tf-sp
	    '()))))

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
    (if (or (null? sublists) (null? ls2))
	'()
	(longer
	 (biggest-overlap (car sublists) ls2)
	 (sublists-include (cdr sublists) ls2)))))

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

; given a quanta list which constitutes a loop, make the lowest #
; quantum the beginning and end

(define canonicalize-loops
  (lambda (q-ls)
    (if (and
	 (> (length q-ls) 1)
	 (eq? (car q-ls) (car (reverse q-ls))))
	(let*
	    ([trim-overlap (cdr q-ls)]
	     [two-laps (append trim-overlap trim-overlap)]
	     [start (apply min q-ls)]
	     [end-trim (reverse (eat-up-to-item start
						(reverse two-laps)))]
	     [begin-trim (eat-up-to-item start end-trim)])
	  begin-trim)
	q-ls)))
		   
(define level-successor
  (lambda (levelname)
    (if (eq? levelname '*universal-sps*)
	levelname
	(cadr (eat-up-to-item levelname *thematic-focus*)))))

(define level-predecessor
  (lambda (levelname)
    (if (or (eq? levelname '*letter-rows*)
	    (eq? levelname '*two-time-sps*))
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
			  '(*relative-nvs* *val-to-val-nvs* *abstract-rules*))
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

(define importance
  (lambda (sp-type level sp)
    (let*
	([per-sp
	  (case sp-type
	    [*abstract-rules* 2.0]
	    [*val-to-val-nvs* 2.0]
	    [*relative-nvs* 0.5]
	    [*literal-motifs* (* 0.6 (length sp) (length sp))]
	    [*translate-motifs* (* 0.3 (length sp) (length sp))]
	    [*turn-180-motifs* (* 0.2 (length sp) (length sp))]
	    [*turn-90-motifs* (* 0.1 (length sp) (length sp))]
	    [else (* 0.05 (length sp) (length sp))])]
	 [adj-level-score (lookup level *adjusted-level-scores*)]
	 [per-level (/ (power 4 adj-level-score) 128.0)])
      (* per-sp per-level))))

(define importance
  (lambda (sp-type level sp)
    (let*
	([per-sp
	  (case sp-type
	    [*abstract-rules* 4.0]
	    [*val-to-val-nvs* 4.0]
	    [*relative-nvs* 0.5]
	    [*literal-motifs* (* 0.6 (length sp) (length sp))]
	    [*translate-motifs* (* 0.3 (length sp) (length sp))]
	    [*turn-180-motifs* (* 0.2 (length sp) (length sp))]
	    [*turn-90-motifs* (* 0.1 (length sp) (length sp))]
	    [else (* 0.05 (length sp) (length sp))])]
	 [adj-level-score (lookup level *adjusted-level-scores*)]
	 [per-level (/ (power 4 adj-level-score) 128.0)])
      (* per-sp (draft-level-score sp-type level sp)))))

; for when a bridge-building fails
; common should produce higher score
(define fail-importance
  (lambda (sp-type level sp)
    (let*
	([per-sp
	  (case sp-type
	    [*abstract-rules* 4.0]
	    [*val-to-val-nvs* 4.0]
	    [*relative-nvs* 0.5]
	    [*literal-motifs* (* 0.6 (length sp) (length sp))]
	    [*translate-motifs* (* 0.3 (length sp) (length sp))]
	    [*turn-180-motifs* (* 0.2 (length sp) (length sp))]
	    [*turn-90-motifs* (* 0.1 (length sp) (length sp))]
	    [else (* 0.05 (length sp) (length sp))])]
	 [adj-level-score (lookup level *adjusted-level-scores*)]
	 [per-level (/ (power 4 adj-level-score) 128.0)])
      (/ (draft-level-score sp-type level sp) per-sp))))

(define sp-prob
  (lambda (sp-type sp)
    (case sp-type
      [*abstract-rules* 0.35]
      [*val-to-val-nvs* 0.08]
      [*relative-nvs* 0.25]
      [*literal-motifs* (power 0.18 (length sp))]
      [*translate-motifs* (power 0.42 (length sp))]
      [*turn-180-motifs* (power 0.6 (length sp))]
      [*turn-90-motifs* (power 0.7 (length sp))]
      [else (power 0.9 (- (length sp) 1))])))

; mean level at which we would expect a random sp to float to
; outliers likely early on, so we assume a one-level presence
; but we subtract 0.1 to bump up and would-be ties
(define level-expect
  (lambda (sp-type sp)
    (round (+ (* (- *full-levels* 1.45) (sp-prob sp-type sp)) 1.55))))

(define draft-level-score
  (lambda (sp-type level sp)
    (max 0 (- (level-score level) (level-expect sp-type sp) -1))))

(define level-score
  (lambda (levelname)
    (order levelname *thematic-focus*)))

(define set-adjusted-level-scores
  (lambda ()
    (let
	([adj-level-score
	  (lambda (level)
	    (+ (level-score level) (- 6 *full-levels*)))])
      (set! *adjusted-level-scores*
	    (map
	     list
	     *thematic-focus*
	     (map adj-level-score *thematic-focus*))))))

(define adjusted-level-score
  (lambda (levelname)
    (let*
	([top (full-levels)]
	 [hypo-top (/ (+ (full-levels) 6) 2)]
	 [this-level (+ (level-score levelname) (- 6 top))]
	 [ratio (/ this-level hypo-top)])
      (* 2.0 ratio ratio))))

; probability that a failure to find an sp in levelname should result
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
      [*val-to-val-nvs* 0.45]
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
; letters in a gridfont, when sps are unlikely to find a match
(define full-levels
  (lambda ()
    (apply + (map level-empty *thematic-focus*))))

(define set-full-levels
  (lambda ()
    (set! *full-levels* (full-levels))))

(define set-favored-levels
  (lambda (exponent)
    (let
	([level-favor
	  (lambda (l)
	    (let
		([score (level-score l)])
	      (if (> score *full-levels*)
		  (list l 0)
		  (list l (power exponent score)))))])
      (set! *favored-tf-levels*
	    (map level-favor *thematic-focus*)))))

; the real motif-matching code
(load "string.ss")

; bridge-caching
; put in to speed up the Adjudicator
; why calculate something twice?
; initial implementation will just skip a second run-through on an SP
; before, Adjudicator would let it affect temperature twice, so
; we may get slightly different temperature scores

(define clear-bridge-cache
  (lambda ()
    (set! *bridge-cache* '())))

(define cache-worthy?
  (lambda (sp-type sp)
    (and (motif-type? sp-type)
	 (> (length sp) 6))))

; cache ahead/back, sp-type and sp
; don't store any info, because we'll just ignore it if it's in here
(define cache-bridge
  (lambda (which-way sp-type sp)
    (if (cache-worthy? sp-type sp)
	(set! *bridge-cache* (append *bridge-cache*
				     (list
				      (list sp-type sp)))))))

; is it in the cache?
(define bridge-in-cache?
  (lambda (which-way sp-type sp)
    (or
     (not (cache-worthy? sp-type sp))
     (member? (list sp-type sp) *bridge-cache*))))
