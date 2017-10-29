; motif matchers

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

(define show-TF
  (lambda ()
    (map eval *thematic-focus*)))

(define clear-TF
  (lambda ()
    (set! *workspace*
	  '(*relative-NVs*
	    *map-NVs*
	    *abstract-rules*
	    *literal-motifs*
	    *translate-motifs*
	    *rotate-motifs*
	    *reflect-motifs*))
    (set! *letter-rows* '((*relative-NVs* ())
			  (*map-NVs* ())
			  (*abstract-rules* ())
			  (*literal-motifs* ())
			  (*translate-motifs* ())
			  (*rotate-motifs* ())
			  (*reflect-motifs* ())))
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

(define prune-size
  (lambda (ls-ls thresh)
    (let*
	([sizes (map length ls-ls)]
	 [index (map list ls-ls sizes)])
      (map car (remove-index-small index thresh)))))

(define bridge-level-motif-search
  (lambda (levelname sp-type near-end size)
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
	  (roulette (prune-size (map cadr far-matches) size))))))

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
		
(define negate
  (lambda (n)
    (if (number? n) (- n) n)))

; reflect -- also consider the unmirrored version?
; nvs and rules have proper structure with that "list" thing?
(define sp-match-old
  (lambda (type current-sp tf-sp)
    (case type
      [(*relative-nvs* *map-nvs*) (if (equal? current-sp tf-sp)
				      tf-sp
				      '())]
      [*abstract-rules* (if (equal? current-sp tf-sp)
			    tf-sp
			    '())]
      [*literal-motifs*
       (shared-sequence-literal current-sp tf-sp)]
      [*translate-motifs*
       (shared-sequence-translate current-sp tf-sp)]
      [*rotate-motifs*
       (shared-sequence-rotate current-sp tf-sp)]
      [*reflect-motifs* 
       (shared-sequence-rotate (map negate current-sp) tf-sp)])))

(define sp-match
  (lambda (type current-sp tf-sp)
    (case type
      [(*relative-nvs* *map-nvs*) (if (equal? current-sp tf-sp)
				      tf-sp
				      '())]
      [*abstract-rules* (if (equal? current-sp tf-sp)
			    tf-sp
			    '())]
      [(*literal-motifs* *translate-motifs* *rotate-motifs*)
       (shared-sequence-literal current-sp tf-sp)]
      [*reflect-motifs* 
       (motif-match (map negate current-sp) tf-sp)])))

(define motif-match
  (lambda (ls1 ls2)
    (let*
	([common (intersect ls1 ls2)])
      (if (null? common)
	  '()
	  (length-heavy-roulette (map linearize (glom-islands common)))))))

(define motif-row
  (lambda (levelname sp-type)
    (cons levelname
	  (map sublists (lookup sp-type (eval levelname))))))

(define motif-type-DB
  (lambda (sp-type)
    (let
	([one-row
	  (lambda (level)
	    (motif-row level sp-type))])
      (map one-row (reverse *thematic-focus*)))))

(define motif-size-level-search
  (lambda (level-info target-sublists size)
    (let*
 	([options (head-as-big-as size level-info)]
	 [common
	  (lambda (option)
	    (intersect target-sublists option))]
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
	      (list top-level top-check))))))

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
    (motif-DB-match (motif-type-DB sp-type) (sublists target)
		    (length target))))

; (clear-tf) (load "bridges.ss") (explore-font 'benzene-right)


(define general-length
  (lambda (item)
    (if (atom? item) 1 (length item))))

; weight the roulette wheel by length
(define length-roulette
  (lambda (ls)
    (let
	([indexed-ls (map list ls (map general-length ls))])
      (weighted-roulette indexed-ls))))

(define length-heavy-roulette
  (lambda (ls)
    (let*
	([heavy-length
	  (lambda (item)
	    (if (atom? item) 1 (* (length item) (length item))))]
	 [indexed-ls (map list ls (map heavy-length ls))])
      (weighted-roulette indexed-ls))))

(define shared-sequence-literal
  (lambda (ls1 ls2)
    (let
	([common (append (common-sublists ls1 ls2)
			 (common-sublists ls1 (reverse ls2)))])
      (if (null? common)
	  '()
	  (find-max (map list common (map length common)))))))

(define shared-sequence-translate
  (lambda (ls1 ls2)
    (let
	([common (append (common-sublists ls1 ls2)
			 (common-sublists ls1 (tip-flip (reverse ls2))))])
      (if (null? common)
	  '()
	  (find-max (map list common (map length common)))))))

(define shared-sequence-rotate
  (lambda (ls1 ls2)
    (let
	([common (append (common-sublists ls1 ls2)
			 (common-sublists ls1 (map negate (reverse ls2))))])
      (if (null? common)
	  '()
	  (find-max (map list common (map length common)))))))

; low-level TF manipulation routines

(define remove-from-TF
  (lambda (levelname sp-type sp)
    (let*
	([level (eval levelname)]
	 [rest-of-level (remove-key sp-type level)]
	 [row-with-sp (car (lookup-list sp-type level))]
	 [row-minus-sp (remove-item sp row-with-sp)]
	 [new-row (list sp-type row-minus-sp)]
	 [new-level (cons new-row rest-of-level)])
      (set-top-level-value! levelname new-level))))

(define add-to-TF
  (lambda (levelname sp-type sp)
    (if (not (null? sp))
	(let*
	    ([level (eval levelname)]
	     [rest-of-level (remove-key sp-type level)]
	     [row-for-sp (car (lookup-list sp-type level))]
	     [new-row (list sp-type (condcons sp row-for-sp))]
	     [new-level (cons new-row rest-of-level)])
	  (set-top-level-value! levelname new-level)))))

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

(define promote-in-TF
  (lambda (sp-type old-level far-sp match)
    (let
	([new-level (level-successor old-level)])
      (if (not (eq? old-level new-level))
	  (begin
	    ; we won't remove old multi-quanta motifs from early levels
	    ; they may form the basis for multiple promotions
	    (if (or
		 (eq? old-level *occasional-sps*)
		 (eq? old-level *common-sps*)
		 (eq? old-level *frequent-sps*)
		 (eq? old-level *universal-sps*)
		 (member? sp-type
			  '(*relative-nvs* *map-nvs* *abstract-rules*))
		 (eq? 1 (length far-sp)))
		(remove-from-TF old-level sp-type far-sp))
	    (add-to-TF new-level sp-type match))))))

(define demote-in-TF
  (lambda (sp-type old-level far-sp)
    (let
	([new-level (level-predecessor old-level)])
      (if (not (eq? old-level new-level))
	  (begin
	    (remove-from-TF old-level sp-type far-sp)
	    (add-to-TF new-level sp-type far-sp))))))

; Adjudicator temperature manipulation
; goes here, because bridge building success and failure is
; the sole determinant of temperature

; idea: GOODNESS will be based on temperature, but terminating a run
; will be based on emptying *bridges*

; helper adjusts temperature towards goal

(set! *temp-jump* 0.01)

(define thermostat
  (lambda (aim rate)
    (let*
      ([step (* rate *temp-jump*)]
       [new-temp (+ (* (- 1 step) *temperature*) (* step aim))])
      (set! *temperature* new-temp))))

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
