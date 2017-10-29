(set! *approved* 'null)
(set! *seed-set* '(b c e f g))

(define tf-check
  (lambda (font)
    (let
	([dummy1 (set! *trained* font)]
	 [dummy2 (if (member? font '(bowtie checkmark double-backslash
					    flournoy-ranch funtnip))
		     (seed-font-4 font)
		     (begin (seed-font-2 font) (seed-font-3 font)))])
      (tf-summary))))

(define font-load
  (lambda (font)
    (let
	([dummy1 (set! *trained* font)])
      (if (member? font '(bowtie checkmark double-backslash
				 flournoy-ranch funtnip))
	  (seed-font-4 font)
	  (begin (seed-font-2 font) (seed-font-3 font))))))

(define draft-test
  (lambda (font)
    (begin
      (font-load font)
      (draw-alphabet))))

(define tf-summary
  (lambda ()
    (list
     *trained*
     (remove-null-cadr (nth 6 (show-tf)))
     (remove-null-cadr (nth 5 (show-tf)))
     (remove-null-cadr (nth 4 (show-tf)))
     (apply + (map length (map cadr (nth 3 (show-tf)))))
     (apply + (map length (map cadr (nth 2 (show-tf)))))
     (apply + (map length (map cadr (nth 1 (show-tf))))))))

(define seed-font-big
  (lambda (font)
    (begin
      (clear-tf)
      (seed-from-font font 'c)
      (seed-from-font font 'd)
      (seed-from-font font 'f)
      (seed-from-font font 'i)
      (seed-from-font font 'q)
      (seed-from-font font 'y))))

(define seed-font-big-2
  (lambda (font)
    (begin
      (clear-tf)
      (seed-from-font font 'f)
      (seed-from-font font 'd)
      (seed-from-font font 'f)
      (seed-from-font font 'i)
      (seed-from-font font 'q)
      (seed-from-font font 'y))))

(define check-gridletter
  (lambda (attempt qls)
    (begin
      (set! *quanta-list* qls)
      (set! *quanta* (quanta-into-bits qls '()))
      (run-examiner)
      (if (< *codelets-run* 8000)
	  (begin
	    (examiner-cleanup)
	    (set! *exam-score*
		  (- 100 (examiner-goodness)))
	    (if (and
		 (not (eq? *answer* 'quit))
		 (equal? (get-category *answer*) attempt))
		(begin
		  (adj-init)
		  (adj-run)
		  (set! *all-codelets* (+ *all-codelets* *codelets-run*))
		  (list *answer*
			(round-3 (+ *temperature* *exam-score*))))
		(list *answer* 300.0)))
	  (begin
	    (set! *answer* 'quit)
	    (list 'quit 300.0))))))	    

; scratchspace entry
; cat, best-quanta, best-scores, losers' quanta, losing tries
(define clear-scratchspace
  (lambda ()
    (set! *scratchspace*
	  '((a () (299.99) () 0)
	    (b () (299.99) () 0)
	    (c () (299.99) () 0)
	    (d () (299.99) () 0)
	    (e () (299.99) () 0)
	    (f () (299.99) () 0)
	    (g () (299.99) () 0)
	    (h () (299.99) () 0)
	    (i () (299.99) () 0)
	    (j () (299.99) () 0)
	    (k () (299.99) () 0)
	    (l () (299.99) () 0)
	    (m () (299.99) () 0)
	    (n () (299.99) () 0)
	    (o () (299.99) () 0)
	    (p () (299.99) () 0)
	    (q () (299.99) () 0)
	    (r () (299.99) () 0)
	    (s () (299.99) () 0)
	    (t () (299.99) () 0)
	    (u () (299.99) () 0)
	    (v () (299.99) () 0)
	    (w () (299.99) () 0)
	    (x () (299.99) () 0)
	    (y () (299.99) () 0)
	    (z () (299.99) () 0)))))

(define get-seeds
  (lambda (q-ls-ls)
    (if (not (null? q-ls-ls))
	(begin
	  (get-seed (car q-ls-ls))
	  (get-seeds (cdr q-ls-ls))))))

(define design-init
  (lambda ()
    (clear-tf)
    (clear-scratchspace)
    (clear-scratchpad)
    (set! *all-codelets* 0)
    (set! *approved* '())
    (set! *choose-rand* 25) ; 3.349492
    (set! *tip-randomness* 2.0) ; 0.26795935
    (set! *borrow-list* *base-borrow-list*)
    (set! *total-draws* 0)))

(define pick-category
  (lambda (power)
    (let
	([index (map list
		     (map car *scratchspace*)
		     (map average (map caddr *scratchspace*)))])
      (weight-to-n-roulette index power))))

; NOTE style-knob doesn't affect NVs as coded now, 8/15/99
(define knob-set
  (lambda (tries)
    (max 0
	 (- 1.0
	    (* (round (/ tries 3)) 0.1)))))

(define exam-check-gridletter ; no Adjudicator! for funny test run
  (lambda (attempt qls)
    (begin
      (set! *quanta-list* qls)
      (set! *quanta* (quanta-into-bits qls '()))
      (run-examiner)
      (if (< *codelets-run* 8000)
	  (begin
	    (examiner-cleanup)
	    (set! *exam-score*
		  (- 100 (examiner-goodness)))
	    (if (and
		 (not (eq? *answer* 'quit))
		 (equal? (get-category *answer*) attempt))
		(list *answer* (round-3 *exam-score*))
		(list *answer* 300.0)))
	  (begin
	    (set! *answer* 'quit)
	    (list 'quit 300.0))))))	    

(define try-cat
  (lambda (category)
    (let*
	([old-version (lookup-list category *scratchspace*)]
	 [old-quanta (car old-version)]
	 [old-scores (cadr old-version)]
	 [old-score (average old-scores)]
	 [old-losers (caddr old-version)]
	 [tries (+ 1 (cadddr old-version))]
	 [dummy (set! *style-knob* (knob-set tries))]
	 [dummy2 (set! *drawn-from* category)]
	 [new-try (beg-borrow-or-draw category)]
	 [drafted (canonicalize-q-ls *quanta-list*)]
	 [new-test (exam-check-gridletter category drafted)]
	 [margin (if
		     (< old-score 299)
		     (abs (- (cadr new-test) old-score))
		     200)])
      (printf "Trying ~s " category)
      (if (eq? *drawn-from* category)
	  (printf "by drafting ~s; " category)
	  (printf "by borrowing ~s; " *drawn-from*))
      (let*
	  ([new-scores (cdr new-test)]
	   [all-comers
	    (prop-list-collapse
	     (append
	      (list
	       (list drafted new-scores)
	       (list old-quanta old-scores))
	      old-losers))]
	   [score-index (map list
			     (map car all-comers)
			     (map list-quantity (map cadr all-comers)))]
	   [new-champ (find-min score-index)]
	   [champ-scores (lookup new-champ all-comers)]
	   [losers (remove-key new-champ all-comers)])
	    (set! *total-draws* (+ 1 *total-draws*))
	    (set! *choose-rand* (* 0.99 *choose-rand*))
	    (set! *tip-randomness* (* 0.99 *tip-randomness*))
	    (cond
	     [(and (equal? new-champ drafted)
		   (equal? drafted old-quanta))
	      (if (cat-keep? category
			     (list-quantity (append new-scores old-scores)))
		  (begin
		    (printf "seedified as ~s on re-election ~s ~s~%"
			    category
			    (round-3 (list-quantity champ-scores))
			    drafted)
		    (add-scratchspace
		     (list category drafted '(0) '() 0))
		    (score-draw category drafted 0)
		    (set! *approved* '())
		    (update-tf))
		  (begin
		    (printf "reelected as ~s score ~s ~s~%"
			    (car new-test)
			    (list-quantity (append new-scores old-scores))
			    drafted)
		    (add-scratchspace
		     (list
		      category
		      drafted
		      (append new-scores old-scores)
		      old-losers
		      (- tries 1)))
		    (score-draw
		     category drafted
		     (list-quantity (append new-scores old-scores)))))]
	     [(equal? new-champ drafted)
	      (if (cat-keep? category (list-quantity new-scores))
		  (begin
		    (printf "seedified as ~s score ~s beat ~s ~s~%"
			    (car new-test)
			    (list-quantity new-scores)
			    (list-quantity old-scores)
			    drafted)
		    (add-scratchspace
		     (list category drafted '(0) '() 0))
		    (score-draw  category drafted 0)
		    (set! *approved* '())
		    (update-tf))
		  (begin
		    (printf "accepted as ~s score ~s beat ~s ~s~%"
			    (car new-test)
			    (list-quantity new-scores)
			    (list-quantity old-scores)
			    drafted)
		    (add-scratchspace
		     (list
		      category
		      drafted
		      new-scores
		      (prop-list-collapse
		       (cons (list old-quanta old-scores) old-losers))
		      (- tries 1)))
		    (score-draw
		     category drafted (list-quantity new-scores))))]
	     [(equal? new-champ old-quanta)
		(begin
		  (printf "rejected as ~s, score ~s ~s~%"
			  (car new-test)
			  (list-quantity new-scores)
			  drafted)
		  (add-scratchspace
		   (list
		    category
		    old-quanta
		    old-scores
		    (prop-list-collapse
		     (cons (list drafted new-scores) old-losers))
		    tries))
		  (score-draw
		   category old-quanta (list-quantity old-scores)))]
	     [else
		(begin
		;   (printf "rejected as ~s, score ~s ~s~%"
		;	  category (list-quantity new-scores) drafted)
		  (printf "promoted as ~s score ~s ~s~%"
			  category
			  (list-quantity champ-scores)
			  new-champ)
		  (add-scratchspace
		   (list
		    category
		    new-champ
		    champ-scores
		    (remove-key new-champ all-comers)
		    (- tries 1)))
		  (score-draw
		   category new-champ (list-quantity champ-scores)))])))))
(define try-cat
  (lambda (category)
    (let*
	([old-version (lookup-list category *scratchspace*)]
	 [old-quanta (car old-version)]
	 [old-scores (cadr old-version)]
	 [old-score (average old-scores)]
	 [old-losers (caddr old-version)]
	 [tries (+ 1 (cadddr old-version))]
	 [dummy (set! *style-knob* (knob-set tries))]
	 [dummy2 (set! *drawn-from* category)]
	 [new-try (beg-borrow-or-draw category)]
	 [drafted (canonicalize-q-ls *quanta-list*)]
	 [new-test (check-gridletter category drafted)]
	 [margin (if
		     (< old-score 299)
		     (abs (- (cadr new-test) old-score))
		     200)])
      (printf "Trying ~s " category)
      (if (eq? *drawn-from* category)
	  (printf "by drafting ~s; " category)
	  (printf "by borrowing ~s; " *drawn-from*))
      (let*
	  ([new-scores
	    (if (< margin 15.0)
		(list
		 (cadr new-test)
		 (cadr (check-gridletter category drafted)))
		(cdr new-test))]
	   [old-scores2
	    (if (< margin 15.0)
		(cons
		 (cadr (check-gridletter category old-quanta))
		 old-scores)
		old-scores)]
	   [all-comers
	    (prop-list-collapse
	     (append
	      (list
	       (list drafted new-scores)
	       (list old-quanta old-scores2))
	      old-losers))]
	   [score-index (map list
			     (map car all-comers)
			     (map list-quantity (map cadr all-comers)))]
	   [new-champ (find-min score-index)]
	   [champ-scores (lookup new-champ all-comers)]
	   [losers (remove-key new-champ all-comers)])
	    (set! *total-draws* (+ 1 *total-draws*))
	    (set! *choose-rand* (* 0.99 *choose-rand*))
	    (set! *tip-randomness* (* 0.99 *tip-randomness*))
	    (cond
	     [(and (equal? new-champ drafted)
		   (equal? drafted old-quanta))
	      (if (cat-keep? category
			     (list-quantity (append new-scores old-scores2)))
		  (begin
		    (printf "seedified as ~s on re-election ~s ~s~%"
			    category
			    (round-3 (list-quantity champ-scores))
			    drafted)
		    (add-scratchspace
		     (list category drafted '(0) '() 0))
		    (score-draw category drafted 0)
		    (set! *approved* '())
		    (update-tf))
		  (begin
		    (printf "reelected as ~s score ~s ~s~%"
			    (car new-test)
			    (list-quantity (append new-scores old-scores2))
			    drafted)
		    (add-scratchspace
		     (list
		      category
		      drafted
		      (append new-scores old-scores2)
		      old-losers
		      (- tries 1)))
		    (score-draw
		     category drafted
		     (list-quantity (append new-scores old-scores2)))))]
	     [(equal? new-champ drafted)
	      (if (cat-keep? category (list-quantity new-scores))
		  (begin
		    (printf "seedified as ~s score ~s beat ~s ~s~%"
			    (car new-test)
			    (list-quantity new-scores)
			    (list-quantity old-scores2)
			    drafted)
		    (add-scratchspace
		     (list category drafted '(0) '() 0))
		    (score-draw  category drafted 0)
		    (set! *approved* '())
		    (update-tf))
		  (begin
		    (printf "accepted as ~s score ~s beat ~s ~s~%"
			    (car new-test)
			    (list-quantity new-scores)
			    (list-quantity old-scores2)
			    drafted)
		    (add-scratchspace
		     (list
		      category
		      drafted
		      new-scores
		      (prop-list-collapse
		       (cons (list old-quanta old-scores2) old-losers))
		      (- tries 1)))
		    (score-draw
		     category drafted (list-quantity new-scores))))]
	     [(equal? new-champ old-quanta)
		(begin
		  (printf "rejected as ~s, score ~s ~s~%"
			  (car new-test)
			  (list-quantity new-scores)
			  drafted)
		  (add-scratchspace
		   (list
		    category
		    old-quanta
		    old-scores2
		    (prop-list-collapse
		     (cons (list drafted new-scores) old-losers))
		    tries))
		  (score-draw
		   category old-quanta (list-quantity old-scores2)))]
	     [else
		(begin
		;   (printf "rejected as ~s, score ~s ~s~%"
		;	  category (list-quantity new-scores) drafted)
		  (printf "promoted as ~s score ~s ~s~%"
			  category
			  (list-quantity champ-scores)
			  new-champ)
		  (add-scratchspace
		   (list
		    category
		    new-champ
		    champ-scores
		    (remove-key new-champ all-comers)
		    (- tries 1)))
		  (score-draw
		   category new-champ (list-quantity champ-scores)))])))))

(define re-draw
  (lambda (category)
    (let
	([info (lookup-list category *scratchspace*)])
      (score-draw
       category (car info) (list-quantity (cadr info))))))

(define get-seed
  (lambda (qls)
    (begin
      (set! *quanta-list* qls)
      (set! *quanta* (quanta-into-bits qls '()))
      (run-examiner) ; error-check: recognized as something?
      (examiner-cleanup)
      (adj-init)
      (adj-run)
      (update-tf)
      (printf "Accepted, as a seed for ~s, ~s~%"
	      (get-category *answer*) qls)
      (add-scratchspace
       (list (get-category *answer*) qls (list 0.0) () 0)) 
      (token-place (get-category *answer*) *quanta-list* "Peru"))))

(define draft-once
  (lambda (cat)
    (begin
      (draw-cat cat)
      (add-scratchspace
       (list cat (uniquify *quanta-list*) (list 20.0) () 0))
      (token-place cat (uniquify *quanta-list*) "NavajoWhite"))))

(define get-seed-stealthy
  (lambda (qls)
    (begin
      (set! *quanta-list* qls)
      (set! *quanta* (quanta-into-bits qls '()))
      (run-examiner) ; error-check: recognized as something?
      (examiner-cleanup)
      (adj-init)
      (adj-run)
      (update-tf)
      (printf "Accepted, as a seed for ~s, ~s~%"
	      (get-category *answer*) qls)
      (add-scratchspace
       (list (get-category *answer*) qls (list 0.0) () 0)))))

; gridletter-info: category, quanta, scores, losers, tries
(define add-scratchspace
  (lambda (gridletter-info)
    (let
	([key (car gridletter-info)]
	 [score (round-3 (list-quantity (caddr gridletter-info)))])
      (if (not (null? (cadr gridletter-info)))
	  (set! *approved*
		(cons (list key score)
		      (remove-key key *approved*))))
      (set! *scratchspace*
	    (cons
	     gridletter-info
	     (remove-key key *scratchspace*))))))

(define seed-with
  (lambda (qls)
      (set! *quanta-list* qls)
      (set! *quanta* (quanta-into-bits qls '()))
      (run-examiner) ; error-check: recognized as something?
      (examiner-cleanup)
      (adj-init)
      (adj-run)
      (update-tf)))

(define adj-score
  (lambda (qls)
    (set! *quanta-list* qls)
    (set! *quanta* (quanta-into-bits qls '()))
    (run-examiner) ; error-check: recognized as something?
    (examiner-cleanup)
    (adj-init)
    (adj-run)
    (printf "Final temperature: ~s~%" *temperature*)))  

(define list-quantity
  (lambda (ls)
    (if (null? ls)
	0.0
	(round-3 (average ls)))))

(define try-cats
  (lambda ()
    (if
	(and
	 (> (eval (cons '+ (map average (map caddr *scratchspace*)))) 0)
	 (< *total-draws* 300))
	(begin
	  (try-cat (pick-category 1.0))
	  (try-cats))
	(begin
	  (printf "~s drafts and ~s codelets~%"
		  *total-draws* *all-codelets*)
	  (printf "~s~%"
		  (list
		   (tf-summary)
		   (scratch-summary)))))))

; ought to dump scores at end of run, separate Exam and Adj?
(define design-gridfont
  (lambda (q-ls-ls)
    (begin
      (design-init)
      (get-seeds q-ls-ls)
      (printf "~s~%" (tf-summary))
      (try-cats))))

(define seeds-from
  (lambda (font letters)
    (if (null? letters)
	'()
	(cons
	 (that-letter font (car letters))
	 (seeds-from font (cdr letters))))))

(define seed-from
  (lambda (font letters)
    (design-init)
    (get-seeds (seeds-from font letters))))

(define try-gridfont
  (lambda (fontname)
    (begin
      (design-gridfont
       (list
	(that-letter fontname 'b)
	(that-letter fontname 'c)
	(that-letter fontname 'e)
	(that-letter fontname 'f)
	(that-letter fontname 'g)))
      (post-run-score fontname))))

(define hex-to-quanta
  (lambda (hex)
    (q-list (hex->bin (string->list hex)) 0)))

; Drafter mid-run graphics

(set! *graf-step* 20) ; show Draft quanta every 20 Codelets
(set! *draft-bg* "Yellow")

(define apply-max
  (lambda (pair)
    (max (car pair) (cadr pair))))

(define positive-candidates
  (lambda ()
    (let*
	([index (choice-info)]
	 [just-scores
	  (map apply-max (map list (map cadr index) (map caddr index)))]
	 [new-index (map list (map car index) just-scores)])
      (map car (over-threshold 0 new-index)))))

(define show-candidates ; could be much more efficient
  (lambda ()
    (let*
	([cat (get-category *the-whole*)]
	 [usual-sus (positive-candidates)]
	 [show-one
	  (lambda (q)
	    (show-quantum cat q))])
      (map show-one usual-sus))))

(define unshow-candidates ; could be much more efficient
  (lambda ()
    (let*
	([cat (get-category *the-whole*)]
	 [usual-sus (positive-candidates)]
	 [unshow-one
	  (lambda (q)
	    (unshow-quantum cat q))])
      (map unshow-one usual-sus))))

(define show-quantum
  (lambda (cat q)
    (let* ([rank (letter-order cat alphabet)]
	   [x (* wide (mod rank 13))]
	   [y (* tall (- 1 (floor (/ rank 13))))]
	   [endpoints (lookup q *endpoints*)]
	   [end1 (car endpoints)]
	   [end2 (cadr endpoints)])
      (draw! *scratch-gc*
	     `(let-sgl ((foreground-color "LightSlateGrey")
			(line-width 3))
		       (line (,(+ x (car end1)) ,(+ *y* y (cadr end1)))
			     (,(+ x (car end2)) ,(+ *y* y (cadr end2)))))
	     'omit-from-database))))

(define unshow-quantum
  (lambda (cat q)
    (let* ([rank (letter-order cat alphabet)]
	   [x (* wide (mod rank 13))]
	   [y (* tall (- 1 (floor (/ rank 13))))]
	   [endpoints (lookup q *endpoints*)]
	   [end1 (car endpoints)]
	   [end2 (cadr endpoints)])
      (draw! *scratch-gc*
	     `(let-sgl ((foreground-color "MistyRose")
			(line-width 3))
		       (line (,(+ x (car end1)) ,(+ *y* y (cadr end1)))
			     (,(+ x (car end2)) ,(+ *y* y (cadr end2)))))
	     'omit-from-database))))

; CATEGORY SCORE manipulation

(define cat-scores
  (lambda ()
    (map list (map car *scratchspace*)
	 (map round-3 (map average (map caddr *scratchspace*))))))

(set! *cat-handicaps*
      '((a 0)
	(b -10)
	(c 20)
	(d -10)
	(e 20)
	(f 15)
	(g -10)
	(h 5)
	(i 40)
	(j 35)
	(k 10)
	(l 0)
	(m 5)
	(n 5)
	(o -45)
	(p -10)
	(q -10)
	(r 0)
	(s 10)
	(t 10)
	(u 15)
	(v 25)
	(w 5)
	(x 8)
	(y 8)
	(z 5)))

(define cat-handicapped
  (lambda (cat)
    (let
	([basic (lookup cat (cat-scores))])
    (list cat
	  (if (eq? basic 0)
	      '()
	      (round-3
	       (- basic
	       (lookup cat *cat-handicaps*))))))))

(define cat-basic
  (lambda (cat)
    (let
	([basic (lookup cat (cat-scores))])
    (list cat
	  (if (eq? basic 0)
	      '()
	      (round-3 basic))))))

(define cats-raw
  (lambda ()
    (remove-null-cadr (map cat-basic alphabet))))

(define cats-handicapped
  (lambda ()
    (remove-null-cadr (map cat-handicapped alphabet))))

(define cats-scaled
  (lambda ()
    (under-threshold 250.0 (cats-handicapped))))

(define cats-basic
  (lambda ()
    (under-threshold 250.0 (cats-raw))))

(define cats-scaled2
  (lambda ()
    (let*
	([raw (cats-accepted)]
	 [scores (map cadr raw)]
	 [mean (average scores)]
	 [normalize
	  (lambda (n) (round-3 (- n mean)))])
      (map list
	   (map car raw)
	   (map normalize scores)))))

; these two versions for tough promotion

; all category scores accepted previously (before cat)
(define prevs
  (lambda (cat)
    (cons 70
	  (remove-item 0 (map cadr (remove-key cat *approved*))))))

(define promotion-cutoff
  (lambda (cat)
    (case (length *library*)
      [0 68]
      [1 52]
      [else
       (- (round-3 (average (prevs cat))) 12)])))

; these two versions for easy promotion

(define prevs
  (lambda (cat)
    (cons 110
	  (map cadr (remove-key cat *approved*)))))

(define promotion-cutoff
  (lambda (cat)
    (case (length *library*)
      [0 68]
      [1 52]
      [else
       (+ 15 (* 0.33 (average (prevs cat))))])))

(define cat-mean-low?
  (lambda (cat cat-score)
    (let
	([cutoff (round-3 (promotion-cutoff cat))])
      ; (printf "seed margin ~s ~s~%" cat-score cutoff)
      (< cat-score cutoff))))

(define cat-keep?
  (lambda (cat cat-score)
    #f)) ; for no-promote

(define cat-keep?
  (lambda (cat cat-score)
    (cat-mean-low? cat cat-score))) ; later, add percentile basis for keeping

; DRAFTER STATS

; assumes pre-set TF

; separate Exam and Adj scores
(define report-card
  (lambda (attempt qls)
    (begin
      (set! *quanta-list* qls)
      (set! *quanta* (quanta-into-bits qls '()))
      (run-examiner)
      (if (< *codelets-run* 8000)
	  (begin
	    (examiner-cleanup)
	    (set! *exam-score*
		  (- 100 (examiner-goodness)))
	    (if (and
		 (not (eq? *answer* 'quit))
		 (equal? (get-category *answer*) attempt))
		(begin
		  (adj-init)
		  (adj-run)
		  (list qls (round-3 *exam-score*)
			(round-3 *temperature*)))
		(list qls 300.0 300.0)))
	    (list qls 300.0 300.0)))))

; second params, same as after 200 drafts in loop
(define draft-test
  (lambda (cat)
    (set! *choose-rand* 25) ; 3.34949187, 25
    (set! *tip-randomness* 2) ; 0.26795935, 2
    (draw-cat cat)
    (report-card cat (canonicalize-q-ls *quanta-list*))))

(define draft-tests
  (lambda (cat n)
    (if (<= n 1)
	(list (draft-test cat))
	(cons (draft-test cat) (draft-tests cat (- n 1))))))

(define draft-test-analyze
  (lambda (cat n)
    (let*
	([output (draft-tests cat n)]
	 [winners (histograph (map car output))]
	 [winner-stat
	  (lambda (w)
	    (let*
		([appearances (lookup-instances w output)]
		 [exam-scores (map car appearances)]
		 [adj-scores (map cadr appearances)])
	      (list w
		    (lookup w winners)
		    (list-quantity exam-scores)
		    (list-quantity (remove-item 300.0 adj-scores)))))])
      (cons cat
	    (map winner-stat (map car winners))))))

(define draft-test-suite
  (lambda ()
    (list
     (draft-test-analyze 'd 100)
     (draft-test-analyze 'l 100)
     (draft-test-analyze 'm 100))))

; seed based on a gridfont and try each category once
(define draft-pass-once
  (lambda (font)
    (begin
      (seed-from font *seed-set*)
      (map draw-cat alphabet))))

(define draft-pass-once
  (lambda (font)
    (begin
      (seed-from font *seed-set*)
      (map draft-once (remove-items *seed-set* alphabet))
      (post-run-score font))))

; this will read from font-DB as loaded up with gridfonts LS
; has designed... bcefg as seeds... and score them
; returns mean of Exam scores and mean of Adj scores
(define ls-test-font-score
  (lambda (font)
    (let*
	([data (lookup-list font font-DB)]
	 [ls-work (remove-keys *seed-set* data)]
	 [test-set (subtract alphabet *seed-set*)]
	 [cat-stat
	  (lambda (cat)
	    (report-card cat (lookup cat ls-work)))]
	 [dummy
	  (seed-from font *seed-set*)] ; won't always ID seeds right!
	 [results (append
		   (map cat-stat test-set)
		   (map cat-stat test-set)
		   (map cat-stat test-set))])
      (list
       font
       results
       (round-3 (average (map cadr results)))
       (lookup-score 300.0 (histograph (map cadr results)))
       (round-3 (average (map caddr results)))
       (round-3 (average (remove-item 300.0 (map caddr results))))))))

(define post-run-score
  (lambda (font)
    (let*
	([data (scratch-summary)]
	 [ls-work (remove-keys *seed-set* data)]
	 [test-set (subtract alphabet *seed-set*)]
	 [cat-stat
	  (lambda (cat)
	    (report-card cat (lookup cat ls-work)))]
	 [dummy
	  (begin
	    (get-seed-stealthy (lookup 'b data))
	    (get-seed-stealthy (lookup 'c data))
	    (get-seed-stealthy (lookup 'e data))
	    (get-seed-stealthy (lookup 'f data))
	    (get-seed-stealthy (lookup 'g data)))]
	 ; won't always ID seeds right!
	 [results (append
		   (map cat-stat test-set)
		   (map cat-stat test-set)
		   (map cat-stat test-set))])
      (list
       font
       results
       (round-3 (average (map cadr results)))
       (lookup-score 300.0 (histograph (map cadr results)))
       (round-3 (average (map caddr results)))
       (round-3 (average (remove-item 300.0 (map caddr results))))))))

(define out-data-score ; data is a scratch-summary from some run
  (lambda (font data)
    (let*
	([ls-work (remove-keys *seed-set* data)]
	 [test-set (subtract alphabet *seed-set*)]
	 [cat-stat
	  (lambda (cat)
	    (report-card cat (lookup cat ls-work)))]
	 [dummy
	  (begin
	    (get-seed-stealthy (lookup 'b data))
	    (get-seed-stealthy (lookup 'c data))
	    (get-seed-stealthy (lookup 'e data))
	    (get-seed-stealthy (lookup 'f data))
	    (get-seed-stealthy (lookup 'g data)))]
	 ; won't always ID seeds right!
	 [results (append
		   (map cat-stat test-set)
		   (map cat-stat test-set)
		   (map cat-stat test-set))])
      (list
       font
       results
       (round-3 (average (map cadr results)))
       (lookup-score 300.0 (histograph (map cadr results)))
       (round-3 (average (map caddr results)))
       (round-3 (average (remove-item 300.0 (map caddr results))))))))

(define post-self-score
  (lambda ()
    (let*
	([data (scratch-summary)]
	 [ls-work (remove-keys *seed-set* data)]
	 [test-set (subtract alphabet *seed-set*)]
	 [cat-stat
	  (lambda (cat)
	    (report-card cat (lookup cat ls-work)))]
	 [dummy
	  (begin
	    (clear-tf)
	    (get-seed-stealthy (lookup 'b data))
	    (get-seed-stealthy (lookup 'c data))
	    (get-seed-stealthy (lookup 'e data))
	    (get-seed-stealthy (lookup 'f data))
	    (get-seed-stealthy (lookup 'g data)))]	  
	 [results (append
		   (map cat-stat test-set)
		   (map cat-stat test-set)
		   (map cat-stat test-set))])
      (list
       'self
       results
       (round-3 (average (map cadr results)))
       (lookup-score 300.0 (histograph (map cadr results)))
       (round-3 (average (map caddr results)))
       (round-3 (average (remove-item 300.0 (map caddr results))))))))

(define score-summary
  (lambda (data)
    (let*
	([ls-work (remove-keys *seed-set* data)]
	 [test-set (subtract alphabet *seed-set*)]
	 [cat-stat
	  (lambda (cat)
	    (report-card cat (lookup cat ls-work)))]
	 [dummy
	  (begin
	    (clear-tf)
	    (get-seed-stealthy (lookup 'b data))
	    (get-seed-stealthy (lookup 'c data))
	    (get-seed-stealthy (lookup 'e data))
	    (get-seed-stealthy (lookup 'f data))
	    (get-seed-stealthy (lookup 'g data)))]	  
	 [results (append
		   (map cat-stat test-set)
		   (map cat-stat test-set)
		   (map cat-stat test-set))])
      (list
       'self
       results
       (round-3 (average (map cadr results)))
       (lookup-score 300.0 (histograph (map cadr results)))
       (round-3 (average (map caddr results)))
       (round-3 (average (remove-item 300.0 (map caddr results))))))))

(define get-o
  (lambda (qls)
    (begin
      (set! *quanta-list* qls)
      (set! *quanta* (quanta-into-bits qls '()))
      (set! *answer* 'o1)
      (set! *fillers* (list (list 'circle qls)))
      (adj-init)
      (adj-run)
      (update-tf)
      (printf "Accepted, as a seed for ~s, ~s~%"
	      (get-category *answer*) qls)
      (add-scratchspace
       (list (get-category *answer*) qls (list 0.0) () 0))       
      (token-place (get-category *answer*) *quanta-list* "Peru"))))

(define stork-try
  (lambda ()
    (design-init)
    (seed-from 'hunt4 '(a d e f h j k m q r s u x))
    (get-o '(4 36 49 39 50))
    (try-cats)))

(define scratch-summary
  (lambda ()
    (map list (map car *scratchspace*) (map cadr *scratchspace*))))

(define self-design
  (lambda ()
    (design-init)
    (try-cats)
    (post-self-score)))

(define examine
  (lambda (quanta)
    (let
	([runned (exam-check-gridletter 'a quanta)])
    (printf "~s end~%" *codelets-run*)
    runned)))

