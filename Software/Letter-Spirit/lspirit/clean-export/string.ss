; find overlap between strings (for motifs)

; find overlap, position-for-position

(define overlap-progress
  (lambda (ls1 ls2 ever active)
    (cond
     [(or (null? ls1) (null? ls2)) (reverse ever)]
     [(eq? (car ls1) (car ls2))
      (let
	  ([act-new (cons (car ls1) active)])
	(if (>= (length active) (length ever))
	    (overlap-progress (cdr ls1) (cdr ls2) act-new act-new)
	    (overlap-progress (cdr ls1) (cdr ls2) ever act-new)))]
     [else
      (overlap-progress (cdr ls1) (cdr ls2) ever '())])))

(define order-overlap
  (lambda (ls1 ls2)
    (overlap-progress ls1 ls2 '() '())))

; doesn't handle all the cases?
(define biggest-overlap
  (lambda (ls1 ls2)
    (if (or (null? ls1) (null? ls2))
	'()
	(let
	    ([even (order-overlap ls1 ls2)]
	     [yank-one (biggest-overlap (cdr ls1) ls2)]
	     [yank-two (biggest-overlap ls1 (cdr ls2))])
	  (longest
	   (list even yank-one yank-two))))))

(define longer
  (lambda (ls1 ls2)
    (if (> (length ls1) (length ls2))
	ls1
	ls2)))

(define longest
  (lambda (ls-ls)
    (if (null? ls-ls)
	'()
	(find-max (map list ls-ls (map length ls-ls))))))
     

; new MOTIF section, 7/9/99

(define motif-match-all
  (lambda (sp-type mtf m-ls)
    (let*
	([submotifs
	  (motif-sublists sp-type mtf)]
	 [match-one
	  (lambda (m)
	    (list
	      (cons m (sublists-include submotifs m))))])
      (apply append (map match-one m-ls)))))

(define biggest-match
  (lambda (sp-type mtf m-ls)
    (longest (motif-match-all sp-type mtf m-ls))))

(define motif-match-all-cache
  (lambda (sp-type mtf m-ls)
    (let*
	([submotifs
	  (find-motif-sublists sp-type mtf)]
	 [match-one
	  (lambda (m)
	    (list
	      (cons m (sublists-include submotifs m))))])
      (apply append (map match-one m-ls)))))

(define biggest-match-cache
  (lambda (sp-type mtf m-ls)
    (longest (motif-match-all-cache sp-type mtf m-ls))))

(define biggest-level-match
  (lambda (sp-type mtf levelname)
    (let
	([level-ls (lookup sp-type (eval levelname))])
      (if (> (level-score levelname) 3)
	  (biggest-match-cache sp-type mtf level-ls)
	  (biggest-match sp-type mtf level-ls)))))

(define all-level-matches
  (lambda (sp-type mtf)
    (let
	([one-level-match
	  (lambda (levelname)
	    (biggest-level-match sp-type mtf levelname))])
      (map cons *thematic-focus*
	   (map one-level-match *thematic-focus*)))))

; re-done for bridge-ahead 7/10/99
; of all the longest matches, takes the one from the highest level
(define best-TF-motif-match
  (lambda (sp-type mtf)
    (let*
	([level-matches (all-level-matches sp-type mtf)]
	 [biggest (apply max (map length level-matches))])
      (if (eq? biggest 1)
	  '()
	  (let*
	      ([winners (as-big-as biggest level-matches)]
	       [ranked (map list winners (map level-score (map car winners)))]
	       [champ (find-max ranked)])
	    (list (car champ) (cadr champ) (cddr champ)))))))
		 
      