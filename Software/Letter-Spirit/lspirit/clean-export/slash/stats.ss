(if (file-exists? "/u/gem/lspirit/data/psychdata")  
    (set! *data-path* "/u/gem/lspirit/data/")
    (set! *data-path* "./"))

(define collect-stats
  (lambda (cat hex n)
    (let
	([qls (q-list (hex->bin (string->list hex)) 0)])
      (if (< n 2)
	  (examine-gridletter cat qls)
	  (begin
	    (examine-gridletter cat qls)
	    (collect-stats cat hex (- n 1)))))))

(define collect-stats
  (lambda (cat qls n)
    (if (< n 2)
	(examine-gridletter cat qls)
	(begin
	  (examine-gridletter cat qls)
	  (collect-stats cat qls (- n 1))))))

(define examine-gridletter
  (lambda (attempt qls)
    (begin
      (set! *quanta-list* qls)
      (set! *quanta* (quanta-into-bits qls '()))
      (run-examiner)
      (if (< *codelets-run* 8000)
	  (examiner-cleanup))
      (printf "~s ~s ~s ~s " *quanta-list* attempt *answer* *codelets-run*)
      (cond
       [(>= *codelets-run* 8000) (printf "qt~%")]
       [(eq? attempt (get-category *answer*))
	(printf "rt ~s ~%" (examiner-goodness))]
       [else (printf "wr ~s ~%" (examiner-goodness))]))))   

(define examine-gridletter
  (lambda (attempt qls)
    (begin
      (set! *quanta-list* qls)
      (set! *quanta* (quanta-into-bits qls '()))
      (run-examiner)
      (if (< *codelets-run* 8000)
	  (examiner-cleanup))
      (printf "~s ~s ~s ~s " (bin->hex *quanta*)
	      attempt *answer* *codelets-run*)
      (cond
       [(>= *codelets-run* 8000) (printf "qt~%")]
       [(eq? attempt (get-category *answer*))
	(printf "rt ~s ~%" (examiner-goodness))]
       [else (printf "wr ~s ~%" (examiner-goodness))]))))   

(define examine-font
  (lambda (font n)
    (let
	([examine-one
	  (lambda (cat)
	    (collect-stats cat (that-letter font cat) n))])
      (set! *the-font* font)
      (map examine-one alphabet))))

(define hard-fonts
  (lambda (n)
    (printf "Testing checkmark~%")
    (examine-font 'checkmark n)
    (printf "Testing funtnip~%")
    (examine-font 'funtnip n)
    (printf "Testing intersect~%")
    (examine-font 'intersect n)
    (printf "Testing sluice~%")
    (examine-font 'sluice n)
    (printf "Testing three-d~%")
    (examine-font 'three-d n)
    (printf "Testing weird-arrow~%")
    (examine-font 'weird-arrow n)))

(define hard-fonts-end
  (lambda (n)
    (printf "Testing sluice~%")
    (examine-font 'sluice n)
    (printf "Testing three-d~%")
    (examine-font 'three-d n)
    (printf "Testing weird-arrow~%")
    (examine-font 'weird-arrow n)))

(define gestalt-rec-gridletter
  (lambda (attempt qls)
    (begin
      (set! *quanta-list* qls)
      (set! *quanta* (quanta-into-bits qls '()))
      (gestalt-init)
      (let
	  ([answer (find-max *gestalt-index*)])
	(printf "~s ~s ~s " *the-font* attempt answer)
	(if (eq? attempt answer)
	    (printf "rt~%") (printf "wr~%"))))))

(define adjudicate-gridletter
  (lambda (attempt qls)
    (let* 
      ([dum1 (set! *quanta-list* qls)]
       [dum2 (set! *quanta* (quanta-into-bits qls '()))]
       [recognize (run-examiner)]
       [correct? (eq? attempt (get-category (car recognize)))])
      (if (and
	   (< *codelets-run* 8000)
	   correct?)
	  (begin
	    (style-rec)
	    'examiner-passed)
	  'examiner-flunked))))

(define adjudicate-from-font
  (lambda (font attempt)
    (adjudicate-gridletter attempt (that-letter font attempt))))

(define seed-gridletter
  (lambda (attempt qls)
    (if (not (eq? 'examiner-flunked
		  (adjudicate-gridletter attempt qls)))
	(update-tf)
	(begin
	  (printf "Examiner failed on ~s, re-trying~%" attempt)
	  (seed-gridletter attempt qls)))))

(define test-gridletter
  (lambda (attempt qls)
    (if (eq? 'examiner-flunked (adjudicate-gridletter attempt qls))
	(begin
	  (printf "Examiner failed on ~s, re-trying~%" attempt)
	  (test-gridletter attempt qls)))))

(define seed-from-font
  (lambda (font attempt)
    (seed-gridletter attempt (that-letter font attempt))))

(define seed-font-seeds
  (lambda (font seeds) ; four seeds
    (seed-from-font font (nth 1 seeds))
    (if (> (length seeds) 1)
	(seed-from-font font (nth 2 seeds)))
    (if (> (length seeds) 2)
	(seed-from-font font (nth 3 seeds)))
    (if (> (length seeds) 3)
	(seed-from-font font (nth 4 seeds)))
    (if (> (length seeds) 4)
	(seed-from-font font (nth 5 seeds)))
    (if (> (length seeds) 5)
	(seed-from-font font (nth 6 seeds)))))

(define seed-font-seeds
  (lambda (font seeds) ; four seeds
    (if (eq? (length seeds) 1)
	(seed-from-font font (car seeds))
	(begin
	  (seed-from-font font (car seeds))
	  (seed-font-seeds font (cdr seeds))))))

(define adj-test-once
  (lambda (font letters)
    (let*
	([test-once
	  (lambda (letter)
	    (begin
	      (test-gridletter letter (that-letter font letter))
	      (set! *adj-data* (cons (round-3 *temperature*)
				     *adj-data*))))]
	 [test-it
	  (lambda (letter)
	    (begin
	      (set! *adj-data* '())
	      (test-once letter)
	      (set! *current-data* (cons
				    (list font letter *adj-data*)
				    *current-data*))
	      (printf "TestSet~s ~s ~s ~s ~s~%"
		      *test-set*
		      *trained*
		      letter font
		      (car *adj-data*))))])
      (map test-it letters))))

(define adj-test-thrice
  (lambda (font letters)
    (let*
	([test-thrice
	  (lambda (letter)
	    (begin
	      (test-gridletter letter (that-letter font letter))
	      (set! *adj-data* (cons (round-3 *temperature*)
				     *adj-data*))
	      (test-gridletter letter (that-letter font letter))
	      (set! *adj-data* (cons (round-3 *temperature*)
				     *adj-data*))
	      (test-gridletter letter (that-letter font letter))
	      (set! *adj-data* (cons (round-3 *temperature*)
				     *adj-data*))))]	      
	 [test-it
	  (lambda (letter)
	    (begin
	      (set! *adj-data* '())
	      (test-thrice letter)
	      (printf "TestSet~s ~s ~s ~s ~s ~s~%"
		      *test-set*
		      *trained*
		      letter font
		      (car *adj-data*)
		      (round-3 (average *adj-data*)))))])
      (map test-it letters))))

(define adj-test-fonts
  (lambda (fonts letters)
    (if (eq? (length fonts) 1)
	(adj-test-once (car fonts) letters) ; normally "once"
	(begin
	  (adj-test-once (car fonts) letters) ; normally "once"
	  (adj-test-fonts (cdr fonts) letters)))))

(define adj-test-one-font
  (lambda (font fonts train test)
    (set! *trained* font)
    (clear-tf)
    (seed-font-seeds font train)
    (set! *current-data* '()) ; we'll have this thing score itself
    (adj-test-fonts fonts test)))

(define adj-test-font-group
  (lambda (fonts train test)
    (let
	([test-one
	  (lambda (font)
	    (adj-test-one-font font fonts train test))])
      (map test-one fonts)
      (set! *test-set* (+ 1 *test-set*)))))

(define big-adj-test
  (lambda ()
    (adj-test-font-group
     '(benzene-left benzene-right boat close house)
     '(b c n p) '(a g o z))
    (adj-test-font-group
     '(hunt4 shorts slant snout standard-square)
     '(d h r z) '(a m s u))
    (adj-test-font-group
     '(hint4 checkmark double-backslash flournoy-ranch funtnip)
     '(d f q t) '(v y))
    (adj-test-font-group
     '(bowtie boat sabretooth slash square-curl)
     '(f l t y) '(h i j k))
    (adj-test-font-group
     '(benzene-left intersect house shorts snout)
     '(b d f o) '(g r x z))
    (adj-test-font-group
     '(hunt4 benzene-right slant close standard-square)
     '(d g i q) '(a m j z))
    (adj-test-font-group
     '(benzene-left weird-arrow hunt4 bowtie hint4)
     '(f i l y) '(j u v w))
    (adj-test-font-group
     '(benzene-right weird-arrow shorts checkmark intersect)
     '(c f l r) '(i o q z))
    (adj-test-font-group
     '(boat slant double-backslash sabretooth house)
     '(e g h i) '(l o q s))
    (adj-test-font-group
     '(close snout flournoy-ranch slash standard-square)
     '(b d g i) '(k l m p))
    (adj-test-font-group
     '(funtnip square-curl standard-square shorts BL)
     '(b d f t) '(h n r w))
    (adj-test-font-group
     '(hunt4 benzene-right double-backslash weird-arrow slash)
     '(f g h i) '(b c d e))
    (adj-test-font-group
     '(house snout flournoy-ranch sabretooth intersect)
     '(j l q r) '(b c d f))
    (adj-test-font-group
     '(boat weird-arrow slant funtnip square-curl)
     '(h n r y) '(d f w))
    (adj-test-font-group
     '(hint4 double-backslash slash slant close)
     '(c d j q) '(e l p x))
    (adj-test-font-group
     '(bowtie hunt4 snout intersect sabretooth)
     '(j l t y) '(f i))
    (adj-test-font-group
     '(square-curl weird-arrow funtnip close checkmark)
     '(d f h n) '(r y))
    (adj-test-font-group
     '(benzene-right checkmark double-backslash bowtie benzene-left)
     '(f h i l) '(t v x y))
    (adj-test-font-group
     '(checkmark boat flournoy-ranch hint4 funtnip)
     '(d f r y) '(q t v))
    (adj-test-font-group
     '(house sabretooth hint4 shorts square-curl)
     '(d f i l) '(b n p s))
    (adj-test-font-group
     '(bowtie intersect flournoy-ranch slash standard-square)
     '(f i j l) '(t y))))

(define full-adj-test
  (lambda ()
    (set! *seeds* '(c d f i q y))
    (adj-test-font-group
     '(benzene-left benzene-right boat close house)
     '(a k o x z))
    (adj-test-font-group
     '(hunt4 shorts slant snout standard-square)
     '(h l n s u))
    (set! *seeds* '(f h m t v y))
    (adj-test-font-group
     '(bowtie checkmark double-backslash flournoy-ranch funtnip)
     '(k n))
    (set! *seeds* '(c d f i q y))
    (adj-test-font-group
     '(boat close flournoy-ranch hint4 hunt4)
     '(e g k p t))
    (adj-test-font-group
     '(intersect sabretooth slash square-curl weird-arrow)
     '(b j l m r))))    

(define hex-rec
  (lambda (hex)
    (let* ([code (string->list hex)]
	   [bls (hex->bin code)])
      (set! *quanta* bls)
      (set! *quanta-list* (q-list *quanta* 0))
      (run-examiner))))

; variation 1: `b c d e f g' as training set
; variation 2: `b c e' as training set
; variation 3: `b' as training set
; variation 4: do the whole thing three times and take the score

(define variation-adj-test
  (lambda ()
    (set! *test-set* 0)
    (printf "Big training~%")
    (adj-test-font-group
     '(close snout flournoy-ranch slash standard-square)
     '(b c d e f g) '(j k l m p))
    (adj-test-font-group
     '(hunt4 benzene-right double-backslash weird-arrow slash)
     '(b c d e f g) '(j l p v y))
    (set! *test-set* 20)
    (printf "Small training~%")
    (adj-test-font-group
     '(close snout flournoy-ranch slash standard-square)
     '(b c e) '(j k l m p))
    (adj-test-font-group
     '(hunt4 benzene-right double-backslash weird-arrow slash)
     '(b c e) '(j l p v y))
    (set! *test-set* 40)
    (printf "Tiny training~%")
    (adj-test-font-group
     '(close snout flournoy-ranch slash standard-square)
     '(b) '(j k l m p))
    (adj-test-font-group
     '(hunt4 benzene-right double-backslash weird-arrow slash)
     '(b) '(j l p v y))))

; MUST RE-RUN!!!
(define big-adj-test
  (lambda ()
    (set! *test-set* 0)
    (adj-test-font-group
     '(benzene-left benzene-right boat close house)
     '(b c n p) '(a g o z))
    (adj-test-font-group
     '(hunt4 shorts slant snout standard-square)
     '(d h r z) '(a m s u))
    (adj-test-font-group
     '(hint4 checkmark double-backslash flournoy-ranch funtnip)
     '(d f q t) '(v y))
    (adj-test-font-group
     '(bowtie boat sabretooth slash square-curl)
     '(f l t y) '(h i j k))
    (adj-test-font-group
     '(benzene-left intersect house shorts snout)
     '(b d f o) '(g r x z))
    (adj-test-font-group
     '(hunt4 benzene-right slant close standard-square)
     '(d g i q) '(a m j z))
    (adj-test-font-group
     '(benzene-left weird-arrow hunt4 bowtie hint4)
     '(f i l y) '(j u v w))
    (adj-test-font-group
     '(benzene-right weird-arrow shorts checkmark intersect)
     '(c f l r) '(i o q z))
    (adj-test-font-group
     '(boat slant double-backslash sabretooth house)
     '(e g h i) '(l o q s))
    (adj-test-font-group
     '(close snout flournoy-ranch slash standard-square)
     '(b d g i) '(k l m p))
    (adj-test-font-group
     '(funtnip square-curl standard-square shorts benzene-left)
     '(b d f t) '(h n r w))
    (adj-test-font-group
     '(hunt4 benzene-right double-backslash weird-arrow slash)
     '(f g h i) '(b c d e))
    (adj-test-font-group
     '(house snout flournoy-ranch sabretooth intersect)
     '(j l q r) '(b c d f))
    (adj-test-font-group
     '(boat weird-arrow slant funtnip square-curl)
     '(h n r y) '(d f w))
    (adj-test-font-group
     '(hint4 double-backslash slash slant close)
     '(c d j q) '(e l p x))
    (adj-test-font-group
     '(bowtie hunt4 snout intersect sabretooth)
     '(j l t y) '(f i))
    (adj-test-font-group
     '(square-curl weird-arrow funtnip close checkmark)
     '(d f h n) '(r y))
    (adj-test-font-group
     '(benzene-right checkmark double-backslash bowtie benzene-left)
     '(f h i l) '(t v x y))
    (adj-test-font-group
     '(checkmark boat flournoy-ranch hint4 funtnip)
     '(d f r y) '(q t v))
    (adj-test-font-group
     '(house sabretooth hint4 shorts square-curl)
     '(d f i l) '(b n p s))
    (adj-test-font-group
     '(bowtie intersect flournoy-ranch slash standard-square)
     '(f i j l) '(t y))))

; fairly consistent training and testing sets
; first test set ideal for variations involving size of training and
; number of test runs
(define big-easy-adj-test
  (lambda ()
    (set! *test-set* 0)
    (adj-test-font-group
     '(benzene-left benzene-right boat close house)
     '(b c e f g) '(d a y))
    (adj-test-font-group
     '(hunt4 shorts slant snout standard-square)
     '(c e f g h) '(d a y))
    (adj-test-font-group
     '(hint4 checkmark double-backslash flournoy-ranch funtnip)
     '(f q t v) '(d y))
    (adj-test-font-group
     '(bowtie boat sabretooth slash square-curl)
     '(f h i j k) '(l t y))
    (adj-test-font-group
     '(benzene-left intersect house shorts snout)
     '(b c f g h) '(d o y))
    (adj-test-font-group
     '(hunt4 benzene-right slant close standard-square)
     '(c e f g h) '(d a y))
    (adj-test-font-group
     '(benzene-left weird-arrow hunt4 bowtie hint4)
     '(f i j l u) '(v w y))
    (adj-test-font-group
     '(benzene-right weird-arrow shorts checkmark intersect)
     '(c f i l q) '(d o y))
    (adj-test-font-group
     '(boat slant double-backslash sabretooth house)
     '(c e f g h) '(d o y))
    (adj-test-font-group
     '(close snout flournoy-ranch slash standard-square)
     '(b c e f g) '(d p y))
    (adj-test-font-group
     '(funtnip square-curl standard-square shorts benzene-left)
     '(b f h n r) '(d t y))
    (adj-test-font-group
     '(hunt4 benzene-right double-backslash weird-arrow slash)
     '(b c e f g) '(d p y))
    (adj-test-font-group
     '(house snout flournoy-ranch sabretooth intersect)
     '(c e f g t) '(d p y))
    (adj-test-font-group
     '(boat weird-arrow slant funtnip square-curl)
     '(f h n r w) '(d y))
    (adj-test-font-group
     '(hint4 double-backslash slash slant close)
     '(c e f g j) '(d p y))
    (adj-test-font-group
     '(bowtie hunt4 snout intersect sabretooth)
     '(f i j l) '(t y))
    (adj-test-font-group
     '(square-curl weird-arrow funtnip close checkmark)
     '(f h n r) '(d y))
    (adj-test-font-group
     '(benzene-right checkmark double-backslash bowtie benzene-left)
     '(f h i v x) '(l t y))
    (adj-test-font-group
     '(checkmark boat flournoy-ranch hint4 funtnip)
     '(f q r v) '(d t y))
    (adj-test-font-group
     '(house sabretooth hint4 shorts square-curl)
     '(b c f j n) '(d t y))
    (adj-test-font-group
     '(bowtie intersect flournoy-ranch slash standard-square)
     '(f i j l) '(t y))))

(define font-dump-tf
  (lambda (font seeds)
    (clear-tf)
    (set! *trained* font)
    (seed-font-seeds font seeds)
    (printf "~s~%" (tf-summary))))

(define all-combo-adj-test
  (lambda (font)
    (set! *test-set* 0)
    (let
	([isolate-cat
	  (lambda (cat)
	    (adj-test-font-group
	     (list font) (subtract alphabet (list cat)) (list cat)))])
      (map isolate-cat alphabet)
      (update-tf)
      (tf-summary))))

; give training that the Examiner can handle
; have to know that in advance on a font-by-font basis
(define tf-test-font
  (lambda (font training)
    (seed-font-seeds font training)
    (tf-summary)))

(define lil-adj-test
  (lambda ()
    (set! *test-set* 0)
    (adj-test-font-group
     '(close snout flournoy-ranch slash standard-square)
     '(b d g i) '(k l m p))
    (adj-test-font-group
     '(hunt4 benzene-right double-backslash weird-arrow slash)
     '(f g h i) '(b c d e))))

(define medium-adj-test
    (lambda ()
    (adj-test-font-group
     '(hunt4 shorts slant snout standard-square)
     '(d h r z) '(a m s u))
    (adj-test-font-group
     '(bowtie boat sabretooth slash square-curl)
     '(f l t y) '(h i j k))
    (adj-test-font-group
     '(boat slant double-backslash sabretooth house)
     '(e g h i) '(l o q s))
    (adj-test-font-group
     '(close snout flournoy-ranch slash standard-square)
     '(b d g i) '(k l m p))
    (adj-test-font-group
     '(hunt4 benzene-right double-backslash weird-arrow slash)
     '(f g h i) '(b c d e))))

; GEM trickies

(define tricky-test
  (lambda (n)
    (collect-stats 'j '(15 17 21 24 27 32 34 36 42) n)
    (collect-stats 'j '(25 28 34 43 49) n)
    (collect-stats 'k '(14 17 20 23 38 21 49 51) n)
    (collect-stats 'm '(23 24 22 25 6 37) n)
    (collect-stats 'm '(21 22 23 24 25 36 37) n)
    (collect-stats 'm '(4 21 22 23 36 37 39) n)
    (collect-stats 'n '(5 22 23 25 36 48) n)
    (collect-stats 'q '(4 20 23 8 9 49 25 41 42 12) n)
    (collect-stats 's '(50 9 51 21 49) n)
    (collect-stats 'u '(20 23 8 39 22) n)
    (collect-stats 'w '(20 22 24 39 50) n)
    (collect-stats 'x '(20 5 25 8 48 51 37 38) n)
    (collect-stats 'x '(37 38 22 36 21 51) n)
    (collect-stats 'z '(5 37 38 8 39) n)))
