; *quanta-list* gives the quanta

(set! *thematic-focus*
  '(*letter-rows* *two-time-sps* *occasional-sps* *common-sps*
    *frequent-sps* *universal-sps*))

(set! *noisy-memory* #f)
(set! *noisy-codelets* #f)
(set! *noisy-coderack* #f)
(set! *noisy-thermometer* #f)
(set! *graphics* #f)

(define adj-load
  (lambda ()
    (load "exam-setup.ss")
    (load "rules.ss")
    (load "thematic-focus.ss")
    (load "motif.ss")
    (load "adjudicator.ss")))

; keep only those items in ls2 whose cars are in ls1
(define filter
  (lambda (ls1 ls2)
     (cond
      [(null? ls2) '()]
      [(member? (caar ls2) ls1) (cons (car ls2) (filter ls1 (cdr ls2)))]
      [else
        (filter ls1 (cdr ls2))])))

(define covers
  (lambda (whole)
    (let*
	([expected (whole-roles whole)]
	 [bound-roles
	  (uniquify
	   (apply append (mapcar get-part-bindings *workspace*)))]
	 [all-bound? (eq? (length expected)
			  (length (intersect bound-roles expected)))])
      (let
	  ([sparked (spark-combos expected)]
	   [found-expected
	    (lambda (found)
	      (same-contents expected found))])
	(car sparked)))))

; STILL TO DEBUG?
; what if the part-role bindings aren't one-to-one?
(define roles-bound
  (lambda ()
    (let*
	([role-names (covers *answer*)]
	 [part-stuff (map get-part-quanta *workspace*)])
      (map list role-names part-stuff))))

(define add-blank-list
  (lambda (i)
    (list i '())))

(define post-comparers
  (lambda (n)
    (if (> n 0)
	(begin
	  (add-to-coderack
	   (comparer-codelet (roulette *fillers*) (roulette *nv-types*))
	   'comparer-codelet
	   *medium-urgency*
	   1)
	  (post-comparers (- n 1))))))

(define post-stenographers
  (lambda (n)
    (if (> n 0)
	(begin
	  (add-to-coderack
	   (stenographer-codelet (roulette *fillers*) (roulette *nv-types*))
	   'stenographer-codelet
	   *medium-urgency*
	   1)
	  (post-stenographers (- n 1))))))

(define post-constables
  (lambda (n)
    (if (> n 0)
	(begin
	  (add-to-coderack
	   (constable-codelet (roulette *rule-types*))
	   'constable-codelet
	   *medium-urgency*
	   1)
	  (post-constables (- n 1))))))

(define post-worms
  (lambda (n)
    (if (> n 0)
	(begin
	  (add-to-coderack
	   (worm-codelet (weighted-roulette *favored-motif-types*))
	   'worm-codelet
	   *medium-urgency*
	   1)
	  (post-worms (- n 1))))))

(define post-bridge-aheads
  (lambda (n)
    (if (> n 0)
	(begin
	  (add-to-coderack
	   bridge-ahead-codelet
	   'bridge-ahead-codelet
	   *medium-urgency*
	   1)
	  (post-bridge-aheads (- n 1))))))

(define post-bridge-backs
  (lambda (n)
    (if (> n 0)
	(begin
	  (add-to-coderack
	   (bridge-back-codelet (weighted-roulette (tf-level-interest)))
	   'bridge-back-codelet
	   *medium-urgency*
	   1)
	  (post-bridge-backs (- n 1))))))

(define post-promoters
  (lambda (n)
    (if (> n 0)
	(begin
	  (add-to-coderack
	   promoter-codelet
	   'promoter-codelet
	   *medium-urgency*
	   1)
	  (post-promoters (- n 1))))))

; not once per Adjudicator run -- once per gridfont
; (clear-TF)

(define gridfont-init
  (lambda ()
    (begin
      (clear-TF))))

(define adj-init
  (lambda ()
    (begin
      (set! *coderack* '())
      (set! *codelets-run* 0)
      (set! *temperature* 50)
      (set-full-levels)
      (set-adjusted-level-scores)
      (set! *part-names* (lookup 'roles (eval *answer*)))
      (set! *relative-nvs* (map add-blank-list *part-names*))
      (set! *val-to-val-nvs* (map add-blank-list *part-names*))
      (clear-sublist-cache) ; even need to do this???
      (set! *bridges* '())
      (set! *promotions* '())
      (set! *abstract-rules* '())
      (set! *literal-motifs* '())
      (set! *translate-motifs* '())
      (set! *turn-180-motifs* '())
      (set! *turn-90-motifs* '())
      (set! *turn-45-motifs* '())
      (set! *rules-to-notice* (whole-rule-norms *answer*))
      (set! *favored-sp-types*
	    '((*relative-nvs* 15) (*val-to-val-nvs* 15) (*abstract-rules* 10)
	      (*literal-motifs* 8) (*translate-motifs* 4) (*turn-180-motifs* 4)
	      (*turn-90-motifs* 3) (*turn-45-motifs* 3)))
      (set! *favored-motif-types*
	    '((*literal-motifs* 10) (*translate-motifs* 5)
	      (*turn-180-motifs* 3)
	      (*turn-90-motifs* 2) (*turn-45-motifs* 2)))
      (set-favored-levels 5)
      (post-comparers 600)
      (post-stenographers 500)
      (post-constables 100)
      (post-worms 400)
      (set! *phase* 1))))

(define adj-show
  (lambda ()
    (map eval '(*relative-nvs*
		*val-to-val-nvs* *abstract-rules*
		*literal-motifs*
		*translate-motifs*
		*turn-180-motifs*
		*turn-90-motifs*
		*turn-45-motifs*))))

(define style-rec
 (lambda ()
   (graf-off)
   (examiner-cleanup)
   (adj-init)
   (adj-run)))

(define direct-style-rec
  (lambda (exam-output)
    (begin
      (set! temp-jolts '())
      (set! *answer* (car exam-output))
      (set! *fillers* (cadr exam-output))
      (set! *quanta-list* (apply append (map cadr *fillers*)))
      (adj-init)
      (adj-run))))

