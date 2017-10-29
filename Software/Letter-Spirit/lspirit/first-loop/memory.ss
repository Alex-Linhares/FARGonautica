;=============================================================================
; memory.ss : functions for manipulation of the conceptual memory
;=============================================================================
; conditional printing of memory messages see tools.ss

(set! *noisy-memory* #f)

(define memmsg
  (ifprint-proto *noisy-memory*))

(set! *noisy-activator* #f) ; put this here since it will usually be #f

; print out activation scoring information
(define actmsg
  (ifprint-proto *noisy-activator*))

;----------------------------[ specific set/lookup ]------------------------

; by JAR, 10/13/98
(define get-activation
  (lambda (node)
    (car (eval (caddr (lookup 'names (eval node)))))))

; Change a role's activation.  upper and lower bounds are enforced here.
; Role activation can range from [-100..100]
(define set-activation
  (lambda (role act)
    (let*
	((role-act (caddr (lookup 'names (eval role))))
	 (role-act-value (eval role-act)))
      (set-top-level-value!
       role-act
       (cons (max -100 (min 100 act))
	     (cdr role-act-value))))))

(define get-category
  (lambda (whole)
    (cadr (cadar (eval whole)))))

; role-part bindings -- redone by JAR 10/28/98

; all bindings will be in a centralized list of role-part pairs

(define clear-bindings
  (lambda ()
    (set! *bindings* '())))

(define bind-role-part
  (lambda (role part flip)
    (set! *bindings* (cons (list role part flip) *bindings*))))

(define bindings-minus-part
  (lambda (part bindings)
    (cond
     ((null? bindings) '())
     ((eq? part (cadar bindings))
      (bindings-minus-part part (cdr bindings)))
     (else
      (cons (car bindings) (bindings-minus-part part (cdr bindings)))))))

(define unbind-part
  (lambda (part)
    (set! *bindings*
	  (bindings-minus-part part *bindings*))))

(define get-part-binding-list
    (lambda (part bindings)
      (cond
       ((null? bindings) '())
       ((eq? part (cadar bindings))
	(cons (caar bindings)
	      (get-part-binding-list part (cdr bindings))))
       (else
	(get-part-binding-list part (cdr bindings))))))

(define get-part-bindings
  (lambda (part)
    (get-part-binding-list part *bindings*)))

(define get-part-quanta
  (lambda (part)
    (lookup 'quanta part)))

; returns list of quantum-lists
; should always be of length one, ideally
(define get-role-binding-list
    (lambda (role bindings)
      (cond
       ((null? bindings) '())
       ((eq? role (caar bindings))
	(cons (list (cadar bindings) (caddar bindings))
		    (get-role-binding-list role (cdr bindings))))
       (else
	(get-role-binding-list role (cdr bindings))))))

(define get-role-bindings
  (lambda (role)
    (get-role-binding-list role *bindings*)))

; top activation associated with part

(define part-top-activation
  (lambda (part)
    (let
	([bindings (get-part-bindings part)])
      (if (not (null? bindings))
	  (apply max (map get-activation bindings))
	  0))))

; sparked stuff -- used in r-roles

(define part-sparked-roles
  (lambda (part)
    (map car (sparked-roles part))))

; replaced part-sparked-roles (re-spark EVERYTHING) with
; get-part-bindings (see what we've sparked)
; this was the cause of a major slowdown

(define parts-sparked-roles
  (lambda (pls)
    (map get-part-bindings pls)))

;---------------------[ Dampening of Activations ]-----------------------
; When new parsings are made, we want to "forget" some of the old
; information, since it may now be outdated
; Added to all codelets involving re-parsing
; by JAR, 5/14/96, improved 11/19/98
; also cues up some gestalt codelets

(define dampen
  (lambda ()
    (begin
      (map dampen-role-act *roles*)
      (map dampen-whole-act *wholes*)
      (set! *bindings* '()) ; maybe too harsh -- keep bindings of survivors?
      (add-n-to-coderack *gestalt-codelets*
			 gestalt-codelet
			 'gestalt
			 *very-high-urgency*
			 1))))

; GRAPHICS -- oughta add
; (draw-role-activation name adjusted-activation)
; (if (equal? type 'role)
; (draw-grid-role name (max adjusted-activation 0)))))

(define dampen-role-act
  (lambda (role)
    (set-activation role (* 0.05 (get-activation role)))))

(define dampen-whole-act
  (lambda (role)
    (set-activation role (* 0.1 (get-activation role)))))

;----------------------[ Feature activation ]----------------------------
; Activate roles with a feature set.  Roles with high-activations get bound
; to a part.

(define get-norms
  (lambda (role)
    (lookup-list 'norms role)))

(define get-features
  (lambda (part)
    (cdr part)))

(define just-atoms
  (lambda (ls)
    (cond
     ((null? ls) '())
     ((atom? (car ls)) (cons (car ls) (just-atoms (cdr ls))))
     (else (just-atoms (cdr ls))))))

(define not-atoms
  (lambda (ls)
    (cond
     ((null? ls) '())
     ((atom? (car ls)) (not-atoms (cdr ls)))
     (else (cons (car ls) (not-atoms (cdr ls)))))))

(set! *punish* -25)
(set! *punish-hard* -40)
(set! *reject* -600)

(define lookup-or-punish
    (lambda (item ls)
    (cond
      ((null? ls) *punish*)
      ((equal? (caar ls) item) (cadar ls))
     (else (lookup-or-punish item (cdr ls))))))

(define lookup-or-punish-hard
    (lambda (item ls)
    (cond
      ((null? ls) *punish-hard*)
      ((equal? (caar ls) item) (cadar ls))
     (else (lookup-or-punish-hard item (cdr ls))))))

; ------------------------------------------------------

(define sparked-roles
  (lambda (part)
    (over-threshold *spark-threshold* (spark-all part))))

(define topo-tip-flip
  (lambda (f-ls)
    (if
	(null? f-ls) '()
	(let
	    ([processed
	      (case (caar f-ls)
		[curve (tip-flip (car f-ls))]
		[curve1 (tip-flip (car f-ls))]
		[curve2 (tip-flip (car f-ls))]
		[contact (tip-flip (car f-ls))]
		[ends (list (caar f-ls) (pair-flip (cadar f-ls)))]
		[tips (list (caar f-ls) (pair-flip (cadar f-ls)))]
		[else (car f-ls)])])
	  (cons processed (topo-tip-flip (cdr f-ls)))))))

(define spark-all
  (lambda (part)
    (let*
	([all-features (get-features part)]
	 [features (just-atoms all-features)]
	 [topos (not-atoms all-features)]
	 [flip-topos (topo-tip-flip topos)]
	 [spark-it
	  (lambda (role-name)
	    (let*
		([role (eval role-name)]
		 [type (lookup 'topology role)])
	      (spark-calc type features topos flip-topos role-name)))])
      (map spark-it (active-nodes *roles*)))))

(define spark-calc
  (lambda (type features topos flip-topos role-name)
    (let*
	([role (eval role-name)]
	 [norms (get-norms role)]
	 [feature-points (part-feature-spark features norms)]
	 [topology-info (part-topo-spark type topos flip-topos norms)]
	 [topology-points (car topology-info)])
      (list
       role-name
       (max 0
	    (round-3
	     (+ feature-points topology-points
		(* 2 (min feature-points topology-points)))))
       (cadr topology-info)))))

(define spark-test
  (lambda (part role)
    (let*
	([type (lookup 'topology role)]
	 [all-features (get-features part)]
	 [features (just-atoms all-features)]
	 [topos (not-atoms all-features)]
	 [flip-topos (topo-tip-flip topos)]
	 [norms (get-norms role)]
	 [feature-points (part-feature-spark features norms)]
	 [topology-info (part-topo-spark type topos flip-topos norms)]
	 [topology-points (car topology-info)])
      (list feature-points topology-points))))

(define part-feature-spark
  (lambda (features norms)
    (let
	([feat-score (features-tally features norms)])
      (round-3 (scale-to-top feat-score *max-feature-score*)))))

(define part-topo-spark
  (lambda (type topos flip-topos norms)
    (let*
	([normal-score (topology-tally type topos norms)]
	 [flip-score (topology-tally type flip-topos norms)]
	 [choice (if (> normal-score flip-score) 'normal 'flipped)]
	 [topo-score
	  (max normal-score flip-score)])
      (list
       (round-3 (scale-to-top topo-score (max-typo-score type)))
       choice))))

; ------------------------------------------------------


(define features-tally
  (lambda (flist nlist)
    (cond
     ((null? flist) 0)
     (else (+ (lookup-or-punish (car flist) nlist)
			    (features-tally (cdr flist) nlist))))))

(define topology-tally
  (lambda (type topos nlist)
    (cond
     ((null? topos) 0)
     (else (+ (topology-score type (car topos) nlist)
	      (topology-tally type (cdr topos) nlist))))))

(define topology-tally
  (lambda (type topos nlist)
    (let
	([item-score
	  (lambda (item)
	    (topology-score type item nlist))])
      (apply + (map item-score topos)))))

(define topology-flip-tally
  (lambda (type flip-topos nlist)
    (cond
     ((null? flip-topos) 0)
     (else (+ (topology-flip-score type (car flip-topos) nlist)
	      (topology-flip-tally type (cdr flip-topos) nlist))))))

(define topology-score
  (lambda (type item norms)
    (case (car item)
      [tips
       (tips-score (cadr item)
		   (lookup-list 'tips norms))]
      [ends
       (ends-score (cadr item)
		   (lookup-list 'ends norms))]
      [neighborhood
       (neighbor-score (cadr item)
			   (car (lookup-list 'neighborhood norms)))]
      [contact
       (contact-score (cadr item)
		   (apply append
			  (map cdr (lookup-list 'contact norms))))]
      [shape
       (shape-score (cadr item)
			  (car (lookup-list 'shape norms)))]
      [curve
       (if (eq? type 'segment)
	   (curve-score (cadr item) (car (lookup-list 'curve norms)))
	   0)]
      [curve1
       (if (eq? type 'bisegment)
	   (curve-score (cadr item) (car (lookup-list 'curve1 norms)))
	   0)]
      [curve2
       (if (eq? type 'bisegment)
	   (curve-score (cadr item) (car (lookup-list 'curve2 norms)))
	   0)]     
      [else 0])))

(define pair-flip
  (lambda (pair)
    (cons (cadr pair) (list (car pair)))))

(define tips-score
  (lambda (tips norms)
    (if (null? norms) 0
	  (+
	   (lookup-or-punish-hard (car (car tips)) (cdar (car norms)))
	   (lookup-or-punish-hard (cadr (car tips)) (cdadr (car norms)))
	   (lookup-or-punish-hard (car (cadr tips)) (cdar (cadr norms)))
	   (lookup-or-punish-hard (cadr (cadr tips)) (cdadr (cadr norms)))))))

(define ends-score
  (lambda (ends norms)
    (if (null? norms) 0
	  (+
	   (lookup-or-punish-hard (car ends) (car norms))
	   (lookup-or-punish-hard (cadr ends) (cadr norms))))))

(define contact-match
  (lambda (touch pat)
    (if (eq? touch (car pat)) (cadr pat) *punish-hard*)))

(define contact-score
  (lambda (touch pats)
    (cond
     ((null? pats) *punish*)
     (else (max (contact-match touch (car pats))
		(contact-score touch (cdr pats)))))))

(define closure-score
  (lambda (observed norm)
    (if (eq? observed norm) 20 *punish-hard*)))

(define curve-score
  (lambda (feature pats)
    (cond
     ((null? pats) 0)
     (else (dimension-score feature pats)))))

(define shape-score
  (lambda (feature pats)
    (cond
     ((null? pats) 0)
     (else (lookup-or-punish feature pats)))))

(define neighborhood-pattern-match
  (lambda (neighbors pat)
    (if (neighbor-watch-match (car pat) neighbors)
	(cadr pat)
	*punish-hard*)))

(define neighbor-score
  (lambda (neighbors pats)
    (cond
     ((null? pats) 0)
     (else (max (neighborhood-pattern-match neighbors (car pats))
		(neighbor-score neighbors (cdr pats)))))))

; the standard for half the sparking
(define scale-to-top
  (lambda (n max)
    (* 25.0 (/ n max))))

; *max-feature-score*
(set! *max-feature-score* 70)

; most points each type can get in the topological check
(define max-typo-score
  (lambda (type)
     (case type
	[loop 80]
	[dot 80]
	[segment 210]
	[bisegment 220]
	[else 'error])))

; -----------------------------------------------------------------------

; haven't added nepotism or veto to new version, 12/2/98

; act-pair is the role-name and the activation

; ought to only spark if value is bigger than old act

(define handle-role-act
  (lambda (spark-entry part)
    (begin
      (bind-role-part (car spark-entry) part (caddr spark-entry))
      (set-activation (car spark-entry) (cadr spark-entry)))))

(define activate-roles-with-part
  (lambda (part gen)
    (let* ([handle-for-part
	    (lambda (spark-entry)
	      (handle-role-act spark-entry part))]
	   [spark-roles (sparked-roles part)])
      (begin
	(unbind-part part)
	(map handle-for-part spark-roles)
	(not (null? spark-roles))))))

; nepotism gives constant factor to role depending upon activation
; let * (nepotism c_act)
	
;veto power
; (draw-grid-role rname (min n_act 100))
; (draw-role-activation rname (min n_act 100))))

(define active-nodes
  (lambda (nls)
    (cond
     ((null? nls) '())
     ((>= (get-activation (car nls)) 0)
      (cons (car nls) (active-nodes (cdr nls))))
     (else (active-nodes (cdr nls))))))

; -----------test sparking ------------------------------

(define topology-item-test
  (lambda (item topology norms)
    (list (car item) (topology-score topology item norms))))

(define topology-test
  (lambda (flist topology nlist)
    (cond
     ((null? flist) '())
     (else (cons (topology-item-test (car flist) topology nlist)
	      (topology-test (cdr flist) topology nlist))))))

(define part-topo-test
  (lambda (part role-name)
    (let*
	([role (eval role-name)]
	 [type (lookup 'topology role)]
	 [features (not-atoms (get-features part))])
      (list
       (list
	'normal
	(topology-test features type (get-norms role)))
       (list
	'flipped
	(topology-test (topo-tip-flip features) type (get-norms role)))))))

; Scaled punishment
; last planned addition to Examiner, 3/4/99
; "Let the punishment fit the crime."
; features which do not appear in the norm list will be punished according
; to how far on the dimension they are from norms

(set! *feature-lists*
      '(*heights-list* *widths-list* *weights-list* *left-edge-list*
		       *right-edge-list* *roof-list* *floor-list*
		       *curves-list*))

(define feature-dimension-look
  (lambda (feature lists)
    (cond
     [(null? lists) 'error]
     [(member? feature (eval (car lists))) (car lists)]
     [else (feature-dimension-look feature (cdr lists))])))

(define feature-dimension-ls
  (lambda (feature)
    (feature-dimension-look feature *feature-lists*)))

(define feature-dimension
  (lambda (feature)
    (case (feature-dimension-ls feature)
      [*heights-list* 'height]
      [*widths-list* 'width]
      [*weights-list* 'weight]
      [*left-edge-list* 'left-edge]
      [*right-edge-list* 'right-edge]
      [*roof-list* 'roof]
      [*floor-list* 'floor]
      [*curves-list* 'curve]
      [else 'dunno])))

(define dimension-distance
  (lambda (feature norms)
    (let*
	([the-dimension (eval (feature-dimension-ls feature))]
	 [acceptable (intersect the-dimension norms)]
	 [dimension-order
	  (lambda (item)
	    (order item the-dimension))]
	 [acceptable-orders (map dimension-order acceptable)]
	 [max-order (apply max acceptable-orders)]
	 [min-order (apply min acceptable-orders)]
	 [feature-order (order feature the-dimension)])
      (min
       (abs (- feature-order max-order))
       (abs (- feature-order min-order))))))

(define dimension-score
  (lambda (feature norms)
    (let
	([norm-vals (map car norms)])
      (if (member? feature norm-vals)
	  (lookup-or-punish feature norms)
	  (* (dimension-distance feature norm-vals) *punish*)))))

(define dimension-score-hard
  (lambda (feature norms)
    (let
	([norm-vals (map car norms)])
      (if (member? feature norm-vals)
	  (lookup-or-punish feature norms)
	  (* (dimension-distance feature norm-vals) *punish-hard*)))))
