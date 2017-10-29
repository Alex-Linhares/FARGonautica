;=============================================================================
; r-roles.ss : a set of routines to process r-roles
;=============================================================================
; also some top-down codelets at the bottom
; there are three types of r-role, all concerned with the parts
; comprising the role:
; FILLED -- are all the roles in the whole filled?
; COVERED -- is all the stuff in the gridletter accounted for by fillers
;            of thew whole's roles?
; TOUCH -- do the parts touch in the right way? A potential norm-violation

; outdated parameters
(set! *filled-penalty* -50)
(set! *covered-penalty* -50)
(set! *touch-penalty* -50)

(set! *r-role-boost* 30)

(define whole-roles
  (lambda (whole)
    (lookup 'roles (eval whole))))

;=============================================================================
; FILLED
;=============================================================================

(define filled?
  (lambda (role)
    (not (null? (get-role-bindings role)))))

(define whole-filled?
  (lambda (whole)
    (eval (cons 'and (map filled? (whole-roles whole))))))

;=============================================================================
; COVERED
;=============================================================================

; THIS WORKS

(define match-all
  (lambda (item ls)
    (let
	([match-item
	  (lambda (other)
	    (if (atom? other)
		(list item other)
		(cons item other)))])
      (map match-item ls))))

(define match-two-lists
  (lambda (ls1 ls2)
    (let
	([match-all-item
	  (lambda (other)
	    (match-all other ls2))])
      (apply append (map match-all-item ls1)))))

(define lists-matchings
  (lambda (ls-ls)
    (let
	([len (length ls-ls)])
      (cond
       [(eq? len 0) '()]
       [(eq? len 1) (map list (car ls-ls))]
       [(eq? len 2) (match-two-lists (car ls-ls) (cadr ls-ls))]
       [else (match-two-lists (car ls-ls) (lists-matchings (cdr ls-ls)))]))))

(define spark-combos
  (lambda (expected)
    (let
	([in-whole
	  (lambda (role-ls)
	    (intersect role-ls expected))])
      (lists-matchings
       (map in-whole (parts-sparked-roles *workspace*))))))

(define whole-covered?
  (lambda (whole)
    (let*
	([expected (whole-roles whole)]
	 [bound-roles
	  (uniquify
	   (apply append (map get-part-bindings *workspace*)))]
	 [all-bound? (eq? (length expected)
			  (length (intersect bound-roles expected)))])
      (if (and all-bound?
	       (eq? (length *workspace*) (length expected)))
	  (let
	      ([sparked (spark-combos expected)]
	       [found-expected
		(lambda (found)
		  (same-contents expected found))])
	    (eval (cons 'or (map found-expected sparked))))
	  #f))))

;=============================================================================
; TOUCH
;=============================================================================

; note: both quoted
(define touch-norms
  (lambda (whole role)
    (lookup-list whole (lookup-list 'contact (get-norms (eval role))))))

(define touch-role-filler
  (lambda (part)
    (lookup-list 'contact (not-atoms part))))

(define touch-role-fillers
  (lambda (role)
    (uniquify
     (apply append
	    (map touch-role-filler
		    (map car (get-role-bindings role)))))))

(define touch-test
  (lambda (filler-touch whole role)
    (let*
	([order-list (map cadr (get-role-bindings role))]
	 [norms? (member? 'normal order-list)]
	 [both? (and (member? 'flipped order-list) norms?)])
      (cond
       [both?
	(max
	 (lookup-score filler-touch
		       (touch-norms whole role))
	 (lookup-score (list-substitute filler-touch *tip-flip-list*)
		       (touch-norms whole role)))]
       [norms?
	 (lookup-score filler-touch
		       (touch-norms whole role))]
       [else
	(lookup-score (list-substitute filler-touch *tip-flip-list*)
		      (touch-norms whole role))]))))	

(define role-touch-test
  (lambda (whole role)
    (let
        ((test-filler
          (lambda (filler)
            (touch-test filler whole role))))
      (apply max (cons 0 (map test-filler
                                 (touch-role-fillers role)))))))
(define whole-touch-test
  (lambda (whole)
    (let*
	([roles (whole-roles whole)]
	 [touch-test-role
	  (lambda (role)
	    (role-touch-test whole role))]
	 [score (apply min (map touch-test-role roles))])
      (if (< score 1) *touch-penalty* score))))

;=============================================================================
; R-ROLES, general code
;=============================================================================

(define r-role-score
  (lambda (whole)
    (+
     (if (whole-filled? whole) 0 *filled-penalty*)
     (if (whole-covered? whole) 0 *covered-penalty*)
     (whole-touch-test whole))))

; don't punish an active whole into the negatives; just whap it

(define r-role-check-whole
  (lambda (whole)
    (let*
	([category (cadr (cadar (eval whole)))]
	 [old-activation (get-activation whole)])
      (if (> old-activation 40)
	  (let
	      ([new-activation
		(min 100 (max 1
			      (+ old-activation
				 (r-role-score whole))))])
	    (set-activation whole new-activation))))))

(define r-role-check-wholes
  (lambda (wholes)
    (cond
     [(null? wholes) #t]
     [else
      (begin
	(r-role-check-whole (car wholes))
	(r-role-check-wholes (cdr wholes)))])))

; GEM r-roles
; check and do graphics and maybe post solver codelet
; oughta add the latter functions to the ultra-sleek JAR version
