; Rules.ss
; All the code for the 30 abstract rules
; 15 zones, 6 orientations, 6 angles, 3 lengths

; Major categories of rules
; Zones, Orientations, Angles, Continuations

(set! *rule-types*
  '(ban-topedge ban-topbox ban-ascender ban-descender ban-bottombox
		ban-bottomedge ban-rightedge ban-leftedge
		ban-vertedges ban-vertboxes ban-topleft ban-topright
		ban-bottomleft ban-bottomright ban-corners
		ban-horiz-quanta ban-vert-quanta ban-foreslash-quanta
		ban-backslash-quanta ban-rectilinear-quanta
		ban-diagonal-quanta ban-45-angle ban-90-angle
		ban-135-angle ban-180-angle ban-45-135-angle
		ban-90-180-angle at-least-2 at-most-2 at-most-3))

; called by the constable codelet which picks a random type

(define rule-type
  (lambda (rtype)
    (case rtype
	[ban-topedge (not (any-in-whole *topedge-zone*))]
	[ban-topbox (not (any-in-whole *topbox-zone*))]
	[ban-ascender (not (any-in-whole *ascender-zone*))]
	[ban-descender (not (any-in-whole *descender-zone*))]
	[ban-bottombox (not (any-in-whole *bottombox-zone*))]
	[ban-bottomedge (not (any-in-whole *bottomedge-zone*))]
	[ban-rightedge (not (any-in-whole *rightedge-zone*))]
	[ban-leftedge (not (any-in-whole *leftedge-zone*))]
	[ban-vertedges (not (any-in-whole *vertedges-zone*))]
	[ban-vertboxes (not (any-in-whole *vertboxes-zone*))]
	[ban-topleft (not (any-in-whole *topleft-zone*))]
	[ban-topright (not (any-in-whole *topright-zone*))]
	[ban-bottomleft (not (any-in-whole *bottomleft-zone*))]
	[ban-bottomright (not (any-in-whole *bottomright-zone*))]
	[ban-corners (not (any-in-whole *corners-zone*))]
	[ban-horiz-quanta (not (any-in-whole *horizontals*))]
	[ban-vert-quanta (not (any-in-whole *verticals*))]
	[ban-foreslash-quanta (not (any-in-whole *foreslashes*))]
	[ban-backslash-quanta (not (any-in-whole *backslashes*))]
	[ban-rectilinear-quanta (not (any-in-whole *rectilinears*))]
	[ban-diagonal-quanta (not (any-in-whole *diagonals*))]
	[ban-45-angle (not (test-sublists *45-degrees*))]
	[ban-90-angle (not (test-sublists *90-degrees*))]
	[ban-135-angle (not (test-sublists *135-degrees*))]
	[ban-180-angle (not (test-sublists *180-degrees*))]
	[ban-45-135-angle (not (or (test-sublists *45-degrees*)
			       (test-sublists *135-degrees*)))]
	[ban-90-180-angle (not (or (test-sublists *90-degrees*)
			       (test-sublists *180-degrees*)))]
	[at-least-2 (not (at-least-two-test))]
	[at-most-2 (not (test-sublists *at-most-two*))]
	[at-most-3 (not (test-sublists *at-most-three*))]
	[else (error 'rtype "no such rule type ~s~%" rtype)])))
      

; rule norms
; these are the ones you should notice if they're enforced
(define whole-rule-norms
  (lambda (whole)
    (lookup (get-category whole) *category-norms*)))

; ------------------------------------------------------------------------
; Tests for abstract rules
; Tests returning true indicate that the rule was broken
; ------------------------------------------------------------------------

; angles

; iff the entire list is filled, then return true
(define all-in-whole
  (lambda (ls)
    (if (= (length ls) (length (intersect ls *quanta-list*)))
	#t
	#f)))

; iff all the members of any of the sublists are turned on,
; then we have broken the rule
; will work for all Angle rules and the two at-most Continuation rules
(define test-sublists
  (lambda (ls)
    (eval (cons 'or (mapcar all-in-whole ls)))))
 
; iff any of the members of the list are turned on,
; then we have broken the rule
; will work for all zone and orientation rules
(define any-in-whole
  (lambda (ls)
    (if (> (length (intersect ls *quanta-list*)) 0)
	#t
	#f)))

; not "hole-in-one"
(define one-in-whole
  (lambda (ls)
    (if (= (length (intersect ls *quanta-list*)) 1)
	#t
	#f)))

; if q is a vertical quantum which is on, and neither of its vertical
; neighbors are, then return true
(define vert-singleton
  (lambda (q)
    (if (and
	 (member? q *quanta-list*)
	 (=
	  (length
	   (intersect
	    *quanta-list*
	    (intersect (lookup q *neighbors*) *verticals*)))
	  0))
	#t
	#f)))

; the one special-case oddball rule
(define at-least-two-test
  (lambda ()
    (or
     (eval (cons 'or (mapcar vert-singleton *verticals*)))
     (eval (cons 'or (mapcar one-in-whole *horiz-diag-pairs*))))))

