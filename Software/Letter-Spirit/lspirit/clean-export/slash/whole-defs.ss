(set! a1
      '((names (a1 a a1-act))
	(roles (a-arch left-downbowl))))
(set! a2
      '((names (a2 a a2-act))
	(roles (a-arch down-circle))))
(set! b1
      '((names (b1 b b1-act))
	(roles (left-post right-bowl))))
(set! b2
      '((names (b2 b b2-act))
	(roles (left-post circle))))
(set! c1
      '((names (c1 c c1-act))
	(roles (left-bowl))))
(set! d1
      '((names (d1 d d1-act))
	(roles (right-post left-bowl))))
(set! d2
      '((names (d2 d d2-act))
	(roles (right-post circle))))
(set! e1
      '((names (e1 e e1-act))
	(roles (e-bowl e-crossbar))))
(set! e2
      '((names (e2 e e2-act))
	(roles (up-circle e-tail))))
(set! f1
      '((names (f1 f f1-act))
	(roles (f-post crossbar))))
(set! g1
      '((names (g1 g g1-act))
	(roles (right-hook left-bowl))))
(set! g2
      '((names (g2 g g2-act))
	(roles (right-hook circle))))
(set! h1
      '((names (h1 h h1-act))
	(roles (left-post right-buttress))))
(set! i1
      '((names (i1 i i1-act))
	(roles (halfpost dot))))
(set! j1
      '((names (j1 j j1-act))
	(roles (right-curl dot))))
(set! k1
      '((names (k1 k k1-act))
	(roles (left-post up-arm down-arm))))
(set! l1
      '((names (l1 l l1-act))
	(roles (center-post))))
(set! m1
      '((names (m1 m m1-act))
	(roles (left-halfpost left-halfarch right-halfarch))))
(set! n1
      '((names (n1 n n1-act))
	(roles (left-halfpost right-buttress))))
(set! o1
      '((names (o1 o o1-act))
	(roles (circle))))
(set! p1
      '((names (p1 p p1-act))
	(roles (left-tail right-bowl))))
(set! p2
      '((names (p2 p p2-act))
	(roles (left-tail circle))))
(set! q1
      '((names (q1 q q1-act))
	(roles (right-tail left-bowl))))
(set! q2
      '((names (q2 q q2-act))
	(roles (right-tail circle))))
(set! r1
      '((names (r1 r r1-act))
	(roles (left-halfpost cap))))
(set! s1
      '((names (s1 s s1-act))
	(roles (cap s-crossbar s-base))))
(set! s2
      '((names (s2 s s2-act))
	(roles (left-upbowl right-downbowl))))
(set! t1
      '((names (t1 t t1-act))
	(roles (t-post crossbar))))
(set! u1
      '((names (u1 u u1-act))
	(roles (right-halfpost left-uparc))))
(set! v1
      '((names (v1 v v1-act))
	(roles (left-wing right-wing))))
(set! w1
      '((names (w1 w w1-act))
	(roles (right-halfpost left-halfarc right-halfarc))))
(set! x1
      '((names (x1 x x1-act))
	(roles (foreslash backslash))))
(set! y1
      '((names (y1 y y1-act))
	(roles (right-curl left-uparc))))
(set! y2
      '((names (y2 y y2-act))
	(roles (right-curl backslash))))
(set! z1
      '((names (z1 z z1-act))
	(roles (z-cap foreslash basebar))))

(set! *categories*
      '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(set! *wholes* '(a1 a2 b1 b2 c1 d1 d2 e1 e2 f1 g1 g2 h1 i1 j1 k1 l1 m1
      n1 o1 p1 p2 q1 q2 r1 s1 s2 t1 u1 v1 w1 x1 y1 y2 z1))

; these are the ones where a rule being upheld is noteworthy
(set!
 *category-norms*
 '((a (ban-rightedge ban-leftedge ban-horiz-quanta ban-vert-quanta
		     ban-foreslash-quanta ban-backslash-quanta
		     ban-rectilinear-quanta ban-diagonal-quanta
		     ban-45-angle ban-90-angle ban-135-angle ban-180-angle
		     ban-45-135-angle ban-90-180-angle at-least-2))
   (b (ban-topbox ban-ascender ban-rightedge ban-leftedge ban-vertboxes
		  ban-topleft ban-corners ban-horiz-quanta ban-vert-quanta
		  ban-foreslash-quanta ban-backslash-quanta
		  ban-rectilinear-quanta ban-diagonal-quanta ban-45-angle
		  ban-90-angle ban-135-angle ban-180-angle ban-45-135-angle
		  ban-90-180-angle at-least-2 at-most-2 at-most-3))
   (c (ban-leftedge ban-horiz-quanta ban-vert-quanta ban-foreslash-quanta
		    ban-backslash-quanta ban-rectilinear-quanta
		    ban-diagonal-quanta ban-90-angle ban-135-angle
		    ban-180-angle ban-45-135-angle
		    ban-90-180-angle at-least-2))
   (d (ban-topbox ban-ascender ban-rightedge ban-leftedge ban-vertboxes
		  ban-topright ban-corners ban-horiz-quanta ban-vert-quanta
		  ban-foreslash-quanta ban-backslash-quanta
		  ban-rectilinear-quanta ban-diagonal-quanta ban-45-angle
		  ban-90-angle ban-135-angle ban-180-angle ban-45-135-angle
		  ban-90-180-angle at-least-2 at-most-2 at-most-3))
   (e (ban-rightedge ban-leftedge ban-horiz-quanta ban-vert-quanta
		     ban-foreslash-quanta ban-backslash-quanta
		     ban-rectilinear-quanta ban-diagonal-quanta ban-45-angle
		     ban-90-angle ban-135-angle ban-180-angle
		     ban-45-135-angle ban-90-180-angle at-least-2))
   (f (ban-topedge ban-topbox ban-ascender ban-vertedges ban-vertboxes
		   ban-topright ban-corners ban-horiz-quanta ban-vert-quanta
		   ban-foreslash-quanta ban-rectilinear-quanta
		   ban-diagonal-quanta ban-90-angle ban-135-angle
		   ban-180-angle ban-45-135-angle ban-90-180-angle at-least-2
		   at-most-2 at-most-3))
   (g (ban-descender ban-bottombox ban-bottomedge ban-rightedge
		     ban-leftedge ban-vertedges ban-vertboxes ban-bottomright
		     ban-corners ban-horiz-quanta ban-vert-quanta
		     ban-foreslash-quanta ban-backslash-quanta
		     ban-rectilinear-quanta ban-diagonal-quanta ban-45-angle
		     ban-90-angle ban-135-angle ban-180-angle
		     ban-45-135-angle ban-90-180-angle at-least-2 at-most-2
		     at-most-3))
   (h (ban-topbox ban-ascender ban-rightedge ban-leftedge ban-vertboxes
		  ban-topleft ban-corners ban-horiz-quanta ban-vert-quanta
		  ban-foreslash-quanta ban-backslash-quanta
		  ban-rectilinear-quanta ban-diagonal-quanta ban-45-angle
		  ban-90-angle ban-135-angle ban-180-angle ban-45-135-angle
		  ban-90-180-angle at-least-2 at-most-2 at-most-3))
   (i (ban-topbox ban-ascender ban-rightedge ban-leftedge ban-vertboxes
		  ban-horiz-quanta ban-vert-quanta ban-foreslash-quanta
		  ban-rectilinear-quanta ban-diagonal-quanta ban-90-angle
		  ban-180-angle ban-90-180-angle at-least-2))
   (j (ban-topbox ban-ascender ban-descender ban-bottombox ban-bottomedge
		  ban-rightedge ban-vertedges ban-vertboxes ban-topright
		  ban-bottomleft ban-bottomright ban-corners ban-horiz-quanta
		  ban-vert-quanta ban-foreslash-quanta ban-rectilinear-quanta
		  ban-diagonal-quanta ban-90-angle ban-135-angle
		  ban-180-angle ban-45-135-angle ban-90-180-angle at-least-2
		  at-most-2 at-most-3))
   (k (ban-topbox ban-ascender ban-leftedge ban-vertboxes ban-topleft
		  ban-corners ban-horiz-quanta ban-vert-quanta
		  ban-foreslash-quanta ban-backslash-quanta
		  ban-rectilinear-quanta ban-diagonal-quanta ban-45-angle
		  ban-90-angle ban-135-angle ban-180-angle ban-45-135-angle
		  ban-90-180-angle at-least-2 at-most-2 at-most-3))
   (l (ban-topbox ban-ascender ban-leftedge ban-vertboxes ban-topleft
		  ban-corners ban-vert-quanta ban-backslash-quanta
		  ban-rectilinear-quanta ban-diagonal-quanta ban-135-angle
		  ban-180-angle ban-45-135-angle
		  ban-90-180-angle at-least-2  at-most-2 at-most-3))
   (m (ban-rightedge ban-leftedge ban-horiz-quanta ban-vert-quanta
		     ban-foreslash-quanta ban-backslash-quanta
		     ban-rectilinear-quanta ban-diagonal-quanta ban-45-angle
		     ban-90-angle ban-135-angle ban-180-angle
		     ban-45-135-angle ban-90-180-angle at-least-2))
   (n (ban-rightedge ban-leftedge ban-horiz-quanta ban-vert-quanta
		     ban-foreslash-quanta ban-backslash-quanta
		     ban-rectilinear-quanta ban-diagonal-quanta ban-45-angle
		     ban-90-angle ban-135-angle ban-180-angle
		     ban-45-135-angle ban-90-180-angle at-least-2))
   (o (ban-rightedge ban-leftedge ban-horiz-quanta ban-vert-quanta
		     ban-foreslash-quanta ban-backslash-quanta
		     ban-rectilinear-quanta ban-diagonal-quanta ban-90-angle
		     ban-180-angle ban-90-180-angle at-least-2))
   (p (ban-descender ban-bottombox ban-rightedge ban-leftedge ban-vertboxes
		     ban-bottomleft ban-corners ban-horiz-quanta
		     ban-vert-quanta ban-foreslash-quanta
		     ban-backslash-quanta ban-rectilinear-quanta
		     ban-diagonal-quanta ban-45-angle ban-90-angle
		     ban-135-angle ban-180-angle ban-45-135-angle
		     ban-90-180-angle at-least-2 at-most-2 at-most-3))
   (q (ban-descender ban-bottombox ban-rightedge ban-leftedge ban-vertboxes
		     ban-bottomright ban-corners ban-horiz-quanta
		     ban-vert-quanta ban-foreslash-quanta
		     ban-backslash-quanta ban-rectilinear-quanta
		     ban-diagonal-quanta ban-45-angle ban-90-angle
		     ban-135-angle ban-180-angle ban-45-135-angle
		     ban-90-180-angle at-least-2 at-most-2 at-most-3))
   (r (ban-leftedge ban-horiz-quanta ban-vert-quanta ban-foreslash-quanta
		    ban-rectilinear-quanta ban-diagonal-quanta ban-45-angle
		    ban-90-angle ban-135-angle ban-180-angle ban-45-135-angle
		    ban-90-180-angle at-least-2))
   (s (ban-rightedge ban-leftedge ban-horiz-quanta ban-vert-quanta
		     ban-foreslash-quanta ban-rectilinear-quanta
		     ban-diagonal-quanta ban-45-angle ban-90-angle
		     ban-135-angle ban-180-angle ban-45-135-angle
		     ban-90-180-angle at-least-2))
   (t (ban-ascender ban-horiz-quanta ban-vert-quanta ban-foreslash-quanta
		    ban-rectilinear-quanta ban-diagonal-quanta
		    ban-45-angle ban-90-angle ban-180-angle ban-45-135-angle
		    ban-90-180-angle at-least-2 at-most-2 at-most-3))
   (u (ban-rightedge ban-leftedge ban-horiz-quanta ban-vert-quanta
		     ban-foreslash-quanta ban-rectilinear-quanta
		     ban-diagonal-quanta ban-45-angle ban-90-angle
		     ban-135-angle ban-180-angle ban-45-135-angle
		     ban-90-180-angle at-least-2))
   (v (ban-rightedge ban-leftedge ban-vert-quanta ban-foreslash-quanta
		     ban-backslash-quanta ban-rectilinear-quanta
		     ban-diagonal-quanta ban-90-angle ban-135-angle
		     ban-45-135-angle ban-90-180-angle at-least-2))
   (w (ban-rightedge ban-leftedge ban-horiz-quanta ban-vert-quanta
		     ban-foreslash-quanta ban-backslash-quanta
		     ban-rectilinear-quanta ban-diagonal-quanta
		     ban-45-angle ban-90-angle ban-135-angle ban-180-angle
		     ban-45-135-angle ban-90-180-angle at-least-2))
   (x (ban-foreslash-quanta ban-backslash-quanta ban-diagonal-quanta
			    ban-90-angle ban-180-angle ban-90-180-angle
			    at-least-2))
   (y (ban-descender ban-bottombox ban-bottomedge ban-rightedge ban-leftedge
		     ban-vertedges ban-vertboxes ban-bottomright ban-corners
		     ban-horiz-quanta ban-vert-quanta ban-foreslash-quanta
		     ban-backslash-quanta ban-rectilinear-quanta
		     ban-diagonal-quanta ban-45-angle ban-90-angle
		     ban-135-angle ban-180-angle ban-45-135-angle
		     ban-90-180-angle at-least-2 at-most-2 at-most-3))
   (z (ban-horiz-quanta ban-foreslash-quanta ban-rectilinear-quanta
			ban-diagonal-quanta ban-45-angle ban-180-angle
			ban-45-135-angle ban-90-180-angle at-least-2))))
