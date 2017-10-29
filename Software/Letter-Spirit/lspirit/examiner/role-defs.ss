; make sure there are no ties for the lead if you want
; to use that for norm-violations
(set! a-arch
      '((names (a-arch a-arch a-arch-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 30) (*cupped* 10) (*spiky-closure* -10)))
	 (neighborhood (((dc n dc dc dc y dc dc) 20)))
	 (contact (a1 (mt2 20) (2ms 15) (t2 11))
		  (a2 (t2 13) (m 20)))
	 (tips ((location (3 10) (4 7) (10 6) (11 6))
		(orientation (*w* 10) (*n* 6) (*sw* 7) (*nw* 7) (*s* 6)))
	       ((location (19 10) (18 7) (12 6))
		(orientation (*s* 10) (*se* 6) (*sw* 6) (*e* 7) (*n* 7))))
	 (ends
	  (((4 *w*) 40) ((20 *n*) 10) ((36 *sw*) 20) ((48 *nw*) 20)
	   ((5 *w*) 10) ((20 *s*) 10) ((37 *sw*) 10))
	  (((25 *s*) 40) ((51 *se*) 7) ((22 *s*) 7) ((25 *n*) 13)
	   ((39 *sw*) 7) ((9 *e*) 7)))
	 (curve
	  ((*square-right* 10) (*straight* 9) (*slight-right* 9)
	   (*slight-left* 7) (*strong-right* 7)))
	 (short 10) (very-short 6) (wide 10) (half-wide 6)
	 (normal-wt 10) (light 6) (roof-x-height 10)
	 (floor-baseline 10) (floor-midline 6)
	 (l-edge-lf 10) (l-edge-md 6) (r-edge-rt 10))))
(set! backslash
      '((names (backslash b-slash backslash-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 0) (*cupped* 20) (*spiky-closure* -10)))
	 (neighborhood (((dc y dc n dc y dc n) 20)))
	 (contact (x1 (m 20)) (y2 (t2 20)))
	 (tips ((location (3 10) (4 9) (10 9) (5 7))
		(orientation (*nw* 10) (*n* 9) (*ne* 9)
			     (*sw* 8) (*s* 9) (*e* 7) (*w* 7)))
	       ((location (19 10) (18 7) (12 7))
		(orientation (*se* 10) (*n* 8) (*s* 7)
			     (*e* 9) (*sw* 8) (*w* 7))))
	 (ends
	    (((48 *nw*) 40) ((20 *n*) 20) ((36 *ne*) 40) ((36 *sw*) 40)
	     ((20 *s*) 40) ((4 *e*) 20) ((21 *n*) 20) ((4 *w*) 20)
	     ((38 *sw*) 20))
	    (((51 *se*) 40) ((25 *n*) 20) ((25 *s*) 10) ((9 *e*) 30)
	     ((39 *sw*) 20) ((9 *w*) 10)))
	 (curve
	  ((*straight* 10) (*square-left* 7) (*slight-left* 7)
	   (*slight-right* 7) (*square-right* 8)))
	 (short 10) (very-short 6) (wide 10) (half-wide 6) (light 7)
	 (heavy 6) (normal-wt 10) (roof-x-height 10)
	 (roof-midline 6) (floor-baseline 10)
	 (l-edge-lf 10) (l-edge-md 6) (r-edge-rt 10))))
(set! basebar
      '((names (basebar b-bar basebar-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 30) (*cupped* 40) (*spiky-closure* -10)))
	 (neighborhood (((y dc dc dc dc dc dc dc) 20)))
	 (contact (z1 (t1 20) (t1-m 11)))
	 (tips ((location (5 10) (4 7) (3 6))
		(orientation (*w* 8) (*n* 6) (*nw* 7) (*e* 6)))
	       ((location (19 8) (17 6) (18 7) (12 6) (20 6))
		(orientation (*e* 9) (*n* 6) (*ne* 6) (*s* 6))))
	 (ends
	  (((8 *w*) 40) ((23 *n*) 8) ((50 *nw*) 16) ((20 *n*) 8)
	   ((8 *e*) 8) ((52 *nw*) 8))
	  (((9 *e*) 40) ((22 *n*) 13) ((39 *ne*) 27) ((8 *e*) 27)
	   ((25 *s*) 13) ((25 *n*) 13) ((11 *e*) 13)))
	 (curve
	  ((*straight* 7) (*square-left* 7)
	   (*slight-left* 7) (*slight-right* 6)))
	 (no-height 7) (short 6) (very-short 9)
	 (wide 10) (half-wide 6) (light 9) (normal-wt 7)
	 (roof-baseline 7) (roof-x-height 6)
	 (roof-midline 8) (floor-baseline 11)
	 (floor-middown 6)
	 (l-edge-lf 10) (r-edge-rt 10) (r-edge-md 6))))
(set! cap
      '((names (cap cap cap-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 40) (*cupped* 40) (*spiky-closure* -10)))
	 (neighborhood (((dc dc n dc dc y dc dc) 20)
			((n n dc dc y dc dc dc) 20)))
	 (contact (r1 (t1 20) (m 11)) (s1 (t1 20) (m 11)))
	 (tips ((location (3 10) (4 9) (10 7) (11 6) (17 6))
		(orientation (*w* 10) (*ne* 7) (*sw* 9) (*s* 8)
			     (*e* 7) (*nw* 6)))
	       ((location (17 10) (10 7) (18 9) (11 6) (16 6) (37 6))
		(orientation (*e* 10) (*ne* 7) (*se* 7) (*n* 7)
			     (*s* 8) (*w* 6) (*sw* 6))))
	 (ends
	  (((4 *w*) 40) ((36 *ne*) 8) ((36 *sw*) 40) ((20 *s*) 8)
	   ((5 *w*) 8) ((48 *nw*) 8) ((4 *e*) 8) ((5 *e*) 8) ((22 *s*) 8)
	   ((21 *s*) 8) ((35 *ne*) 8))
	  (((5 *e*) 40) ((36 *ne*) 10) ((49 *se*) 20) ((22 *n*) 20)
	   ((22 *s*) 40) ((7 *w*) 10) ((4 *e*) 20) ((37 *ne*) 10)
	   ((35 *ne*) 10) ((7 *e*) 10) ((37 *sw*) 10)))
	 (curve
	  ((*straight* 10) (*slight-left* 7) (*square-right* 9)
	   (*strong-right* 6) (*slight-right* 7)))
	 (no-height 6) (very-short 10) (wide 10) (half-wide 8)
	 (skinny 6) (light 10) (normal-wt 8) (roof-x-height 10)
	 (roof-t-height 6) (floor-x-height 7) (floor-midline 10)
	 (l-edge-lf 10) (l-edge-md 7) (l-edge-rt 6) (r-edge-rt 10)
	 (r-edge-md 6))))
(set! center-post
      '((names (center-post c-post center-post-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* -80) (*cupped* 30) (*spiky-closure* -10)))
	 (neighborhood (((n n n n n n n n) 20)))
	 (contact (l1 (nt 20)))
	 (tips ((location (8 10) (15 7) (2 7) (9 7) (1 7))
		(orientation (*n* 10) (*ne* 7) (*s* 6) (*w* 6) (*sw* 6)))
	       ((location (12 7) (19 10) (18 9) (5 6) (17 6))
		(orientation (*s* 9) (*n* 9) (*e* 10) (*se* 8) (*ne* 9)
			     (*sw* 7))))
	 (ends
	  (((15 *n*) 40) ((33 *ne*) 13) ((17 *n*) 13) ((18 *n*) 13)
	   ((15 *s*) 7) ((0 *w*) 7) ((32 *ne*) 7) ((14 *n*) 7) ((32 *sw*) 7))
	  (((24 *s*) 20) ((25 *s*) 10) ((9 *e*) 40) ((25 *n*) 30)
	   ((39 *ne*) 20) ((51 *se*) 20) ((37 *ne*) 10) ((38 *sw*) 10)))
	 (curve
	  ((*straight* 8) (*slight-left* 10) (*square-left* 8)
	   (*strong-left* 6)))
	 (tall 10) (medium 7) (skinny 7) (half-wide 10) (wide 9)
	 (normal-wt 10) (heavy 8) (roof-top 10) (roof-t-height 7)
	 (floor-baseline 10)
	 (l-edge-md 10) (l-edge-lf 9) (r-edge-md 6) (r-edge-rt 10))))
(set! circle
      '((names (circle circle circle-act))
	(topology loop)
	(norms
	 (shape
	  ((*closure* 40) (*spiky-closure* -10)
	   (*bactrian* -80) (*cupped* -80) (*simple* -80)))
	 (neighborhood (((dc dc n n dc dc dc y) 20)    ; b
			((dc y dc dc dc n n dc) 20)    ; d
			((dc dc dc y y dc dc n) 20)    ; g
			((n n n n n n n n) 20)         ; o
			((dc n dc dc dc y dc dc) 20)   ; p
			((dc dc dc y dc dc dc n) 20))) ; q 
	 (contact (o1 (nt 19))
		  (b2 (t 20))
		  (d2 (t 20))
		  (g2 (t 20))
		  (p2 (t 20))
		  (q2 (t 20)))
	 (short 10) (very-short 6) (wide 10) (half-wide 6) (heavy 10)
	 (normal-wt 8) (roof-x-height 10)
	 (roof-midline 6) (floor-baseline 10)
	 (l-edge-lf 10) (l-edge-md 6) (r-edge-rt 10))))
(set! crossbar
      '((names (crossbar c-bar crossbar-act))
	(topology segment) 
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 40) (*cupped* 40) (*spiky-closure* -10)))
	 (neighborhood (((y dc dc dc y dc dc dc) 20)))
	 (contact (f1 (m 20) (t1 14) (t2 11)) (t1 (m 20) (t1 14) (t2 11)))
	 (tips ((location (3 9) (4 10) (5 7) (9 6) (10 6) (2 6))
		(orientation (*w* 9) (*n* 6) (*sw* 10) (*e* 8) (*nw* 6)
			     (*se* 6)))
	       ((location (17 10) (18 8) (10 6) (16 7) (11 6))
		(orientation (*e* 10) (*ne* 8) (*se* 7) (*s* 6))))
	 (ends
	  (((4 *w*) 27) ((6 *w*) 27) ((36 *sw*) 40) ((20 *n*) 13)
	   ((4 *e*) 13) ((6 *e*) 13) ((38 *sw*) 27) ((47 *se*) 13)
	   ((5 *e*) 13) ((46 *nw*) 13))
	  (((5 *e*) 40) ((7 *e*) 10) ((49 *se*) 20) ((35 *ne*) 20)
	   ((4 *e*) 10) ((6 *e*) 10) ((37 *ne*) 20) ((47 *se*) 10)
	   ((22 *s*) 10)))
	 (curve
	  ((*straight* 10) (*slight-left* 7) (*square-right* 6)
	   (*slight-right* 7)))
	 (no-height 9) (short 9) (very-short 10) (wide 10) (half-wide 7)
	 (light 10) (normal-wt 6) (roof-x-height 10) (roof-midline 6)
	 (roof-t-height 7) (floor-x-height 8) (floor-midline 10)
	 (floor-baseline 7) (l-edge-lf 10) (l-edge-md 6)
	 (r-edge-rt 10) (r-edge-md 6))))
(set! dot
      '((names (dot dot dot-act))
	(topology dot)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 40) (*cupped* 40) (*spiky-closure* 40)
	   (*closure* 40)))
	 (neighborhood (((n dc dc dc y dc dc dc) 20)))
	 (contact (i1 (nt 20) (t 11)) (j1 (nt 20) (t 11)))
	 (no-height 6) (very-short 10) (short 6) (half-wide 10)
	 (wide 7) (light 10) (normal-wt 9) (roof-t-height 8)
	 (roof-top 10) (roof-x-height 6) (floor-t-height 10)
	 (floor-x-height 9) (floor-midline 6)
	 (l-edge-md 10) (l-edge-lf 8) (r-edge-rt 10) (r-edge-md 6))))
(set! down-arm
      '((names (down-arm down-arm down-arm-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 10) (*cupped* 20) (*spiky-closure* 0)))
	 (neighborhood (((y y dc n dc dc y y) 20)))
	 (contact (k1 (t1 20) (m 11)))
	 (tips ((location (11 9) (10 7) (4 10) (3 6))
		(orientation (*se* 6) (*n* 6) (*nw* 10) (*w* 9)))
	       ((location (19 10) (18 9) (12 7))
		(orientation (*se* 7) (*n* 10) (*e* 7) (*s* 9) (*ne* 7)
			     (*sw* 8))))
	 (ends
	  (((51 *se*) 13) ((21 *n*) 13) ((50 *nw*) 27) ((6 *w*) 40)
	   ((49 *nw*) 13) ((7 *w*) 27) ((48 *nw*) 13) ((51 *nw*) 13))
	  (((51 *se*) 10) ((25 *n*) 40) ((9 *e*) 10) ((25 *s*) 30)
	   ((39 *ne*) 10) ((39 *sw*) 20)))
	 (curve
	  ((*straight* 7) (*strong-left* 9) (*slight-left* 10)
	   (*square-right* 7) (*square-left* 7) (*strong-right* 9)
	   (*slight-right* 9)))
	 (very-short 10) (short 7) (half-wide 10) (wide 9) (light 10)
	 (normal-wt 9) (roof-midline 10) (roof-x-height 7) (floor-baseline 10)
	 (l-edge-md 9) (l-edge-lf 10) (r-edge-rt 10))))
(set! down-circle
      '((names (down-circle dwn-circ down-circle-act))
	(topology loop)
	(norms
	 (shape
	  ((*closure* 40) (*spiky-closure* 10)))
	 (neighborhood (((y dc dc dc dc n dc dc) 20)))
	 (contact (a2 (nt 11) (t 20) (m 11) (t1 11) (t2 11)))
	 (very-short 10) (short 6) (wide 10) (half-wide 6)
	 (heavy 7) (normal-wt 10) (roof-midline 10)
	 (roof-x-height 6) (floor-baseline 10)
	 (l-edge-lf 10) (r-edge-rt 10) (r-edge-md 6))))
(set! e-bowl
      '((names (e-bowl e-bowl e-bowl-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 10) (*cupped* 10) (*spiky-closure* -10)))
	 (neighborhood (((n n n n n n n n) 20)))
	 (contact (e1 (t1m 20) (t1 12)))
	 (tips ((location (18 10) (10 6) (11 6) (17 7) (19 6))
		(orientation (*s* 10) (*se* 9) (*e* 9) (*sw* 6)))
	       ((location (19 10) (17 6) (18 7) (12 7))
		(orientation (*e* 10) (*ne* 7) (*n* 6) (*se* 6))))
	 (ends
	  (((22 *s*) 40) ((49 *se*) 40) ((5 *e*) 40) ((37 *sw*) 40)
	   ((25 *s*) 8))
	  (((9 *e*) 40) ((39 *ne*) 15) ((8 *e*) 15) ((51 *se*) 5)
	   ((25 *n*) 5)))
	 (curve ((*strong-left* 10)))
	 (short 10) (very-short 6) (wide 10) (heavy 8) (normal-wt 10)
	 (roof-x-height 10) (floor-baseline 10)
	 (l-edge-lf 10) (r-edge-rt 10))))
(set! e-crossbar
      '((names (e-crossbar e-cross e-crossbar-act))
       (topology segment)
       (norms
	 (shape
	  ((*simple* 40) (*bactrian* 10) (*cupped* 40) (*spiky-closure* -10)))
	(neighborhood (((y y y dc y y y y) 20)))
	(contact (e1 (2ts 20) (t2 12) (mt2 11) (2ms 11) (t1m 11)))
	(tips ((location (4 10) (3 9) (5 7) (11 9))
	       (orientation (*w* 10) (*nw* 9) (*sw* 7) (*e* 7) (*ne* 7)
			    (*n* 5)))
	      ((location (18 10) (19 9) (17 9) (11 6))
	       (orientation (*e* 10) (*se* 9) (*ne* 9))))
	(ends
	 (((6 *w*) 40) ((48 *nw*) 27) ((38 *sw*) 13) ((7 *e*) 13)
	  ((37 *ne*) 13) ((21 *n*) 8))
	 (((7 *e*) 40) ((51 *se*) 27) ((6 *e*) 27) ((37 *ne*) 40)))
	(curve
	 ((*straight* 10) (*slight-left* 7) (*slight-right* 6)
	  (*square-left* 6) (*square-right* 6)))
	(no-height 8) (short 8) (very-short 10) (wide 10)
	(half-wide 7) (light 10) (roof-midline 8)
	(roof-x-height 10) (floor-midline 10) (floor-baseline 8)
	(l-edge-lf 10) (l-edge-md 7) (r-edge-rt 10))))
(set! e-tail
      '((names (e-tail e-tail e-tail-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 30) (*cupped* 30) (*spiky-closure* -10)))
	 (neighborhood (((y y dc n n n dc y) 20)))
	 (contact (e2 (t1 20) (m 11)))
	 (tips ((location (4 10) (11 9) (5 7) (3 6) (10 6))
		(orientation (*n* 10) (*ne* 6) (*nw* 8) (*se* 6) (*w* 7)))
	       ((location (19 10) (18 9) (12 6) (5 6))
		(orientation (*e* 10) (*ne* 8) (*w* 6) (*se* 6) (*n* 6))))
	 (ends
	  (((23 *n*) 40) ((24 *n*) 13) ((50 *nw*) 27) ((20 *n*) 13)
	   ((8 *w*) 27) ((38 *ne*) 13) ((51 *nw*) 13) ((51 *se*) 13)
	   ((36 *ne*) 10))
	  (((9 *e*) 40) ((39 *ne*) 24) ((8 *e*) 8) ((51 *se*) 8)
	   ((8 *w*) 8) ((25 *n*) 8)))
	 (curve
	  ((*slight-left* 9) (*square-left* 10) (*strong-right* 7)
	   (*strong-left* 8) (*straight* 8)))
	 (very-short 10) (short 6) (no-height 6) (wide 10)
	 (half-wide 9) (normal-wt 8) (light 10) (roof-midline 10)
	 (roof-x-height 6) (roof-baseline 6) (floor-baseline 10)
	 (l-edge-lf 10) (l-edge-md 7) (r-edge-rt 10) (r-edge-md 6))))
(set! f-post
      '((names (f-post f-post f-post-act))
	(topology bisegment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 10) (*cupped* 30) (*spiky-closure* -10)))
	 (neighborhood (((n n dc n n n dc n) 20)))
	 (contact (f1 (m 20) (nt 11)))
	 (tips ((location (15 8) (17 8) (16 10) (8 6))
		(orientation (*e* 9) (*se* 10) (*s* 7) (*ne* 7)))
	       ((location (12 10) (5 8) (19 7))
		(orientation (*s* 10) (*se* 9) (*sw* 6))))
	 (ends
	  (((1 *e*) 13) ((47 *se*) 27) ((45 *se*) 40) ((3 *e*) 27)
	   ((16 *s*) 13) ((19 *s*) 13) ((0 *e*) 13) ((33 *ne*) 27))
	  (((24 *s*) 40) ((23 *s*) 30) ((50 *se*) 30) ((51 *se*) 20)
	   ((38 *sw*) 10)))
	 (curve1
	  ((*slight-left* 10) (*square-left* 9) (*strong-left* 6)))
	 (curve2
	  ((*straight* 10) (*slight-left* 8) (*slight-right* 6)))
	 (tall 10) (medium 7) (half-wide 7) (wide 10) (normal-wt 10)
	 (heavy 7) (roof-top 10) (roof-t-height 7) (floor-baseline 10)
	 (l-edge-lf 10) (l-edge-md 7) (r-edge-rt 10) (r-edge-md 6))))
(set! foreslash
      '((names (foreslash f-slash foreslash-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 0) (*cupped* 20) (*spiky-closure* -10)))
	 (neighborhood (((dc n dc y dc n dc y) 20)
			((dc dc dc y dc dc dc y) 20)))
	 (contact (x1 (m 20)) (z1 (2ts 20) (t1m 11) (mt2 11)))
	 (tips ((location (5 10) (12 8) (4 6))
		(orientation (*sw* 10) (*w* 7) (*se* 9) (*s* 9) (*nw* 6)
			     (*n* 6) (*e* 6)))
	       ((location (17 10) (10 6) (18 9))
		(orientation (*ne* 10) (*w* 6) (*e* 7) (*n* 7) (*se* 7)
			     (*nw* 6) (*s* 6))))
	 (ends
	  (((38 *sw*) 40) ((8 *w*) 8) ((50 *se*) 32) ((23 *s*) 32)
	   ((6 *w*) 8) ((39 *sw*) 8) ((50 *nw*) 8) ((23 *n*) 32)
	   ((8 *e*) 8) ((21 *n*) 6))
	  (((37 *ne*) 40) ((5 *w*) 7) ((7 *e*) 13) ((39 *ne*) 7)
	   ((49 *se*) 13) ((22 *n*) 20) ((22 *s*) 7) ((49 *nw*) 7)))
	 (curve
	  ((*straight* 10) (*slight-left* 9) (*slight-right* 9)
	   (*square-right* 8)))
	 (short 10) (very-short 7) (wide 10) (half-wide 6) (light 7)
	 (normal-wt 10) (roof-x-height 10) (roof-midline 6)
	 (floor-baseline 10) (floor-midline 6)
	 (l-edge-lf 10) (l-edge-md 6) (r-edge-rt 10))))
(set! halfpost
      '((names (halfpost h-post halfpost-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 0) (*cupped* 30) (*spiky-closure* -10)))
	 (neighborhood (((y dc dc n n n dc dc) 20)))
	 (contact (i1 (nt 20) (t 11)))
	 (tips ((location (10 10) (12 6) (11 9)
			  (17 8) (18 6) (4 7) (3 6))
		(orientation (*w* 10) (*sw* 7) (*s* 9) (*e* 6) (*nw* 7)
			     (*n* 7) (*ne* 6)))
	       ((location (19 9) (12 10) (5 7) (18 6))
		(orientation (*s* 10) (*w* 6) (*sw* 9) (*n* 6) (*se* 6))))

	 (ends
	  (((5 *w*) 40) ((39 *sw*) 13) ((24 *s*) 13) ((5 *e*) 13)
	   ((21 *s*) 27) ((6 *w*) 27) ((22 *s*) 13) ((49 *nw*) 13)
	   ((48 *nw*) 13) ((37 *sw*) 13) ((37 *ne*) 13) ((22 *n*) 13)
	   ((21 *n*) 13))
	  (((25 *s*) 40) ((24 *s*) 24) ((39 *sw*) 24) ((9 *w*) 8)
	   ((51 *se*) 8) ((38 *sw*) 24) ((25 *n*) 8)))
	 (curve
	  ((*slight-right* 10) (*strong-right* 9) (*straight* 8)
	   (*slight-left* 8) (*square-right* 9) (*square-left* 7)))
	 (short 10) (very-short 6) (half-wide 10) (wide 7)
	 (skinny 6) (normal-wt 10) (light 8)
	 (roof-x-height 10) (roof-midline 6) (floor-baseline 10)
	 (l-edge-md 10) (l-edge-lf 8) (r-edge-rt 10) (r-edge-md 6))))
(set! left-bowl
      '((names (left-bowl l-bowl left-bowl-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 20) (*cupped* 40) (*spiky-closure* 10)))
	 (neighborhood (((n n n n n n n n) 20)
			((dc y dc dc dc n dc dc) 20)))
	 (contact (c1 (nt 20))
		  (d1 (2ts 20) (t1 13) (t2 13) (t1m 11) (mt2 11) (2ms 11))
		  (g1 (2ts 20) (t1 13) (t1m 11) (mt2 11) (2ms 11))
		  (q1 (2ts 20) (t1 13) (t2 13) (t1m 11) (mt2 11) (2ms 11)))
	 (tips ((location (17 10) (18 8) (10 6) (11 6) (19 6) (21 6))
		(orientation (*e* 14) (*se* 8) (*ne* 7) (*n* 6) (*s* 6)))
	       ((location (19 10) (18 7) (12 6) (11 6))
		(orientation (*e* 10) (*n* 6) (*ne* 7) (*se* 6) (*nw* 6)
			     (*s* 6))))
	 (ends
	   (((5 *e*) 40) ((49 *se*) 25) ((4 *e*) 15) ((7 *e*) 10)
	    ((37 *ne*) 20) ((36 *ne*) 5) ((22 *s*) 5) ((22 *n*) 5)
	    ((21 *s*) 5))
	   (((9 *e*) 40) ((50 *se*) 3) ((39 *ne*) 12) ((25 *n*) 6)
	    ((8 *e*) 9) ((51 *se*) 6) ((25 *s*) 3) ((51 *nw*) 3)))
	 (curve ((*strong-left* 10) (*square-left* 6)))
	 (short 10) (very-short 6) (wide 10) (half-wide 6) (heavy 6)
	 (normal-wt 10) (light 6) (roof-x-height 10) (roof-midline 6)
	 (floor-baseline 10)
	 (l-edge-lf 10) (r-edge-rt 10) (r-edge-md 6))))
(set! left-downbowl
      '((names (left-downbowl l-dnbwl left-downbowl-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 20) (*cupped* 40) (*spiky-closure* -10)))
	 (neighborhood (((y dc dc dc dc n dc dc) 20)))
	 (contact (a1 (2ts 20) (t2 -8) (t1 -8)))
	 (tips ((location (18 10) (17 8) (11 6))
		(orientation (*e* 10) (*ne* 9)))
	       ((location (19 10) (18 7) (12 7))
		(orientation (*e* 10) (*ne* 7) (*se* 6))))
	 (ends
	  (((7 *e*) 40) ((37 *ne*) 20) ((38 *ne*) 7))
	  (((9 *e*) 40) ((39 *ne*) 13) ((50 *se*) 7) ((8 *e*) 7)))
	 (curve ((*strong-left* 10)))
	 (very-short 10) (short 9) (wide 10) (normal-wt 10) (heavy 6)
	 (roof-midline 10) (roof-x-height 9) (floor-baseline 10)
	 (l-edge-lf 10) (r-edge-rt 10))))
(set! left-halfarc
      '((names (left-halfarc l-hlfarc left-halfarc-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 0) (*cupped* 40) (*spiky-closure* -10)))
	 (neighborhood (((dc dc y dc dc n n n) 20)))
	 (contact (w1 (t2 20) (m 11)))
	 (tips ((location (3 9) (4 7) (10 7))
		(orientation (*n* 9) (*w* 6) (*ne* 7) (*nw* 6)))
	       ((location (12 9) (11 8) (5 6))
		(orientation (*e* 8) (*ne* 7) (*se* 6) (*n* 6) (*s* 6))))
	 (ends
	  (((20 *n*) 40) ((4 *w*) 8) ((36 *ne*) 32) ((23 *n*) 16)
	   ((50 *nw*) 8))
	  (((8 *e*) 40) ((38 *ne*) 30) ((50 *se*) 20) ((24 *n*) 20)
	   ((23 *s*) 10) ((6 *e*) 10)))
	 (curve
	  ((*slight-left* 8) (*straight* 6) (*square-left* 10)
	   (*strong-left* 9)))
	 (short 10) (very-short 7) (half-wide 10) (normal-wt 9) (light 10)
	 (roof-x-height 10) (roof-midline 7) (floor-baseline 10)
	 (floor-midline 6)
	 (l-edge-lf 10) (r-edge-md 10))))
(set! left-halfarch
      '((names (left-halfarch l-hlfarch left-halfarch-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 0) (*cupped* 40) (*spiky-closure* -10)))
	 (neighborhood (((dc dc y dc dc dc y dc) 20)))
	 (contact (m1 (t1m 20) (t1 15) (2ts 11) (m 11) (nt 11)))
	 (tips ((location (3 10) (11 6) (4 6) (10 6))
		(orientation (*w* 10) (*se* 8) (*sw* 8) (*nw* 9) (*s* 9)
			     (*n* 6)))
	       ((location (12 10) (11 9) (5 6) (19 7))
		(orientation (*s* 10) (*se* 7) (*sw* 6))))

	 (ends
	  (((4 *w*) 40) ((48 *se*) 20) ((36 *sw*) 20) ((48 *nw*) 40)
	   ((21 *s*) 20) ((24 *s*) 20) ((21 *n*) 10))
	  (((24 *s*) 40) ((48 *se*) 10) ((21 *s*) 20) ((38 *sw*) 10)
	   ((51 *se*) 10)))
	 (curve
	  ((*slight-right* 10) (*straight* 9) (*strong-right* 7)
	   (*square-right* 7)))
	 (short 10) (very-short 9) (half-wide 10) (wide 6)
	 (skinny 7) (normal-wt 7) (light 10) (roof-x-height 10)
	 (roof-midline 6) (floor-baseline 10) (floor-midline 8)
	 (l-edge-lf 10) (l-edge-md 7) (r-edge-md 10) (r-edge-rt 6))))
(set! left-halfpost
      '((names (left-halfpost l-hlfpost left-halfpost-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 10) (*cupped* 30) (*spiky-closure* -10)))
	 (neighborhood (((dc dc y dc dc n n n) 20)))
	 (contact (m1 (t1 20) (m 18) (nt 11))
		  (n1 (t1 20) (m 18) (nt 11))
		  (r1 (t1 20) (m 18) (nt 11)))
	 (tips ((location (3 10) (4 6) (10 7) (17 1) (11 1))
		(orientation (*n* 10) (*nw* 8) (*ne* 8) (*sw* 6)
			     (*w* 7) (*s* 6)))
	       ((location (5 10) (12 8) (4 6) (19 6))
		(orientation (*s* 10) (*sw* 8) (*se* 8) (*w* 1)
			     (*e* 1))))
	 (ends
	  (((20 *n*) 40) ((48 *nw*) 40) ((36 *ne*) 27) ((36 *sw*) 13)
	   ((4 *w*) 27) ((21 *n*) 27) ((20 *s*) 13) ((37 *ne*) 13)
	   ((23 *n*) 13) ((38 *ne*) 5) ((38 *sw*) 5))
	  (((23 *s*) 40) ((24 *s*) 16) ((50 *se*) 24) ((38 *sw*) 32)
	   ((20 *s*) 8) ((51 *se*) 8) ((38 *ne*) 5) ((38 *sw*) 5)))
	 (curve
	  ((*straight* 10) (*slight-right* 8) (*slight-left* 7)
	   (*strong-right* 6) (*square-left* 6) (*square-right* 6)))
	 (short 10) (very-short 6) (skinny 7) (half-wide 10) (wide 6)
	 (light 10) (normal-wt 8) (roof-x-height 10) (roof-midline 6)
	 (floor-baseline 16) (floor-midline 6) (l-edge-lf 10) (l-edge-md 6)
	 (r-edge-lf 7) (r-edge-md 10) (r-edge-rt 6))))
(set! left-post
      '((names (left-post l-post left-post-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 10) (*cupped* 30) (*spiky-closure* -10)))
	 (neighborhood (((n dc dc y dc dc dc n) 20)))
	 (contact (b1 (mt2 20) (m 11) (t2 11) (2ms 18))
		  (b2 (m 20) (t2 15))
		  (h1 (m 20) (t2 10))
		  (k1 (m 20) (t2 15) (2ms 10)))
	 (tips ((location (1 10) (8 8) (2 8) (9 6) (15 6))
		(orientation (*n* 10) (*w* 6) (*ne* 7) (*nw* 7) (*se* 6)))
	       ((location (5 10) (4 7) (12 7) (11 6))
		(orientation (*s* 10) (*sw* 7) (*se* 7) (*n* 6) (*w* 6))))
	 (ends
	  (((14 *n*) 40) ((0 *w*) 13) ((17 *n*) 40) ((44 *nw*) 27)
	   ((32 *ne*) 27) ((15 *n*) 13) ((33 *ne*) 13) ((18 *n*) 13)
	   ((44 *se*) 10))
	  (((23 *s*) 40) ((38 *sw*) 30) ((20 *s*) 20) ((23 *n*) 10)
	   ((50 *se*) 20) ((24 *s*) 10) ((21 *s*) 10) ((8 *w*) 10)
	   ((8 *e*) 10)))
	 (curve
	  ((*straight* 10) (*slight-right* 7) (*slight-left* 7)
	   (*square-right* 6)))
	 (tall 10) (short 6) (medium 7) (skinny 10)
	 (half-wide 9) (wide 6) (normal-wt 10) (light 6) (heavy 6)
	 (roof-top 10) (roof-t-height 7) (floor-baseline 10)
	 (floor-midline 7) (l-edge-lf 10) (l-edge-md 6)
	 (r-edge-lf 7) (r-edge-md 10) (r-edge-rt 6))))
(set! left-tail
      '((names (left-tail l-tail left-tail-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 20) (*cupped* 30) (*spiky-closure* -10)))
	 (neighborhood (((dc y dc n n n dc dc) 20)))
	 (contact (p1 (t1m 20) (m 11) (2ms 15)
		  (t1 13) (2ms 15) (nt 11))
		  (p2 (m 20) (t1 18)))
	 (tips ((location (3 10) (5 7) (4 7) (12 6))
		(orientation (*n* 10) (*sw* 6) (*w* 6) (*nw* 7) (*ne* 6)))
	       ((location (7 10) (14 6) (5 6) (6 7) (13 6))
		(orientation (*s* 10) (*sw* 6) (*w* 7) (*ne* 6))))

	 (ends
	  (((20 *n*) 40) ((38 *sw*) 13) ((4 *w*) 13) ((48 *nw*) 27)
	   ((26 *n*) 13) ((23 *n*) 27) ((40 *ne*) 13))
	  (((29 *s*) 40) ((30 *s*) 10) ((23 *s*) 10) ((42 *sw*) 10)
	   ((12 *w*) 10) ((10 *w*) 10) ((26 *s*) 10) ((42 *ne*) 10)))
	 (curve
	  ((*straight* 10) (*square-right* 7) (*slight-right* 7)
	   (*slight-left* 6) (*square-left* 6)))
	 (tall 8) (medium 10) (short 8) (skinny 9) (half-wide 10)
	 (normal-wt 10) (light 7) (heavy 6) (roof-x-height 10)
	 (roof-midline 8) (roof-baseline 7) (floor-bottom 10)
	 (floor-middown 7) (floor-baseline 6)
	 (l-edge-lf 10) (r-edge-lf 9) (r-edge-md 10))))
(set! left-uparc
      '((names (left-uparc l-uparc left-uparc-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* -200) (*cupped* 40)
	   (*spiky-closure* -10)))
	 (neighborhood (((dc dc y dc dc n n n) 20)   ; u
			((dc dc y y y dc dc n) 20))) ; y
	 (contact (u1 (t2 20) (m 13) (nt 11))
		  (y1 (t2 20) (m 13) (nt 11)))
	 (tips ((location (3 10) (4 7) (10 7))
		(orientation (*n* 10) (*w* 6) (*ne* 7) (*nw* 6) (*s* 6)))
	       ((location (19 10) (18 9) (12 7) (11 7) (5 7) (17 6))
		(orientation (*e* 10) (*ne* 9) (*n* 7) (*se* -60))))
	 (ends
	  (((20 *n*) 40) ((23 *n*) 10) ((36 *ne*) 20) ((4 *w*) 10)
	   ((20 *s*) 5) ((48 *nw*) 10) ((21 *n*) 5))
	  (((9 *e*) 40) ((39 *ne*) 27) ((8 *e*) 13) ((24 *n*) 7) ((37 *ne*) 7)
	   ((38 *ne*) 7)))
         (curve
          ((*square-left* 9) (*strong-left* 10) (*slight-left* 8)
           (*slight-right* 7) (*straight* 6)))
	 (short 10) (very-short 6) (wide 10) (half-wide 8) (skinny 6)
	 (normal-wt 10) (light 6) (roof-x-height 10) (roof-midline 6)
	 (floor-baseline 10)
	 (l-edge-lf 10) (r-edge-rt 10) (r-edge-md 8) (r-edge-lf 6))))
(set! left-upbowl
      '((names (left-upbowl l-upbwl left-upbowl-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 10) (*cupped* 40) (*spiky-closure* -10)))
	 (neighborhood (((n n dc dc y dc dc n) 20)))
	 (contact (s2 (t2 20) (mt2 11)))
	 (tips ((location (17 10) (10 7) (18 7))
		(orientation (*e* 10) (*ne* 6) (*se* 6) (*n* 6) (*s* 6)))
	       ((location (4 7) (18 9) (11 10) (17 6))
		(orientation (*s* 7) (*e* 10) (*sw* 6) (*se* 7) (*ne* 6))))
	 (ends
	  (((5 *e*) 40) ((37 *ne*) 4) ((49 *se*) 12) ((22 *n*) 4)
	   ((4 *e*) 20) ((22 *s*) 4))
	  (((20 *s*) 13) ((7 *e*) 40) ((6 *e*) 27) ((48 *se*) 27)
	   ((36 *sw*) 13) ((37 *ne*) 7) ((37 *sw*) 7) ((21 *s*) 7)))
	 (curve
	  ((*slight-left* 6) (*strong-left* 10) (*square-left* 8)))
	 (very-short 10) (wide 10) (half-wide 7) (normal-wt 10) (light 7)
	 (roof-x-height 10) (floor-midline 10)
	 (l-edge-lf 10) (l-edge-md 6) (r-edge-rt 10) (r-edge-md 6))))
(set! left-wing
      '((names (left-wing l-wing left-wing-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* -200) (*cupped* -200)
	   (*spiky-closure* -10)))
	 (neighborhood (((dc dc y dc dc n n n) 20)))
	 (contact (v1 (t2 20)))
	 (tips ((location (3 10) (4 7) (10 7))
		(orientation (*n* 10) (*s* 7) (*ne* 8) (*nw* 8)
			     (*se* 7) (*w* 9) (*sw* 7)))
	       ((location (12 10) (19 7) (5 9))
		(orientation (*se* 9) (*s* 10) (*sw* 7))))	

	 (ends
	  (((20 *n*) 40) ((20 *s*) 13) ((36 *ne*) 27) ((48 *nw*) 27)
	   ((50 *se*) 13) ((4 *w*) 40) ((21 *n*) 13) ((36 *sw*) 13))
	  (((50 *se*) 30) ((24 *s*) 40) ((23 *s*) 30) ((51 *se*) 20)
	   ((38 *sw*) 20)))
	 (curve
	  ((*slight-left* 7) (*square-right* 6) (*square-left* 6)
	   (*straight* 9) (*slight-right* 10) (*strong-right* 6)))
	 (short 10) (very-short 6) (half-wide 10) (wide 6)
	 (skinny 6) (light 10) (normal-wt 9) (roof-x-height 10)
	 (roof-midline 6) (floor-baseline 10)
	 (l-edge-lf 10) (r-edge-md 10) (r-edge-rt 6) (r-edge-lf 6))))
(set! right-bowl
      '((names (right-bowl r-bowl right-bowl-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 10) (*cupped* 40) (*spiky-closure* -10)))
	 (neighborhood (((dc dc dc n dc dc dc y) 20)   ; b
			((dc n dc dc dc y dc dc) 20))) ; p
	 (contact (b1 (2ts 20) (t2 11))
		  (p1 (2ts 20) (t1 11) (t2 11)))
	 (tips ((location (3 10) (4 9) (10 8) (11 6))
		(orientation (*w* 10) (*sw* 7) (*nw* 7) (*s* 6)))
	       ((location (5 10) (4 8) (12 6))
		(orientation (*w* 10) (*nw* 8) (*sw* 6))))
	 (ends
	  (((4 *w*) 40) ((36 *sw*) 30) ((5 *w*) 30) ((6 *w*) 10)
	   ((48 *nw*) 20) ((21 *s*) 10) ((37 *sw*) 10))
	  (((8 *w*) 40) ((50 *nw*) 23) ((9 *w*) 6) ((38 *sw*) 6)))
	 (curve ((*strong-right* 10)))
	 (short 10) (wide 10) (half-wide 6) (heavy 6) (normal-wt 10)
	 (roof-x-height 10) (floor-baseline 10)
	 (l-edge-lf 10) (l-edge-md 6) (r-edge-rt 10))))
(set! right-buttress
      '((names (right-buttress r-butt right-buttress-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* -200) (*cupped* 30)
	   (*spiky-closure* -10)))
	 (neighborhood (((dc dc dc n dc dc y y) 20)    ; h
			((dc dc dc n dc dc y dc) 20))) ; n
	 (contact (h1 (t1 20) (nt 11)) (n1 (t1 20) (nt 11) (m -40)))
	 (tips ((location (3 10) (10 7) (4 7) (11 6) (5 6) (12 6)
			  (17 3))
		(orientation (*w* 10) (*nw* 8) (*sw* 8) (*se* 6) (*s* 6)
			     (*n* 3)))
	       ((location (19 10) (12 8) (18 6) (11 6))
		(orientation (*s* 10) (*sw* 8) (*se* 6) (*e* 6) (*w* 6)
			     (*n* 6))))
	 (ends
	  (((4 *w*) 40) ((49 *nw*) 13) ((36 *sw*) 13) ((7 *w*) 7)
	   ((5 *w*) 7) ((50 *se*) 7) ((38 *sw*) 13) ((37 *sw*) 7)
	   ((48 *nw*) 20))
	  (((25 *s*) 40) ((39 *sw*) 22) ((51 *se*) 4) ((9 *e*) 4)
	   ((25 *n*) 4) ((24 *s*) 4) ((50 *se*) 4) ((9 *w*) 4)))
         (curve
          ((*square-right* 10) (*slight-right* 9) (*strong-right* 9)
           (*straight* 7) (*slight-left* 6)))
	 (short 10) (very-short 6) (wide 10)
	 (half-wide 8)  (skinny 0) (normal-wt 10) (light 7) (heavy 6)
	 (roof-x-height 10) (roof-midline 6) (floor-baseline 10)
	 (l-edge-lf 10) (l-edge-md 6) (l-edge-rt 0) (r-edge-rt 10))))
(set! right-curl
      '((names (right-curl r-curl right-curl-act))
	(topology bisegment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 30) (*cupped* 40) (*spiky-closure* -10)))
	 (neighborhood (((y dc dc n n n dc dc) 20)   ; j
			((dc n n n n dc dc y) 20)))  ; y
	 (contact (j1 (nt 20) (m 20) (t1 11)) (y1 (nt 11) (m 20) (t1 11))
		  (y2 (nt 11) (m 20) (t1 11)))
	 (tips ((location (10 9) (17 10) (11 7) (4 6) (18 6) (3 6))
		(orientation (*w* 8) (*e* 7) (*n* 10) (*nw* 8) (*s* 8)
			     (*sw* 6) (*ne* 7)))
	       ((location (14 7) (7 10) (6 9) (21 6) (5 6) (13 7) (20 6)
			  (12 7))
		(orientation (*w* 10) (*s* 9) (*sw* 9) (*e* 6) (*nw* 8)
			     (*n* 7) (*se* 6))))
	 (ends
	  (((5 *w*) 25) ((5 *e*) 15) ((22 *n*) 40) ((49 *nw*) 20)
	   ((24 *n*) 5) ((21 *s*) 15) ((22 *s*) 5) ((37 *sw*) 5)
	   ((37 *ne*) 15) ((6 *w*) 5) ((48 *nw*) 10) ((21 *n*) 5)
	   ((20 *s*) 5) ((25 *n*) 30))
	  (((13 *w*) 20) ((29 *s*) 40) ((12 *w*) 30) ((13 *e*) 10)
	   ((10 *w*) 30) ((29 *n*) 10) ((40 *sw*) 10) ((42 *sw*) 30)
	   ((52 *nw*) 10) ((11 *w*) 20) ((30 *n*) 10) ((27 *n*) 10)
	   ((43 *sw*) 20) ((54 *nw*) 20) ((55 *se*) 10) ((53 *nw*) 20)
	   ((28 *s*) 20) ((26 *s*) 10) ((41 *sw*) 10) ((23 *n*) 10)))
	 (curve1
	  ((*slight-right* 8) (*slight-left* 7) (*straight* 10)
	   (*square-right* 8) (*square-left* 6) (*strong-right* 7)))
	 (curve2
	  ((*slight-right* 10) (*square-left* 6) (*square-right* 8)
	   (*strong-right* 6) (*straight* 7) (*slight-left* 6)))
	 (tall 20) (medium 8) (short 6)
	 (half-wide 15) (wide 10) (heavy 8) (normal-wt 10)
	 (roof-x-height 10) (roof-midline 6) (floor-bottom 10)
	 (floor-middown 8)
	 (l-edge-md 8) (l-edge-lf 10) (r-edge-rt 10) (r-edge-md 6))))
(set! right-downbowl
      '((names (right-downbowl r-dwnbwl right-downbowl-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 10) (*cupped* 40)
	   (*spiky-closure* -10)  (*closure* -40)))
	 (neighborhood (((y dc dc dc n dc dc dc) 20)))
	 (contact (s2 (t1 20) (t1m 11) (nt 11)))
	 (tips ((location (4 8) (11 10) (18 7) (19 6) (3 6) (17 6))
		(orientation (*w* 10) (*nw* 8) (*n* 6) (*e* 6) (*ne* 6)
			     (*sw* 6)))
	       ((location (5 10) (4 7) (12 7))
		(orientation (*w* 10) (*s* 6) (*n* 6) (*nw* 7) (*sw* 6))))
	 (ends
	  (((6 *w*) 40) ((51 *nw*) 25) ((7 *w*) 35) ((9 *e*) 5) ((25 *n*) 10)
	   ((22 *n*) 5) ((39 *ne*) 10) ((37 *sw*) 5) ((38 *ne*) 5)
	   ((48 *nw*) 10))
	  (((8 *w*) 40) ((23 *s*) 12) ((50 *nw*) 15) ((39 *sw*) 6)
	   ((9 *w*) 15) ((23 *n*) 15)))
	 (curve
	  ((*strong-right* 10) (*square-right* 8) (*slight-right* 7)))
	 (very-short 10) (short 6) (wide 10) (half-wide 6)
	 (normal-wt 10) (light 7) (heavy 6) (roof-midline 10)
	 (roof-x-height 6) (floor-baseline 10)
	 (l-edge-lf 10) (l-edge-md 6) (r-edge-rt 10))))
(set! right-halfarc
      '((names (right-halfarc r-hlfarc right-halfarc-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 0) (*cupped* 40) (*spiky-closure* -10)))
	 (neighborhood (((dc dc y dc dc dc y dc) 20)))
	 (contact (w1 (mt2 20) (t2 15) (m 11) (2ts 11)))
	 (tips ((location (10 9) (17 6) (11 10) (3 7))
		(orientation (*n* 10) (*se* 6) (*s* 7) (*nw* 8) (*ne* 6)))
	       ((location (19 10) (18 9) (12 8) (11 6))
		(orientation (*e* 10) (*se* 9) (*ne* 9) (*s* 9) (*n* 7))))
	 (ends
	  (((21 *n*) 40) ((51 *se*) 10) ((24 *n*) 20) ((48 *nw*) 20)
	   ((24 *s*) 10) ((51 *nw*) 10) ((21 *s*) 10) ((37 *ne*) 10)
	   ((38 *ne*) 10))
	  (((9 *e*) 27) ((51 *se*) 40) ((39 *ne*) 40) ((24 *s*) 27)
	   ((21 *s*) 13) ((25 *n*) 13) ((8 *e*) 13)))
	 (curve
	  ((*slight-left* 9) (*straight* 9) (*square-left* 8)
	   (*slight-right* 7) (*strong-left* 10)))
	 (short 9) (very-short 10) (half-wide 10) (wide 6) (skinny 6)
	 (normal-wt 7) (light 10) (roof-x-height 10) (roof-midline 9)
	 (floor-baseline 10) (floor-midline 6)
	 (l-edge-md 10) (l-edge-lf 7) (r-edge-rt 10) (r-edge-md 7))))
(set! right-halfarch
      '((names (right-halfarch r-hlfarch right-halfarch-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 0) (*cupped* 30) (*spiky-closure* -10)))
	 (neighborhood (((dc dc y dc dc dc y dc) 20)))
	 (contact (m1 (t1 20) (nt 11)))
	 (tips ((location (10 10) (11 9) (17 7))
		(orientation (*w* 10) (*s* 9) (*nw* 9) (*n* 7) (*sw* 7)))
	       ((location (19 10) (12 8) (18 6))
		(orientation (*s* 10) (*w* 6) (*sw* 7) (*se* 6))))
	 (ends
	  (((5 *w*) 40) ((21 *s*) 40) ((49 *nw*) 40) ((22 *n*) 20)
	   ((37 *sw*) 20) ((7 *w*) 20))
	  (((25 *s*) 40) ((9 *w*) 8) ((39 *sw*) 16) ((49 *se*) 8)))
	 (curve
	  ((*slight-right* 7) (*strong-right* 7) (*square-right* 7)))
	 (short 10) (very-short 7) (half-wide 10) (normal-wt 9) (light 10)
	 (roof-x-height 10) (roof-midline 6) (floor-baseline 10)
	 (floor-midline 6)
	 (l-edge-md 10) (r-edge-rt 10))))
(set! right-halfpost
      '((names (right-halfpost r-hlfpst right-halfpost-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 10) (*cupped* 30) (*spiky-closure* -10)))
	 (neighborhood (((dc n n n dc dc y dc) 20)))
	 (contact (u1 (t2 20) (m 18)) (w1 (t2 20) (m 18)))
	 (tips ((location (17 10) (10 19) (18 7) (11 6))
		(orientation (*n* 9) (*w* 7) (*s* 10) (*ne* 8)
			     (*se* 7) (*nw* 8)))
	       ((location (19 10) (18 9) (12 8))
		(orientation (*s* 10) (*se* 7) (*sw* 7) (*ne* 6)
			     (*e* 6))))
	 (ends
	  (((22 *n*) 40) ((5 *w*) 20) ((22 *s*) 40) ((21 *s*) 20)
	   ((49 *se*) 20) ((37 *ne*) 40) ((49 *nw*) 40) ((21 *n*) 20)
	   ((25 *s*) 20))
	  (((25 *s*) 40) ((22 *s*) 20) ((49 *se*) 10) ((51 *se*) 10)
	   ((39 *sw*) 20) ((39 *ne*) 10) ((24 *s*) 10) ((9 *e*) 10)))
	 (curve
	  ((*straight* 10) (*square-right* 7) (*slight-right* 7)
	   (*strong-right* 6) (*square-left* 7) (*strong-left* 6)))
	 (short 10) (very-short 7) (skinny 7) (half-wide 10) (light 10)
	 (normal-wt 7) (roof-x-height 10) (roof-midline 6)
	 (floor-baseline 10) (floor-midline 7)
	 (l-edge-rt 7) (l-edge-md 10) (r-edge-rt 10) (r-edge-md 6))))
(set! right-hook
      '((names (right-hook r-hook right-hook-act))
	(topology bisegment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 20) (*cupped* 40) (*spiky-closure* -10)))
	 (neighborhood (((dc dc n n n n dc dc) 20)))
	 (contact (g1 (t1m 20) (m 5) (2ms 20) (t1 11) (nt 11))
		  (g2 (m 20) (t1 18) (nt 11)))
	 (tips ((location (10 7) (3 6) (18 7) (17 10) (11 6))
		(orientation (*w* 7) (*nw* 6) (*n* 12) (*e* 6) (*ne* 6)))
	       ((location (14 6) (6 10) (7 7) (5 6) (21 6) (12 6) (13 6))
		(orientation (*w* 10) (*n* 7) (*nw* 7) (*se* 6) (*sw* 6))))
	 (ends
	  (((5 *w*) 10) ((48 *nw*) 5) ((25 *n*) 15) ((5 *e*) 5)
	   ((37 *ne*) 5) ((39 *ne*) 5) ((24 *n*) 5) ((49 *nw*) 5)
	   ((22 *n*) 40) ((5 *e*) 10))
	  (((13 *w*) 8) ((29 *n*) 24) ((12 *w*) 24) ((54 *nw*) 16)
	   ((10 *w*) 40) ((52 *nw*) 16) ((54 *se*) 8) ((27 *n*) 8)
	   ((11 *w*) 8) ((55 *nw*) 8) ((53 *nw*) 8) ((41 *sw*) 8)))
	 (curve1
	  ((*slight-right* 7) (*square-left* 6) (*straight* 10)
	   (*slight-left* 6)))
	 (curve2
	  ((*slight-right* 10) (*square-right* 9) (*straight* 6)
	   (*strong-right* 7) (*slight-left* -80)))
	 (tall 9) (medium 10) (short 6) (half-wide 7) (wide 10) (heavy 7)
	 (normal-wt 10) (roof-x-height 10) (roof-midline 7) (floor-bottom 10)
	 (floor-middown 9)
	 (l-edge-md 6) (l-edge-lf 10) (r-edge-rt 10) (r-edge-md 6))))
(set! right-post
      '((names (right-post r-post right-post-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 10) (*cupped* 30) (*spiky-closure* -10)))
	 (neighborhood (((n n dc dc dc y dc dc) 20)))
	 (contact (d1 (mt2 20) (m 11) (2ms 11) (t2 11))
		  (d2 (m 20) (t2 15)))
	 (tips ((location (15 10) (9 8) (16 8) (8 7))
		(orientation (*n* 10) (*ne* 7) (*nw* 6) (*sw* 6)))
	       ((location (19 10) (18 7) (12 6))
		(orientation (*s* 10) (*se* 8) (*sw* 6))))
	 (ends
	  (((16 *n*) 40) ((33 *ne*) 40) ((19 *n*) 40) ((18 *n*) 20)
	   ((45 *nw*) 20) ((33 *sw*) 20))
	  (((25 *s*) 40) ((51 *se*) 40) ((22 *s*) 27) ((39 *sw*) 13)))
	 (curve
	  ((*straight* 10) (*slight-left* 9) (*slight-right* 7)
	   (*square-right* 7)))
	 (tall 10) (medium 8) (short 6) (skinny 10) (half-wide 9)
	 (normal-wt 10) (light 6) (roof-top 10) (roof-t-height 8)
	 (floor-baseline 10) (floor-midline 7)
	 (l-edge-rt 9) (l-edge-md 10) (r-edge-rt 10))))
(set! right-tail
      '((names (right-tail r-tail right-tail-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 30) (*cupped* 40) (*spiky-closure* -10)))
	 (neighborhood (((dc dc n n n n dc dc) 20)))
	 (contact (q1 (t1m 20) (m 11) (2ms 15) (t1 11) (nt 11))
		  (q2 (m 20) (t1 18) (nt 11)))
	 (tips ((location (17 10) (19 7) (18 7) (3 6) (10 6))
		(orientation (*n* 10) (*se* 6) (*nw* 7) (*ne* 7) (*s* 6)
			     (*e* 4)))
	       ((location (21 10) (14 6) (20 9))
		(orientation (*s* 10) (*se* 6) (*e* 7) (*ne* 5) (*n* 5))))
	 (ends
	  (((22 *n*) 40) ((51 *se*) 13) ((25 *n*) 27) ((37 *ne*) 27)
	   ((49 *nw*) 13) ((28 *s*) 13) ((48 *nw*) 13) ((5 *e*) 9))
	  (((31 *s*) 20) ((30 *s*) 10) ((28 *s*) 40) ((55 *se*) 10)
	   ((13 *e*) 20) ((11 *e*) 10) ((43 *ne*) 9)))
	 (curve
	  ((*straight* 10) (*square-left* 7) (*slight-right* 6)
	   (*slight-left* 6)))
	 (tall 9) (medium 10) (short 6) (very-short 6) (skinny 10)
	 (half-wide 10) (wide 6) (normal-wt 10) (light 6)
	 (roof-x-height 10) (roof-midline 7) (roof-baseline 6)
	 (floor-bottom 10) (floor-middown 9)
	 (l-edge-rt 10) (l-edge-md 9) (l-edge-lf 6) (r-edge-rt 10))))
(set! right-wing
      '((names (right-wing r-wing right-wing-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* -200) (*cupped* -200)
	   (*spiky-closure* -10)))
	 (neighborhood (((dc n n n dc dc y dc) 20)))
	 (contact (v1 (t2 20)))
	 (tips ((location (17 10) (11 7) (10 9))
		(orientation (*n* 10) (*s* 7) (*nw* 7) (*w* 9) (*ne* 7)))
	       ((location (12 10) (19 9) (5 9))
		(orientation (*sw* 10) (*s* 7))))
	 (ends
	  (((22 *n*) 40) ((21 *s*) 13) ((49 *nw*) 13) ((5 *w*) 27)
	   ((37 *ne*) 13))
	  (((39 *sw*) 40) ((25 *s*) 27) ((38 *sw*) 27) ((8 *w*) 13)
	   ((24 *s*) 27)))
	 (curve
	  ((*slight-right* 10) (*strong-right* 7) (*square-right* 7)
	   (*straight* 8) (*slight-left* 6)))
	 (short 10) (half-wide 10) (wide 9) (skinny 7) (light 10)
	 (normal-wt 10) (roof-x-height 10) (floor-baseline 10)
	 (l-edge-md 10) (l-edge-lf 9) (l-edge-rt 7) (r-edge-rt 10))))
(set! s-base
      '((names (s-base s-base s-base-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 0) (*cupped* 40) (*spiky-closure* -10)))
	 (neighborhood (((y dc dc n n n dc dc) 20)))
	 (contact (s1 (t2 20) (mt2 11)))
	 (tips ((location (5 10) (3 6) (4 8) (12 7))
		(orientation (*w* 10) (*s* 8) (*nw* 8) (*e* 6)))
	       ((location (19 10) (18 9) (5 6) (17 6))
		(orientation (*e* 10) (*ne* 9) (*n* 9) (*se* 7) (*s* 7))))
	 (ends
	  (((8 *w*) 40) ((23 *s*) 30) ((50 *nw*) 20) ((48 *nw*) 10)
	   ((9 *e*) 10) ((9 *w*) 10))
	  (((9 *e*) 40) ((39 *ne*) 30) ((25 *n*) 20) ((51 *se*) 10)
	   ((23 *s*) 10) ((22 *n*) 10)))
	 (curve
	  ((*straight* 10) (*slight-left* 8) (*square-left* 8)
	   (*slight-right* 6)))
	 (no-height 7) (very-short 10) (short 7) (wide 10) (skinny 6)
	 (half-wide 6) (light 10) (normal-wt 8) (roof-baseline 7)
	 (roof-midline 10) (roof-x-height 7) (floor-baseline 10)
	 (l-edge-lf 10) (l-edge-md 6) (r-edge-rt 10) (r-edge-lf 6))))
(set! s-crossbar
      '((names (s-crossbar s-cross s-crossbar-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 10) (*cupped* 40) (*spiky-closure* -10)))
	 (neighborhood (((y dc dc dc y dc dc dc) 20)))
	 (contact (s1 (2ts 20)))
	 (tips ((location (4 10) (3 8) (11 7) (10 7))
		(orientation (*w* 10) (*nw* 9) (*e* 9)))
	       ((location (18 10) (19 9) (11 7) (17 9))
		(orientation (*e* 10) (*se* 7) (*ne* 6))))
	 (ends
	  (((6 *w*) 40) ((48 *nw*) 27) ((6 *e*) 13) ((5 *e*) 13)
	   ((37 *ne*) 13) ((7 *e*) 13))
	  (((7 *e*) 40) ((51 *se*) 27) ((6 *e*) 13) ((37 *ne*) 13)
	   ((38 *sw*) 13) ((5 *e*) 13)))
	 (curve
	  ((*straight* 10) (*slight-left* 7) (*slight-right* 6)))
	 (no-height 10) (very-short 9) (short 7) (wide 10) (half-wide 8)
	 (light 10) (roof-midline 10) (roof-x-height 9) (floor-midline 10)
	 (floor-baseline 7) (floor-x-height 6)
	 (l-edge-lf 10) (l-edge-md 7) (r-edge-rt 10) (r-edge-md 6))))
(set! t-post
      '((names (t-post t-post t-post-act))
	(topology bisegment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 20) (*cupped* 40) (*spiky-closure* -10)))
	 (neighborhood (((n n dc n n n dc n) 20)))
	 (contact (t1 (m 20)))
	 (tips ((location (9 10) (16 6) (8 6) (2 9))
		(orientation (*n* 10) (*ne* 6) (*nw* 6)))
	       ((location (19 10) (11 7) (18 9) (12 6))
		(orientation (*e* 10) (*w* 7) (*ne* 8) (*sw* 7)
			     (*se* 8) (*n* 9) (*s* 7))))
	 (ends
	  (((18 *n*) 40) ((35 *ne*) 6) ((17 *n*) 17) ((46 *nw*) 11)
	   ((15 *n*) 6))
	  (((9 *e*) 40) ((7 *w*) 10) ((39 *ne*) 20) ((39 *sw*) 10)
	   ((51 *se*) 20) ((25 *n*) 20) ((24 *n*) 10) ((24 *s*) 10)))
	 (curve1
	  ((*straight* 10) (*slight-left* 7) (*square-right* 6)
	   (*slight-right* 7)))
	 (curve2
	  ((*square-left* 9) (*strong-right* 6) (*slight-left* 8)
	   (*strong-left* 10) (*straight* 6)))
	 (medium 10) (tall 6) (half-wide 9) (skinny 9) (wide 9) (normal-wt 10)
	 (roof-t-height 10) (roof-top 6) (floor-baseline 10)
	 (l-edge-md 9) (l-edge-lf 10) (r-edge-rt 10) (r-edge-md 6))))
(set! up-arm
      '((names (up-arm up-arm up-arm-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 0) (*cupped* 30) (*spiky-closure* -10)))
	 (neighborhood (((y dc dc dc y y y y) 20)))
	 (contact (k1 (t1 20) (t1m 20) (m 11)))
	 (tips ((location (5 6) (4 10) (11 6) (3 8) (10 6) (12 6))
		(orientation (*sw* 10) (*w* 9) (*ne* 8) (*e* 8) (*s* 6)))
	       ((location (17 10) (11 6) (18 8) (10 7))
		(orientation (*ne* 10) (*e* 9) (*se* 8) (*s* 8))))
	 (ends
	  (((38 *sw*) 13) ((6 *w*) 27) ((37 *ne*) 13) ((4 *w*) 27)
	   ((36 *sw*) 40) ((4 *e*) 13) ((5 *e*) 13) ((36 *ne*) 13)
	   ((24 *s*) 10))
	  (((37 *ne*) 40) ((5 *e*) 40) ((49 *se*) 27) ((21 *s*) 13)
	   ((36 *ne*) 13) ((22 *s*) 13) ((4 *e*) 13)))
	 (curve
	  ((*straight* 10) (*slight-left* 6) (*square-right* 6)
	   (*strong-right* 6) (*slight-right* 8)))
	 (short 6) (very-short 10) (no-height 7) (wide 10)
	 (half-wide 9) (light 10) (normal-wt 6) (roof-x-height 10)
	 (floor-baseline 6) (floor-midline 10) (floor-x-height 7)
	 (l-edge-lf 10) (l-edge-md 6) (r-edge-rt 10) (r-edge-md 7))))
(set! up-circle
      '((names (up-circle up-circ up-circle-act))
	(topology loop)
	(norms
	 (shape
	  ((*closure* 40) (*spiky-closure* 10)))
	 (neighborhood (((n n dc dc y y dc dc) 20)))
	 (contact (e2 (nt 11) (t 20) (m 11) (t1 11) (t2 11)))
	 (very-short 10) (short 7) (wide 10) (half-wide 6) (heavy 7)
	 (normal-wt 10) (roof-x-height 10) (floor-midline 10)
	 (floor-baseline 7)
	 (l-edge-lf 10) (l-edge-md 6) (r-edge-rt 10))))
(set! z-cap
      '((names (z-cap z-cap z-cap-act))
	(topology segment)
	(norms
	 (shape
	  ((*simple* 40) (*bactrian* 40) (*cupped* 40) (*spiky-closure* -10)))
	 (neighborhood (((n dc dc dc y dc dc dc) 20)))
	 (contact (z1 (t2 20) (m 13)))
	 (tips ((location (3 10) (4 9) (10 7))
		(orientation (*w* 10) (*ne* 8) (*sw* 9) (*e* 9) (*s* 6)))
	       ((location (17 10) (18 9) (10 9))
		(orientation (*e* 10) (*se* 8) (*ne* 7))))
	 (ends
	  (((4 *w*) 40) ((36 *ne*) 20) ((36 *sw*) 40) ((5 *w*) 20)
	   ((5 *e*) 10) ((4 *e*) 20) ((20 *s*) 10) ((20 *n*) 7))
	  (((5 *e*) 40) ((49 *se*) 27) ((4 *e*) 13) ((36 *ne*) 13)))
	 (curve
	  ((*straight* 10) (*slight-right* 8) (*square-right* 7)))
	 (no-height 9) (very-short 10) (wide 10) (half-wide 9) (light 10)
	 (roof-x-height 10) (floor-x-height 9) (floor-midline 10)
	 (l-edge-lf 10) (l-edge-md 6) (r-edge-rt 10) (r-edge-md 7))))

(set! *roles* '(a-arch backslash basebar cap center-post circle
      crossbar dot down-arm down-circle e-bowl e-crossbar e-tail
      f-post foreslash halfpost left-bowl left-downbowl left-halfarc
      left-halfarch left-halfpost left-post left-tail left-uparc
      left-upbowl left-wing right-bowl right-buttress
      right-curl right-downbowl right-halfarc right-halfarch
      right-halfpost right-hook right-post right-tail right-wing
      s-base s-crossbar t-post up-arm up-circle z-cap))
