(set! *base-borrow-list*
     '((a ((steal e *180-turn-list*)))
	(b ((steal q *180-turn-list*) (steal d *horiz-flip-list*)
	    (steal p *vert-flip-list*)))
	(c ((chop o *rightedge-zone*)))
	(d ((steal p *180-turn-list*) (steal b *horiz-flip-list*)
	    (steal q *vert-flip-list*)))
	(e ((steal a *180-turn-list*)))
	(h ((chop b *base-bar*) (steal y *180-turn-list*)))
	(i ((chop j *descender-zone*)))
	(m ((steal w *180-turn-list*)))
	(n ((chop o *base-bar*) (steal u *180-turn-list*)
	    (chop h *ascender-zone*)))
	(o ((chop b *ascender-zone*) (chop d *ascender-zone*)
	    (chop g *descender-zone*) (chop p *descender-zone*)
	    (chop q *descender-zone*)))
	(p ((steal d *180-turn-list*) (steal q *horiz-flip-list*)
	    (steal b *vert-flip-list*)))
	(q ((steal b *180-turn-list*) (steal p *horiz-flip-list*)
	    (steal d *vert-flip-list*)))
	(s ((steal z *horiz-flip-list*)))
	(u ((chop o *x-bar*) (chop y *descender-zone*)
	    (steal n *180-turn-list*)))
	(w ((steal m *180-turn-list*)))
	(y ((chop g *x-bar*)  (steal h *180-turn-list*)))
	(z ((steal s *horiz-flip-list*)))
	(f ())
	(g ())
	(j ())
	(k ())
	(l ())
	(r ((chop c *base-bar*) (chop n *rightedge-zone*)))
	(t ())
	(v ())
	(x ())))

(set! *role-borrow-list*
      '((a-arch (a-arch right-buttress))
	(backslash (backslash))
	(basebar (basebar s-base))
	(cap (cap z-cap crossbar))
	(center-post (center-post))
	(circle (circle))
	(crossbar (crossbar cap z-cap))
	(dot (dot))
	(down-arm (down-arm))
	(down-circle (down-circle))
	(e-bowl (e-bowl left-bowl))
	(e-crossbar (e-crossbar s-crossbar))
	(e-tail (e-tail))
	(f-post (f-post))
	(foreslash (foreslash))
	(halfpost
	 (halfpost right-halfarc left-halfarch right-halfarch right-halfpost))
	(left-bowl (left-bowl e-bowl))
	(left-downbowl (left-downbowl))
	(left-halfarc (left-halfarc))
	(left-halfarch (left-halfarch))
	(left-halfpost (left-halfpost))
	(left-post (left-post))
	(left-tail (left-tail))
	(left-uparc (left-uparc))
	(left-upbowl (left-upbowl))
	(left-wing (left-wing))
	(right-bowl (right-bowl))
	(right-buttress (a-arch right-buttress))
	(right-curl (right-curl right-hook))
	(right-downbowl (right-downbowl))
	(right-halfarc (right-halfarc))
	(right-halfarch (right-halfarch))
	(right-halfpost (right-halfpost))
	(right-hook (right-hook right-curl))
	(right-post (right-post))
	(right-tail (right-tail))
	(right-wing (right-wing))
	(s-base (s-base basebar))
	(s-crossbar (s-crossbar e-crossbar))
	(t-post (t-post))
	(up-arm (up-arm))
	(up-circle (up-circle))
	(z-cap (z-cap cap crossbar))))

(set! *flip-flag* #f)

(define borrow-options
  (lambda (cat)
    (intersect
     (map cadr (car (lookup-list cat *borrow-list*)))
     (map car *library*))))

(define borrow-execute
  (lambda (borrow-string)
    (let*
	([operation (car borrow-string)]
	 [cat (cadr borrow-string)]
	 [old-stuff (car (lookup-list cat *library*))]
	 [operand (caddr borrow-string)])
      (if (eq? operation 'steal)
	  (deep-substitute old-stuff (eval operand))
	  (subtract old-stuff (eval operand))))))

; finds the command to turn cat2 into a cat1 candidate
(define cat-cat-command
  (lambda (cat1 cat2)
    (let*
	([command-choices (car (lookup-list cat1 *borrow-list*))]
	 [index (map list (map cadr command-choices) command-choices)])
      (lookup cat2 index))))

(define beg-borrow-or-draw
  (lambda (cat)
    (let*
	([choices (borrow-options cat)]
	 [stolen (if (not (null? choices)) (roulette choices) '())]
	 [code (if (not (null? choices)) (cat-cat-command cat stolen))])
      (set! *drawn-from* cat)
      (if (and
	   (not (null? choices))
	   (n-percent 85))
	  (begin
	    ; (printf "Borrow ~s " stolen)
	    (set! *drawn-from* stolen)
	    (set! *borrow-list*
		  (cons
		   (list
		    cat
		    (remove
		     code
		     (lookup cat *borrow-list*)))
		   (remove-key cat *borrow-list*)))
	    ; REMOVE stolen from *borrow-list*
	    (set! *quanta-list*
		  (borrow-execute code)))
	  (draw-cat cat)))))
