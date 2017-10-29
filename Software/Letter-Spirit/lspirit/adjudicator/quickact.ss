; include r-roles right here?! -100..+100 bounds checking?


; (cons (eval (cadr a1-act)) (cdr a1-act))


; looks to me like 11 operations to calculate new activation
; for a two-role whole

(set! *trio-weight* 0.2)
(set! *pair-weight* 0.3)
(set! *link-weight* 0.6)

(set! whole-acts '(a1-act a2-act b1-act b2-act c1-act d1-act d2-act
      e1-act e2-act f1-act g1-act g2-act h1-act i1-act j1-act k1-act
      l1-act m1-act n1-act o1-act p1-act p2-act q1-act q2-act r1-act
      s1-act s2-act t1-act u1-act v1-act w1-act x1-act y1-act y2-act
      z1-act))

(set! role-acts '(a-arch-act backslash-act basebar-act cap-act
      center-post-act circle-act crossbar-act dot-act down-arm-act
      down-circle-act e-bowl-act e-crossbar-act e-tail-act f-post-act
      foreslash-act halfpost-act left-bowl-act left-downbowl-act
      left-halfarc-act left-halfarch-act left-halfpost-act
      left-post-act left-tail-act left-uparc-act left-upbowl-act
      left-wing-act right-bowl-act right-buttress-act right-curl-act
      right-downbowl-act right-halfarc-act right-halfarch-act
      right-halfpost-act right-hook-act right-post-act right-tail-act
      right-wing-act s-base-act s-crossbar-act t-post-act up-arm-act
      up-circle-act z-cap-act))

(set! a1-act '(0 (* *pair-weight* (+ (car a-arch-act)
				     (car left-downbowl-act)))))
(set! a2-act '(0 (* *pair-weight* (+ (car a-arch-act)
				     (car down-circle-act)))))
(set! b1-act '(0 (* *pair-weight* (+ (car left-post-act)
				     (car right-bowl-act)))))
(set! b2-act '(0 (* *pair-weight* (+ (car left-post-act)
				     (car circle-act)))))
(set! c1-act '(0 (* 0.6 (car left-bowl-act))))
(set! d1-act '(0 (* *pair-weight* (+ (car right-post-act)
				     (car left-bowl-act)))))
(set! d2-act '(0 (* *pair-weight* (+ (car right-post-act)
				     (car circle-act)))))
(set! e1-act '(0 (* *pair-weight* (+ (car e-bowl-act)
				     (car e-crossbar-act)))))
(set! e2-act '(0 (* *pair-weight* (+ (car up-circle-act)
				     (car e-tail-act)))))
(set! f1-act '(0 (* *pair-weight* (+ (car f-post-act)
				     (car crossbar-act)))))
(set! g1-act '(0 (* *pair-weight* (+ (car right-hook-act)
				     (car left-bowl-act)))))
(set! g2-act '(0 (* *pair-weight* (+ (car right-hook-act)
				     (car circle-act)))))
(set! h1-act '(0 (* *pair-weight* (+ (car left-post-act)
				     (car right-buttress-act)))))
(set! i1-act '(0 (* *pair-weight* (+ (car halfpost-act)
				     (car dot-act)))))
(set! j1-act '(0 (* *pair-weight* (+ (car right-curl-act)
				     (car dot-act)))))
(set! k1-act '(0 (* *trio-weight* (+ (car left-post-act)
				     (car up-arm-act)
				     (car down-arm-act)))))
(set! l1-act '(0 (* 0.6 (car center-post-act))))
(set! m1-act '(0 (* *trio-weight* (+ (car left-halfpost-act)
				     (car left-halfarch-act)
				     (car right-halfarch-act)))))
(set! n1-act '(0 (* *pair-weight* (+ (car left-halfpost-act)
				     (car right-buttress-act)))))
(set! o1-act '(0 (* 0.6 (car circle-act))))
(set! p1-act '(0 (* *pair-weight* (+ (car left-tail-act)
				     (car right-bowl-act)))))
(set! p2-act '(0 (* *pair-weight* (+ (car left-tail-act)
				     (car circle-act)))))
(set! q1-act '(0 (* *pair-weight* (+ (car right-tail-act)
				     (car left-bowl-act)))))
(set! q2-act '(0 (* *pair-weight* (+ (car right-tail-act)
				     (car circle-act)))))
(set! r1-act '(0 (* *pair-weight* (+ (car left-halfpost-act)
				     (car cap-act)))))
(set! s1-act '(0 (* *trio-weight* (+ (car cap-act)
				     (car s-crossbar-act)
				     (car s-base-act)))))
(set! s2-act '(0 (* *pair-weight* (+ (car left-upbowl-act)
				     (car right-downbowl-act)))))
(set! t1-act '(0 (* *pair-weight* (+ (car t-post-act)
				     (car crossbar-act)))))
(set! u1-act '(0 (* *pair-weight* (+ (car right-halfpost-act)
				     (car left-uparc-act)))))
(set! v1-act '(0 (* *pair-weight* (+ (car left-wing-act)
				     (car right-wing-act)))))
(set! w1-act '(0 (* *trio-weight* (+ (car right-halfpost-act)
				     (car left-halfarc-act)
				     (car right-halfarc-act)))))
(set! x1-act '(0 (* *pair-weight* (+ (car foreslash-act)
				     (car backslash-act)))))
(set! y1-act '(0 (* *pair-weight* (+ (car right-curl-act)
				     (car left-uparc-act)))))
(set! y2-act '(0 (* *pair-weight* (+ (car right-curl-act)
				     (car backslash-act)))))
(set! z1-act '(0 (* *trio-weight* (+ (car z-cap-act)
				     (car foreslash-act)
				     (car basebar-act)))))

(set! a-arch-act
      '(0 (* *link-weight* (max -1 (car a1-act) (car a2-act)))))
(set! backslash-act
      '(0 (* *link-weight* (max -1 (car x1-act) (car y2-act)))))
(set! basebar-act
      '(0 (* *link-weight* (max -1 (car z1-act)))))
(set! cap-act
      '(0 (* *link-weight* (max -1 (car r1-act) (car s1-act)))))
(set! center-post-act
      '(0 (* *link-weight* (max -1 (car l1-act)))))
(set! circle-act
      '(0 (* *link-weight* (max -1 (car b2-act) (car d2-act) (car g2-act)
				(car o1-act) (car p2-act) (car q2-act)))))
(set! crossbar-act
      '(0 (* *link-weight* (max -1 (car f1-act) (car t1-act)))))
(set! dot-act
      '(0 (* *link-weight* (max -1 (car i1-act) (car j1-act)))))
(set! down-arm-act
      '(0 (* *link-weight* (max -1 (car k1-act)))))
(set! down-circle-act
      '(0 (* *link-weight* (max -1 (car a2-act)))))
(set! e-bowl-act
      '(0 (* *link-weight* (max -1 (car e1-act)))))
(set! e-crossbar-act
      '(0 (* *link-weight* (max -1 (car e1-act)))))
(set! e-tail-act
      '(0 (* *link-weight* (max -1 (car e2-act)))))
(set! f-post-act
      '(0 (* *link-weight* (max -1 (car f1-act)))))
(set! foreslash-act
      '(0 (* *link-weight* (max -1 (car x1-act) (car z1-act)))))
(set! halfpost-act
      '(0 (* *link-weight* (max -1 (car i1-act)))))
(set! left-bowl-act
      '(0 (* *link-weight* (max -1 (car c1-act) (car d1-act)
				(car g1-act) (car q1-act)))))
(set! left-downbowl-act
      '(0 (* *link-weight* (max -1 (car a1-act)))))
(set! left-halfarc-act
      '(0 (* *link-weight* (max -1 (car w1-act)))))
(set! left-halfarch-act
      '(0 (* *link-weight* (max -1 (car m1-act)))))
(set! left-halfpost-act
      '(0 (* *link-weight* (max -1 (car m1-act) (car n1-act) (car r1-act)))))
(set! left-post-act
      '(0 (* *link-weight* (max -1 (car b1-act) (car b2-act)
				(car h1-act) (car k1-act)))))
(set! left-tail-act
      '(0 (* *link-weight* (max -1 (car p1-act) (car p2-act)))))
(set! left-uparc-act
      '(0 (* *link-weight* (max -1 (car u1-act) (car y1-act)))))
(set! left-upbowl-act
      '(0 (* *link-weight* (max -1 (car s2-act)))))
(set! left-wing-act
      '(0 (* *link-weight* (max -1 (car v1-act)))))
(set! right-bowl-act
      '(0 (* *link-weight* (max -1 (car b1-act) (car p1-act)))))
(set! right-buttress-act
      '(0 (* *link-weight* (max -1 (car h1-act) (car n1-act)))))
(set! right-curl-act
      '(0 (* *link-weight* (max -1 (car j1-act) (car y1-act)))))
(set! right-downbowl-act
      '(0 (* *link-weight* (max -1 (car s2-act)))))
(set! right-halfarc-act
      '(0 (* *link-weight* (max -1 (car w1-act)))))
(set! right-halfarch-act
      '(0 (* *link-weight* (max -1 (car m1-act)))))
(set! right-halfpost-act
      '(0 (* *link-weight* (max -1 (car u1-act) (car w1-act)))))
(set! right-hook-act
      '(0 (* *link-weight* (max -1 (car g1-act) (car g2-act)))))
(set! right-post-act
      '(0 (* *link-weight* (max -1 (car d1-act) (car d2-act)))))
(set! right-tail-act
      '(0 (* *link-weight* (max -1 (car q1-act) (car q2-act)))))
(set! right-wing-act
      '(0 (* *link-weight* (max -1 (car v1-act)))))
(set! s-base-act
      '(0 (* *link-weight* (max -1 (car s1-act)))))
(set! s-crossbar-act
      '(0 (* *link-weight* (max -1 (car s1-act)))))
(set! t-post-act
      '(0 (* *link-weight* (max -1 (car t1-act)))))
(set! up-arm-act
      '(0 (* *link-weight* (max -1 (car k1-act)))))
(set! up-circle-act
      '(0 (* *link-weight* (max -1 (car e2-act)))))
(set! z-cap-act
      '(0 (* *link-weight* (max -1 (car z1-act)))))

(define change
  (lambda (x y)
    (set-top-level-value! x y)))

(define buffer-role-act
  (lambda (node)
    (let*
	((node-eval (eval node))
	 (old-act (car node-eval))
	 (new-act (min 100 (max -100
				(+ (eval (cadr node-eval))
				   (min 40 old-act))))))
      (set! *act-buffer* (cons (list node new-act)
			       *act-buffer*)))))

(define buffer-whole-act
  (lambda (node)
    (let*
	((node-eval (eval node))
	 (old-act (car node-eval))
	 (new-act (min 100 (max -100
				(+ (eval (cadr node-eval))
				   (min 20 old-act))))))
      (set! *act-buffer* (cons (list node new-act)
			       *act-buffer*)))))

(define clear-act-buffer
  (lambda ()
    (set! *act-buffer* '())))

(define clear-act
  (lambda (role)
    (set-activation role 0)))

(define clear-acts
  (lambda ()
    (begin
      (set! *all-sparked* #f)
      (clear-act-buffer)
      (clear-bindings)
      (mapcar clear-act *roles*)
      (mapcar clear-act *wholes*))))

(define flush-act
  (lambda (pair)
    (set-top-level-value! (car pair)
			  (cons (cadr pair) (cdr (eval (car pair)))))))

(define flush-acts
  (lambda (a-ls)
    (if
	(null? a-ls)
	(set! *act-buffer* '())
	(begin
	  (flush-act (car a-ls))
	  (flush-acts (cdr a-ls))))))

(define examiner-spread-acts
  (lambda ()
    (begin
      (mapcar buffer-role-act role-acts)
      (mapcar buffer-whole-act whole-acts)
      (flush-acts *act-buffer*))))
