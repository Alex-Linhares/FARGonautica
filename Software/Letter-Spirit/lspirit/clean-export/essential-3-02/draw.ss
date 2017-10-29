(define pick-whole
  (lambda (cat)
    (weighted-roulette
     (lookup-list cat *letter-concepts*))))

(define draft-whole-role
  (lambda (whole role)
    (set! *the-whole* whole)
    (set! *the-role* role)
    (set! *other-stuff* '())
    (set! *touch-points* '())
    (set! *avoid-points* '())
    (set! *avoid-quanta* '())
    (draft *the-role*)))

(define prep-next
  (lambda (role)
    (set! *the-role* role)
    (draft-init)))

(define draw-init
  (lambda ()
    (set-full-levels)
    (set! *favored-sp-types*
	  '((*relative-nvs* 10) (*val-to-val-nvs* 10) (*abstract-rules* 10)
	    (*literal-motifs* 10) (*translate-motifs* 10)
	    (*turn-180-motifs* 10) (*turn-90-motifs* 10)
	    (*turn-45-motifs* 10)))
    (set! *favored-motif-types*
	  '((*literal-motifs* 10) (*translate-motifs* 10)
	    (*turn-180-motifs* 10)
	    (*turn-90-motifs* 10) (*turn-45-motifs* 10)))
    (clear-sublist-cache) ; even need to do this???
    (set-favored-levels 2)
    (set! *other-stuff* '())
    (set! *touch-points* '())
    (set! *avoid-points* '())
    (set! *avoid-quanta* '())))

; add option of library lookup of roles
; make decisions probabilistic, and perhaps based on
; global weights that vary over a run
(define draw-cat
  (lambda (cat)
    (begin
      (set! *the-whole* (pick-whole cat))
      (draw-init)
      (let*
	  ([roles (scramble '() (whole-roles *the-whole*))]
	   [cat-and-fillers
	    (cons cat
		  (map borrow-or-draft roles))]
	   [quanta
	    (apply append (cdr cat-and-fillers))])
	(set! *quanta-list* quanta)
	(token-place cat quanta "white")
	cat-and-fillers))))

(define draw-alphabet
  (lambda ()
    (begin
      (clear-scratchpad)
      (draw-cat 'a) (draw-cat 'b) (draw-cat 'c) (draw-cat 'd)
      (draw-cat 'e) (draw-cat 'f) (draw-cat 'g) (draw-cat 'h)
      (draw-cat 'i) (draw-cat 'j) (draw-cat 'k) (draw-cat 'l)
      (draw-cat 'm) (draw-cat 'n) (draw-cat 'o) (draw-cat 'p)
      (draw-cat 'q) (draw-cat 'r) (draw-cat 's) (draw-cat 't)
      (draw-cat 'u) (draw-cat 'v) (draw-cat 'w) (draw-cat 'x)
      (draw-cat 'y) (draw-cat 'z))))

(define tester
  (lambda ()
    (begin
      (print "SS~%")
      (explore-font 'ss)
      (map draft *roles*)
      (print "BR~%")
      (explore-font 'br)
      (map draft *roles*)
      (print "BL~%")
      (explore-font 'bl)
      (map draft *roles*)
      (print "BT~%")
      (explore-font 'bt)
      (map draft *roles*))))
