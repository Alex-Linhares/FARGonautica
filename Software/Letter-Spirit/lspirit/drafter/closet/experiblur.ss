; these routines will compute intercategory distances based on the
; gestalt code
; not for use in LS, but rather for analyzing psych experiment

(define gestalt-compare
  (lambda ()
    (let*
	([quanta-raw
	  (lambda (cat)
	    (map
	     round-3
	     (list-to-ten
	      (peel-list
	       (make-square-blur-list (lookup cat *blurred-prototypes*)
				      *blurred-prototypes*)))))]
	 [square-raw
	  (lambda (cat)
	    (map
	     round-3
	     (list-to-ten
	      (peel-list
	       (make-square-blur-list (lookup cat *square-prototypes*)
				      *square-prototypes*)))))]
	 [tip-raw
	  (lambda (cat)
	    (map
	     round-3
	     (list-to-ten
	   (peel-list
	    (make-square-blur-list (lookup cat *tip-prototypes*)
				   *tip-prototypes*)))))]
	 [closure-raw
	  (lambda (cat)
	    (property-check *closure-prototypes*
			    (eq? 1 (lookup cat *closure-prototypes*))))]
	 [ascender-raw
	  (lambda (cat)
	    (property-check *ascend-prototypes*
			    (eq? 1 (lookup cat *ascend-prototypes*))))]
	 [descender-raw
	  (lambda (cat)
	    (property-check *descend-prototypes*
			    (eq? 1 (lookup cat *descend-prototypes*))))]
	 [all-cats
	  (map car *blurred-prototypes*)])
      (list
       (map quanta-raw all-cats)
       (map square-raw all-cats)
       (map tip-raw all-cats)
       (map closure-raw all-cats)
       (map ascender-raw all-cats)
       (map descender-raw all-cats)))))

(define convert-this
  (lambda (a b c d e f)
    (cond
     ((null? a) nil)
     (else
      (cons
       ; scales to allow values up to 20 or down to -55
	    (round-3 (- 20
			(* (/ *gestalt-perk* 16.0)
			   (-
			    (+
			     (car a) (car b) (car c)
			     (* 0.25
				(+ (car d) (car e) (car f)))) 21.5))))
	    (convert-this
	     (cdr a) (cdr b) (cdr c) (cdr d) (cdr e) (cdr f)))))))


(define make-square-blur-list
  (lambda (qls pls)
    (cond
     [(null? pls) '()]
     [else (cons (list (caar pls)
		       (make-square-blur qls (cadar pls)))
		 (make-square-blur-list qls (cdr pls)))])))

(define make-square-blur
  (lambda (qls letvals)
    (cond
     [(null? letvals) 0]
     [else (+ (make-square-blur (cdr qls) (cdr letvals))
	   (abs (- (car qls) (car letvals))))])))

(define junction-squares
  (lambda (qls)
    (let*
	([junc?
	  (lambda (n)
	    (if
	     (> (length (intersect (lookup n *point-list*) qls)) 2)
		1
		0))]
	 [vert1 (junc? 1)]
	 [vert2 (junc? 2)]
	 [vert3 (junc? 3)]
	 [vert4 (junc? 4)]
	 [vert5 (junc? 5)]
	 [vert6 (junc? 6)]
	 [vert7 (junc? 7)]
	 [vert8 (junc? 8)]
	 [vert9 (junc? 9)]
	 [vert10 (junc? 10)]
	 [vert11 (junc? 11)]
	 [vert12 (junc? 12)]
	 [vert13 (junc? 13)]
	 [vert14 (junc? 14)]
	 [vert15 (junc? 15)]
	 [vert16 (junc? 16)]
	 [vert17 (junc? 17)]
	 [vert18 (junc? 18)]
	 [vert19 (junc? 19)]
	 [vert20 (junc? 20)]
	 [vert21 (junc? 21)])
      (list
       (+ vert1 vert2 vert8 vert9)
       (+ vert8 vert9 vert15 vert16)
       (+ vert2 vert3 vert9 vert10)
       (+ vert9 vert10 vert16 vert17)
       (+ vert3 vert4 vert10 vert11)
       (+ vert10 vert11 vert17 vert18)
       (+ vert4 vert5 vert11 vert12)
       (+ vert11 vert12 vert18 vert19)
       (+ vert5 vert6 vert12 vert13)
       (+ vert12 vert13 vert19 vert20)
       (+ vert6 vert7 vert13 vert14)
       (+ vert13 vert14 vert20 vert21)))))

(define junction-process
  (lambda (code)
    (let*
	([qls (q-list (hex->bin (string->list (cadr code))) 0)]
	 [these-juncts (junction-squares qls)]
	 [cat (car code)]
	 [old-total (lookup (car code) *junctions*)]
	 [new-total (map + old-total these-juncts)]
	 [new-entry (list cat new-total)])
    (set!
     *junctions*
     (cons
      new-entry
      (remove-key cat *junctions*))))))

(set! *junctions*
      (map list
	   *categories*
	   (n-copies '(0 0 0 0 0 0 0 0 0 0 0 0) 26)))

; based on psych-fonts
; junctions per square
(set! *junctions*
      '((a (0 0 0 0.23 0 1 0 0.92 0 0.15 0 0))
	(b (0 0 0.67 0 1.25 0 0.58 0 0 0 0 0))
	(c (0 0 0 0 0 0 0 0 0 0 0 0))
	(d (0 0 0 0.69 0 1.23 0 0.54 0 0 0 0))
	(e (0 0 0.19 0 0.81 0 0.81 0 0.19 0 0 0))
	(f (0 0 1 1 1 1 0 0 0 0 0 0))
	(g (0 0 0 0 0 0.6 0 1.07 0 0.47 0 0))
	(h (0 0 0.56 0 1 0 0.44 0 0 0 0 0))
	(i (0 0 0 0 0 0 0 0 0 0 0 0))
	(j (0 0 0 0 0 0 0 0 0 0 0 0))
	(k (0 0 0.23 0 1.31 0.62 1.08 0.62 0 0 0 0))
	(l (0 0 0 0 0 0 0 0 0 0 0 0))
	(m (0 0 0.47 0.47 0.84 0.74 0.37 0.26 0 0 0 0))
	(n (0 0 0.09 0.09 0.27 0.09 0.18 0 0 0 0 0))
	(o (0 0 0 0 0 0 0 0 0 0 0 0))
	(p (0 0 0.04 0 0.5 0 1.13 0 0.67 0 0 0))
	(q (0 0 0 0.04 0 0.54 0 1 0 0.5 0 0))
	(r (0 0 0 0 0.43 0 0.43 0 0 0 0 0))
	(s (0 0 0 0 0 0 0 0 0 0 0 0))
	(t (0 0 0.92 0.92 0.92 0.92 0 0 0 0 0 0))
	(u (0 0 0 0 0 0.6 0 0.6 0 0 0 0))
	(v (0 0 0 0 0 0 0 0 0 0 0 0))
	(w (0 0 0 0 0.24 0.24 0.76 0.71 0.52 0.48 0 0))
	(x (0 0 0 0 0.92 0.92 0.92 0.92 0 0 0 0))
	(y (0 0 0 0 0 0.52 0 0.9 0 0.38 0 0))
	(z (0 0 0 0 0 0 0 0 0 0 0 0))))

(define tip-raw
  (lambda (cat)
    (map
     round-3
     (peel-list
       (make-square-blur-list (lookup cat *tip-prototypes*)
			      *tip-prototypes*)))))
