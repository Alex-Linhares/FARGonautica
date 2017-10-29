;===========================================================================
; shaker.ss : tree-shaking program phase one | shaking functions
;===========================================================================
; globals: *joints* list of ((quantum quantum) glue) pairs loaded by bond.ss
;          *parts* list of ((q q) ((q q) (q))..) pairs representing parts
;          *quanta-list* list of all on quanta (numbers)
;---------------------------------------------------------------------------
; process the *joints* list until some of the bonds have been broken
; use the number of quanta in the letter as a guide to the shaking process
; use the *shake-part-size* constant from glom.ss
(define shake-tree
  (lambda ()
    (if *graphics*
	(begin
	  (draw-info-bar "shaking...")
	  (erase-info workGP)))
    (if *graphics* (draw-shake *quanta-list*))
    (let* ((jls *joints*)
	   (qnta (length *quanta-list*))
	   (pts (add1 (round (/ qnta *shake-part-size*))))
	   (jts (length *joints*))
	   (rem (- jts pts)))
      (set! *parts* (shake-joints jls rem)))))

; run the dissolver until *shake-threshold* percent of them are left
; then make a parts list
(define shake-joints
  (lambda (joints num-left)
    (if (and (> (length joints) num-left)
	     (not (negative? num-left)))
	(let ((njoints (dissolve joints)))
	  (shake-joints njoints num-left))
	(begin
	  (set! *joints* joints)
	  (make-parts (erase-glue joints))))))

; cycle through the joints list once dissolving units of glue *solvant*
; percent of the time.  Dissolve-ant returns '() if joint is gone
(define dissolve
  (lambda (joints)
    (cond
      ((null? joints) '())
      (else (let* ((disans (dissolve-ant (car joints)))
		   (gone? (null? disans)))
	      (if gone?
		  (begin
		    ; sun graphics call
		    (if *graphics*
			(begin
;			  (erase! workGP *d-obj*)
			  (erase-subgloms (lookup (caar joints)
					    *part-numbers*))))
		    (dissolve (cdr joints)))
		  (begin
		    ; sun graphics
;		    (if *graphics* (erase! workGP *d-obj*))
		    (cons disans
			  (dissolve (cdr joints))))))))))
  
; dissolve one unit of glue from a joint probabalistically
(define dissolve-ant
  (lambda (joint)
    ; sun graphics
;    (if *graphics* (draw-dissolve (car joint)))
    (let ((number (add1 (random 99))))
      (if (< number *solvant*)
	  (if (zero? (sub1 (cadr joint)))
	      '()
	      (list (car joint) (sub1 (cadr joint))))
	  joint))))

; Make lists of joints which preserve connectivity info.
(define make-parts
  (lambda (u-j-list)
    (let ((j-list (sort q-listsortkey u-j-list)))
      (check-singles (merge-parts j-list)))))

; check for any single quantum parts that may have been formed...add them.
(define check-singles
  (lambda (pts)
    (let ((qls *quanta-list*))
      (letrec ((loop (lambda (pts qls)
		       (cond
			 ((null? qls) '())
			 ((not (member*? (car qls) pts)) (cons (list (car qls))
							 (loop pts (cdr qls))))
			 (else (loop pts (cdr qls)))))))
	(append (loop pts qls) pts)))))

; bug in combine fixed 1/10/95
; should now combine joints better 
(define combine
  (lambda (i1 i2)
    (cond
      ((list? (car i1)) (if (list? (car i2))
			    (append i2 i1)
			    (cons i2 i1)))
      (else (if (list? (car i2))
		(append i2 (list i1))
		(cons i2 (list i1)))))))

(define merge-parts
  (lambda (pls)
    (let ((times (length pls)))
      (letrec ((cycle (lambda (item remainder)
			(cond
			  ((null? remainder) (list item))
			  ((overlap*? item (car remainder))
			     (cycle (combine item (car remainder))
				    (cdr remainder)))
			  (else (cons (car remainder)
				      (cycle item (cdr remainder))))))))
	(letrec ((outloop (lambda (times ls)
			    (cond
			      ((or (= times 0)
				   (null? (cdr ls))) ls)
			      (else (outloop (sub1 times)
				      (cycle (car ls)
					     (cdr ls))))))))
	  (outloop times pls))))))

(define q-listsortkey
  (lambda (l1 l2)
    (let ((cl1 (car l1))
	  (dl1 (cadr l1))
	  (cl2 (car l2))
	  (dl2 (cadr l2)))
      (cond
	((and (= cl1 cl2)
	      (< dl1 dl2)) #t)
	((< cl1 cl2) #t)
	(else #f)))))

; collapse the joints list making a list of bonded quanta pairs      
(define erase-glue
  (lambda (joints)
    (cond
      ((null? joints) '())
      (else (cons (caar joints)
		  (erase-glue (cdr joints)))))))

;----------------------------------------------------------------------------
; make a list of on-quanta (by number) for merging with parts list.
(define q-list
  (lambda (bits c)
    (cond
      ((null? bits) '())
      ((= (car bits) 1) (cons c
			      (q-list (cdr bits) (add1 c))))
      (else (q-list (cdr bits) (add1 c))))))


