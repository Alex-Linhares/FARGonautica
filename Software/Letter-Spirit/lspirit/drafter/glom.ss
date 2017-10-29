;===========================================================================
; glom.ss : bond-ant program phase one   |  main program and help functions
;===========================================================================
;  The glomming module gets a grid letter from the user using the get-mystery
;  gui program.  This letter is then attacked by bond-ants who swarm around
;  the letter laying down glue.  After a threshold of redundancy has been
;  exceeded, the letter (all glued together now) is shaken into a few major
;  parts.  It is these parts that are sent on to the labeling module.
;---------------------------------------------------------------------------
; globals:   *quanta*  letter to process in standard form
;            *quanta-list* letter to process (list of on-quanta)
;            *neighbors* list of neighboring quanta
;            *joints* list of ((q q) glue) pairs loaded by bond.ss
;---------------------------------------------------------------------------
(set! *ant-obj* '())
(sload 'know)
(sload 'bond)
(sload 'shaker)
(sload 'display)

; main function for the entire glomming module...returns control to
; the appropriate recog caller after glomming and shaking into parts.
(define glom
  (lambda ()
    (clean)
    (reset-glom-globals)
    (if *graphics* (draw-bonding-info))
    (bond-quanta)
    (shake-tree)
    #t))

;----------------------------( parameters )-----------------------------

; pick a quantum for an ant to land on
; now set to only consider on-quanta
(define pick-quantum
  (lambda ()
    (let* ((ons (length *quanta-list*))
	   (roll (random ons))
	   (pick (list-ref *quanta-list* roll)))
      (if (= *ant-increment* *ant-counter*)
	  (if *graphics*
	      (begin
;		(if (not (null? *ant-obj*))
;		    (erase! workGP *ant-obj*))
		(draw-ant pick))))
      pick)))
	  

;amount of glue to put down for given angle-types
(set! *f* 6)       ; straight ahead
(set! *45* 4)
(set! *90* 3)
(set! *135* 1)

;number of redund-ants to allow.  This affects the Gaussian aspect of
;randomness.
(set! *redund-ant-c* 500)

; shaking effect: this number corresponds to a rough "part size"
; divide quanta by this to get an estimated number of parts to result
; from a shake, then use that to figure out number of joints to dissolve
; note: calculations on NORMAL say to use a part size of 2.71
(set! *shake-part-size* 2.7)

; probability that dissolving solution will work on glue
(set! *solvant* 90)

(define reset-glom-globals
  (lambda ()
    (set! *joints* '())     ; list of joints and their associated glue
    (set! *parts* '())      ; list of parts 
    (set! *redund-ants* 0)  ; number of ants whose job was previously done
    (set! *globs-down* 0)   ; number of ants who put down glue
    (set! *total-glue* 0)   ; total glue allocated
    (set! *bond-ants* 0)))  ; number of ants that ran

;-----------------------( local help functions )-----------------------------

(define sortjointkey
  (lambda (ls1 ls2)
    (if (< (caar ls1) (caar ls2))
	#t
	#f)))

(define joint-print
  (lambda (ls)
    (cond
      ((null? ls) #t)
      (else (bondmsg "  ~s-~s: ~s~%" (caaar ls) (cadaar ls) (cadar ls))
	(joint-print (cdr ls))))))

;---------------------------( neighbor processing )-------------------------
; This function picks a neighbor or list of neighbors (given a quantum) which
; ants should consider bonding with.  It returns either a list
; ((<quantum> <glue>)), nil if no pairing is possible, or a list of
; ((<quantum> <glue>)...) pairs if the quantum has multiple neighbors.
; If a quantum can be bonded with n other possibilities, call in n-1 ants
; to run on the same quanta thus causing glue to be more evenly distributed.
; The n-1 ants do not process each pair in the list deterministically,
; instead they choose a random pair from the list <list length> times.

(define pick-neighbor-to-glom
  (lambda (quantum)
    (let* ((pn-list (prob-neighbors-list quantum))
	   (lngth (length pn-list)))
      (letrec ((multi-run-reserves
		 (lambda (lngth n)
		   (cond
		     ((= n 0) '())
		     (else (let ((pick (add1 (random lngth))))
			     (cons (nth pick pn-list)
				   (multi-run-reserves lngth (sub1 n)))))))))
	(cond
	  ((= lngth 0) '())
	  ((= lngth 1) (list (nth 1 pn-list)))
	  (else (multi-run-reserves lngth lngth)))))))
	     

; return a list of pairs of the form (<on-quanta> <glue>).  these pairs are
; the basis for the pick-neighbor-to-glom function.  The amount of glue
; is calculated based on the angle between a quanta pair.
(define prob-neighbors-list
  (lambda (quantum)
    (let ((n-list (get-on-neighbors-list quantum)))
      (letrec ((collapse (lambda (ls)
			   (cond
			     ((null? ls) '())
			     ((equal? (caar ls) '-) (collapse (cdr ls)))
			     (else (cons (car ls)
					 (collapse (cdr ls))))))))
	(collapse
	  (list (list (nth 4 n-list) *f*) (list (nth 11 n-list) *f*)
		(list (nth 3 n-list) *45*) (list (nth 5 n-list) *45*)
		(list (nth 10 n-list) *45*) (list (nth 12 n-list) *45*)
		(list (nth 2 n-list) *90*) (list (nth 6 n-list) *90*)
		(list (nth 9 n-list) *90*) (list (nth 13 n-list) *90*)
		(list (nth 1 n-list) *135*) (list (nth 7 n-list) *135*)
		(list (nth 8 n-list) *135*) (list (nth 14 n-list) *135*)))))))

; returns a list of all a quantum's neighbors that are on.  the list has
; a form as described in ANGLES in the knowledge.ss file.      
(define get-on-neighbors-list
  (lambda (quantum)
    (let ((n-list (lookup quantum *neighbors*)))
      (letrec ((keep-ons (lambda (ls)
                           (cond
                             ((null? ls) '())
                             ((equal? (car ls) '-) (cons (car ls)
                                                         (keep-ons (cdr ls))))
                             ((member? (car ls) *quanta-list*) (cons
								 (car ls)
								 (keep-ons
								   (cdr ls))))
                             (else (cons '- (keep-ons (cdr ls))))))))
        (keep-ons n-list)))))

;------------------------------------------------------------------------------


