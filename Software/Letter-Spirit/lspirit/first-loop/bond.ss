;===========================================================================
; bond.ss : bond-ant program phase one   |  bond functions
;===========================================================================
; globals:   *quanta*  letter to process in standard form
;            *neighbors* list of neighboring quanta
;            *joints* list of all joints between quanta which have glue
;            *redund-ants* number of ants whose job has been done before
; counters:  *globs-down*  *total-glue*  *bond-ants*  *ant-increment*
;            *ant-counter*
;---------------------------------------------------------------------------
; In this section of the program, a grid letter is swarmed by bond-ants
; which lay down globs of glue on quanta intersection points.  Glue
; globbing is done until a redundancy level is reached. 

; See glom.ss for more information RE angles and glue.

(define bondmsg
  (ifprint-proto #f))

(set! *joints* '())     ; list of ((quantum quantum) glue) pairs
(set! *redund-ants* 0)  ; number of ants whose job was previously done
(set! *globs-down* 0)   ; number of ants who put down glue
(set! *total-glue* 0)   ; total glue allocated
(set! *bond-ants* 0)    ; number of ants that ran
(set! *ant-increment* 30000) ; draw every other nth ant (saves memory and time)
                        ; JAR set it to where we won't be seeing
                        ; a lot of the ants, 4/8/97
(set! *ant-counter* 0)  ; number drawn so far

; run bond-quantum until the *redund-ant-c* threshold has been passed
; graphics is updated every *ant-increment* steps
(define bond-quanta
  (lambda ()
    (if (> *redund-ants* *redund-ant-c*)
	(begin
;	  (if *graphics*
;	      (erase! workGP *ant-obj*))
	  (bondmsg "Redund-ant-c: ~s~%" *redund-ant-c*)
	  (bondmsg "  Ants: ~s     Glue-ants: ~s     Glue: ~s~%"
	    *bond-ants* *globs-down* *total-glue*)
	  (bondmsg "Joints with glue are:~%")
	  (joint-print (sort sortjointkey *joints*)))
	(if
	    (> (length *quanta-list*) 1)
	    (begin
	      (bond-quantum)
	      (if (not (>= *ant-counter* *ant-increment*))
		  (set! *ant-counter* (add1 *ant-counter*))
		  (begin
		    (set! *ant-counter* 0)
		    (if *graphics*
			(begin
			  (draw-subgloms (make-display-list *joints*))
			  (update-bonding-info)))))
	      (bond-quanta))))))
	
; pick a random quanta, if ON find an on-neighbor (if any) and bond with glue
; handles a list of bonds to be made returned by pick-neighbor-to-glom
(define bond-quantum
  (lambda ()
    (let* ((quantum (pick-quantum))
	   (neighbor (pick-neighbor-to-glom quantum))
	   (lngth (length neighbor))
	   (b-ants-temp *bond-ants*))
      (set! *bond-ants* (add1 b-ants-temp))
      (cond
	((= lngth 0)
	 ; (printf "Quantum ~s has no neighbors.  Ant dies.~%" quantum)
	 '())
	((= lngth 1) (update-globs quantum (car neighbor)))
	(else (update-multiple-globs quantum neighbor))))))

; process a list of quantum pairs to glue 
(define update-multiple-globs
  (lambda (q nls)
    (cond
      ((null? nls) '())
      (else (update-globs q (car nls))
	    (update-multiple-globs q (cdr nls))))))

; either add a glob-list or update an existing entry
(define update-globs
  (lambda (quantum neighbor)
    (let* ((n-quantum (car neighbor))
	   (glob-pair (if (< quantum n-quantum)
			  (list quantum n-quantum)
			  (list n-quantum quantum)))
	   (glue (cadr neighbor))
	   (glob-entry (list glob-pair glue))
	   (old-joints *joints*)
	   (glue-down-temp *globs-down*)
	   (glue-temp *total-glue*))
      (set! *globs-down* (add1 glue-down-temp))
      (set! *total-glue* (+ glue-temp glue))
      (if (member*? glob-pair *joints*)
	  (update-existing-glob glob-pair glue)
	  (set! *joints* (cons glob-entry old-joints))))))
; hope I didn't break this

(define update-existing-glob
  (lambda (g-pair glue)
    (let ((j-temp *joints*)
	  (ra-temp *redund-ants*))
      (letrec ((update (lambda (j-ls)
			 (cond
			   ((null? j-ls) '())
			   ((equal? (caar j-ls)
			      g-pair) (cons (list g-pair (+ glue (cadar j-ls)))
					    (cdr j-ls)))
			   (else (cons (car j-ls)
				       (update (cdr j-ls))))))))
	(set! *joints* (update j-temp))
	(set! *redund-ants* (add1 ra-temp))
	#t))))



