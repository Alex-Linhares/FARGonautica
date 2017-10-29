;===========================================================================
; display.ss : display parts
;===========================================================================
; the code here is gui specific.  
;
;---------------------------( data capture )--------------------------------
; in this section a front end gui program is run for data entry purposes.

(set! *mystery-letter* '())

; A function for loading a letter given a hex value.
(define load-letter
  (lambda (lt hex)
    (printf "Loading mystery letter ~s by hex.~%" lt)
    (let ([bin (hex->bin (string->list hex))])
      (set! *mystery-letter* (cons lt bin))
      (set! *quanta* bin)
      (set! *quanta-list* (q-list *quanta* 0)))))

; Get a letter using the get-mystery SXM program that uses the gridfont
; database.  (Or default to standard-square B if graphics are off)
(define get-data
  (lambda ()
    ; make the sgl code stand-alone
    (if *graphics*
	; Note that the get-mystery program runs inside a call/cc
	; the program will refer to k to get back out
	(call/cc (lambda (k)
		   (set! toplevel-k k)
		   (get-mystery)
		   (graf-flush)
		   (let f () (*flush-event-queue*) (f))))
	(if (null? *mystery-letter*)
	    ; resort to standard square `b' if no *mystery-letter* has been
	    ; loaded (say by collect-stats or some other front-end)
	    (set! *mystery-letter* '(b 0 0 0 0 1 1 0 0 1 1 0 0 0 0 1 0 0 1 0 0 1 0 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
    (set! *quanta* (cdr *mystery-letter*))
    (set! *quanta-list* (q-list *quanta* 0))
    (if *graphics* (draw-pt-quanta workGP *quanta-list*))))
