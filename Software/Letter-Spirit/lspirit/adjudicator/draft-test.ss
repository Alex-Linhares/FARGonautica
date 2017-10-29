; test rules for being broken, norms + NVs for being violated

(define breaks-rule?
  (lambda (quantum quanta-so-far rtype)
    (let
	([original-quanta *quanta-list*]
	 [pre-test (rule-type rtype)])
      (begin
	(set! *quanta-list* (condcons quantum  quanta-so-far))
	(let
	    ([post-test (rule-type rtype)])
	  (and pre-test (not post-test)))))))

; this quantum will lead to what, vis-a-vis this norm?
(define hypo-norm-violation
  (lambda (quantum quanta-so-far nv-type)
    (compare-nv-type
     (list
      *role-to-draw*
      (condcons quantum quanta-so-far))
     nv-type)))

; of course, you may want the norm violated

; motif continuation -- for literal, pretty easy
; for other motif types, you must, in principle, search paths
