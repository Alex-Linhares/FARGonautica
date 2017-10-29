; demo: d in Benz-Rt BCEFG environment
; post 16 19 22 25
; bowl 9 8 23 in progress
(define demo-d
  (lambda ()
    (begin
      ; (get-seeds 'benzene-right *seed-set*)
      (set! *the-whole* 'd1)
      (set! *the-role* 'right-post)
      (draw-init)
      (set! *own-stuff*  '(16 19 22 25))
      (record-touching-info)
      (set! *the-role* 'left-bowl)
      (draft-init)
      (set! *other-stuff* '(16 19 22 25))
      (set! *own-stuff*  '(9 8 23))
      (set! *last-point* 5)
      (set! *current-point* 4)
      (step-init))))

; need *tip-2* to be 17 to proceed

(define maybe-codelet
  (lambda ()
    (if (not (null? *coderack*))
	(begin
	  (run-codelet)
	  (prune-candidates)))))

(define run-thirty
  (lambda ()
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)
    (maybe-codelet)))

(define choice-show
  (lambda ()
    (let
	([bums (subtract (map car (choice-info)) (map cadr *candidates*))])
      (remove-keys bums (choice-info)))))

