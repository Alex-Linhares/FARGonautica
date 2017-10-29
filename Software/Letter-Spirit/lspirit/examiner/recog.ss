;=============================================================================
; recog.ss : grid letter recognizer  |  main program and help functions
;=============================================================================

; switch for graphics vs ACSII display
(define *graphics* #t)    ;;;; <----- for recog.ss this should ALWAYS be #t
                          ;;;; use stat-recog.ss for a non-graphical version

(set! *display-roles* #f)

; load the smart loader code (check for object files and load them first)
; (load "loader.so")

(set! *codelet-pause* #f)       ; pause after every codelet

; this will always be false for the interactive program
; this flag is used in (collect-stats ...) to write data to a file or not
(set! *write-stats* #f)

; track how many codelets run here
(set! *codelets-run* 0)

(if *graphics*
    ; sxm windows for each of these which can be turned off
    ; they MUST be on initially in order for the windows to set
    ; themselves up
    (begin
      (set! workTrove '())
      (set! tempTrove '())
      (set! roleTrove '())
      (set! rackTrove '())
;      (set! *display-roles* #t)
      (set! *display-temp* #t)
      (set! *display-coderack* #t)
      (set! *draw-codelets* #t)
      ; draw activated roles in the workspace
      (set! *draw-roles* #t))     ; <-- overridden by display-roles
    (begin
      ; these are always false since they require *graphics*
      (set! *display-roles* #f)
      (set! *display-temp* #f)
      (set! *display-coderack* #f)
      (set! *draw-roles* #f)))

(load "glom.ss")
(load "bond.ss")
(set! *rolesNwholes* (append *roles* *wholes*))


(randomize)  ; get a REALLY random seed (Jim's randomizer is in tools.ss)

(define statmsg
  (ifprint-proto *write-stats*))

(define recognize
  (lambda optional-args
    (set! *codelets-run* 0)
    (if (not (null? optional-args))
	(begin
	  (set! *seed* (car optional-args))
	  (random-seed (car optional-args)))
	(set! *seed* (random-seed)))
    (clear-acts)
    (get-data)
    (glom)
    (set! *spark-threshold* 50)
    (set! *punish* -25)  (set! *punish-hard* -40)
    (g-start)
    (label-parts)
    (get-highest-whole *wholes* '($ 0))))

(define re-rec
  (lambda optional-args
    (set! *codelets-run* 0)
    (if (not (null? optional-args))
	(begin
	  (set! *seed* (car optional-args))
	  (random-seed (car optional-args)))
	(set! *seed* (random-seed)))
    (clear-acts)
    (glom)
    (set! *spark-threshold* 50)
    (set! *punish* -25)  (set! *punish-hard* -40)
    (g-start)
    (label-parts)
    (get-highest-whole *wholes* '($ 0))))

(define clean
  (lambda ()
    (set! *temperature* 100)
    (set! *coderack* '())
    (set! *solved* #f)
    (if *graphics*
	(begin
	  (if *display-roles* (draw-role-activations))
	  (if *display-temp* (draw-thermometer))
	  (if *display-coderack* (draw-coderack *coderack*))
	  (set! *ant-obj* '())
	  (draw-grid-back workGP)
	  (draw-grid workGP)
	  (erase-info workGP)
	  (draw-info-bar "information bar")))
    (collect 4)))

; (recognize)
