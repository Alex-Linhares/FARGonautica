;=============================================================================
; run-judge.ss : grid letter style recognizer
;=============================================================================

; switch for graphics vs ACSII display
(define *graphics* #f)

; load the smart loader code (check for object files and load them first)
(load "loader.so")

(set! *codelet-pause* #f)       ; pause after every codelet

(set! *noisy-codelets* #f)      ; codemsg <-
(set! *noisy-bonder* #f)        ; bondmsg <-
(set! *noisy-coderack* #f)      ; rackmsg
(set! *noisy-thermometer* #f)   ; tempmsg
(set! *noisy-memory* #f)        ; memmsg <-
(set! *statflag* #t)            ; load stats files and set statmsg

(set! *max-codelets* 2500)

; track how many codelets run here
(set! *codelets-run* 0)

(if *graphics*
    ; sxm windows for each of these which can be turned off
    (begin
      (set! *display-roles* #t)
      (set! *display-temp* #t)
      (set! *display-coderack* #t)
      ; draw activated roles in the workspace
      (set! *draw-roles* #t))     ; <-- overridden by display-roles
    (begin
      ; these are always false since they require *graphics*
      (set! *display-roles* #f)
      (set! *display-temp* #f)
      (set! *display-coderack* #f)
      (set! *draw-roles* #f)))
    
; from now on smart loader will check for compiled versions of the files!
(sload 'tools)
(sload 'glom)
(sload 'labeler)
(sload 'memory)
; (sload 'get-mystery)
(if *graphics* (sload 'graphics))
(if *statflag* (sload 'stats))
(randomize)  ; get a REALLY random seed
(define statmsg
  (ifprint-proto *statflag*))

(define recognize
  (lambda optional-args
    (set! *codelets-run* 0)
    (if (not (null? optional-args))
	(begin
	  (set! *seed* (car optional-args))
	  (random-seed (car optional-args)))
	(set! *seed* (random-seed)))
    (glom)
    (g-start)
    (label-parts)))

(define re-rec
  (lambda optional-args
    (set! *codelets-run* 0)
    (if (not (null? optional-args))
	(begin
	  (set! *seed* (car optional-args))
	  (random-seed (car optional-args)))
	(set! *seed* (random-seed)))
    (reglom)
    (label-parts)))

(define clean
  (lambda ()
    (init-memory)
    (if (and (top-level-bound? '*clamped-roles*)
	     (not (null? *clamped-roles*)))
	(clamp-wholes (car *clamped-roles*) (cadr *clamped-roles*)))
    (set! *role-looseness* 1)   ; these are now reset by coderack during run
    (set! *max-roles* 7)
    (set! *solved* #f)
    (set! *temperature* 100)
    (set! *coderack* '())
    (if *graphics*
	(begin
	  (if *display-roles* (draw-roles))
	  (if *display-temp* (draw-thermometer))
	  (if *display-coderack* (draw-coderack *coderack*))
	  (set! *ant-obj* '())
	  (draw-grid-back *gc*)
	  (draw-grid *gc*)
	  (erase-info *gc*)
	  (draw-info-bar "information bar")))
    (collect 4)))
  
