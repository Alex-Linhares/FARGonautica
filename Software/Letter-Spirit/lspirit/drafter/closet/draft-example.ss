; *current-point* -- where we are right now
; *tip-1* -- where we started
; *tip-2* -- where we'll end - THIS WILL BE A LIST OF CHOICES
; *last-quantum* -- last quantum drawn
; *last-point* -- point before the current one
; *other-stuff* -- quanta in previously-filled roles
; *other-points* -- points in previously-filled roles
; *own-stuff*
; *own-points*
; *whole*

(set! *current-point* 18)
(set! *tip-1* 19)
(set! *tip-2* 3)
(set! *last-quantum* 25)
(set! *last-point* 19)
(set! *other-stuff* '())
(set! *other-points* '())
(set! *own-stuff* '(25))
(set! *own-points* '(19 18))
(set! *whole* 'a1)
(set! *the-role* 'a-arch)
(set! *norms* (get-norms (eval *the-role*)))

