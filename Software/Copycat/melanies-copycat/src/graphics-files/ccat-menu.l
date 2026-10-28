;---------------------------------------------
; CCAT-MENU:  Menu stuff for graphics interface.
;---------------------------------------------

(in-package 'user)

(defun init-init-time-menu (&aux init-time-menu-list)
; Sets up the initialization menu.
  (setq init-time-menu-list
	(list (if* (null %demo-graphics%)
               then '("Set demo graphics on" . demo-on)
	       else '("Set demo graphics off" . demo-off))
	      (if* (null %verbose%) 
               then '("Set verbose on" . verbose-on)
               else '("Set verbose off" . verbose-off))
              (if* (null %slightly-verbose%) 
               then '("Set slightly verbose on" . 
		      slightly-verbose-on)
               else '("Set slightly verbose off" . 
		      slightly-verbose-off))
             (if* (null %description-graphics%)
              then '("Display descriptions" . 
		     display-descriptions)
              else '("Don't display descriptions" . 
		     dont-display-descriptions))
            (if* (null %coderack-graphics%)
             then '("Display coderack" . display-coderack)
             else '("Don't display coderack" . 
		    dont-display-coderack))
            (if* (null %slipnet-graphics%)
             then '("Display slipnet" . display-slipnet)
             else '("Don't display slipnet" . 
		    dont-display-slipnet))
            (if* (null %temperature-graphics%)
             then '("Display temperature" . 
		    display-temperature)
             else '("Don't display temperature" . 
		    dont-display-temperature))
           (if* (eq %graphics-rate% 'fast)
            then '("Slow graphics rate" . slow-graphics-rate)
            else '("Fast graphics rate" . fast-graphics-rate))
           '("Okay" . okay)))

  (setq *init-time-menu* 
	(make-pop-up-menu init-time-menu-list)))

;---------------------------------------------

(defun get-init-time-menu-command (target-string-input &aux command)
; Gets a command from the initialization menu.
  (move-mouse 250 0) ; Move the mouse to the top of the 
                     ; screen.
  (setq command (pop-up-menu-choose *init-time-menu*))
  (case command
        (demo-on (setq %demo-graphics% t)
	         (setq %coderack-graphics% nil))
        (demo-off (setq %demo-graphics% nil)
	          (setq %coderack-graphics% t))
        (verbose-on (setq %verbose% t))
	(verbose-off (setq %verbose% nil))
	(slightly-verbose-on (setq %slightly-verbose% t))
	(slightly-verbose-off (setq %slightly-verbose% nil))
 	(display-descriptions (setq %description-graphics% t))
 	(dont-display-descriptions (setq %description-graphics% nil))
	(display-coderack 
	    (if* (not %demo-graphics%)
             then (setq %coderack-graphics% t)
	     else (format t "Sorry, can't display the coderack ")
	          (format t "with demo-graphics.~&")
	          (very-long-pause)))
	(dont-display-coderack (setq %coderack-graphics% nil))
	(display-slipnet (setq %slipnet-graphics% t))
	(dont-display-slipnet (setq %slipnet-graphics% nil))
	(display-temperature (setq %temperature-graphics% t))
	(dont-display-temperature (setq %temperature-graphics% nil))
	(slow-graphics-rate (setq %graphics-rate% 'slow))
	(fast-graphics-rate (setq %graphics-rate% 'fast))
        (okay 'okay)))

;---------------------------------------------

(defun init-begin-run-time-menu (&aux begin-run-time-menu-list)
; Sets up the second menu, given just before beginning the run.
  (setq begin-run-time-menu-list 
	(list (if* (null %verbose%) 
               then '("Set verbose on" . verbose-on)
               else '("Set verbose off" . verbose-off))
              (if* (null %slightly-verbose%) 
               then '("Set slightly verbose on" . 
		      slightly-verbose-on)
               else '("Set slightly verbose off" . 
		      slightly-verbose-off))
             (if* (null %description-graphics%)
              then '("Display descriptions" . 
		     display-descriptions)
              else '("Don't display descriptions" . 
		     dont-display-descriptions))
             (if* (null %coderack-graphics%)
              then '("Display coderack" . display-coderack)
              else '("Don't display coderack" 
		     . dont-display-coderack))
            (if* (null %slipnet-graphics%)
             then '("Display slipnet" . display-slipnet)
             else '("Don't display slipnet" . 
		    dont-display-slipnet))
            (if* (null %temperature-graphics%)
             then '("Display temperature" . 
		    display-temperature)
             else '("Don't display temperature" . 
		    dont-display-temperature))
           (if* (eq %graphics-rate% 'fast)
            then '("Slow graphics rate" . slow-graphics-rate)
            else '("Fast graphics rate" . fast-graphics-rate))
           '("Redisplay graphics" . redisplay-graphics)
           '("Quit program" . quit-program)
           '("Begin run" . begin-run)))
  (setq *begin-run-time-menu* 
	(make-pop-up-menu begin-run-time-menu-list)))

;---------------------------------------------

(defun get-begin-run-time-menu-command (&aux command)
; Gets a command from the begin-run-time menu.
  (move-mouse 250 0) ; Move the mouse to the top of the 
                     ; screen.
  (setq command (pop-up-menu-choose *begin-run-time-menu*))
  (case command
        (verbose-on (setq %verbose% t))
	(verbose-off (setq %verbose% nil))
	(slightly-verbose-on (setq %slightly-verbose% t))
	(slightly-verbose-off (setq %slightly-verbose% nil))
 	(display-descriptions 
	    (display-descriptions)
	    (setq %description-graphics% t))
 	(dont-display-descriptions 
	    (erase-descriptions)
	    (setq %description-graphics% nil))
	(display-coderack 
	    (if* (not %demo-graphics%)
             then (display-coderack)
                  (setq %coderack-graphics% t)
	     else (format t "Sorry, can't display the coderack ")
	          (format t "with demo-graphics.~&")
	          (very-long-pause)))
	(dont-display-coderack 
	    (erase-coderack) (setq %coderack-graphics% nil))
	(display-slipnet (display-slipnet) (setq %slipnet-graphics% t))
	(dont-display-slipnet (erase-slipnet) (setq %slipnet-graphics% nil))
	(display-temperature (display-temperature) 
	                     (setq %temperature-graphics% t))
	(dont-display-temperature (erase-temperature)
	                          (setq %temperature-graphics% nil))
	(slow-graphics-rate (setq %graphics-rate% 'slow))
	(fast-graphics-rate (setq %graphics-rate% 'fast))
	(redisplay-graphics (display-ccat))
	(quit-program 'quit-program)
        (begin-run 'begin-run)))

;---------------------------------------------
