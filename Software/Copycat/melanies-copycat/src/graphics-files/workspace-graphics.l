;---------------------------------------------
; WORKSPACE-GRAPHICS: This file contains graphics functions for the 
;                      workspace.
;---------------------------------------------

(in-package 'user)

;---------------------------------------------

(defun init-graphics-constants ()
; Initializes the various constants used in the graphics routines.
 
  ; Width and height of visible part of the window.
  (setf %window-width% (- %graphics-viewport-width% 20))

  (setf %window-height% (- %graphics-viewport-height% 25))

  ; Coordinates of the upper-left-hand corner of the window.
  (setq %origin-x% 0)
  (setq %origin-y% 0)

  ; Y-TOP is the y-coordinate of the initial and modified strings;
  ; Y-BOTTOM is the y-coordinate of the target string.
  (if* %demo-graphics% 
   then (setq %y-top% (round (* %window-height% .23)))
        (setq %y-bottom% (round (* %window-height% .48)))
   else (setq %y-top% (round (* %window-height% .2)))
        (setq %y-bottom% (round (* %window-height% .40))))
  
  (setq i-vector (send *initial-string* :letter-vector))
  (setq m-vector (send *modified-string* :letter-vector))
  (setq t-vector (send *target-string* :letter-vector))

  (setq %left-side-space% ; Space to left of initial and target strings.
	(round (/ %window-width% 15))) 
  (setq %right-side-space% ; Space to right of modified and answer strings.
	(round (/ %window-width% 15)))
  (setq %arrow-width% (round (/ %window-width% 6)))
  (setq %middle-space% ; Space between right side of initial-string (or
		       ; target string) and arrow.
	(round (/ %window-width% 18)))

  (setq %string-width% ; The width of each of the three strings.
	(round (/ (- %window-width% (+ %left-side-space% %right-side-space%) 
		     (* 2 %middle-space%) %arrow-width%) 2)))

  (setq %initial-space% ; Space between letters in the initial-string.
	(if* (= (vsize i-vector) 1)
	 then 0 
	 else (round (/ %string-width% (1- (vsize i-vector))))))
  (setq %modified-space% ; Space between letters in the modified-string.
	(if* (= (vsize m-vector) 1)
	 then 0 
	 else (round (/ %string-width% (1- (vsize m-vector))))))
  (setq %target-space% ; Space between letters in the target-string.
	(if* (= (vsize t-vector) 1)
         then 0 
	 else (round (/ %string-width% (1- (vsize t-vector))))))


  ; Coordinates of the strings.
  (setq %initial-x% (+ %origin-x% %left-side-space%))
  (setq %modified-x% (- (+ %initial-x% %string-width% 
	                   (* 2 %middle-space%) %arrow-width%) 10))
  (setq %target-x% (+ %origin-x% %left-side-space%))
  (setq %answer-x% %modified-x%)
  (setq %arrow-x% (+ %initial-x% %string-width% %middle-space%))

  ; Coordinates for coderack display.
  (setq %coderack-x1% 5)
  (setq %coderack-y1% (+ %y-bottom% 80))
  (setq %coderack-x2% (+ %window-width% 6))
  (setq %coderack-y2% (+ %coderack-y1% 60))
  ; y-coord of top of separator line for codelet names.
  (setq %codelet-name-top-y% %coderack-y2%)   
  ; y-coord of bottom of separator line for codelet names.
  (setq %codelet-name-bottom-y% (+ %codelet-name-top-y% 40)) 
                                                
  ; x-coordinate and y-coordinate of origin of Slipnet display
  (setq %slipnet-x% %coderack-x1%) 
  (if* %demo-graphics%
   then (setq %slipnet-y% (+ %y-bottom% 100))
   else (setq %slipnet-y% (+ %codelet-name-bottom-y% 25)))

  (setq %slipnet-width% (- %window-width% %slipnet-x%))
  (setq %slipnet-height% (- %window-height% %slipnet-y% 2))

  ; Coordinates for temperature display
  (if* %temperature-graphics% 
   then (setq %temperature-display-x1% (+ %origin-x% 5))
        (setq %temperature-display-width% 15)
        (setq %temperature-display-y1% (- %y-top% 30))
        (setq %temperature-display-x2% 
	      (+ %temperature-display-x1% %temperature-display-width%))
        (setq %temperature-display-y2% %y-bottom%)
        (setq %temperature-display-height% 
	      (- %temperature-display-y2% %temperature-display-y1%))
        (setq %temperature-number-x% 
	      (+ %temperature-display-x2% 2))
	(setq *old-temperature-string* nil))

  ; Coordnates for rule and translated-rule display.
  (if* (eq %slipnet-display-level% 'low)
   then (setq %rule-x% (round (/ %window-width% 3)))
        (setq %translated-rule-x% (round (/ %window-width% 3)))
        (setq %rule-y% 15)
        (setq %translated-rule-y% (- %slipnet-y% 20))
   else (setq %rule-x% (round (- (/ (+ %arrow-x% %answer-x%) 2) 5)))
        (setq %translated-rule-x% (round (- (/ (+ %arrow-x% %answer-x%) 2) 5)))
        (setq %rule-y% 15)
        (setq %translated-rule-y% (+ %y-bottom% 25)))

  ; Modes for drawing rule or translated-rule.
  (setq %rule-mode% 0)
  (setq %translated-rule-mode% 1)

  ; Offsets for displaying various structures.
  (setq %bond-left-x-offset% 0)
  (setq %bond-right-x-offset% 10)
  (setq %bond-y-offset% 8)
  (setq %replacement-x-offset% 7)
  (setq %replacement-y-offset% 15)
  (setq %left-concept-mapping-x-offset% 60)
  (setq %right-concept-mapping-x-offset% 20)
  (setq %left-group-concept-mapping-x-offset% 60)
  (setq %right-group-concept-mapping-x-offset% 60)
  (setq %concept-mapping-x-offset% 40)
  (setq %concept-mapping-y-offset% 30)
  (if* %demo-graphics%
   then (setq %string-spanning-group-concept-mapping-y-offset% 45)
   else (setq %string-spanning-group-concept-mapping-y-offset% 35))
  (setq %group-concept-mapping-y-offset% 10)
  (setq %space-between-concept-mappings% 15)

  ; Maximum concept-mapping length is 16 letters, so get the text width of
  ; 16 letters.
  (set-font %relevant-concept-mapping-font%)
  (setq %concept-mapping-text-width% (text-length "aaaaaaaaaaaaaaaa"))
  (set-font %workspace-font%)
   
  ; Constants for specifying the widths of lines.

  (setq %light-intensity% 1)
  (setq %medium-intensity% 2)
  (setq %heavy-intensity% 3)

  ; Constants for specifying the lengths of dashes and spaces for drawing
  ; dashed lines.
  (setq %long-group-dash-length% 6)
  (setq %short-group-dash-length% 2)
  (setq %group-space-length% 12)
  (setq %short-bond-dash-length% 1)
  (setq %long-bond-dash-length% 4)
  (setq %short-bond-space-length% 2)
  (setq %long-bond-space-length% 8)

  (setq %long-correspondence-dash-length% 6)
  (setq %short-correspondence-dash-length% 1)
  (setq %correspondence-space-length% 8)
  (setq %space-between-descriptions% 12)
  (setq %jag-length% 10)
  (setq %horizontal-jag-length% 4)
  (setq %vertical-jag-length% 4))

;---------------------------------------------

(defun init-workspace-graphics (&aux graphics-obj)
; Initializes the workspace display.

  (init-graphics-constants)

  (set-font %workspace-font%)

  ; Set up coordinates for the strings.
  (send *initial-string* :set-x %initial-x%)
  (send *initial-string* :set-y %y-top%)
  (send *target-string* :set-x %target-x%)
  (send *target-string* :set-y %y-bottom%)

; Set up letter positions and draw letters.
  (loop for letter being the elements of i-vector do
        (setq graphics-obj 
	      (make-instance 'letter-graphics-obj))
        (send graphics-obj :set-x 
	      (+ %initial-x% 
		 (* (send letter :left-string-position) 
		    %initial-space%)))
        (send graphics-obj :set-y %y-top%)
	(send graphics-obj :set-parent letter)
	(send graphics-obj :set-bond-left-x 
	      (+ (send graphics-obj :x) %bond-left-x-offset%))
	(send graphics-obj :set-bond-right-x 
	      (+ (send graphics-obj :x) %bond-right-x-offset%))
	(send graphics-obj :set-bond-y 
	      (- (send graphics-obj :y) %bond-y-offset%))
        (send graphics-obj :set-replacement-x 
	      (+ (send graphics-obj :x) 
		 %replacement-x-offset%))
        (send graphics-obj :set-replacement-y 
	      (- (send graphics-obj :y) 
		 %replacement-y-offset%))
        (send graphics-obj :set-correspondence-x 
	      (+ (send graphics-obj :x) 3))
        (send graphics-obj :set-correspondence-y 
	      (+ (send graphics-obj :y) 3))
        (send graphics-obj :set-description-x (+ (send graphics-obj :x) 10))
        (send graphics-obj :set-description-y 
	      (- (send graphics-obj :bond-y) 8))
	(send graphics-obj :draw)
        (send letter :set-graphics-obj graphics-obj))


; Draw arrow.
  (draw-centered-text %arrow-x% %y-top% "--->" %arrow-width%)

  (loop for letter being the elements of m-vector do
	(setq graphics-obj 
	      (make-instance 'letter-graphics-obj))
        (send graphics-obj :set-x 
	      (+ %modified-x% 
		 (* (send letter :left-string-position) 
		    %modified-space%)))
	(send graphics-obj :set-y %y-top%)
	(send graphics-obj :set-parent letter)
	(send graphics-obj :set-bond-left-x 
	      (+ (send graphics-obj :x) %bond-left-x-offset%))
	(send graphics-obj :set-bond-right-x 
	      (+ (send graphics-obj :x) %bond-right-x-offset%))
	(send graphics-obj :set-bond-y 
	      (- (send graphics-obj :y) %bond-y-offset%))
        (send graphics-obj :set-replacement-x 
	      (+ (send graphics-obj :x) 
		 %replacement-x-offset%))
        (send graphics-obj :set-replacement-y 
	      (- (send graphics-obj :y) 
		 %replacement-y-offset%))
        (send graphics-obj :set-description-x (+ (send graphics-obj :x) 10))
        (send graphics-obj :set-description-y 
	      (- (send graphics-obj :bond-y) 8))
	(send graphics-obj :draw)
        (send letter :set-graphics-obj graphics-obj))

  (loop for letter being the elements of t-vector do
	(setq graphics-obj 
	      (make-instance 'letter-graphics-obj))
        (send graphics-obj :set-x 
	      (+ %target-x% 
		 (* (send letter :left-string-position) 
		    %target-space%)))
        (send graphics-obj :set-y %y-bottom%)
	(send graphics-obj :set-parent letter)
	(send graphics-obj :set-bond-left-x 
	      (+ (send graphics-obj :x) %bond-left-x-offset%))
	(send graphics-obj :set-bond-right-x 
	      (+ (send graphics-obj :x) %bond-right-x-offset%))
	(send graphics-obj :set-bond-y 
	      (- (send graphics-obj :y) %bond-y-offset%))
        (send graphics-obj :set-correspondence-x 
	      (+ (send graphics-obj :x) 3))
        (send graphics-obj :set-correspondence-y 
	      (- (send graphics-obj :y) 10))
        (send graphics-obj :set-description-x (+ (send graphics-obj :x) 10))
        (send graphics-obj :set-description-y 
	      (- (send graphics-obj :bond-y) 8))
	(send graphics-obj :draw)
        (send letter :set-graphics-obj graphics-obj))

  (draw-centered-text %arrow-x% %y-bottom% "--->" 
                      %arrow-width%))
  
;---------------------------------------------

(defun display-workspace ()
; Draws the current state of the workspace.

  (set-font %workspace-font%)

  ; Draw letters. 
  (loop for letter in (send *initial-string* :letter-list) do
    (send letter :draw))

  (draw-centered-text %arrow-x% %y-top% "--->" %arrow-width%)

  (loop for letter in (send *modified-string* :letter-list) do
    (send letter :draw))

  (loop for letter in (send *target-string* :letter-list) do
    (send letter :draw))

  (draw-centered-text %arrow-x% %y-bottom% "--->" 
                      %arrow-width%)

  (if* *answer-string* 
   then (loop for letter in (send *answer-string* :letter-list) do
              (send letter :draw)))

  ; Draw descriptions.
  (if* %description-graphics% then (display-descriptions))

  ; Draw proposed-bonds and bonds.
  (loop for proposed-bond 
	in (send *initial-string* :proposed-bond-list) do
        (if* (send proposed-bond :drawn?)
	 then (send proposed-bond :draw-proposed)))

  (loop for bond in (send *initial-string* :bond-list) do
        (if* (send bond :drawn?) then (send bond :draw)))

  (loop for proposed-bond 
	in (send *target-string* :proposed-bond-list) do
        (if* (send proposed-bond :drawn?) 
	 then (send proposed-bond :draw-proposed)))

  (loop for bond in (send *target-string* :bond-list) do
        (if* (send bond :drawn?) then (send bond :draw)))

  ; Draw proposed-groups and groups.
  (loop for proposed-group 
	in (send *initial-string* :proposed-group-list) do
        (if* (send proposed-group :drawn?) 
	 then (send proposed-group :draw-proposed)))

  (loop for group in (send *initial-string* :group-list) do
        (if* (send group :drawn?) then (send group :draw)))

  (loop for proposed-group 
	in (send *target-string* :proposed-group-list) do
        (if* (send proposed-group :drawn?) 
	 then (send proposed-group :draw-proposed)))

  (loop for group in (send *target-string* :group-list) do
        (if* (send group :drawn?) then (send group :draw)))

  ; Draw replacements.
  (loop for replacement 
	in (send *workspace* :replacement-list) do (send replacement :draw))

  ; Draw proposed-correspondences and correspondences.
  (loop for proposed-correspondence 
	in (send *workspace* :proposed-correspondence-list) do
        (if* (send proposed-correspondence :drawn?) 
	 then (send proposed-correspondence :draw-proposed)))

  (loop for correspondence in (send *workspace* :correspondence-list) 
	when correspondence do
        (if* (send correspondence :drawn?) then (send correspondence :draw)))

  ; Draw rule and translated rule.
  (if* *rule* then (send *rule* :draw %rule-mode%))
  (if* *translated-rule* 
   then (send *translated-rule* :draw %translated-rule-mode%)))

;---------------------------------------------

(defun erase-workspace ()
; Erases the entire workspace display.
  (erase-solid-rectangle 0 0 %window-width% 
                         (- %minimal-coderack-y% 8)))

;---------------------------------------------

(defun display-ccat ()
; Displays all graphics representing the current state of the program.
  (clear-window)
  (if* (and %workspace-graphics% *workspace-initialized*) 
   then (display-workspace))
  (if* (and %coderack-graphics% *coderack-initialized*) 
   then (display-coderack))
  (if* %minimal-coderack-graphics% 
   then (display-minimal-coderack))
  (if* (and %slipnet-graphics% *slipnet-initialized*) 
   then (display-slipnet))
  (if* %temperature-graphics% 
   then (init-temperature-display)
        (update-temperature-display t)))

;---------------------------------------------

(defun init-answer-graphics (&aux letvec graphics-obj)
; Initializes the graphics for displaying the answer string.
  (setq letvec (send *answer-string* :letter-vector))
  (setq %answer-space% ; Space between letters
	(if* (= (vsize letvec) 1)
         then 0 
	 else (round (/ %string-width% (1- (vsize letvec))))))  
  (loop for letter being the elements of letvec do
        (setq graphics-obj 
	      (make-instance 'letter-graphics-obj :parent letter))
        (send graphics-obj :set-x 
	      (+ %answer-x% (* (send letter :left-string-position) 
			       %answer-space%)))
	(send graphics-obj :set-y %y-bottom%)
	(send graphics-obj :draw)
	(send letter :set-graphics-obj graphics-obj)))
      
;---------------------------------------------

(defun redraw-graphics ()
  ; Redraw initial-string-graphics.
  (loop for letter in (send *initial-string* :letter-list) do
	(send letter :draw))

  (loop for proposed-bond 
	in (send *initial-string* :proposed-bond-list) do
	(if* (send proposed-bond :drawn?) 
	 then (send proposed-bond :draw-proposed)))

  (loop for bond in (send *initial-string* :bond-list) do
	(if* (send bond :drawn?) then (send bond :draw)))

  (loop for proposed-group 
	in (send *initial-string* :proposed-group-list) do
	(if* (send proposed-group :drawn?) 
	 then (send proposed-group :draw-proposed)))

  (loop for group in (send *initial-string* :group-list) 
	when group do
	(if* (send group :drawn?) then (send group :draw)))

  (loop for replacement 
	in (send *workspace* :replacement-list) do 
	(send (send replacement :graphics-obj) :draw))

  (loop for proposed-correspondence 
	in (send *workspace* :proposed-correspondence-list) do
	(if* (send proposed-correspondence :drawn?) 
         then (send proposed-correspondence :draw-proposed)))

  (loop for correspondence in (send *workspace* :correspondence-list) 
	when correspondence do
	(if* (send correspondence :drawn?) 
         then (send correspondence :draw)))

  ; Redraw target-string-graphics.
  (loop for letter in (send *target-string* :letter-list) do
	(send letter :draw))

  (loop for proposed-bond 
	in (send *target-string* :proposed-bond-list) do
	(if* (send proposed-bond :drawn?) 
         then (send proposed-bond :draw-proposed)))

  (loop for bond in (send *target-string* :bond-list) do
	(if* (send bond :drawn?) then (send bond :draw)))

  (loop for proposed-group 
	in (send *target-string* :proposed-group-list) do
	(if* (send proposed-group :drawn?) 
         then (send proposed-group :draw-proposed)))

  (loop for group in (send *target-string* :group-list) 
	when group do
	(if* (send group :drawn?) then (send group :draw)))

  ; Redraw descriptions.
  (if* %description-graphics% then (redisplay-descriptions)))

;---------------------------------------------


