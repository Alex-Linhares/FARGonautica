;---------------------------------------------
; CONSTANTS: This file contains constants for Copycat.
;---------------------------------------------

(in-package 'user)

;---------------------------------------------

(defun init-constants ()
; This function initializes all the constants.
  
; CONSTANTS FOR GRAPHICS

  (setq %verbose% nil) ; T turns on verbose trace of codelet runs.
  (setq %slightly-verbose% nil) ; T turns on less verbose trace of codelet 
                                ; runs.
  (setq %workspace-graphics% t) ; T means workspace-graphics are drawn.
  (setq %coderack-graphics% nil) ; T means coderack bar-graph is 
                                    ; displayed.
  (setq %minimal-coderack-graphics% t) ; T means number of codelets run
                                          ; so far is displayed.
  (setq %temperature-graphics% t) ; T means temperature is displayed.
  (setq %slipnet-graphics% t) ; T means slipnet-graphics are drawn.
  (setq %slipnet-display-level% 'medium) ; Determines how many nodes are 
                                       ; displayed.
  (setq %description-graphics% nil) ; T means descriptions are displayed.
  (setq %demo-graphics% t) ; T means that the graphics are set up in a  
                             ; special way for demos.
  (setq %graphics-rate% 'fast) ; There are three settings: 'fast, 'medium, and
                               ; 'slow.

  ; Set up the fonts.
  (if* %demo-graphics% 
   then (setq %slipnet-font% "/usr/lib/fonts/fixedwidthfonts/cour.r.10")
   else (setq %slipnet-font% "/usr/lib/fonts/fixedwidthfonts/screen.r.7"))
  (setq %slipnet-activation-font% "/usr/lib/fonts/fixedwidthfonts/cour.r.10")

  (if* %demo-graphics% 
   then (setq %slipnet-letter-font% "/usr/lib/fonts/fixedwidthfonts/cour.r.14")
   else (setq %slipnet-letter-font%
	      "/usr/lib/fonts/fixedwidthfonts/cour.r.10"))

  (setq %workspace-font% "/usr/lib/fonts/fixedwidthfonts/cour.b.24")
  (setq %group-font% "/usr/lib/fonts/fixedwidthfonts/cour.b.24")
  (setq %relevant-length-font% "/usr/lib/fonts/fixedwidthfonts/cour.b.18")
  (setq %irrelevant-length-font% "/usr/lib/fonts/fixedwidthfonts/cour.r.18")
  (setq %rule-font% "/usr/lib/fonts/fixedwidthfonts/cour.b.12")
  (setq %relevant-concept-mapping-font% 
	"/usr/lib/fonts/fixedwidthfonts/cour.b.12")
  (setq %irrelevant-concept-mapping-font% 
        "/usr/lib/fonts/fixedwidthfonts/cour.r.12")
  (setq %relevant-description-font% 
	"/usr/lib/fonts/fixedwidthfonts/cour.b.12")
  (setq %irrelevant-description-font% 
	"/usr/lib/fonts/fixedwidthfonts/cour.r.12")
  (setq %temperature-font% 
	"/usr/lib/fonts/fixedwidthfonts/cour.r.10")

  (setq %coderack-font% "/usr/lib/fonts/fixedwidthfonts/cour.r.10")
  (setq %minimal-coderack-font% "/usr/lib/fonts/fixedwidthfonts/cour.b.12")
  (setq %codelet-name-font% "/usr/lib/fonts/fixedwidthfonts/cour.r.10")
  (setq %codelet-group-font% "/usr/lib/fonts/fixedwidthfonts/cour.b.14")

  ; Set up values for the font-heights.
  (setq %codelet-name-font-height% 11) 
  (setq %group-font-height% 10)
  (setq %slipnet-font-height% 10)
  (setq %slipnet-activation-font-height% 5)
  (setq %relevant-concept-mapping-font-height% 10)

;---------------------------------------------

; CONSTANTS FOR CODELETS

  ; These are all the different types of codelets.
  (setq %codelet-types% 
	'(bottom-up-description-scout top-down-description-scout 	     
	  description-strength-tester description-builder 
	  bottom-up-bond-scout top-down-bond-scout--category
	  top-down-bond-scout--direction 
	  bond-strength-tester bond-builder 
	  top-down-group-scout--category top-down-group-scout--direction
	  group-scout--whole-string group-strength-tester group-builder 
	  bottom-up-correspondence-scout 
	  important-object-correspondence-scout 
	  correspondence-strength-tester
	  correspondence-builder 
	  rule-scout rule-strength-tester rule-builder rule-translator 
  	  replacement-finder breaker 

	 ))

  ; Short names for all the different types of codelets, used to label
  ; the coderack bar graph.
  (setq %codelet-short-names% 
	'(("btmup" "descr" "scout") 
	  ("TPDWN" "descp" "scout") 
	  ("descp" "strgth" "tester")
          ("descp" "bldr")
	  ("btmup" "bond" "scout")
	  ("TPDWN" "bond" "scout:" "cat")
	  ("TPDWN" "bond" "scout:" "dir")
	  ("bond" "strgth" "tester")
	  ("bond" "bldr")
	  ("TPDWN" "group" "scout:" "cat")
	  ("TPDWN" "group" "scout:" "dir")
	  ("group" "scout:" "whole" "strng" )
	  ("group" "strgth" "tester")
	  ("group" "bldr") 
	  ("btmup" "cor" "scout")
	  ("imptnt" "obj" "cor" "scout")
	  ("cor" "strgth" "tester")
	  ("cor" "bldr")
	  ("rule" "scout") 
	  ("rule" "strgth" "tester")
	  ("rule" "bldr") 
	  ("rule" "trnsr")
  	  ("rplmt" "finder")
	  ("brker")))

  (setq %built% 3)  ; The number assigned to a structure to indicate
                    ; that it has been built.

;---------------------------------------------

; CONSTANTS FOR CODERACK

  (setq %max-coderack-size% 100)  ; The maximum number of codelets on the 
                                       ; coderack.

  (setq %num-of-urgency-bins% 7) ; The number of different urgency bins.

  ; The URGENCY-VALUE-ARRAY is a table containing the value of an urgency 
  ; bin as a function of temperature.
  (setq %urgency-value-array% 
	(make-array (list %num-of-urgency-bins% 101) :initial-element nil))

  ; Set up URGENCY-VALUE-ARRAY.
  (loop for bin-number from 0 to (1- %num-of-urgency-bins%) do
	(loop for temperature from 0 to 100 do
	      (aset %urgency-value-array% bin-number temperature 
		    (round (expt (1+ bin-number) 
			          (/ (+ (fake-reciprocal temperature) 10)
				     15.0))))))

;---------------------------------------------

; CONSTANTS FOR SLIPNET
  
  (setq %max-activation% 100)

  (setq %workspace-activation% 100)
  
  ; Threshold activation level for a node to have a chance to go up to full
  ; activation.
  (setq %full-activation-threshold% 50)

  ; Number of codelets run before a slipnet update.
  (setq %time-step-length% 15) 

  ; Number of slipnet cycles for initially-clamped slipnodes to be clamped.
  (setq %initial-slipnode-clamp-time% 50) 
                  
;---------------------------------------------

; PROBABILITY DISTRIBUTIONS
   
  ; Answer-temperature threshold distributions:
  ; These are used by the rule-translator codelet to decide whether 
  ; or not to fizzle as a function of temperature.  The distribution 
  ; used is a function of how many bonds have been built.  The idea is 
  ; that if many bonds have been built, then it's likely that more 
  ; structure will be discovered, and the rule-translator should tend to wait
  ; until the temperature is low.  If few bonds have been built (and,
  ; since the rule translator is running, there should have already been 
  ; opportunity for bonds to be built), then there is not much structure to 
  ; be discovered, and the rule-translator should tend to go ahead even 
  ; though the temperature is relatively high.

  (setq %very-low-answer-temperature-threshold-distribution% 
	(make-probability-distribution "very-low" 101))
  (loop for i from 0 to 100 do 
	(send %very-low-answer-temperature-threshold-distribution% :vset i 0))
  (send %very-low-answer-temperature-threshold-distribution% :vset 10 5)
  (send %very-low-answer-temperature-threshold-distribution% :vset 20 150)
  (send %very-low-answer-temperature-threshold-distribution% :vset 30 5)
  (send %very-low-answer-temperature-threshold-distribution% :vset 40 2)
  (send %very-low-answer-temperature-threshold-distribution% :vset 50 1)
  (send %very-low-answer-temperature-threshold-distribution% :vset 60 1)
  (send %very-low-answer-temperature-threshold-distribution% :vset 70 1)
  (send %very-low-answer-temperature-threshold-distribution% :vset 80 1)
  (send %very-low-answer-temperature-threshold-distribution% :vset 90 1)
  (send %very-low-answer-temperature-threshold-distribution% :vset 100 1)
  (send %very-low-answer-temperature-threshold-distribution% :update)

  (setq %low-answer-temperature-threshold-distribution% 
	(make-probability-distribution "low" 101))
  (loop for i from 0 to 100 do 
	(send %low-answer-temperature-threshold-distribution% :vset i 0))
  (send %low-answer-temperature-threshold-distribution% :vset 10 2)
  (send %low-answer-temperature-threshold-distribution% :vset 20 5)
  (send %low-answer-temperature-threshold-distribution% :vset 30 150)
  (send %low-answer-temperature-threshold-distribution% :vset 40 5)
  (send %low-answer-temperature-threshold-distribution% :vset 50 2)
  (send %low-answer-temperature-threshold-distribution% :vset 60 1)
  (send %low-answer-temperature-threshold-distribution% :vset 70 1)
  (send %low-answer-temperature-threshold-distribution% :vset 80 1)
  (send %low-answer-temperature-threshold-distribution% :vset 90 1)
  (send %low-answer-temperature-threshold-distribution% :vset 100 1)
  (send %low-answer-temperature-threshold-distribution% :update)	

  (setq %medium-answer-temperature-threshold-distribution% 
	(make-probability-distribution "medium" 101))
  (loop for i from 0 to 100 do 
	(send %medium-answer-temperature-threshold-distribution% :vset i 0))
  (send %medium-answer-temperature-threshold-distribution% :vset 10 1)
  (send %medium-answer-temperature-threshold-distribution% :vset 20 2)
  (send %medium-answer-temperature-threshold-distribution% :vset 30 5)
  (send %medium-answer-temperature-threshold-distribution% :vset 40 150)
  (send %medium-answer-temperature-threshold-distribution% :vset 50 5)
  (send %medium-answer-temperature-threshold-distribution% :vset 60 2)
  (send %medium-answer-temperature-threshold-distribution% :vset 70 1)
  (send %medium-answer-temperature-threshold-distribution% :vset 80 1)
  (send %medium-answer-temperature-threshold-distribution% :vset 90 1)
  (send %medium-answer-temperature-threshold-distribution% :vset 100 1)
  (send %medium-answer-temperature-threshold-distribution% :update)	

  (setq %high-answer-temperature-threshold-distribution% 
	(make-probability-distribution "high" 101))
  (loop for i from 0 to 100 do
	(send %high-answer-temperature-threshold-distribution% :vset i 0))
  (send %high-answer-temperature-threshold-distribution% :vset 10 1)
  (send %high-answer-temperature-threshold-distribution% :vset 20 1)
  (send %high-answer-temperature-threshold-distribution% :vset 30 2)
  (send %high-answer-temperature-threshold-distribution% :vset 40 5)
  (send %high-answer-temperature-threshold-distribution% :vset 50 150)
  (send %high-answer-temperature-threshold-distribution% :vset 60 5)
  (send %high-answer-temperature-threshold-distribution% :vset 70 2)
  (send %high-answer-temperature-threshold-distribution% :vset 80 1)
  (send %high-answer-temperature-threshold-distribution% :vset 90 1)
  (send %high-answer-temperature-threshold-distribution% :vset 100 1)
  (send %high-answer-temperature-threshold-distribution% :update)	

  (setq %very-high-answer-temperature-threshold-distribution% 
        (make-probability-distribution "very-high" 101))
  (loop for i from 0 to 100 do 
	(send %very-high-answer-temperature-threshold-distribution% :vset i 0))
  (send %very-high-answer-temperature-threshold-distribution% :vset 10 1)
  (send %very-high-answer-temperature-threshold-distribution% :vset 20 1)
  (send %very-high-answer-temperature-threshold-distribution% :vset 30 1)
  (send %very-high-answer-temperature-threshold-distribution% :vset 40 2)
  (send %very-high-answer-temperature-threshold-distribution% :vset 50 5)
  (send %very-high-answer-temperature-threshold-distribution% :vset 60 150)
  (send %very-high-answer-temperature-threshold-distribution% :vset 70 5)
  (send %very-high-answer-temperature-threshold-distribution% :vset 80 2)
  (send %very-high-answer-temperature-threshold-distribution% :vset 90 1)
  (send %very-high-answer-temperature-threshold-distribution% :vset 100 1)
  (send %very-high-answer-temperature-threshold-distribution% :update)

)

;---------------------------------------------

