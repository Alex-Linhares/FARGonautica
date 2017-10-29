;---------------------------------------------
; CCAT-SYS:  This file contains the system definition for Copycat 
;            (a list of all the files that need to be loaded into Lisp).
;            It also proclaims all the constants (names surrounded by '%'s) and 
;            global variables (names surrounded by '*'s).
;---------------------------------------------

(in-package 'user)

(defsys 'ccat-sys
        '("util"
          "constants"
          "workspace"
	  "workspace-structures"
	  "workspace-strings"
	  "workspace-objects"
          "initialization"
          "run"
          "slipnet-def"
          "slipnet-links"
          "slipnet-functions"
          "slipnet-graphics"
          "descriptions"
          "bonds"
          "groups"
          "concept-mappings"
	  "correspondences"
	  "replacements"
          "breakers"
	  "coderack"
	  "rule"
	  "answer"
          "workspace-object-formulas"
          "workspace-structure-formulas"
          "formulas"
          "graphics-util"
          "coderack-graphics"
          "letter-graphics"
          "description-graphics"
	  "bond-graphics"
	  "group-graphics"
	  "correspondence-graphics"
	  "replacement-graphics"
	  "rule-graphics"
	  "workspace-graphics"
	  "temperature-graphics"
          "ccat-bar-graph"
	  "ccat-menu"
	  "copystat"
	 ))

(production-mode)  ; Puts the compiler in production mode so that
                   ; the most efficient compiled code will be created.

; Proclaim constants and global variables.
(proclaim 
    '(special 
         ; WORKSPACE CONSTANTS AND GLOBAL VARIABLES
	 *initial-string* *modified-string* *target-string* *answer-string* 
	 *rule* *translated-rule* *snag-structure-list* *workspace* 
	 %length-description-probability-vector%
	 %very-low-answer-temperature-threshold-distribution% 
	 %low-answer-temperature-threshold-distribution% 
	 %medium-answer-temperature-threshold-distribution% 
         %high-answer-temperature-threshold-distribution% 
         %very-high-answer-temperature-threshold-distribution% 
         *codelet-count* *temperature* *codelets-to-post* *found-answer*
         *snag-condition* *snag-count* *single-letter-group-count*
	 *single-letter-group-at-end-count* *length-description-count*
	 *length-relevant-at-end*
	 *clamp-temperature* *snag-object*
	 *last-snag-time* *quit-program* 
	 *updating-everything* *i* *m* *t* *a* %built%
 	 *modified-letter-list* *changed-length-group* 
	 *amount-length-changed*
	 *break-on-each-step*

         ; CODERACK CONSTANTS AND GLOBAL VARIABLES
         *coderack* %codelet-types% %codelet-short-names%  
	 %max-coderack-size%
         %coderack-bin-names% %urgency-value-array% %num-of-urgency-bins%
         *coderack-bins* *extremely-low-bin* *very-low-bin* *low-bin* 
	 *medium-bin* *high-bin* *very-high-bin* *extremely-high-bin* 
         *urgency-list* *codelet-list*
         
         ; SLIPNET CONSTANTS AND GLOBAL VARIABLES
         plato-one plato-two plato-three plato-four plato-five 
	 %max-activation%  %workspace-activation% 
         %initial-slipnode-clamp-time% %full-activation-threshold%
         plato-string-position-category 
	 plato-sameness plato-object-category 
	 plato-letter-category  
         plato-leftmost plato-rightmost plato-middle
         *initially-clamped-slipnodes* plato-a plato-b  plato-c plato-d
         plato-e plato-f plato-g plato-h plato-i plato-j plato-k plato-l 
         plato-m plato-n plato-o plato-p plato-q plato-r plato-s plato-t
         plato-u plato-v plato-w plato-x plato-y plato-z 
	 plato-length
         plato-alphabetic-position-category 
         plato-direction-category plato-bond-category plato-group-category 
         plato-letter plato-group plato-first plato-last 
	 plato-single plato-whole plato-left plato-right 
         plato-predecessor plato-successor plato-predgrp 
	 plato-succgrp plato-samegrp plato-identity plato-opposite
	 plato-bond-facet *slipnet-letters* *slipnet-numbers* *slipnet*
	 *nodes-to-display* 
	 a-letter-category-link z-letter-category-link
	 1-length-link 2-length-link
	 samegrp-letter-category-link predgrp-length-link
	 succgrp-length-link samegrp-length-link
	 first-last-link last-first-link 
         leftmost-rightmost-link rightmost-leftmost-link left-right-link 
	 right-left-link predgrp-succgrp-link
         successor-predecessor-link predecessor-successor-link
	 succgrp-predgrp-link a-first-link  z-last-link  
         letter-object-category-link object-category-letter-link
         group-object-category-link object-category-group-link
         leftmost-string-position-category-link
         string-position-category-leftmost-link 
         rightmost-string-position-category-link
         string-position-category-rightmost-link 
         middle-string-position-category-link
         string-position-category-middle-link
         single-whole-link whole-single-link
         string-position-category-single-link  
         single-string-position-category-link
         string-position-category-whole-link  
         whole-string-position-category-link
         first-alphabetic-position-category-link 
         alphabetic-position-category-first-link
         last-alphabetic-position-category-link 
         alphabetic-position-category-last-link
         left-direction-category-link direction-category-left-link
         right-direction-category-link direction-category-right-link
         predecessor-bond-category-link bond-category-predecessor-link 
         successor-bond-category-link bond-category-successor-link 
         sameness-bond-category-link bond-category-sameness-link  
         predgrp-group-category-link
	 group-category-predgrp-link succgrp-group-category-link 
         group-category-succgrp-link samegrp-group-category-link 
         group-category-samegrp-link sameness-samegrp-link 
         samegrp-sameness-link 
         successor-succgrp-link succgrp-successor-link 
         predecessor-predgrp-link predgrp-predecessor-link
         letter-category-bond-facet-link bond-facet-letter-category-link 
         length-bond-facet-link bond-facet-length-link 
         letter-category-length-link 
         length-letter-category-link
	 letter-group-link group-letter-link left-leftmost-link
         leftmost-left-link right-leftmost-link leftmost-right-link 
	 right-rightmost-link rightmost-right-link leftmost-first-link 
	 first-leftmost-link rightmost-first-link first-rightmost-link
	 leftmost-last-link last-leftmost-link rightmost-last-link 
	 last-rightmost-link left-rightmost-link rightmost-left-link
         indent visited-nodes 

         ; GRAPHICS CONSTANTS AND GLOBAL VARIABLES 
         %slipnet-font% %slipnet-letter-font% 
	 %slipnet-activation-font% %workspace-font% %group-font% 
	 %rule-font% 
	 %relevant-concept-mapping-font%  %irrelevant-concept-mapping-font% 
         %coderack-font% %codelet-name-font% 	%minimal-coderack-font%
         *old-minimal-coderack-string* %relevant-description-font% 
	 %irrelevant-description-font% %relevant-length-font% 
	 %irrelevant-length-font% %temperature-font% %codelet-group-font%
         %codelet-name-font-height% %group-font-height%
         %slipnet-font-height% %slipnet-activation-font-height%  
	 %relevant-concept-mapping-font-height%
	 %graphics-rate% *description-graphics-obj-list*
         %window-width% %window-height% 
         %slipnet-x% %slipnet-y% 
         %slipnet-width% 
         %slipnet-height% 
         slipnode-region-height 
         slipnode-region-width 
         *slipnode-boxsizes* 
         %rule-mode% %translated-rule-mode%
         %coderack-x1% %coderack-y1% %coderack-x2% 
	 %coderack-y2% 
         *coderack-bar-graph* *waiting-codelets-string* 
         %waiting-codelets-string-x% %waiting-codelets-string-y% 
	 %minimal-coderack-x% %minimal-coderack-y% %minimal-coderack-string%
         %space-between-descriptions%  %light-intensity% %medium-intensity% 
	 %jag-length% %long-bond-dash-length% %medium-bond-dash-length%
         %short-bond-dash-length%
         %long-bond-space-length% %medium-bond-space-length%
         %short-bond-space-length%
         %group-space-length%
         %bond-left-x-offset% %bond-right-x-offset% %bond-y-offset%
         %concept-mapping-x-offset%  %concept-mapping-y-offset% 
         %group-concept-mapping-y-offset% 
         %string-spanning-group-concept-mapping-x-offset% 
         %string-spanning-group-concept-mapping-y-offset% 
         %vertical-jag-length%   
         %short-correspondence-dash-length% 
         %long-correspondence-dash-length% 
         %correspondence-space-length% 
         %space-between-concept-mappings% 
         %concept-mapping-text-width%
         %arrow-x% %origin-x% %origin-y%
         %arrow-width% %left-side-space% %right-side-space% %middle-space% 
         %string-width% %initial-space% 
         %modified-space% %target-space% %answer-space% 
         %y-top% %y-bottom% 
         %rule-y% %translated-rule-y% 
         %replacement-x-offset% %replacement-y-offset% 
         %left-concept-mapping-x-offset% 
         %right-concept-mapping-x-offset% 
         %left-group-concept-mapping-x-offset% 
         %right-group-concept-mapping-x-offset% 
         %initial-x% %modified-x% %target-x% %answer-x% 
         %temperature-display-width% 
         %temperature-display-height% 
         %temperature-number-x% *old-temperature-string* *old-temperature-y*
         %heavy-intensity% 
         %short-group-dash-length% %long-group-dash-length% 
         long-correspondence-dash-length% 
         %correspondence-short-space% 
         %horizontal-jag-length% 
         i-vector m-vector t-vector
         %codelet-name-top-y% %codelet-name-bottom-y%
         %temperature-display-x1% 
         %temperature-display-y1% 
         %temperature-display-x2% 
         %temperature-display-y2% 
         *temperature-height* 

         ; OTHER CONSTANTS AND GLOBAL VARIABLES
         %verbose% %slightly-verbose% 
         %demo-graphics% %workspace-graphics% 
	 %coderack-graphics% 
         %minimal-coderack-graphics%
	 %slipnet-graphics% %slipnet-display-level%
         %description-graphics% 
         *workspace-initialized* *coderack-initialized* 
	 *slipnet-initialized*
         %temperature-graphics%  *init-time-menu* 
         *run-time-menu* *begin-run-time-menu* 
         %time-step-length% *random-state-this-run*
	 *data-file* *random-state-file* *summary-file*))







