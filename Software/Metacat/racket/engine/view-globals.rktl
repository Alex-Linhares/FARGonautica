;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;
;; Globals that the model code reads but whose values come from the graphics
;; files: colours and fonts (constants.ss's graphics part, general-graphics.ss,
;; workspace-graphics.ss), which need racket/draw.  In the original they are
;; top-level definitions of those files, or created by set! when the windows
;; are made.  Here they are engine variables, #f until the views
;; (racket/gui/views.rkt) are loaded: its define and set! of a name the
;; engine owns go through set-global! (docs/porting-notes.md, item 13).  A
;; headless run never draws, so it never reads them; trace.ss's events keep
;; the colours current when they are made.

(define-syntax-rule (view-variables name ...)
  (begin (define name #f) ...))

;; constants.ss: common colour names
(view-variables =white= =black= =grey= =red= =green= =blue= =yellow= =pink= =orange=)
;; constants.ss: colours the model files keep or draw with
(view-variables %vertical-slippage-color% %dim-vertical-slippage-color%
                %coattail-inducing-slippage-color% %dim-coattail-inducing-slippage-color%
                %top-bridge-color% %vertical-bridge-color% %bottom-bridge-color%
                %bridge-label-background-color% %faded-bridge-label-background-color%
                %top-rule-color% %bottom-rule-color% %snag-color%
                %theme-supporting-concept-mapping-color%
                %faded-workspace-structure-color% %workspace-event-structure-color%
                %clamp-event-concept-pattern-color%
                %concept-activation-event-concept-pattern-color%
                %concept-mapping-event-concept-pattern-color%
                %group-event-concept-pattern-color%
                %top-rule-event-concept-pattern-color%
                %bottom-rule-event-concept-pattern-color%
                %snag-event-concept-pattern-color%
                %coderack-background-color% %current-codelet-color%
                %extremely-low-urgency-color% %very-low-urgency-color%
                %low-urgency-color% %medium-urgency-color% %high-urgency-color%
                %very-high-urgency-color% %extremely-high-urgency-color%)
;; general-graphics.ss: the default foreground colour, which init-mcat
;; (run.ss) and the Temporal Trace (trace.ss) set *fg-color* back to
(view-variables %default-fg-color% *fg-color*)
;; workspace-graphics.ss: fonts created by set! when the Workspace window is
;; made (select-workspace-fonts), never defined; groups.ss, bridge-graphics.ss
;; and rule-graphics.ss read the first four
(view-variables %group-letter-category-font% %relevant-group-length-font%
                %bridge-label-font% %rule-font%
                %workspace-title-font% %codelet-count-font% %letter-font%
                %irrelevant-group-length-font% %relevant-concept-mapping-font%
                %irrelevant-concept-mapping-font% %concept-mapping-list-superscript-font%)
;; workspace-graphics.ss: run.ss's go calls it when *display-mode?* is on,
;; which only the Workspace window's handlers turn on
(define restore-current-state
  (lambda () (error 'restore-current-state "no views are loaded")))
;; coderack-graphics.ss: a font created by set! when the Coderack window is
;; made (select-coderack-fonts), never defined; coderack.ss's codelet types
;; draw their counts with it
(view-variables %coderack-codelet-count-font%)
;; gui.ss: the speed settings, read by the windows (flashes, pauses) and set
;; by the control panel's speed slider (racket/gui/gui.rktl); views.rkt's
;; attach-views! sets them as at full speed for offscreen views
(view-variables %num-of-flashes% %flash-pause% %snag-pause%
                %codelet-highlight-pause% %text-scroll-pause%)
