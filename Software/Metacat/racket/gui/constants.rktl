;;=============================================================================
;; Copyright (c) 1999, 2003 by James B. Marshall
;;
;; This file is part of Metacat.
;;
;; Metacat is based on Copycat, which was originally written in Common
;; Lisp by Melanie Mitchell.
;;
;; Metacat is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.
;;
;; Metacat is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;=============================================================================
;; Ported to Racket, 2026: the graphics part of constants.ss (window sizes,
;; colours, window titles), included by racket/gui/views.rkt; the model part
;; (the probability distributions) is racket/engine/constants.rktl, and
;; swl-color with *color-names* is racket/gui/colors.rkt.  Colours the model
;; files read are engine globals (racket/engine/view-globals.rktl), which
;; views.rkt's define installs in the engine.  Verbatim otherwise.

;; Default window sizes
(define %default-trace-width% #f)
(define %default-trace-height% #f)
(define %virtual-trace-length% #f)
(define %default-coderack-width% #f)
(define %default-13x5-slipnet-width% #f)
(define %default-temperature-width% #f)
(define %default-memory-width% #f)
(define %default-memory-height% #f)
(define %virtual-memory-length% #f)
(define %default-comment-window-width% #f)
(define %default-comment-window-height% #f)
(define %virtual-comment-window-length% #f)
(define %EEG-window-width% #f)
(define %EEG-window-height% #f)
(define %virtual-EEG-length% #f)
(define %top-theme-window-size% #f)
(define %bottom-theme-window-size% #f)
(define %vertical-theme-window-size% #f)
(define %default-workspace-width% #f)

;; standard screen sizes: 800x600 1024x768 1152x864 1280x1024 1400x1050

(define set-window-size-defaults
  (lambda (scale)
    (let* ((screen-width (swl:screen-width))
	   (screen-height (swl:screen-height))
	   (width (lambda (w) (round (* scale w))))
	   (height (lambda (h) (round (* scale h)))))
      (set! %default-13x5-slipnet-width% (width 650))
      (set! %default-coderack-width% (width 230))
      (set! %default-temperature-width% (width 70))
      (set! %default-trace-width% (width 1000))
      (set! %default-trace-height% (height 69))
      (set! %virtual-trace-length% (width 7000))
      (set! %default-memory-width% (width 260))
      (set! %default-memory-height% (height 400))
      (set! %virtual-memory-length% (height 2000))
      (set! %default-comment-window-width% (width 300))
      (set! %default-comment-window-height% (height 600))
      (set! %virtual-comment-window-length% (height 4000))
      (set! %EEG-window-width% (width 900))
      (set! %EEG-window-height% (height 120))
      (set! %virtual-EEG-length% (width 2000))
      (set! %top-theme-window-size% (list (width 600) (height 140)))
      (set! %bottom-theme-window-size% (list (width 600) (height 140)))
      (set! %vertical-theme-window-size% (list (width 160) (height 590)))
      (set! %default-workspace-width% (width 800))
      'done)))

;; Common color names
(define =white= (swl-color "white"))
(define =black= (swl-color "black"))
(define =grey= (swl-color "grey"))
(define =red= (swl-color "red"))
(define =green= (swl-color "green"))
(define =blue= (swl-color "blue"))
(define =yellow= (swl-color "yellow"))
(define =pink= (swl-color "pink"))
(define =orange= (swl-color "orange"))

;;----------------------------------------------------------------------

;; Control Panel
(define %gui-command-line-color% (swl-color "azure"))
(define %gui-speed-controls-color% (swl-color "lavender"))
(define %gui-checkbox-select-color% (swl-color "royal blue"))
(define %gui-menu-item-on-color% (swl-color "black"))
(define %gui-menu-item-off-color% (swl-color "grey55"))

;; bug workaround: for some reason on mac OS X, set-background-color!
;; seems to affect the menu font *foreground* color instead of the
;; background color, and grey85 is not very visible as a foreground color.
(define %gui-menu-background-color% (if (eq? *platform* 'macintosh)
				      (swl-color "black")
				      (swl-color "grey85")))

(define %gui-help-window-color% (swl-color "bisque"))
(define %gui-run-mode-foreground-color% (swl-color "green"))
(define %gui-run-mode-background-color% (swl-color "black"))

;; Workspace
(define %workspace-background-color% (swl-color "white"))

;; Slippages
(define %vertical-slippage-color% (swl-color "magenta"))
(define %dim-vertical-slippage-color% (swl-color "dark magenta"))
(define %coattail-inducing-slippage-color% (swl-color "magenta"))
(define %dim-coattail-inducing-slippage-color% (swl-color "dark magenta"))

;; Bridges
(define %top-bridge-color% (swl-color "red"))
(define %vertical-bridge-color% (swl-color "dark violet"))
(define %bottom-bridge-color% (swl-color "blue"))
(define %bridge-label-background-color% (swl-color "yellow"))
(define %faded-bridge-label-background-color% (swl-color "grey97"))

;; Rules
(define %top-rule-color% (swl-color "firebrick2"))
(define %bottom-rule-color% (swl-color "medium blue"))

;; Snags
(define %snag-color% (swl-color "orange"))

;; Answer descriptions
(define %theme-supporting-concept-mapping-color% (swl-color "forest green"))

;; Slipnet:
(define %slipnet-background-color% (swl-color "lemon chiffon"))
(define %slipnode-activation-color% (swl-color "midnight blue"))
(define %frozen-slipnode-activation-color% (swl-color "deep sky blue"))

;; Themespace:
(define %theme-background-color:thematic-pressure-off% (swl-color "grey"))
(define %theme-background-color:thematic-pressure-on% (swl-color "spring green"))
(define %panel-highlight-color:thematic-pressure-off% (swl-color "lemon chiffon"))
(define %panel-highlight-color:thematic-pressure-on% (swl-color "yellow"))
(define %positive-theme-activation-color% (swl-color "forest green"))
(define %negative-theme-activation-color% (swl-color "firebrick2"))
(define %theme-edit-mode-color% (swl-color "white"))

;; Temporal Trace:
(define %trace-background-color% (swl-color "aquamarine"))
(define %faded-workspace-structure-color% (swl-color "grey"))
(define %workspace-event-structure-color% (swl-color "magenta"))

;; Temporal Trace icon highlight colors
(define %answer-event-icon-highlight-color% (swl-color "yellow"))
(define %clamp-event-icon-highlight-color% (swl-color "spring green"))
(define %concept-activation-event-icon-highlight-color% (swl-color "cyan"))
(define %concept-mapping-event-icon-highlight-color% (swl-color "violet"))
(define %group-event-icon-highlight-color% (swl-color "violet"))
(define %top-rule-event-icon-highlight-color% %top-rule-color%)
(define %bottom-rule-event-icon-highlight-color% %bottom-rule-color%)
(define %snag-event-icon-highlight-color% (swl-color "red"))

;; Comment window
(define %comment-window-background-color% (swl-color "pink"))

;; Concept-pattern colors
(define %clamp-event-concept-pattern-color% %frozen-slipnode-activation-color%)
(define %concept-activation-event-concept-pattern-color% %slipnode-activation-color%)
(define %concept-mapping-event-concept-pattern-color% (swl-color "violet"))
(define %group-event-concept-pattern-color% (swl-color "violet"))
(define %top-rule-event-concept-pattern-color% %top-rule-color%)
(define %bottom-rule-event-concept-pattern-color% %bottom-rule-color%)
(define %snag-event-concept-pattern-color% %snag-color%)

;; Coderack:
(define %coderack-background-color% (swl-color "misty rose"))
(define %last-codelet-color% (swl-color "hot pink"))
(define %current-codelet-color% (swl-color "yellow"))

(define %extremely-low-urgency-color% (swl-color "grey40"))
(define %very-low-urgency-color% (swl-color "grey50"))
(define %low-urgency-color% (swl-color "grey60"))
(define %medium-urgency-color% (swl-color "grey70"))
(define %high-urgency-color% (swl-color "grey80"))
(define %very-high-urgency-color% (swl-color "grey90"))
(define %extremely-high-urgency-color% (swl-color "grey100"))

;; Episodic Memory:
;; grey75 = grey, grey100 = white, grey0 = black
(define %memory-background-grey-level% 50)

;; Temperature:
(define %temperature-background-color% (swl-color "LightCyan2"))
(define %thermometer-mercury-color% (swl-color "firebrick2"))

;; EEG:
(define %EEG-background-color% (swl-color "black"))
(define %EEG-title-color% (swl-color "white"))

;; Mcat logo:
(define %logo-background-color% (swl-color "light sky blue"))
(define %logo-font% (swl-font sans-serif 18 'bold 'italic))

;; incomplete
(define b/w-mode
  (lambda ()
    (set! %vertical-slippage-color% (swl-color "black"))
    (set! %dim-vertical-slippage-color% (swl-color "black"))
    (set! %coattail-inducing-slippage-color% (swl-color "black"))
    (set! %dim-coattail-inducing-slippage-color% (swl-color "black"))
    (set! %top-bridge-color% (swl-color "black"))
    (set! %vertical-bridge-color% (swl-color "black"))
    (set! %bottom-bridge-color% (swl-color "black"))
    (set! %bridge-label-background-color% (swl-color "grey93"))
    (set! %faded-bridge-label-background-color% (swl-color "grey97"))
    (set! %top-rule-color% (swl-color "black"))
    (set! %bottom-rule-color% (swl-color "black"))
    (set! %snag-color% (swl-color "black"))
    (set! %theme-supporting-concept-mapping-color% (swl-color "black"))
    (set! %slipnode-activation-color% (swl-color "black"))
    (set! %frozen-slipnode-activation-color% (swl-color "grey50"))
    (set! %faded-workspace-structure-color% (swl-color "grey40"))
    (set! %workspace-event-structure-color% (swl-color "black"))
    ;; Concept-patterns
    (set! %clamp-event-concept-pattern-color% %frozen-slipnode-activation-color%)
    (set! %concept-activation-event-concept-pattern-color% %slipnode-activation-color%)
    (set! %concept-mapping-event-concept-pattern-color% (swl-color "grey50"))
    (set! %group-event-concept-pattern-color% (swl-color "grey50"))
    (set! %top-rule-event-concept-pattern-color% (swl-color "grey50"))
    (set! %bottom-rule-event-concept-pattern-color% (swl-color "grey50"))
    (set! %snag-event-concept-pattern-color% %snag-color%)
    ;; Comment window
    (set! %comment-window-background-color% (swl-color "white"))
    (set! %default-comment-window-width% 500)
    (set! %default-comment-window-height% 450)
    'ok))

;;----------------------------------------------------------------------
;; Window titles and icons

(define %workspace-icon-label% "Workspace")
(define %workspace-icon-image% #f)
(define %workspace-window-title% "Workspace")
(define %temperature-icon-image% #f)
(define %temperature-window-title% "Temperature")
(define %slipnet-icon-label% "Slipnet")
(define %slipnet-icon-image% #f)
(define %slipnet-window-title% "Slipnet")
(define %coderack-icon-label% "Coderack")
(define %coderack-icon-image% #f)
(define %coderack-window-title% "Coderack")
(define %top-bridge-themes-icon-label% "Top Themes")
(define %top-bridge-themes-icon-image% #f)
(define %top-bridge-themes-window-title% "Top Themes")
(define %bottom-bridge-themes-icon-label% "Bottom Themes")
(define %bottom-bridge-themes-icon-image% #f)
(define %bottom-bridge-themes-window-title% "Bottom Themes")
(define %vertical-bridge-themes-icon-label% "Vertical Themes")
(define %vertical-bridge-themes-icon-image% #f)
(define %vertical-bridge-themes-window-title% "Vertical Themes")
(define %trace-icon-label% "Temporal Trace")
(define %trace-icon-image% #f)
(define %trace-window-title% "Temporal Trace")
(define %memory-window-icon-label% "Episodic Memory")
(define %memory-window-icon-image% #f)
(define %memory-window-title% "Episodic Memory")
(define %comment-window-icon-label% "Commentary")
(define %comment-window-icon-image% #f)
(define %comment-window-title% "Commentary")
(define %EEG-icon-label% "EEG")
(define %EEG-icon-image% #f)
(define %EEG-window-title% "EEG")
(define %logo-icon-label% "Logo")
(define %logo-icon-image% #f)
(define %logo-window-title% "Logo")
