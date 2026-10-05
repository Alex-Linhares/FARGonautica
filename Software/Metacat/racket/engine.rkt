#lang s-exp "engine-lang.rkt"
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
;; Ported to Racket, 2026: the model files of metacat.ss's load order, as one
;; module.

;; The engine.  The original loads its files into one global top level, where
;; every definition can refer to every other and any file can set! any
;; global.  Racket modules cannot be mutually recursive, and a module cannot
;; set! a variable it imports, so the port keeps the original's shape: one
;; module that includes the ported files (racket/engine/*.rktl) in the load
;; order of metacat.ss, after compat.rkt and utilities.rkt (syntactic-sugar.ss
;; and utilities.ss, the first two files loaded).  See docs/porting-notes.md,
;; item 04.
;;
;; engine/pending.rktl defines the names the original refers to but never
;; defines (until item 15 it also held stand-ins for files not ported yet;
;; a name defined twice is a compile error).  engine/view-globals.rktl
;; declares, as #f, what the model reads but the graphics files define.
;;
;; engine-lang.rkt discards the values of module-level expressions, which
;; Chez's top level drops and racket/base would print.

(require racket/include "compat.rkt" "utilities.rkt")

(provide (all-defined-out) set-global!)

(include "engine/constants.rktl")      ; constants.ss (model constants only)
(include "engine/setup.rktl")          ; setup.ss (without setup, enable-resizing)
(include "engine/coderack.rktl")       ; coderack.ss
(include "engine/descriptions.rktl")   ; descriptions.ss
(include "engine/bonds.rktl")          ; bonds.ss
(include "engine/groups.rktl")         ; groups.ss
(include "engine/bridges.rktl")        ; bridges.ss
(include "engine/breakers.rktl")       ; breakers.ss
(include "engine/workspace.rktl")      ; workspace.ss
(include "engine/workspace-objects.rktl") ; workspace-objects.ss
(include "engine/workspace-structures.rktl") ; workspace-structures.ss
(include "engine/workspace-strings.rktl") ; workspace-strings.ss
(include "engine/concept-mappings.rktl") ; concept-mappings.ss
(include "engine/workspace-structure-formulas.rktl") ; workspace-structure-formulas.ss
(include "engine/run.rktl")            ; run.ss (without prompt, no-prompt)
(include "engine/formulas.rktl")       ; formulas.ss
(include "engine/slipnet.rktl")        ; slipnet.ss
(include "engine/images.rktl")         ; images.ss
(include "engine/rules.rktl")          ; rules.ss
;; port: utilities.ss's reveal-obj names slipnodes with rules.ss's
;; format-slipnode, which utilities.rkt (loaded first, a module of its own)
;; looks up as a top-level value (porting-notes.md, items 03 and 17)
(define-top-level-value 'format-slipnode format-slipnode)
(include "engine/answers.rktl")        ; answers.ss
(include "engine/themes.rktl")         ; themes.ss
(include "engine/justify.rktl")        ; justify.ss
(include "engine/trace.rktl")          ; trace.ss
(include "engine/jootsing.rktl")       ; jootsing.ss
(include "engine/memory.rktl")         ; memory.ss
;; the graphics files' pexp builders, which the model calls when
;; %workspace-graphics% is on, and the parts of the other graphics files
;; the model uses (their windows are in racket/gui/)
(include "engine/general-graphics.rktl") ; general-graphics.ss (without the windows)
(include "engine/group-graphics.rktl") ; group-graphics.ss
(include "engine/bridge-graphics.rktl") ; bridge-graphics.ss
(include "engine/rule-graphics.rktl")  ; rule-graphics.ss
(include "engine/theme-graphics.rktl") ; theme-graphics.ss (relation-name only)
(include "engine/trace-graphics.rktl") ; trace-graphics.ss (group-event-pexp-text-string only)
(include "engine/eeg-graphics.rktl")   ; eeg-graphics.ss (the EEG object, without the window)
(include "engine/demos.rktl")          ; demos.ss
(include "engine/view-globals.rktl")   ; colours and fonts the views install
(include "engine/pending.rktl")        ; names the original never defines

;; (set-global! 'name value): set! one of the engine's global variables from
;; outside the module (a run's driver, the GUI, the tests).  The original's
;; globals are top-level variables anyone may set!; importers of a Racket
;; module may not, so the ones set from outside are listed here.  Listing a
;; variable here also keeps Racket from treating it as a constant.
(define-syntax-rule (global-setter name ...)
  (lambda (sym value)
    (case sym
      [(name) (set! name value)] ...
      [else (error 'set-global! "not a settable engine global: ~s" sym)])))

(define set-global!
  (global-setter
    ;; setup.ss
    *codelet-count* *temperature*
    *workspace-window* *slipnet-window* *coderack-window* *themespace-window*
    *top-themes-window* *bottom-themes-window* *vertical-themes-window*
    *memory-window* *comment-window* *trace-window* *temperature-window*
    *EEG-window* *control-panel*
    %eliza-mode% %justify-mode% %self-watching-enabled% %verbose%
    %workspace-graphics% %slipnet-graphics% %coderack-graphics%
    %codelet-count-graphics% %highlight-last-codelet% %nice-graphics%
    *repl-thread*
    ;; slipnet.ss
    *top-down-slipnodes*
    ;; workspace.ss
    *workspace* *initial-string* *modified-string* *target-string* *answer-string*
    *top-strings* *bottom-strings* *vertical-strings* *non-answer-strings*
    *all-strings*
    ;; formulas.ss
    temp-adjusted-probability
    ;; themes.ss, trace.ss, memory.ss (item 10); the batteries of items
    ;; 05-09 replace them with fakes
    *themespace* *trace* *memory*
    monitor-slipnode-activation-change monitor-new-groups
    monitor-new-concept-mappings monitor-new-rules
    make-answer-event make-snag-event
    abstract-answer-description abstract-snag-description
    ;; never defined by the original (engine/pending.rktl), and the EEG
    ;; (engine/eeg-graphics.rktl)
    *temperature-clamped?* *EEG*
    ;; groups.ss
    contains?
    ;; wrapped by a run's trace (chez_scheme/oracle/trace.ss does it by set!)
    *coderack* build-bond break-bond build-group break-group build-bridge
    break-bridge build-description update-temperature update-slipnet-activations
    ;; run.ss: the run's state, set by the GUI's control panel in the
    ;; original; break and quiet-break are replaced by headless drivers
    *this-run* *display-mode?* *running?* *interrupt?* *break-time*
    *step-mode?* %step-cycles% break quiet-break
    ;; run.ss: the rules battery (item 09) replaces them with fakes
    suspend update-everything post-initial-codelets
    ;; engine/view-globals.rktl: installed by the views (racket/gui/views.rkt)
    =white= =black= =grey= =red= =green= =blue= =yellow= =pink= =orange=
    %vertical-slippage-color% %dim-vertical-slippage-color%
    %coattail-inducing-slippage-color% %dim-coattail-inducing-slippage-color%
    %top-bridge-color% %vertical-bridge-color% %bottom-bridge-color%
    %bridge-label-background-color% %faded-bridge-label-background-color%
    %top-rule-color% %bottom-rule-color% %snag-color%
    %theme-supporting-concept-mapping-color%
    %faded-workspace-structure-color% %workspace-event-structure-color%
    %clamp-event-concept-pattern-color% %concept-activation-event-concept-pattern-color%
    %concept-mapping-event-concept-pattern-color% %group-event-concept-pattern-color%
    %top-rule-event-concept-pattern-color% %bottom-rule-event-concept-pattern-color%
    %snag-event-concept-pattern-color%
    %coderack-background-color% %current-codelet-color%
    %extremely-low-urgency-color% %very-low-urgency-color% %low-urgency-color%
    %medium-urgency-color% %high-urgency-color% %very-high-urgency-color%
    %extremely-high-urgency-color%
    %default-fg-color% *fg-color*
    %group-letter-category-font% %relevant-group-length-font% %bridge-label-font%
    %rule-font% %workspace-title-font% %codelet-count-font% %letter-font%
    %irrelevant-group-length-font% %relevant-concept-mapping-font%
    %irrelevant-concept-mapping-font% %concept-mapping-list-superscript-font%
    restore-current-state %coderack-codelet-count-font%
    ;; gui.ss's speed settings (engine/view-globals.rktl), read by the windows
    %num-of-flashes% %flash-pause% %snag-pause% %codelet-highlight-pause%
    %text-scroll-pause%))
