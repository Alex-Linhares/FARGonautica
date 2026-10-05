#lang racket/base
;; Item 04: the structure of racket/engine.rkt (one module including the
;; ported files; see docs/porting-notes.md).  The model's behaviour is
;; checked against the original by coderack-diff-test.rkt and
;; slipnet-diff-test.rkt.
(require rackunit
         racket/port
         racket/runtime-path)

(define-runtime-path engine-path "../engine.rkt")

;; Loading the engine prints nothing: engine-lang.rkt discards module-level
;; expression values such as the 'done of each define-codelet-procedure*.
(define load-output
  (with-output-to-string
    (lambda ()
      (parameterize ([current-namespace (make-base-namespace)])
        (dynamic-require engine-path #f)))))
(check-equal? load-output "")

(define (engine name) (dynamic-require engine-path name))

;; set-global! sets the listed globals, and only those
(check-equal? (engine '*temperature*) 0)
((engine 'set-global!) '*temperature* 42)
(check-equal? (engine '*temperature*) 42)
((engine 'set-global!) '*temperature* 0)
(check-exn #rx"not a settable engine global" (lambda () ((engine 'set-global!) 'urgency-name 1)))

;; codelet types are module-level variables and top-level values
(check-equal? (length (engine '*codelet-types*)) 27)
(check-eq? (engine 'breaker) (list-ref (engine '*codelet-types*) 26))
(check-eq? ((dynamic-require (build-path engine-path 'up "compat.rkt") 'top-level-value) 'breaker)
           (engine 'breaker))

;; item 05: slipnodes are module-level variables and top-level values, and
;; so are slipnet links, under the names the link macros give them
(define top-level-value (dynamic-require (build-path engine-path 'up "compat.rkt") 'top-level-value))
(check-equal? (length (engine '*slipnet-nodes*)) 59)
(check-eq? (engine 'plato-a) (car (engine '*slipnet-nodes*)))
(check-eq? (top-level-value 'plato-bond-facet) (engine 'plato-bond-facet))
(check-eq? (top-level-value 'a-b-link)
           (car ((dynamic-require (build-path engine-path 'up "utilities.rkt") 'tell)
                 (engine 'plato-a) 'get-lateral-links)))

;; item 06: the Workspace exists at load time, as in the original, and a
;; workspace string can be made headless
(define tell (dynamic-require (build-path engine-path 'up "utilities.rkt") 'tell))
(check-equal? (tell (engine '*workspace*) 'object-type) 'workspace)
(let ([s ((engine 'make-workspace-string) 'target 'mrrjjj)])
  (check-equal? (tell s 'print-name) "mrrjjj")
  (check-equal? (tell s 'get-max-object-capacity) 12)
  (check-equal? (map (lambda (l) (tell l 'ascii-name)) (tell s 'get-letters))
                '("m:0" "r:1" "r:2" "j:3" "j:4" "j:5")))
;; formulas.ss at temperature 0 and 100
((engine 'set-global!) '*temperature* 100)
(check-equal? ((engine 'temp-adjusted-probability) 0.5) 0.5)
(check-equal? ((engine 'temp-adjusted-values) '(1 2 3)) '(1 1 2)) ; round(v^0.5)
((engine 'set-global!) '*temperature* 0)

;; item 07: a bond and a group between letters of a headless string; the
;; original's undefined same-direction? (bonds.ss) raises, as under Chez
(let* ([s ((engine 'make-workspace-string) 'target 'abc)]
       [a (tell s 'get-letter 0)]
       [b (tell s 'get-letter 1)]
       [bond ((engine 'make-bond) a b (engine 'plato-successor) (engine 'plato-letter-category)
                                  (engine 'plato-a) (engine 'plato-b))])
  (check-equal? (tell bond 'object-type) 'bond)
  (check-eq? (tell bond 'get-direction) (engine 'plato-right))
  (check-equal? (tell bond 'calculate-internal-strength)
                ((engine 'bond-degree-of-assoc) (engine 'plato-successor)))
  (check-eq? (tell (tell bond 'make-flipped-version) 'get-bond-category)
             (engine 'plato-predecessor))
  (let ([cm ((engine 'make-concept-mapping)
             a (engine 'plato-letter-category) (engine 'plato-a)
             b (engine 'plato-letter-category) (engine 'plato-b))])
    (check-equal? (tell cm 'print-name) "a=>b")
    (check-eq? (tell cm 'get-label) (engine 'plato-successor))))
(check-exn #rx"same-direction[?] is not bound"
           (lambda () ((engine 'same-direction?) 'a 'b)))
(check-true (procedure? (engine 'group-graphics)))

;; view globals are #f until views are loaded (engine/view-globals.rktl;
;; no pending procedures are left since item 13)
(check-false (engine '%coderack-codelet-count-font%))

;; the graphics files' pexp builders (item 13) are the engine's; the colours
;; and fonts the views install are #f until views are loaded, and
;; restore-current-state raises
(for ([name '(circle outline-box solid-box arrowhead dotted-line-points dashed-line-points
              zigzag-line-points elliptical-arc break-into-lines make-group-pexp
              draw-group-grope bridge-graphics make-bridge-pexp draw-bridge-grope
              new-bridge-label-number initialize-rule-graphics update-rule-pexps!
              make-new-rule-pexp)])
  (check-true (procedure? (engine name)) (symbol->string name)))
(for ([name '(=white= %top-bridge-color% %default-fg-color% *fg-color* %rule-font%
              %bridge-label-font% %group-letter-category-font%)])
  (check-false (engine name) (symbol->string name)))
(check-exn #rx"no views are loaded" (lambda () ((engine 'restore-current-state))))

;; run.ss (item 11)
(for ([name '(ss step-mode-on step-mode-off runtil clear-breakpoint break quiet-break go
              suspend rerun run-mcat step-mcat init-mcat init-workspace
              clamp-initial-slipnodes post-initial-codelets
              add-string-position-descriptions-to-letters update-everything
              update-workspace-values update-all-graphics)])
  (check-true (procedure? (engine name)) (symbol->string name)))
(check-equal? (list (engine '%update-cycle-length%) (engine '%initial-slipnode-clamp-cycles%)
                    (engine '%garbage-collect-cycles%) (engine '*this-run*)
                    (engine '*break-time*) (engine '*step-mode?*) (engine '%step-cycles%))
              '(15 50 100 #f #f #f 1))

;; rules.ss and answers.ss (item 09), general-graphics.ss's
;; find-next-space-position (an early copy in engine/pending.rktl until item
;; 13, now engine/general-graphics.rktl) and themes.ss's diff (item 10)
(for ([name '(make-rule rule-describable-bridge? verbatim-clause? apply-rule
              transcribe-to-english translate report-new-answer give-up
              make-translated-string make-slippage-log)])
  (check-true (procedure? (engine name)) (symbol->string name)))
(for ([name '(rule-scout rule-evaluator rule-builder answer-finder)])
  (check-true (and (memq (engine name) (engine '*codelet-types*)) #t)
              (symbol->string name)))
(check-equal? ((engine 'find-next-space-position) "ab cd" 0) 2)
(check-equal? ((engine 'find-next-space-position) "abcd" 0) 4)
(check-false (engine 'diff))

;; bridges.ss and breakers.ss (item 08), and themes.ss's pure helpers (early
;; copies in engine/pending.rktl until item 10)
(for ([name '(make-horizontal-bridge make-vertical-bridge bridge-between? break-bridge
              build-bridge propose-bridge incompatible-horizontal-CMs?
              incompatible-vertical-CMs?)])
  (check-true (procedure? (engine name)) (symbol->string name)))
(check-true (and (memq (engine 'breaker) (engine '*codelet-types*)) #t))
(check-equal? (map (engine 'bridge-type->theme-type) '(top bottom vertical))
              '(top-bridge bottom-bridge vertical-bridge))
(check-equal? ((engine 'bridge-theme-compatibility-sigmoid) 0) 0)

;; themes.ss, justify.ss, trace.ss, jootsing.ss, memory.ss (item 10): their
;; objects exist at load time, as in the original, and their codelet types
;; have procedures
(check-equal? (tell (engine '*themespace*) 'object-type) 'themespace)
(check-equal? (tell (engine '*trace*) 'object-type) 'temporal-trace)
(check-equal? (tell (engine '*memory*) 'object-type) 'memory)
(for ([name '(thematic-bridge-scout answer-justifier progress-watcher jootser)])
  (check-true (and (memq (engine name) (engine '*codelet-types*)) #t)
              (symbol->string name)))
(for ([name '(make-answer-event make-snag-event monitor-new-rules abstract-answer-description
              compare-rule-clause-lists theme-pattern-entries-equal?)])
  (check-true (procedure? (engine name)) (symbol->string name)))
;; the parts of the panel files the model uses are in the engine (item 14:
;; engine/theme-, trace- and eeg-graphics.rktl)
(check-equal? ((engine 'relation-name) #f) "diff")
(check-equal? ((engine 'relation-name) (engine 'plato-successor)) "succ")
(check-true (procedure? (engine 'group-event-pexp-text-string)))
(check-equal? (tell (engine '*EEG*) 'object-type) 'EEG)
(check-equal? (map car (engine '%EEG-table%)) '(0 1 2))
(check-false (engine '%coderack-codelet-count-font%) "a view global, #f until views load")

;; the engine never requires racket/gui (CLAUDE.md, TASK.md): loading it
;; into a fresh namespace declares no racket/gui or racket/draw module
(parameterize ([current-namespace (make-base-empty-namespace)])
  (dynamic-require engine-path #f)
  (check-false (module-declared? 'racket/gui/base #f))
  (check-false (module-declared? 'racket/gui #f))
  (check-false (module-declared? 'racket/draw #f)))
