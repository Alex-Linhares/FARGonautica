;;; codelet-harness.scm -- the codelet-level differential harness (item 07):
;;; bonds.ss, groups.ss and concept-mappings.ss, run codelet by codelet in the
;;; oracle and in the port side by side.
;;;
;;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Read form by form, after tests/diff/helpers.scm, by two runners:
;;;   chez_scheme/oracle/diff-eval.ss       (Chez, with the whole original loaded)
;;;   racket/tests/codelet-diff-test.rkt    (Racket: compat + utilities + engine)
;;; The rules of utilities-battery.scm apply: no two side-effecting
;;; subexpressions in one call or one `let'; lists for `map' are computed.
;;;
;;; b:run-codelets is a copy of run.ss's run-mcat loop (step-mcat, the
;;; unclamping of the initially clamped slipnodes, re-posting when the
;;; Coderack is empty, update-everything every 15 codelets) with only the
;;; codelet types of these files enabled:
;;;   - the initial codelets are only the bottom-up bond scouts (run.ss also
;;;     posts as many bottom-up bridge scouts);
;;;   - the bottom-up codelets posted every cycle are only
;;;     bottom-up-bond-scout and group-scout:whole-string;
;;;   - *top-down-slipnodes* holds only the bond and group nodes (left, right,
;;;     pred, succ, sameness, predgrp, succgrp, samegrp), whose top-down
;;;     codelets are the top-down bond and group scouts; self-watching is off,
;;;     so no thematic codelet is posted;
;;;   - update-everything leaves out what needs files not ported yet: rules
;;;     (check-if-rules-possible), the Trace's snag and clamp periods, and the
;;;     Themespace (no themes are ever active).
;;; Codelets evaluate, build and break bonds and groups, and propose
;;; concept mappings inside groups' and bonds' incompatibility tests.
;;;
;;; The trace of a run, compared line for line, records for every codelet
;;; its type, urgency, time stamp, the generator state after it ran, and every
;;; structure built or broken (with its strength); every 15 codelets the
;;; temperature, all slipnet activations, the Coderack's size and every
;;; object of the Workspace (descriptions, importance, unhappiness, salience);
;;; every message sent to the Workspace window and every call of the trace.ss
;;; monitors (group and concept-activation events).

(load "tests/diff/workspace-dump.scm")

;;;---------------------------------------------------------------------------
;;; Settings and fakes (both runners)

(define b:events '())
(define b:event! (lambda (e) (set! b:events (cons e b:events)) 'done))

(define b:nm (lambda (node) (if node (tell node 'get-name-symbol) #f)))

;; No theme is ever active.  Bridge-builder boosts themes through the
;; Themespace (add-theme-if-possible for each pair of descriptions that
;; affect it, then update-dominant-themes and the Themespace window's
;; update-graphics); the calls are recorded, and no theme is created.
(define b:fake-themespace
  (lambda msg
    (record-case (cdr msg)
      (get-active-themes (types) '())
      (get-all-active-themes () '())
      (add-theme-if-possible (theme-type dimension relation)
        (b:event! (list 'add-theme theme-type (b:nm dimension) (b:nm relation)))
        #f)
      (update-dominant-themes (theme-type)
        (b:event! (list 'update-dominant-themes theme-type)))
      ;; rules.ss (item 09): with no theme, every cluster lacks a dominant
      ;; theme, and the real pattern is just the type
      (get-dominant-theme-pattern (theme-type) (list theme-type))
      (else (error 'fake-themespace "unexpected message" msg)))))

(define b:null-themespace-window
  (lambda msg
    (b:event! (list 'themespace-window (cadr msg)))))

(define b:fake-eeg (lambda msg 'done))

;; the Workspace window with the display off: groups.ss calls group-graphics
;; ungated (group-builder, when it consolidates sameness groups), which
;; messages it; every message is recorded
(define b:null-workspace-window
  (lambda msg
    (b:event! (list 'window (cadr msg)))))

(define b:null-coderack-window (lambda msg 'done))

(b:set-global! '*themespace* b:fake-themespace)
(b:set-global! '*EEG* b:fake-eeg)
(b:set-global! '*workspace-window* b:null-workspace-window)
(b:set-global! '*themespace-window* b:null-themespace-window)
(b:set-global! '%workspace-graphics% #f)
(b:set-global! '%slipnet-graphics% #f)
(b:set-global! '%coderack-graphics% #f)
(b:set-global! '%verbose% #f)
(b:set-global! '%self-watching-enabled% #f)
(b:set-global! '*top-down-slipnodes*
  (list plato-left plato-right plato-predecessor plato-successor
        plato-sameness plato-predgrp plato-succgrp plato-samegrp))
(for-each
  (lambda (type)
    (tell type 'set-graphics-parameters b:null-coderack-window #f #f #f #f #f #f #f #f))
  *codelet-types*)

;; trace.ss's monitors, recording their arguments
(b:set-global! 'monitor-slipnode-activation-change
  (lambda (node previous new)
    (b:event! (list 'activation (b:nm node) previous new))))

;; b:canon for trace entries, with proper lists written as (x y ...)
(define b:compact
  (lambda (x)
    (cond
      ((null? x) "()")
      ((and (pair? x) (list? x))
       (let loop ((l (cdr x)) (acc (list (b:compact (car x)) "(")))
         (if (null? l)
             (apply string-append (reverse (cons ")" acc)))
             (loop (cdr l) (cons (b:compact (car l)) (cons " " acc))))))
      ((pair? x) (string-append "(" (b:compact (car x)) " . " (b:compact (cdr x)) ")"))
      ((symbol? x) (symbol->string x))
      (else (b:canon x)))))

;;;---------------------------------------------------------------------------
;;; Structures as data

(define b:obj-id
  (lambda (obj)
    (list (tell obj 'which-string)
          (tell obj 'object-type)
          (tell obj 'get-left-string-pos)
          (tell obj 'get-right-string-pos))))

(define b:bond-data
  (lambda (bond)
    (list 'bond
          (b:obj-id (tell bond 'get-from-object))
          (b:obj-id (tell bond 'get-to-object))
          (b:nm (tell bond 'get-bond-category))
          (b:nm (tell bond 'get-bond-facet))
          (b:nm (tell bond 'get-direction))
          (b:nm (tell bond 'get-from-object-descriptor))
          (b:nm (tell bond 'get-to-object-descriptor)))))

(define b:group-data
  (lambda (group)
    (list 'group
          (b:obj-id group)
          (b:nm (tell group 'get-group-category))
          (b:nm (tell group 'get-bond-facet))
          (b:nm (tell group 'get-direction))
          (map b:obj-id (tell group 'get-constituent-objects))
          (map b:bond-data (tell group 'get-constituent-bonds)))))

(b:set-global! 'monitor-new-groups
  (lambda (group flipped?)
    (b:event! (list 'new-group (b:group-data group) flipped?
                    (tell group 'get-strength)))))

(define b:bridge-data
  (lambda (bridge)
    (list 'bridge
          (tell bridge 'get-bridge-type)
          (b:obj-id (tell bridge 'get-object1))
          (b:obj-id (tell bridge 'get-object2))
          (tell bridge 'flipped-group1?)
          (tell bridge 'flipped-group2?))))

(define b:cm-names
  (lambda (cms) (map (lambda (cm) (tell cm 'print-name)) cms)))

(b:set-global! 'monitor-new-concept-mappings
  (lambda (cms bridge)
    (b:event! (list 'new-cms (b:cm-names cms) (b:bridge-data bridge)))))

(define b:strings
  (lambda ()
    (if %justify-mode%
        (list *initial-string* *modified-string* *target-string* *answer-string*)
        (list *initial-string* *modified-string* *target-string*))))

;; every built bond and group, with its strengths and proposal level
(define b:structures
  (lambda ()
    (append
     (apply append
      (map (lambda (s)
             (append
               (map (lambda (b)
                      (list (b:bond-data b) (tell b 'get-proposal-level)
                            (tell b 'get-strength) (tell b 'get-time-stamp)))
                    (tell s 'get-bonds))
               (map (lambda (g)
                      (list (b:group-data g) (tell g 'get-proposal-level)
                            (tell g 'get-strength) (tell g 'get-time-stamp)
                            (map b:dump-description (tell g 'get-descriptions))))
                    (tell s 'get-groups))))
           (b:strings)))
      ;; every built bridge, with its concept mappings
      (map (lambda (b)
             (list (b:bridge-data b) (tell b 'get-proposal-level)
                   (tell b 'get-strength) (tell b 'get-time-stamp)
                   (b:cm-names (tell b 'get-concept-mappings))
                   (b:cm-names (tell b 'get-bond-concept-mappings))
                   (b:cm-names (tell b 'get-symmetric-slippages))))
           (b:all-bridges))
      ;; every built rule (item 09)
      (if b:rules?
          (map b:rule-entry (tell *workspace* 'get-all-rules))
          '()))))

(define b:all-bridges (lambda () (tell *workspace* 'get-all-bridges)))

;; rules (item 09): a rule as data, and any datum with the model's objects
;; in it (rule clauses, failure results, transforms) as data
(define b:datum
  (lambda (x)
    (cond
      ((pair? x) (let* ((a (b:datum (car x))) (d (b:datum (cdr x)))) (cons a d)))
      ((not (procedure? x)) x)
      ((slipnode? x) (b:nm x))
      ((or (letter? x) (group? x)) (b:obj-id x))
      ((workspace-string? x) (list 'string (tell x 'get-string-type)))
      ((bridge? x) (b:bridge-data x))
      ((concept-mapping? x) (list 'cm (tell x 'print-name)))
      (else 'object))))

(define b:rule-data
  (lambda (rule)
    (list 'rule (tell rule 'get-rule-type)
          (tell rule 'get-english-transcription)
          (b:datum (tell rule 'get-rule-clauses)))))

(define b:rule-entry
  (lambda (rule)
    (list (b:rule-data rule)
          (tell rule 'get-proposal-level)
          (tell rule 'get-strength) (tell rule 'get-time-stamp)
          (list (tell rule 'get-quality) (tell rule 'get-relative-quality)
                (tell rule 'get-uniformity) (tell rule 'get-abstractness)
                (tell rule 'get-succinctness))
          (tell rule 'supported?)
          (b:datum (tell rule 'get-tagged-supporting-horizontal-bridges))
          (tell rule 'get-theme-pattern))))

;; the number of descriptions of every object (description-builder)
(define b:description-counts
  (lambda ()
    (b:map-objects (lambda (obj) (length (tell obj 'get-descriptions))))))

(define b:member-equal?
  (lambda (x l) (and (pair? l) (or (equal? x (car l)) (b:member-equal? x (cdr l))))))

;; the structures of after (identified by their data) that are not in before
(define b:difference
  (lambda (after before)
    (let ((keys (map car before)))
      (let loop ((l after) (acc '()))
        (cond
          ((null? l) (reverse acc))
          ((b:member-equal? (car (car l)) keys) (loop (cdr l) acc))
          (else (loop (cdr l) (cons (car l) acc))))))))

(define b:proposed-counts
  (lambda ()
    (let* ((counts (map (lambda (s)
                          (list (length (tell s 'get-all-bonds))
                                (length (tell s 'get-all-groups))))
                        (b:strings))))
      (if b:bridges?
          (append counts
                  (list (length (tell *workspace* 'get-proposed-bridges 'top))
                        (length (tell *workspace* 'get-proposed-bridges 'vertical))
                        (b:description-counts)))
          counts))))

(define b:activations
  (lambda () (map (lambda (n) (tell n 'get-activation)) *slipnet-nodes*)))

;; a letter as workspace-dump.scm dumps it; a group likewise, as far as
;; groups answer the same messages
(define b:dump-ws-object
  (lambda (obj)
    (if (letter? obj)
        (b:dump-object obj)
        (list (b:group-data obj)
              (tell obj 'print-name)
              (tell obj 'get-id-num)
              (map b:dump-description (tell obj 'get-descriptions))
              (list 'importance
                    (tell obj 'get-raw-importance)
                    (tell obj 'get-relative-importance))
              (list 'unhappiness
                    (tell obj 'get-intra-string-unhappiness)
                    (tell obj 'get-inter-string-unhappiness 'horizontal)
                    (tell obj 'get-inter-string-unhappiness 'vertical)
                    (tell obj 'get-average-unhappiness))
              (list 'salience
                    (tell obj 'get-intra-string-salience)
                    (tell obj 'get-inter-string-salience 'horizontal)
                    (tell obj 'get-inter-string-salience 'vertical)
                    (tell obj 'get-average-salience))
              (list 'strength (tell obj 'get-proposal-level) (tell obj 'get-strength))
              (b:deep-nm (tell (tell obj 'get-image) 'generate))))))

;;;---------------------------------------------------------------------------
;;; The run loop (run.ss's run-mcat, step-mcat, update-everything,
;;; clamp-initial-slipnodes and post-initial-codelets, restricted as above)

;; b:bridges? (item 08) also enables bridges.ss and breakers.ss, and with
;; them the description codelets (descriptions.ss), which bridges post:
;;   - the initial codelets are run.ss's: bottom-up bond and bridge scouts;
;;   - the bottom-up types are those of *bottom-up-codelet-types* but rules,
;;     answers and self-watching (rule-scout, answer-finder,
;;     answer-justifier, progress-watcher, jootser);
;;   - *top-down-slipnodes* is the original's (adding the description
;;     scouts of StrPosCtgy, AlphaPosCtgy and Length).
(define b:bridges? #f)

;; b:rules? (item 09) also enables rules.ss and answers.ss: on top of the
;; bridges setting, update-everything starts with check-if-rules-possible
;; as run.ss's does, and posts the bottom-up codelets with the original's
;; add-bottom-up-codelets over all of *bottom-up-codelet-types* (with
;; self-watching off, progress-watcher and jootser get probability 0, and
;; answer-justifier too unless justifying), so rule scouts, evaluators,
;; builders and answer finders run.  A run stops after the codelet that
;; reports the first answer (suspend sets b:answered).
(define b:rules? #f)
(define b:answered #f)

(define b:unclamp-time 0)

(define b:clamp-initial-slipnodes
  (lambda ()
    (for-each (lambda (node) (tell node 'clamp %max-activation%))
      *initially-clamped-slipnodes*)
    (set! b:unclamp-time (+ *codelet-count* (* 50 %update-cycle-length%)))))

(define b:post-initial-codelets
  (lambda ()
    (let loop ((i (* 2 (length (tell *workspace* 'get-objects)))))
      (if (> i 0)
          (begin
            (tell *coderack* 'add-deferred-codelet
              (tell bottom-up-bond-scout 'make-codelet %very-low-urgency%))
            (if b:bridges?
                (tell *coderack* 'add-deferred-codelet
                  (tell bottom-up-bridge-scout 'make-codelet %very-low-urgency%)))
            (loop (- i 1)))))
    (tell *coderack* 'post-deferred-codelets)))

(define b:bottom-up-types (list bottom-up-bond-scout group-scout:whole-string))

(define b:bond-group-slipnodes
  (list plato-left plato-right plato-predecessor plato-successor
        plato-sameness plato-predgrp plato-succgrp plato-samegrp))

;; switch to the bridges setting (b:bridges?), or back
(define b:enable-bridges!
  (lambda (on?)
    (set! b:bridges? on?)
    (if on?
        (begin
          (set! b:bottom-up-types
            (list bottom-up-bond-scout group-scout:whole-string
                  bottom-up-bridge-scout important-object-bridge-scout
                  bottom-up-description-scout breaker))
          (b:set-global! '*top-down-slipnodes*
            (append b:bond-group-slipnodes
                    (list plato-string-position-category
                          plato-alphabetic-position-category plato-length))))
        (begin
          (set! b:bottom-up-types (list bottom-up-bond-scout group-scout:whole-string))
          (b:set-global! '*top-down-slipnodes* b:bond-group-slipnodes)))))

(define b:add-bottom-up-codelets
  (lambda ()
    (for-each
      (lambda (codelet-type)
        (if (prob? (post-codelet-probability codelet-type))
            (let* ((urgency (bottom-up-urgency codelet-type)))
              (let loop ((i (num-of-codelets-to-post codelet-type)))
                (if (> i 0)
                    (begin
                      (tell *coderack* 'add-deferred-codelet
                        (tell codelet-type 'make-codelet urgency))
                      (loop (- i 1))))))))
      b:bottom-up-types)))

(define b:update-everything
  (lambda ()
    (if b:rules? (tell *workspace* 'check-if-rules-possible))
    (b:update-workspace-values)
    ;; run.ss's end of a snag period, with rule-battery.scm's fake Trace
    (if (and b:rules? (tell *trace* 'within-snag-period?))
        (let ((progress-achieved (tell *trace* 'progress-since-last-snag)))
          (stochastic-if* (% progress-achieved)
            (tell *trace* 'undo-snag-condition))))
    (update-slipnet-activations)
    (update-temperature)
    (if b:rules? (add-bottom-up-codelets) (b:add-bottom-up-codelets))
    (add-top-down-codelets)
    (tell *coderack* 'post-deferred-codelets)))

(define b:step
  (lambda ()
    (let* ((codelet (tell *coderack* 'choose-codelet))
           (before (b:structures)))
      (set! b:events '())
      (tell codelet 'run)
      (b:set-global! '*codelet-count* (+ 1 *codelet-count*))
      (let* ((after (b:structures))
             (built (b:difference after before))
             (broken (b:difference before after))
             (rng (random-seed)))
        (list *codelet-count*
              (tell codelet 'get-codelet-type-name)
              (tell codelet 'get-relative-urgency)
              (tell codelet 'get-time-stamp)
              rng
              (list 'built built)
              (list 'broken broken)
              (list 'events (reverse b:events))
              (list 'proposed (b:proposed-counts)))))))

;; the trace of the first k codelets of a problem under a seed, as a list
;; of entries
(define b:run-codelets
  (lambda (strings seed k)
    (b:init-problem strings seed)
    (set! b:answered #f)
    (tell *coderack* 'initialize)
    (b:clamp-initial-slipnodes)
    (b:post-initial-codelets)
    (let loop ((trace (list (list 'start strings seed (random-seed)
                                  (b:map-objects b:dump-ws-object)))))
      (if (or (= *codelet-count* k) b:answered)
          (reverse (cons (list 'end (b:structures) (b:map-objects b:dump-ws-object)
                               (b:activations) *temperature* (random-seed))
                         trace))
          (let* ((entry (b:step)))
            (if (= *codelet-count* b:unclamp-time)
                (for-each (lambda (node) (tell node 'unfreeze))
                  *initially-clamped-slipnodes*))
            (if (tell *coderack* 'empty?)
                (begin (b:post-initial-codelets) (b:clamp-initial-slipnodes)))
            (if (= 0 (modulo *codelet-count* %update-cycle-length%))
                (begin
                  (set! b:events '())
                  (b:update-everything)
                  (let* ((cycle (list 'update *codelet-count* *temperature*
                                      (b:activations)
                                      (tell *coderack* 'get-num-of-codelets)
                                      (random-seed)
                                      (reverse b:events)
                                      ;; the objects every 4th update
                                      (if (= 0 (modulo *codelet-count*
                                                       (* 4 %update-cycle-length%)))
                                          (b:map-objects b:dump-ws-object)
                                          '()))))
                    (loop (cons cycle (cons entry trace)))))
                (loop (cons entry trace))))))))

(define b:map-objects
  (lambda (f)
    (let loop ((l (tell *workspace* 'get-objects)) (acc '()))
      (if (null? l)
          (reverse acc)
          (let* ((v (f (car l)))) (loop (cdr l) (cons v acc)))))))

