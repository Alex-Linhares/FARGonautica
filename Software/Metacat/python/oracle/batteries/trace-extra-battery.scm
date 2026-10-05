;;; trace-extra-battery.scm -- themes.ss and trace.ss cases that the golden
;;; traces do not reach (loop0002 item 10).
;;;
;;; Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Captured like tests/diff/*-battery.scm by python/oracle/capture.py:
;;; chez_scheme/oracle/diff-eval.ss evaluates tests/diff/helpers.scm, then this
;;; file, with the original loaded.  The Python side is the end of
;;; python/tests/test_golden.py.  Same rules as the frozen batteries: no two
;;; side-effecting subexpressions in one call or one `let'.  It loads
;;; tests/diff/workspace-dump.scm (unedited) for b:init-problem, and uses the
;;; real Themespace and Temporal Trace, with null windows that record the
;;; events the Trace adds.
;;;
;;; On the 109 goldens every active theme is fully active (so a theme's spread
;;; to the Slipnet draws against 0 or 1), no concept-mapping importance falls
;;; between 60 and 65, and no group's strength is 99: these tests reach the
;;; values in between.

(load "tests/diff/workspace-dump.scm")

(b:set-global! '*EEG* (lambda msg 'done))
(b:set-global! '%workspace-graphics% #f)
(b:set-global! '*themespace-window* (lambda msg 'done))
(define x:events '())
(b:set-global! '*trace-window*
  (lambda (self . msg)
    (when (eq? (car msg) 'add-event)
      (set! x:events (cons (tell (cadr msg) 'print-name) x:events)))
    'done))

(define x:new-events
  (lambda (thunk)
    (set! x:events '())
    (thunk)
    (reverse x:events)))

(define x:start
  (lambda (strings)
    (b:init-problem strings 1)
    (tell *themespace* 'initialize)
    (tell *trace* 'initialize)))

;; a bridge as concept-mapping-importance and make-concept-mapping-event see it
(define x:bridge
  (lambda (spanning? object1 object2)
    (lambda msg
      (record-case (cdr msg)
        (get-theme-type () 'vertical-bridge)
        (get-bridge-type () 'vertical)
        (spanning-bridge? () spanning?)
        (get-object1 () object1)
        (get-object2 () object2)
        (else (error 'x:bridge "unexpected message" msg))))))

(define x:cm-cases
  (list (list plato-letter-category plato-a plato-z)
        (list plato-letter-category plato-a plato-b)
        (list plato-letter-category plato-c plato-x)
        (list plato-string-position-category plato-leftmost plato-rightmost)
        (list plato-string-position-category plato-leftmost plato-middle)
        (list plato-string-position-category plato-single plato-whole)
        (list plato-alphabetic-position-category plato-alphabetic-first plato-alphabetic-last)
        (list plato-object-category plato-letter plato-group)
        (list plato-length plato-one plato-two)
        (list plato-length plato-two plato-five)
        (list plato-direction-category plato-left plato-right)
        (list plato-bond-category plato-successor plato-predecessor)
        (list plato-group-category plato-succgrp plato-predgrp)
        (list plato-group-category plato-samegrp plato-succgrp)
        (list plato-letter-category plato-a plato-a)))

;; concept-mapping-importance and monitor-new-concept-mappings (its 65
;; threshold) for vertical concept mappings between the a of abc and the x of
;; xyz, on spanning and non-spanning bridges, with no active theme
(test concept-mapping-importance
  (begin
    (x:start (list 'abc 'abd 'xyz))
    (let* ((a (tell *initial-string* 'get-letter 0))
           (x (tell *target-string* 'get-letter 0)))
      (map (lambda (spanning?)
             (map (lambda (c)
                    (let* ((cm (make-concept-mapping a (car c) (cadr c) x (car c) (caddr c)))
                           (bridge (x:bridge spanning? a x))
                           (importance (concept-mapping-importance cm bridge))
                           (events (x:new-events
                                     (lambda ()
                                       (monitor-new-concept-mappings (list cm) bridge)))))
                      (list (tell cm 'print-name) importance events)))
                  x:cm-cases))
           (list #f #t)))))

;; monitor-new-groups (its 100 threshold) on the group a-b of abc, its strength
;; and spanning seen as given
(define x:group-as-fixed
  (lambda (group strength spans?)
    (lambda msg
      (case (cadr msg)
        ((get-strength) strength)
        ((spans-whole-string?) spans?)
        (else (apply group (cons group (cdr msg))))))))

(test group-importance
  (begin
    (x:start (list 'abc 'abd 'xyz))
    (let* ((a (tell *initial-string* 'get-letter 0))
           (b (tell *initial-string* 'get-letter 1))
           (group (make-group *initial-string* plato-succgrp plato-letter-category plato-right
                              a b (list a b) '())))
      (map (lambda (c)
             (let* ((g (x:group-as-fixed group (car c) (cadr c)))
                    (flipped? (caddr c))
                    (importance (group-importance g flipped?))
                    (events (x:new-events (lambda () (monitor-new-groups g flipped?)))))
               (list c importance events)))
           (list (list 97 #f #f) (list 98 #f #f) (list 99 #f #f) (list 100 #f #f)
                 (list 50 #t #f) (list 50 #f #t))))))

;; a theme (made directly; the Themespace makes its themes as bridges need
;; them) and its spread to the Slipnet at activations between 0 and 100 (both
;; draws, the dimension's and the relation's): the activation buffers that
;; reach the two nodes, and the generator state, under each seed
(test theme-spread-to-slipnet
  (begin
    (x:start (list 'abc 'abd 'xyz))
    (let* ((theme (make-bridge-theme 'vertical-bridge
                    plato-letter-category plato-successor)))
      (map (lambda (activation)
             (map (lambda (seed)
                    (tell plato-letter-category 'set-activation 0)
                    (tell plato-successor 'set-activation 0)
                    (tell theme 'set-activation activation)
                    (b:seeded seed
                      (lambda ()
                        (tell theme 'spread-activation-to-slipnet)
                        (tell plato-letter-category 'flush-activation-buffer)
                        (tell plato-successor 'flush-activation-buffer)
                        (list (tell plato-letter-category 'get-activation)
                              (tell plato-successor 'get-activation)))))
                  (list 1 2 3 7 42 1000)))
           (list 30 50 80 -60 100)))))
