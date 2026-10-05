;;; bridge-extra-battery.scm -- bridges.ss cases that tests/diff/bridge-battery.scm
;;; does not pin (loop0002 item 08).
;;;
;;; Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Captured like tests/diff/*-battery.scm by python/oracle/capture.py:
;;; chez_scheme/oracle/diff-eval.ss evaluates tests/diff/helpers.scm, then this
;;; file, with the original loaded.  The Python side is the end of
;;; python/tests/test_bridges.py.  Same rules as the frozen batteries: no two
;;; side-effecting subexpressions in one call or one `let'.  It loads
;;; tests/diff/codelet-harness.scm (unedited) with the bridges setting on, as
;;; bridge-battery.scm does.
;;;
;;;   - fresh-bridges-*: after a harness run, a fresh vertical bridge for every
;;;     pair of initial and target objects, with its relevant distinguishing
;;;     concept mappings, coherence and internal strength.  In abc abd glz
;;;     (seed 2, 1500 codelets) the a--z bridge is internally coherent with
;;;     an internal strength under 100, so the 2.5 coherence factor shows; no
;;;     case of bridge-battery.scm has one.  Nothing here draws.

(load "tests/diff/codelet-harness.scm")

(b:enable-bridges! #t)

(define x:map-in-order
  (lambda (f l)
    (let loop ((l l) (acc '()))
      (if (null? l)
          (reverse acc)
          (let* ((v (f (car l)))) (loop (cdr l) (cons v acc)))))))

(define x:fresh-vertical
  (lambda (o1 o2)
    (let* ((cms (all-possible-bridge-CMs 'vertical
                  o1 (tell o1 'get-relevant-descriptions)
                  o2 (tell o2 'get-relevant-descriptions)))
           (b (make-vertical-bridge o1 o2 cms))
           (rd (tell b 'get-relevant-distinguishing-CMs))
           (names (b:cm-names rd))
           (strengths (x:map-in-order (lambda (cm) (tell cm 'get-strength)) rd))
           (coherent (tell b 'internally-coherent?))
           (internal (tell b 'calculate-internal-strength)))
      (list (b:obj-id o1) (b:obj-id o2) names strengths coherent internal))))

(define x:fresh-bridges
  (lambda (strings seed k)
    (b:run-codelets strings seed k)
    (let* ((initial (tell *initial-string* 'get-objects))
           (target (tell *target-string* 'get-objects)))
      (apply append
        (x:map-in-order
          (lambda (o1) (x:map-in-order (lambda (o2) (x:fresh-vertical o1 o2)) target))
          initial)))))

(test fresh-bridges-abc-abd-glz-2 (x:fresh-bridges '(abc abd glz) 2 1500))
