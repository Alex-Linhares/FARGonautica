;;; bridge-battery.scm -- the codelet-level differential checks of item 08:
;;; bridges.ss and breakers.ss.
;;;
;;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Read form by form, after tests/diff/helpers.scm, by two runners:
;;;   chez_scheme/oracle/diff-eval.ss       (Chez, with the whole original loaded)
;;;   racket/tests/bridge-diff-test.rkt     (Racket: compat + utilities + engine)
;;; The harness of item 07 (codelet-harness.scm) with b:bridges? on: run.ss's
;;; initial bond and bridge scouts, every bottom-up codelet type but those of
;;; rules, answers and self-watching (so bridge scouts, description scouts and
;;; the breaker too), and all of *top-down-slipnodes*.  For every problem in
;;; tests/problems.txt and each of its seeds, the trace of the first
;;; b:codelet-cap codelets, one line per trace entry: bridges built and broken
;;; (with their concept mappings, flipped groups and strengths), proposed
;;; bridges, descriptions per object, the concept-mapping monitor and the
;;; Themespace boosts, besides everything item 07 records.  Then longer runs.

(load "tests/diff/codelet-harness.scm")

(b:enable-bridges! #t)

(define b:codelet-cap 1000)

(define b:trace-lines
  (lambda (strings seed k)
    (let* ((trace (b:run-codelets strings seed k)))
      (apply string-append
        (map (lambda (entry)
               (string-append (b:compact strings) " " (number->string seed) " "
                              (b:compact entry) "\n"))
             trace)))))

;; the trace lines of problem i, all its seeds, as one string
(define b:problem-trace
  (lambda (i)
    (let* ((p (list-ref b:problems i)))
      (let loop ((seeds (cadr p)) (acc '()))
        (if (null? seeds)
            (apply string-append (reverse acc))
            (let* ((lines (b:trace-lines (car p) (car seeds) b:codelet-cap)))
              (loop (cdr seeds) (cons lines acc))))))))

(test bridges-problem-count (length b:problems))
(test bridges-00 (b:problem-trace 0))
(test bridges-01 (b:problem-trace 1))
(test bridges-02 (b:problem-trace 2))
(test bridges-03 (b:problem-trace 3))
(test bridges-04 (b:problem-trace 4))
(test bridges-05 (b:problem-trace 5))
(test bridges-06 (b:problem-trace 6))
(test bridges-07 (b:problem-trace 7))
(test bridges-08 (b:problem-trace 8))
(test bridges-09 (b:problem-trace 9))
(test bridges-10 (b:problem-trace 10))
(test bridges-11 (b:problem-trace 11))
(test bridges-12 (b:problem-trace 12))
(test bridges-13 (b:problem-trace 13))
(test bridges-14 (b:problem-trace 14))
(test bridges-15 (b:problem-trace 15))
(test bridges-16 (b:problem-trace 16))
(test bridges-17 (b:problem-trace 17))
(test bridges-18 (b:problem-trace 18))
(test bridges-19 (b:problem-trace 19))
(test bridges-20 (b:problem-trace 20))
(test bridges-21 (b:problem-trace 21))
(test bridges-22 (b:problem-trace 22))
(test bridges-23 (b:problem-trace 23))
(test bridges-24 (b:problem-trace 24))
(test bridges-25 (b:problem-trace 25))
(test bridges-26 (b:problem-trace 26))
(test bridges-27 (b:problem-trace 27))
(test bridges-28 (b:problem-trace 28))
(test bridges-29 (b:problem-trace 29))
(test bridges-30 (b:problem-trace 30))
(test bridges-31 (b:problem-trace 31))
(test bridges-32 (b:problem-trace 32))
(test bridges-33 (b:problem-trace 33))
(test bridges-34 (b:problem-trace 34))
(test bridges-35 (b:problem-trace 35))

;;;---------------------------------------------------------------------------
;;; Bridges examined directly.  After k codelets of the harness, a fresh
;;; (unproposed) bridge is made for every pair of objects of the initial and
;;; modified strings (horizontal) and of the initial and target strings
;;; (vertical), from their relevant descriptions as the bridge scouts make
;;; them; for each, its concept mappings and the values that the runs mostly
;;; see clipped to 100 or never reach: internal and external strength,
;;; internal coherence, incompatible bridges (through
;;; group-incompatible-bridges and direction-incompatible-bridges), the
;;; incompatible bond, reverse-direction-orientation?.  Then, for every pair
;;; of groups with direction descriptions, direction-incompatible-bridges
;;; under both direction mappings, and every pair of built bridges of each
;;; type against each other.  Nothing here draws.

(define b:pairs
  (lambda (l1 l2)
    (apply append (map (lambda (x) (map (lambda (y) (cons x y)) l2)) l1))))

(define b:fresh-bridge-data
  (lambda (orientation o1 o2)
    (let* ((cms (all-possible-bridge-CMs orientation
                  o1 (tell o1 'get-relevant-descriptions)
                  o2 (tell o2 'get-relevant-descriptions)))
           (b (if (eq? orientation 'horizontal)
                  (make-horizontal-bridge o1 o2 cms)
                  (make-vertical-bridge o1 o2 cms)))
           (bond (if (eq? orientation 'horizontal) (tell b 'get-incompatible-bond) #f)))
      (list (b:obj-id o1) (b:obj-id o2)
            (b:cm-names cms)
            (map (lambda (cm) (tell cm 'get-strength)) cms)
            (tell b 'internally-coherent?)
            (tell b 'calculate-internal-strength)
            (tell b 'calculate-external-strength)
            (map b:bridge-data (tell b 'get-incompatible-bridges))
            (if bond (b:bond-data bond) #f)
            (reverse-direction-orientation? cms)
            (letter-category-mappable-objects? o1 o2)
            (singleton-letter-factor o1 o2)))))

(define b:direction-cm-data
  (lambda (orientation g1 g2)
    (let* ((d1 (tell g1 'get-descriptor-for plato-direction-category))
           (d2 (tell g2 'get-descriptor-for plato-direction-category)))
      (if (and d1 d2)
          (map (lambda (cm)
                 (list (b:obj-id g1) (b:obj-id g2) (tell cm 'print-name)
                       (map b:bridge-data
                            (direction-incompatible-bridges orientation g1 g2 cm))))
               (list (make-concept-mapping g1 plato-direction-category d1
                                           g2 plato-direction-category d2)
                     (make-concept-mapping g1 plato-direction-category d1
                                           g2 plato-direction-category
                                           (if (eq? d2 plato-left) plato-right plato-left))))
          '()))))

(define b:bridge-pair-data
  (lambda (b1 b2)
    (list (b:bridge-data b1) (b:bridge-data b2)
          (if (eq? (tell b1 'get-orientation) 'horizontal)
              (list (incompatible-horizontal-bridges? b1 b2)
                    (supporting-horizontal-bridges? b1 b2)
                    (incompatible-horizontal-CM-lists?
                      (tell b1 'get-all-concept-mappings) (tell b2 'get-all-concept-mappings)))
              (list (incompatible-vertical-bridges? b1 b2)
                    (supporting-vertical-bridges? b1 b2)
                    (incompatible-vertical-CM-lists?
                      (tell b1 'get-all-concept-mappings) (tell b2 'get-all-concept-mappings))))
          (enclosing-bridge? b1 b2))))

(define b:bridge-matrix
  (lambda (strings seed k)
    (b:run-codelets strings seed k)
    (let* ((initial (tell *initial-string* 'get-objects))
           (modified (tell *modified-string* 'get-objects))
           (target (tell *target-string* 'get-objects))
           (groups (lambda (l) (filter group? l))))
      (list
        (map (lambda (p) (b:fresh-bridge-data 'horizontal (car p) (cdr p)))
             (b:pairs initial modified))
        (map (lambda (p) (b:fresh-bridge-data 'vertical (car p) (cdr p)))
             (b:pairs initial target))
        (map (lambda (p) (b:direction-cm-data 'horizontal (car p) (cdr p)))
             (b:pairs (groups initial) (groups modified)))
        (map (lambda (p) (b:direction-cm-data 'vertical (car p) (cdr p)))
             (b:pairs (groups initial) (groups target)))
        (map (lambda (p) (b:bridge-pair-data (car p) (cdr p)))
             (b:pairs (tell *workspace* 'get-bridges 'top)
                      (tell *workspace* 'get-bridges 'top)))
        (map (lambda (p) (b:bridge-pair-data (car p) (cdr p)))
             (b:pairs (tell *workspace* 'get-bridges 'vertical)
                      (tell *workspace* 'get-bridges 'vertical)))))))

(test bridge-matrix-abc-abd-xyz (b:bridge-matrix '(abc abd xyz) 1 1500))
(test bridge-matrix-abc-abd-mrrjjj (b:bridge-matrix '(abc abd mrrjjj) 2 1500))
(test bridge-matrix-abc-abd-kji (b:bridge-matrix '(abc abd kji) 1 1500))
(test bridge-matrix-abc-abd-kji-3 (b:bridge-matrix '(abc abd kji) 3 1500))
(test bridge-matrix-aabc-aabd-ijkk (b:bridge-matrix '(aabc aabd ijkk) 2 1500))
(test bridge-matrix-abc-aabbcc-kkjjii (b:bridge-matrix '(abc aabbcc kkjjii) 1 1500))
(test bridge-matrix-eqe-qeq-abbbc (b:bridge-matrix '(eqe qeq abbbc) 3 1500))
(test bridge-matrix-xqc-xqd-mrrjjj (b:bridge-matrix '(xqc xqd mrrjjj mrrjjjj) 2 1500))
(test bridge-matrix-rst-rsu-xyz (b:bridge-matrix '(rst rsu xyz uyz) 1 1500))
