;;; codelet-battery.scm -- the codelet-level differential checks of item 07:
;;; bonds.ss, groups.ss and concept-mappings.ss.
;;;
;;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Read form by form, after tests/diff/helpers.scm, by two runners:
;;;   chez_scheme/oracle/diff-eval.ss       (Chez, with the whole original loaded)
;;;   racket/tests/codelet-diff-test.rkt    (Racket: compat + utilities + engine)
;;; For every problem in tests/problems.txt and each of its seeds, the trace
;;; of the first b:codelet-cap codelets of a run with only the bond and group
;;; codelet types enabled (codelet-harness.scm), one line per trace entry.
;;; codelet-diff-test.rkt compares the lines one by one and reports the first
;;; entry (problem, seed, codelet) where the port leaves the oracle.

(load "tests/diff/codelet-harness.scm")

(define b:codelet-cap 400)

;; the trace lines of problem i, all its seeds, as one string
(define b:problem-trace
  (lambda (i)
    (let* ((p (list-ref b:problems i)))
      (let loop ((seeds (cadr p)) (acc '()))
        (if (null? seeds)
            (apply string-append (reverse acc))
            (let* ((trace (b:run-codelets (car p) (car seeds) b:codelet-cap))
                   (lines (map (lambda (entry)
                                 (string-append
                                   (b:compact (car p)) " " (number->string (car seeds)) " "
                                   (b:compact entry) "\n"))
                               trace)))
              (loop (cdr seeds) (append (reverse lines) acc))))))))

(test codelets-problem-count (length b:problems))
(test codelets-00 (b:problem-trace 0))
(test codelets-01 (b:problem-trace 1))
(test codelets-02 (b:problem-trace 2))
(test codelets-03 (b:problem-trace 3))
(test codelets-04 (b:problem-trace 4))
(test codelets-05 (b:problem-trace 5))
(test codelets-06 (b:problem-trace 6))
(test codelets-07 (b:problem-trace 7))
(test codelets-08 (b:problem-trace 8))
(test codelets-09 (b:problem-trace 9))
(test codelets-10 (b:problem-trace 10))
(test codelets-11 (b:problem-trace 11))
(test codelets-12 (b:problem-trace 12))
(test codelets-13 (b:problem-trace 13))
(test codelets-14 (b:problem-trace 14))
(test codelets-15 (b:problem-trace 15))
(test codelets-16 (b:problem-trace 16))
(test codelets-17 (b:problem-trace 17))
(test codelets-18 (b:problem-trace 18))
(test codelets-19 (b:problem-trace 19))
(test codelets-20 (b:problem-trace 20))
(test codelets-21 (b:problem-trace 21))
(test codelets-22 (b:problem-trace 22))
(test codelets-23 (b:problem-trace 23))
(test codelets-24 (b:problem-trace 24))
(test codelets-25 (b:problem-trace 25))
(test codelets-26 (b:problem-trace 26))
(test codelets-27 (b:problem-trace 27))
(test codelets-28 (b:problem-trace 28))
(test codelets-29 (b:problem-trace 29))
(test codelets-30 (b:problem-trace 30))
(test codelets-31 (b:problem-trace 31))
(test codelets-32 (b:problem-trace 32))
(test codelets-33 (b:problem-trace 33))
(test codelets-34 (b:problem-trace 34))
(test codelets-35 (b:problem-trace 35))

;; Longer runs, where group-builder also consolidates sameness groups (the
;; path that calls group-graphics ungated, seen as Workspace window messages)
(define b:long-trace
  (lambda (strings seed k)
    (let* ((trace (b:run-codelets strings seed k))
           (lines (map (lambda (entry)
                         (string-append (b:compact strings) " " (number->string seed) " "
                                        (b:compact entry) "\n"))
                       trace)))
      (apply string-append lines))))

(test codelets-long-aaabaaa-1 (b:long-trace '(eqe qeq abbba aaabaaa) 1 2000))
(test codelets-long-aaabaaa-2 (b:long-trace '(eqe qeq abbba aaabaaa) 2 2000))
(test codelets-long-aaabaaa-4 (b:long-trace '(eqe qeq abbba aaabaaa) 4 2000))
(test codelets-long-bbbxbbb-1 (b:long-trace '(eqe qeq bxxxb bbbxbbb) 1 2000))
(test codelets-long-mrrjjjj-2 (b:long-trace '(xqc xqd mrrjjj mrrjjjj) 2 2000))
(test codelets-long-iijjkk-3 (b:long-trace '(abc abd iijjkk) 3 2000))
(test codelets-long-xxixx-3 (b:long-trace '(eeqee qeeq xxixx) 3 2000))

;;;---------------------------------------------------------------------------
;;; Concept mappings (concept-mappings.ss).  With bridges disabled, codelets
;;; make concept mappings only inside the bridge-incompatibility tests of
;;; bonds and groups, which need a bridge; so the mappings are checked
;;; directly here: every message, for every pair of instances of every
;;; slipnet category, and for the real descriptions of workspace objects
;;; after a run of the harness (letters and groups).

(define b:cm-data
  (lambda (cm)
    (let* ((link (tell cm 'get-slipnet-link))
           (sym (tell cm 'symmetric-mapping)))
      (list (tell cm 'print-name)
            (tell cm 'english-name)
            (tell cm 'long-name)
            (b:capture (lambda () (tell cm 'print)))
            (b:nm (tell cm 'get-CM-type))
            (if link
                (list (b:nm (tell link 'get-from-node)) (b:nm (tell link 'get-to-node))
                      (tell link 'get-degree-of-assoc))
                #f)
            (tell cm 'CM-type? plato-letter-category)
            (tell cm 'CM-type? plato-string-position-category)
            (tell cm 'bond-concept-mapping?)
            (tell cm 'reversible-CM-type?)
            (b:nm (tell cm 'get-descriptor1))
            (b:nm (tell cm 'get-descriptor2))
            (b:nm (tell cm 'get-label))
            (tell cm 'slippage?)
            (tell cm 'identity?)
            (tell cm 'opposite-mapping?)
            (tell cm 'identity/opposite-mapping?)
            (tell cm 'relevant?)
            (tell cm 'distinguishing?)
            (tell cm 'relevant-distinguishing?)
            (tell cm 'distinguishing-identity/opposite?)
            (tell cm 'get-degree-of-assoc)
            (tell cm 'get-conceptual-depth)
            (tell cm 'get-strength)
            (tell cm 'get-slippability)
            (b:deep-nm (tell cm 'get-concept-pattern))
            (tell sym 'print-name)
            (eq? sym cm)
            (tell cm 'symmetric? sym)
            (tell sym 'symmetric? cm)
            (CMs-equal? cm sym)
            (tell cm 'previously-relevant?)
            (tell cm 'object-type)))))

;; the categories: every node with instances
(define b:categories
  (filter (lambda (n) (tell n 'category?)) *slipnet-nodes*))

;; all mappings between instances of category c, between two letters
(define b:category-cms
  (lambda (c object1 object2)
    (let* ((instances (tell c 'get-instance-nodes)))
      (apply append
        (map (lambda (d1)
               (map (lambda (d2)
                      (make-concept-mapping object1 c d1 object2 c d2))
                    instances))
             instances)))))

(define b:category-cm-test
  (lambda (c)
    (b:init-problem '(abc abd mrrjjj) 1)
    (let* ((cms (b:category-cms c (tell *initial-string* 'get-letter 0)
                                (tell *target-string* 'get-letter 0))))
      (list (b:nm c) (length cms) (map b:cm-data cms)
            (map (lambda (cm) (tell cm 'print-name)) (remove-duplicate-CMs cms))))))

(test cm-category-count (map b:nm b:categories))
(test cm-categories-00 (b:category-cm-test (list-ref b:categories 0)))
(test cm-categories-01 (b:category-cm-test (list-ref b:categories 1)))
(test cm-categories-02 (b:category-cm-test (list-ref b:categories 2)))
(test cm-categories-03 (b:category-cm-test (list-ref b:categories 3)))
(test cm-categories-04 (b:category-cm-test (list-ref b:categories 4)))
(test cm-categories-05 (b:category-cm-test (list-ref b:categories 5)))
(test cm-categories-06 (b:category-cm-test (list-ref b:categories 6)))
(test cm-categories-07 (b:category-cm-test (list-ref b:categories 7)))
(test cm-categories-08 (b:category-cm-test (list-ref b:categories 8)))

;; mappings between the descriptions of objects of the initial and target
;; strings, after k codelets of the harness, with all relevant (fully
;; active) description types; then the activations after every mapping's
;; activate-descriptions and activate-label, once the buffers are flushed,
;; with the monitor calls
(define b:workspace-cm-test
  (lambda (strings seed k)
    (b:run-codelets strings seed k)
    (let* ((objects1 (tell *initial-string* 'get-objects))
           (objects2 (tell *target-string* 'get-objects))
           (cms
             (apply append
               (map (lambda (o1)
                      (apply append
                        (map (lambda (o2)
                               (apply append
                                 (map (lambda (d1)
                                        (map (lambda (d2)
                                               (make-concept-mapping
                                                 o1 (tell d1 'get-description-type)
                                                 (tell d1 'get-descriptor)
                                                 o2 (tell d2 'get-description-type)
                                                 (tell d2 'get-descriptor)))
                                             (filter
                                               (lambda (d2)
                                                 (eq? (tell d2 'get-description-type)
                                                      (tell d1 'get-description-type)))
                                               (tell o2 'get-descriptions))))
                                      (tell o1 'get-descriptions))))
                             objects2)))
                    objects1)))
           (data (map b:cm-data cms)))
      (set! b:events '())
      (for-each (lambda (cm) (tell cm 'activate-descriptions) (tell cm 'activate-label)) cms)
      (for-each (lambda (n) (tell n 'flush-activation-buffer)) *slipnet-nodes*)
      (list (length cms) (map b:obj-id objects1) (map b:obj-id objects2) data
            (map (lambda (cm) (tell cm 'print-name)) (remove-duplicate-CMs cms))
            (b:activations) (reverse b:events)))))

(define b:map-in-order
  (lambda (f l)
    (let loop ((l l) (acc '()))
      (if (null? l)
          (reverse acc)
          (let* ((v (f (car l)))) (loop (cdr l) (cons v acc)))))))

(test cm-workspace-abc-abd-xyz (b:workspace-cm-test '(abc abd xyz) 1 600))
(test cm-workspace-abc-abd-mrrjjj (b:workspace-cm-test '(abc abd mrrjjj) 2 600))
(test cm-workspace-eqe-qeq-abbbc (b:workspace-cm-test '(eqe qeq abbbc) 3 600))
(test cm-workspace-abc-abd-kji (b:workspace-cm-test '(abc abd kji) 1 600))
(test cm-workspace-aabc-aabd-ijkk (b:workspace-cm-test '(aabc aabd ijkk) 2 600))
