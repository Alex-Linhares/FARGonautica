;;; codelet-extra-battery.scm -- bonds.ss and groups.ss cases that
;;; tests/diff/codelet-battery.scm does not pin (loop0002 item 07).
;;;
;;; Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Captured like tests/diff/*-battery.scm by python/oracle/capture.py:
;;; chez_scheme/oracle/diff-eval.ss evaluates tests/diff/helpers.scm, then this
;;; file, with the original loaded.  The Python side is the end of
;;; python/tests/test_codelets.py.  Same rules as the frozen batteries: no two
;;; side-effecting subexpressions in one call or one `let'.  It loads
;;; tests/diff/codelet-harness.scm (unedited), with its settings and fakes.
;;;
;;;   - local-densities-*: the local density and support of every built bond
;;;     and group after a harness run, drawn three times under a seed (both
;;;     choose neighbours at random); 100k/n is rounded, not floored;
;;;   - group-builder-flips-*: group-builder on a proposed group whose bonds
;;;     are the flipped versions of built ones, so that it breaks and builds
;;;     several bonds in map's order (the string's bond list shows the order).

(load "tests/diff/codelet-harness.scm")

(define x:map-in-order
  (lambda (f l)
    (let loop ((l l) (acc '()))
      (if (null? l)
          (reverse acc)
          (let* ((v (f (car l)))) (loop (cdr l) (cons v acc)))))))

(define x:density-entry
  (lambda (s)
    (let* ((density (tell s 'get-local-density))
           (support (tell s 'get-local-support)))
      (list density support))))

(define x:densities
  (lambda (strings seed k)
    (b:run-codelets strings seed k)
    (random-seed 77)
    (let* ((structures
             (apply append
               (x:map-in-order
                 (lambda (s) (append (tell s 'get-bonds) (tell s 'get-groups)))
                 (b:strings)))))
      (let loop ((i 0) (acc '()))
        (if (= i 3)
            (list (length structures) (reverse acc) (random-seed))
            (let* ((values (x:map-in-order x:density-entry structures)))
              (loop (+ i 1) (cons values acc))))))))

(test local-densities-iijjkk (x:densities '(abc abd iijjkk) 2 600))
(test local-densities-mrrjjj (x:densities '(abc abd mrrjjj) 1 600))
(test local-densities-mrrkkk (x:densities '(xqc xqd mrrjjj mrrkkk) 1 800))
(test local-densities-abbbc (x:densities '(eqe qeq abbbc) 3 600))
(test local-densities-kkjjii (x:densities '(abc aabbcc kkjjii) 1 800))
(test local-densities-abcdxyzz (x:densities '(abc abd abcdxyzz) 1 800))
(test local-densities-aabbbcd (x:densities '(abc abd aabbbcd) 1 800))

;; target ijkl with the rightward successor bonds built; a leftward
;; predecessor group over the same letters, with the flipped bonds
(define x:group-builder-flips
  (lambda (seed)
    (b:init-problem '(abc abd ijkl) seed)
    (tell *coderack* 'initialize)
    (let* ((s *target-string*)
           (letters (tell s 'get-letters))
           (ls (x:map-in-order (lambda (i) (list-ref letters i)) '(0 1 2 3))))
      (x:map-in-order
        (lambda (i)
          (let* ((l1 (list-ref ls i))
                 (l2 (list-ref ls (+ i 1)))
                 (b (make-bond l1 l2 plato-successor plato-letter-category
                      (tell l1 'get-letter-category) (tell l2 'get-letter-category))))
            (build-bond b)))
        '(0 1 2))
      (let* ((flipped
               (x:map-in-order
                 (lambda (i)
                   (let* ((l1 (list-ref ls (+ i 1)))
                          (l2 (list-ref ls i)))
                     (make-bond l1 l2 plato-predecessor plato-letter-category
                       (tell l1 'get-letter-category) (tell l2 'get-letter-category))))
                 '(0 1 2)))
             (group (make-group s plato-predgrp plato-letter-category plato-left
                      (car ls) (list-ref ls 3) ls flipped))
             (codelet (tell group-builder 'make-codelet 50 group)))
        (set! b:events '())
        (tell codelet 'run)
        (list (map b:bond-data (tell s 'get-bonds))
              (map b:group-data (tell s 'get-groups))
              (reverse b:events)
              (random-seed))))))

(test group-builder-flips-1 (x:group-builder-flips 1))
(test group-builder-flips-2 (x:group-builder-flips 2))
(test group-builder-flips-3 (x:group-builder-flips 3))
(test group-builder-flips-4 (x:group-builder-flips 4))
(test group-builder-flips-5 (x:group-builder-flips 5))
(test group-builder-flips-6 (x:group-builder-flips 6))
