;;; utilities-extra-battery.scm -- utilities.ss and syntactic-sugar.ss cases that
;;; tests/diff/utilities-battery.scm does not reach (loop0002 item 03).
;;;
;;; Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Captured like tests/diff/*-battery.scm by python/oracle/capture.py:
;;; chez_scheme/oracle/diff-eval.ss evaluates tests/diff/helpers.scm, then this
;;; file, with the original loaded.  The Python side is the end of
;;; python/tests/test_utilities.py.

;; Ties: the probability equals the draw.  prob? draws (random 1.0) and keeps
;; (> p draw); stochastic-if* keeps (< draw p).  The draw is repeated by
;; re-seeding, so p is exactly the next value.
(define u:next-draw
  (lambda (seed)
    (random-seed seed)
    (random 1.0)))

(test prob?-ties
  (map (lambda (seed)
         (let* ((r (u:next-draw seed)))
           (random-seed seed)
           (let* ((at (prob? r)))
             (random-seed seed)
             (let* ((above (prob? (+ r 1e-15))))
               (list at above)))))
       b:seeds))

(test stochastic-if*-ties
  (map (lambda (seed)
         (let* ((r (u:next-draw seed)))
           (random-seed seed)
           (let* ((at (stochastic-if* r 'taken)))
             (random-seed seed)
             (let* ((above (stochastic-if* (+ r 1e-15) 'taken)))
               (list at above)))))
       b:seeds))

;; select-extreme takes the first element whose value is eqv? to the extreme
(test select-extreme-ties
  (list (select-extreme max abs (list -3 3 1)) (select-extreme min abs (list 2 -1 1))
        (select-extreme max (lambda (x) x) (list 2 2.0)) (select-extreme max (lambda (x) x) (list 2.0 2))
        (select-extreme min (lambda (x) (* 0.5 x)) (list 4 2 2.0))))

(test misc
  (list (stochastic-pick '() '()) (weighted-average (list 1 2) (list 0.5 0.5)) (% 1.5)
        (sum (list 1/2 0.5)) (average (list 1/3 2/3)) (round-to-10ths 0.25) (round-to-10ths 0.35)
        (100* 0.005) (100* 1/200) (log10 1/10)))
