;;; coderack-extra-battery.scm -- coderack.ss cases that
;;; tests/diff/coderack-battery.scm does not reach (loop0002 item 04).
;;;
;;; Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Captured like tests/diff/*-battery.scm by python/oracle/capture.py:
;;; chez_scheme/oracle/diff-eval.ss evaluates tests/diff/helpers.scm, then this
;;; file, with the original loaded.  The Python side is the end of
;;; python/tests/test_coderack.py.  Nothing here draws a window: the coderack
;;; display is off and no codelet runs.

(b:set-global! '%coderack-graphics% #f)
(b:set-global! '%codelet-count-graphics% #f)

(define x:bin-index
  (lambda (bin)
    (let loop ((l (tell *coderack* 'get-all-bins)) (i 0))
      (cond
        ((null? l) #f)
        ((eq? bin (car l)) i)
        (else (loop (cdr l) (+ i 1)))))))

(define x:urgencies
  (lambda (type)
    (map (lambda (c) (list (tell c 'get-relative-urgency) (x:bin-index (tell c 'get-coderack-bin))))
         (tell *coderack* 'get-codelets-of-type type))))

(define x:post-all
  (lambda (type urgencies)
    (for-each
      (lambda (u)
        (set! *codelet-count* (+ *codelet-count* 1))
        (tell *coderack* 'post (tell type 'make-codelet u)))
      urgencies)))

(define x:reset
  (lambda ()
    (set! *codelet-count* 0)
    (set! *temperature* 0)
    (tell *coderack* 'initialize)))

;; adjust-urgency clips with (min 100 (max 0 new-value)): Chez's min and max
;; are inexact if either argument is, so clipping a flonum gives 0.0 or 100.0,
;; and clipping an exact value past a flonum bound does too.
(test adjust-urgency-clipping
  (map (lambda (delta)
         (x:reset)
         (x:post-all rule-scout (b:copy (list 7.5 33.3 0 100 50 99.99 1/3 102/5 0.0 100.0)))
         (tell *coderack* 'adjust-urgencies rule-scout delta)
         (x:urgencies rule-scout))
       (b:copy (list -200 500 -200.0 500.0 0.5 -0.5 1/3 0 0.0))))

;; clamp compares urgencies with =, so 90 and 90.0 are the same clamp
(test clamp-exactness
  (begin
    (x:reset)
    (x:post-all bond-builder (b:copy (list 10 20.5 30)))
    (let* ((r1 (tell bond-builder 'clamp 90))
           (u1 (tell bond-builder 'get-clamped-urgency))
           (r2 (tell bond-builder 'clamp 90.0))
           (u2 (tell bond-builder 'get-clamped-urgency))
           (a2 (x:urgencies bond-builder))
           (r3 (tell bond-builder 'clamp 45.5))
           (u3 (tell bond-builder 'get-clamped-urgency))
           (a3 (x:urgencies bond-builder))
           (r4 (tell bond-builder 'unclamp))
           (a4 (x:urgencies bond-builder)))
      (list r1 u1 r2 u2 a2 r3 u3 a3 r4 a4))))
