;; Item 06: the initial workspace that tests/diff/workspace-dump.scm builds
;; (b:init-problem, a copy of the Workspace part of run.ss's init-mcat, which
;; the Racket port's differential battery relies on while run.ss is not
;; ported) is the one the original's own init-mcat builds, for every problem
;; and first seed in tests/problems.txt.  Here the real Themespace, EEG and
;; groups.ss's contains? are used, where the battery uses stand-ins.
;; Run from the repository root (tests/run-tests.sh does).
;;
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).

(load "chez_scheme/oracle/prelude.ss")
(load-metacat)
(install-headless-windows!)

(define b:set-global! set-top-level-value!)
(load "tests/diff/helpers.scm")
(load "tests/diff/workspace-dump.scm")

(define failures 0)

(for-each
  (lambda (p)
    (let* ((strings (car p))
           (seed (car (cadr p)))
           (answer (if (= (length strings) 4) (cadddr strings) #f)))
      (set! %justify-mode% (and answer #t))
      (init-mcat (car strings) (cadr strings) (caddr strings) answer seed)
      (let* ((real (b:canon (b:dump-workspace)))
             (real-state (random-seed)))
        (b:init-problem strings seed)
        (let ((copy (b:canon (b:dump-workspace))))
          (unless (string=? real copy)
            (set! failures (+ failures 1))
            (printf "differs: ~s seed ~a~%" strings seed))
          ;; init-mcat's only draws are its initial codelets' (none of
          ;; them in the workspace part); the copy makes none
          (unless (= (random-seed) seed)
            (set! failures (+ failures 1))
            (printf "b:init-problem drew random numbers: ~s~%" strings))))))
  b:problems)

(printf "workspace-init-check: ~a problems, ~a failures~%" (length b:problems) failures)
(exit (if (= failures 0) 0 1))
