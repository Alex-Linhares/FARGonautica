;; Item 02: the golden traces.  Regenerating every trace listed in
;; tests/problems.txt with chez_scheme/oracle/make-golden.ss must reproduce
;; the committed tests/golden/*.jsonl byte for byte, with no file missing
;; and none extra.  (make-golden.ss --check does the comparison; it writes
;; into a temporary directory, never into tests/golden/.)  Every golden
;; trace must also pass chez_scheme/oracle/validate-trace.py.

(define scheme-program
  (if (= 0 (system "command -v scheme > /dev/null 2>&1")) "scheme" "chezscheme"))

(define status
  (system (format "~a --script chez_scheme/oracle/make-golden.ss --check" scheme-program)))

(unless (= status 0)
  (printf "golden-check: FAILED (make-golden.ss --check exited ~a)~%" status)
  (exit 1))
(unless (= 0 (system "python3 chez_scheme/oracle/validate-trace.py tests/golden/*.jsonl"))
  (printf "golden-check: FAILED (invalid golden trace)~%")
  (exit 1))
(printf "golden-check: tests/golden/ reproduced byte for byte, every trace valid~%")
