;;; diff-eval.ss -- evaluate a differential battery against the original.
;;;
;;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;;   scheme --script chez_scheme/oracle/diff-eval.ss FILE ...
;;;
;;; Loads chez_scheme/original/ unmodified through prelude.ss, then reads
;;; each FILE in turn (tests/diff/helpers.scm, then a battery) form by form: (test NAME EXPR) prints "NAME => <canonical value>"
;;; (or "NAME => ERROR"), any other form is evaluated.  The Racket side of
;;; the comparison is racket/tests/utilities-diff-test.rkt, which evaluates
;;; the same battery against racket/compat.rkt and racket/utilities.rkt;
;;; racket/tests/coderack-diff-test.rkt does the same for the engine.
;;; The format of a battery is described in tests/diff/utilities-battery.scm.

(define $oracle-directory
  (let ((script (car (command-line))))
    (let loop ((i (- (string-length script) 1)))
      (cond
        ((< i 0) (string-append (current-directory) "/chez_scheme/oracle"))
        ((char=? (string-ref script i) #\/) (substring script 0 i))
        (else (loop (- i 1)))))))
(load (string-append $oracle-directory "/prelude.ss"))
(load-metacat)

;; The original's printf/newline write to the port syntactic-sugar.ss
;; captured at load time, the prelude's forwarding port, which writes to
;; $stdout; capture by redirecting $stdout.
(define b:capture
  (lambda (thunk)
    (let-values (((port get) (open-string-output-port)))
      (let ((saved $stdout))
        (set! $stdout port)
        (thunk)
        (set! $stdout saved)
        (get)))))

;; The battery's way to set a global of the original (the Racket runner
;; uses the engine's set-global!).
(define b:set-global! set-top-level-value!)

(define $out (current-output-port))

(define run-battery
  (lambda (path)
    (call-with-input-file path
      (lambda (in)
        (let loop ()
          (let ((form (read in)))
            (unless (eof-object? form)
              (if (and (pair? form) (eq? (car form) 'test))
                  (let ((name (cadr form)) (expr (caddr form)))
                    (let ((text (call/cc
                                  (lambda (k)
                                    (with-exception-handler
                                      (lambda (c) (k "ERROR"))
                                      (lambda ()
                                        (let ((v (eval expr (interaction-environment))))
                                          ((eval 'b:canon (interaction-environment)) v))))))))
                      (put-string $out (format "~a => ~a\n" name text))))
                  (eval form (interaction-environment)))
              (loop))))))))

(for-each run-battery (cdr (command-line)))
(flush-output-port $out)
