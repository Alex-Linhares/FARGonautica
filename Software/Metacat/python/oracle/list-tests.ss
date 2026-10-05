;;; list-tests.ss -- print the names of the (test NAME EXPR) forms of a
;;; differential battery, one per line, in order, as Chez's reader sees them.
;;;
;;; Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;;   scheme --script python/oracle/list-tests.ss FILE
;;;
;;; The same top-level reading as chez_scheme/oracle/diff-eval.ss, without
;;; evaluating anything.  python/oracle/capture.py uses it to split Chez's
;;; output per test.

(call-with-input-file (cadr (command-line))
  (lambda (in)
    (let loop ()
      (let ((form (read in)))
        (unless (eof-object? form)
          (when (and (pair? form) (eq? (car form) 'test))
            (put-string (current-output-port) (format "~a\n" (cadr form))))
          (loop))))))
