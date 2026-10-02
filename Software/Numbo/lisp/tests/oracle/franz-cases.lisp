;;; franz-cases.lisp -- write ../python/fixtures/franz_cases.json from the oracle.
;;;
;;; Run from anywhere: sbcl --non-interactive --load tests/oracle/franz-cases.lisp
;;; (or ../python/scripts/regen_fixtures.sh).
;;;
;;; Each case applies one Franz built-in of src/franz-compat.lisp (as loaded
;;; in oracle mode, so SORTCAR is the copying one) to literal arguments and
;;; records the result, or the type of the error it signals.  The Python
;;; helpers in ../python/numbo/franz.py are checked against these values by
;;; ../python/tests/test_franz.py.
;;;
;;; JSON: {"cases": [{"op", "args", "result" | "error", ["package"]}, ...]}.
;;; "op" is the Lisp name, downcased.  "args" and "result" use the trace's
;;; Lisp-data encoding (src/oracle.lisp): a symbol is its name, a string is
;;; {"str": ...}, nil is null.  For a symbol result, "package" is its home
;;; package's name, or null for an uninterned symbol.
;;;
;;; This file is READ with SBCL's default float format (single), so every
;;; float literal below is written with d0.

(load (merge-pathnames "common.lisp" *load-truename*))

(in-package :numbo)

(cl:defun franz-case-brick-keyword (i)
  "codelets.lisp read-brick, line 1191."
  (intern (uconcat "brick" i) (find-package "keyword")))

(cl:defparameter *franz-cases*
  (append
   ;; quotient and / : truncating on integers, float division otherwise
   (cl:loop for op in '(quotient /)
            append (cl:mapcar
                    (cl:lambda (args) (cons op args))
                    '((7 2) (-7 2) (7 -2) (-7 -2) (1 3) (-1 3) (2 -3) (0 5)
                      (31 10) (-31 10) (98 2) (20 300) (20 7) (-20 7)
                      (100 5 2) (60 4 3) (-60 7 2) (9) (-9) ()
                      (7.0d0 2) (7 2.0d0) (-7 2.0d0) (-7.0d0 -2) (1.0d0 4.0d0)
                      (20 7.0d0) (300 60.0d0) (1 3.0d0) (2.0d0 3 4)
                      (1180591620717411303424 3) (-1180591620717411303424 7)
                      (1 0) (1.0d0 0) (0 0))))
   ;; *quo: truncated integer quotient
   (cl:mapcar (cl:lambda (args) (cons '*quo args))
              '((456 10) (456 100) (-7 2) (7 -2) (-7 -2) (-456 10) (0 3)
                (9 10) (-9 10) (1 0)))
   ;; mod = rem: the sign of the dividend
   (cl:mapcar (cl:lambda (args) (cons 'mod args))
              '((17 5) (-17 5) (17 -5) (-17 -5) (20 10) (0 7) (-20 10) (4 5)
                (-4 5) (1180591620717411303425 10) (-1180591620717411303425 10)
                (7.5d0 2) (-7.5d0 2) (7 0)))
   ;; *mod: balanced residue, every i in [-60, 60] for the divisors round uses
   (cl:loop for div in '(5 10 50 7 2 1 -10)
            append (cl:loop for i from -60 to 60 collect (list '*mod i div)))
   (list (list '*mod 125 50) (list '*mod 130 50) (list '*mod 175 50)
         (list '*mod 7 0))
   ;; fix: round down
   (cl:mapcar (cl:lambda (args) (cons 'fix args))
              '((3.7d0) (3) (-3.7d0) (-3) (2.5d0) (-2.5d0) (0.0d0) (-0.0d0)
                (0.999d0) (-0.001d0) (10000000000.5d0) (1180591620717411303424)))
   ;; max (with (max) = 0) and min: ties keep the first argument
   (cl:mapcar (cl:lambda (args) (cons 'max args))
              '(() (3 7 2) (2 2.5d0) (2 2.0d0) (2.0d0 2) (-1 -5) (3 2.5d0) (5)
                (0.5d0 0.25d0) (-0.0d0 0.0d0) (0.0d0 -0.0d0) (0 -0.0d0)))
   (cl:mapcar (cl:lambda (args) (cons 'min args))
              '((3 1 2) (2 1.5d0) (2 2.0d0) (2.0d0 2) (-1 -5) (7)
                (-0.0d0 0.0d0) (0.0d0 -0.0d0)))
   ;; nequal = (not (equal x y))
   (cl:mapcar (cl:lambda (args) (cons 'nequal args))
              '(("2b" "2b") ("2b" "1b") ("free" "FREE") ((1 2) (1 2)) ((1 2) (1 3))
                ((1 2) (1 2 3)) (1 1.0d0) (1 1) (1.0d0 1.0d0) (0.0d0 -0.0d0)
                (foo foo) (foo bar) (foo "FOO") (free "free") (nil nil) (nil (nil))
                (2 "2") (1180591620717411303424 1180591620717411303424)
                ((a "b" (3)) (a "b" (3))) ((a "b" (3)) (a "b" (3.0d0)))))
   ;; alphalessp: print names (the case rule of franz-compat.lisp)
   (cl:mapcar (cl:lambda (args) (cons 'alphalessp args))
              '((abc abd) ("b" "a") (zeta "zz") ("1t" "2b") ("2b" "1t") ("3dt" "4bl")
                ("2b" "2b") (|MixedCase| "Mixed") (|MixedCase| "mixedcase")
                ("abc" "abcd") ("abcd" "abc") (node-10 node-9) (12 "2")
                (abc "ABC") ("ABC" abc) (abc "abc") ("" "a") ("a" "")))
   ;; sortcar: nil predicate = alphalessp; stable; copying in oracle mode
   (list
    (list 'sortcar '(("2b" x) ("1b" y) ("3" z)) nil)
    (list 'sortcar '((cyto 1) (block 2)) nil)
    (list 'sortcar '(("2b" a) ("1t" b) ("2b" c) ("1t" d) ("3dt" e) ("4bl" f) ("1t" g)) nil)
    (list 'sortcar '(("4bl" node-1) ("3dt" node-2)) nil)
    (list 'sortcar '(("1t" only)) nil)
    (list 'sortcar nil nil)
    (list 'sortcar '((3 c) (1 a) (2 b)) '<)
    (list 'sortcar '((3 c) (1 a) (3 d) (2 b) (1 e)) '<)
    (list 'sortcar '((3 c) (1 a) (3 d) (2 b) (1 e)) '>)
    (list 'sortcar '((2.5d0 x) (2 y) (-1 z)) '<))
   ;; name builders: interned upper-case print names, as the oracle prints them
   (cl:mapcar (cl:lambda (args) (cons 'concat args))
              '((node- 31) (node- 1) (node- 150) (cyto-block 12 -v 3) (times 3 - 5 -v 1)
                (plus 3 - 4 -v 10) (cyto-target- -4 -v 2) (cyto-brick 6) (brick 2)
                ("brick" 2) (gr 7) (|MixedCase| 1) ("ABC") ("abc") ("aBc" 1)
                (node- 2.5d0) (node- 1.0d7) (node- 1234567.5d0) (node- 0.001d0)
                (node- 0.0001d0) (node- -0.0d0) (node- 1.0d20) (node- 100.0d0)
                (node- 1.5d-10) (node- 0.1d0) (node- 123456.789d0) (node- -2.5d0)
                (node- 9999999.0d0) (node- 9999999.5d0) (node- 0.00099d0)
                (node- 1.2345678901234567d-5) (node- 1.7976931348623157d308)
                (node- 2.2250738585072014d-308) (node- 0.30000000000000004d0)
                (node- 1180591620717411303424) (a b c) (a) ()))
   (cl:mapcar (cl:lambda (args) (cons 'uconcat args))
              '(("brick" 1) (a) (node- 3) (|MixedCase| 2)))
   (cl:mapcar (cl:lambda (args) (cons 'get-pname args))
              '((gr12) (gr7) (|MixedCase|) ("str") (node-31) (12) (|ABC|) (|abc|)))
   (cl:mapcar (cl:lambda (args) (cons 'string-length args))
              '(("WINDOW") ("") (abc) (|MixedCase|) (12)))
   (cl:mapcar (cl:lambda (args) (cons 'franz-case-brick-keyword args))
              '((1) (2) (6)))
   ;; memq (eq) and member (equal): the tail, or nil
   (list (list 'memq 'b '(a b c)) (list 'memq 'd '(a b c)) (list 'memq 3 '(1 2 3 4))
         (list 'member 3 '(1 2 3 4)) (list 'member 3 '(1 2 3.0d0)) (list 'member 3.0d0 '(1 3 3.0d0))
         (list 'member '(1) '((0) (1) (2))) (list 'member "b" '("a" "b" "c"))
         (list 'member 'x nil))))

(cl:defun franz-case-json-string (s)
  "S as a plain JSON string (not Lisp data)."
  (with-output-to-string (out) (oracle-write-json-string s out)))

(cl:defun franz-case-json (case)
  (destructuring-bind (op &rest args) case
    (let* ((outcome (handler-case (list :result (apply op args))
                      (error (c) (list :error (type-of c)))))
           (fields (list (format nil "\"op\":~a"
                                 (franz-case-json-string
                                  (string-downcase
                                   (cl:if (eq op 'franz-case-brick-keyword)
                                          "brick-keyword"
                                          (symbol-name op)))))
                         (format nil "\"args\":~a" (oracle-json-string args)))))
      (cl:if (eq (first outcome) :error)
             (setq fields (append fields
                                  (list (format nil "\"error\":~a"
                                                (franz-case-json-string
                                                 (symbol-name (second outcome)))))))
             (let ((r (second outcome)))
               (setq fields (append fields
                                    (list (format nil "\"result\":~a" (oracle-json-string r)))))
               (when (and r (symbolp r) (not (eq r t)))
                 (setq fields
                       (append fields
                               (list (format nil "\"package\":~a"
                                             (cl:if (symbol-package r)
                                                    (franz-case-json-string
                                                     (package-name (symbol-package r)))
                                                    "null"))))))))
      (format nil "{~{~a~^,~}}" fields))))

(cl-user::write-fixture
 "franz_cases.json"
 (format nil "{\"cases\":[~%~{~a~^,~%~}~%]}~%"
         (cl:mapcar #'franz-case-json *franz-cases*)))
