;;; solution-checker.lisp -- write python/fixtures/solution_checker.json from
;;; the oracle (loop0002 item 13).
;;;
;;; Run from anywhere: sbcl --non-interactive --load tests/oracle/solution-checker.lisp
;;; (or python/scripts/regen_fixtures.sh).
;;;
;;; Each case calls (check-solution text problem) (src/solution-checker.lisp)
;;; and records its three values.  The texts are:
;;;   - the decompositions of tests/solution-tests.lisp, and the mutations
;;;     that test makes of them;
;;;   - one text per way of being invalid (every FAIL call in check-solution),
;;;     and texts for the arithmetic forms (+, both -, x, both /, x by 0);
;;;   - case and spacing variants (STRING-EQUAL, EQUALP hash keys, "Done :"
;;;     found case-sensitively, words between paragraphs skipped);
;;;   - one text per value token, to pin the integer reader check-solution
;;;     uses (READ-FROM-STRING with *read-eval* nil, then INTEGERP).
;;; Also (solution-tokens text) on a few texts.
;;;
;;; The decompositions of real runs are checked elsewhere: tests/oracle/lib/
;;; full-run.lisp records check-solution's verdict on every full run, and
;;; python/tests/test_full_runs.py compares it with the Python's.
;;;
;;; JSON: {"cases": [{"label", "text", "problem", "valid", "reason",
;;;                   "expression"}, ...],
;;;        "tokens": [{"text", "tokens"}, ...]}

(load (merge-pathnames "common.lisp" *load-truename*))

(in-package :numbo)

(cl:defun sc-substitute (s old new)
  "S with the first OLD replaced by NEW (tests/solution-tests.lisp)."
  (let ((p (search old s)))
    (cl:if p (concatenate 'string (subseq s 0 p) new (subseq s (+ p (length old)))) s)))

(cl:defun sc-lines (&rest lines)
  (format nil "~{~a~%~}" lines))

(cl:defun sc-para (op a va b vb r)
  "One paragraph as decompose prints it (with the space after \"applied\")."
  (format nil "Operation ~a has been applied ~%to ~a ( ~a) and to ~a ( ~a)~%to get ~a~%"
          op a va b vb r))

(cl:defparameter *sc-seed-18* (sc-lines
  "Node PLUS28-3-V30 created"
  "Done : Operation PLUS28-3-V30 has been applied "
  "to CYTO-BLOCK28-V29 ( 28) and to CYTO-BRICK1 ( 3)"
  "to get CYTO-TARGET"
  "Operation TIMES2-14-V29 has been applied "
  "to CYTO-BRICK5 ( 14) and to CYTO-BLOCK2-V28 ( 2)"
  "to get CYTO-BLOCK28-V29"
  "Operation PLUS2-3-V28 has been applied "
  "to CYTO-BRICK4 ( 3) and to CYTO-BRICK2 ( 5)"
  "to get CYTO-BLOCK2-V28"))

(cl:defparameter *sc-seed-93* (sc-lines
  "Done : Operation PLUS24-7-V1 has been applied"
  "to CYTO-TARGET-7-V1 ( 7) and to CYTO-BRICK3 ( 24)"
  "to get CYTO-TARGET"
  "Operation PLUS7-2-V4 has been applied"
  "to CYTO-BLOCK9-V3 ( 9) and to CYTO-BLOCK2-V6 ( 2)"
  "to get CYTO-TARGET-7-V1"
  "Operation PLUS2-3-V6 has been applied"
  "to CYTO-BRICK4 ( 3) and to CYTO-BRICK2 ( 5)"
  "to get CYTO-BLOCK2-V6"))

(cl:defparameter *sc-p3* '(31 3 5 24 3 14))

(cl:defun sc-one (a va b vb &optional (op "PLUS2-3-V1") (r "CYTO-TARGET"))
  (concatenate 'string "Done : " (sc-para op a va b vb r)))

(cl:defun sc-chain (n)
  "A chain of N operations, B_i = B_i-1 + brick i, so the deepest brick is
expanded at depth N: depth 21 is \"cycle through\" (depth > 20)."
  (list (format nil "chain of ~a" n)
        (apply #'concatenate 'string "Done : "
               (cl:loop for i from n downto 1
                        collect (sc-para (format nil "PLUS~a-1-V~a" i i)
                                         (cl:if (= i 1) "CYTO-BRICK25" (format nil "CYTO-B~a" (1- i)))
                                         (cl:if (= i 1) 0 (1- i))
                                         (format nil "CYTO-BRICK~a" i) 1
                                         (cl:if (= i n) "CYTO-TARGET" (format nil "CYTO-B~a" i)))))
        (append (list n) (make-list 24 :initial-element 1) (list 0))))

(cl:defparameter *sc-token-values*
  ;; value tokens for brick 1 in "PLUS on brick 1 and brick 2 (3) gives 5"
  `("2" "+2" "-2" "2." "02" "4/2" "2/1" "1/2" "1/0" "2/-1" "2.0" "2e0" "2d0"
    "#x2" "#X2" "#b10" "#o2" "#3r2" "#x4/2" "#.2" "#c" "#\\2" "abc" "2a" "a2"
    "1+1" "|2|" "\\2" "2\\" "2;x" ";2" "2'x" "'2" "2," ",2" "2\"" "\"2\"" "`2"
    "." ".." "-" "+" ":2" "2:" "-0" "+0" "0002." "-2." "1_0" "#x1_0" "#xg" "#3r3" "#36rZ" "#37r1"
    ,(string (code-char #x0662)) ,(format nil "#x~a" (code-char #x0662))
    ,(format nil "~a/1" (code-char #x0662))
    "99999999999999999999999" "-99999999999999999999997"))

(cl:defparameter *sc-cases*
  (append
   ;; --- tests/solution-tests.lisp ---------------------------------------
   (list
    (list "seed 18" *sc-seed-18* *sc-p3*)
    (list "brick 1 used twice"
          (sc-substitute *sc-seed-18* "CYTO-BRICK4 ( 3)" "CYTO-BRICK1 ( 3)") *sc-p3*)
    (list "wrong brick value"
          (sc-substitute *sc-seed-18* "CYTO-BRICK2 ( 5)" "CYTO-BRICK3 ( 5)") *sc-p3*)
    (list "bad arithmetic"
          (sc-substitute (sc-substitute *sc-seed-18* "CYTO-BLOCK28-V29 ( 28)"
                                        "CYTO-BLOCK28-V29 ( 27)")
                         "PLUS28-3" "PLUS27-3")
          *sc-p3*)
    (list "root is not the target" *sc-seed-18* '(32 3 5 24 3 14))
    (list "no Done" "Node CYTO-TARGET created" *sc-p3*)
    (list "no operation" "Done : " *sc-p3*)
    (list "truncated" "Done : Operation PLUS2-3-V1 has been applied to" *sc-p3*)
    (list "seed 93 (kill-block gap)" *sc-seed-93* *sc-p3*)
    (list "decompx division" (sc-lines
      "Done : Operation TIMES4-5-V1 has been applied"
      "to CYTO-TARGET-5-V1 ( 5) and to CYTO-BRICK1 ( 4)"
      "to get CYTO-TARGET"
      "Operation TIMES2-5-V3 has been applied"
      "to CYTO-BRICK3 ( 2) and to CYTO-BLOCK10-V2 ( 10)"
      "to get CYTO-TARGET-5-V1"
      "Operation PLUS1-9-V2 has been applied"
      "to CYTO-BRICK4 ( 1) and to CYTO-BRICK5 ( 9)"
      "to get CYTO-BLOCK10-V2")
          '(20 4 6 2 1 9))
    (list "Obvious." (sc-lines
      "Obvious. Done : Operation PLUS3-3-V1 has been applied"
      "to CYTO-BRICK1 ( 3) and to CYTO-BRICK2 ( 3)"
      "to get CYTO-BLOCK6-V1")
          '(6 3 3 17 11 22)))
   ;; --- every FAIL ---------------------------------------------------------
   (list
    (list "truncated after op" "Done : Operation PLUS2-3-V1" *sc-p3*)
    (list "truncated before value" "Done : Operation P has been applied to CYTO-BRICK1" *sc-p3*)
    (list "truncated before result"
          "Done : Operation P has been applied to CYTO-BRICK1 ( 3) and to CYTO-BRICK2 ( 5) to get"
          *sc-p3*)
    (list "expected has" "Done : Operation PLUS2-3-V1 was applied to" *sc-p3*)
    (list "expected and, got empty"
          "Done : Operation P has been applied to CYTO-BRICK1 ( 3)" *sc-p3*)
    (list "expected get" (sc-substitute (sc-one "CYTO-BRICK1" 3 "CYTO-BRICK2" 5) "to get" "to got")
          '(8 3 5 1 1 1))
    (list "expected word, escaped"
          "Done : Operation P has been applied to CYTO-BRICK1 ( 3) a\"n\\d to" *sc-p3*)
    (list "derived twice"
          (concatenate 'string (sc-one "CYTO-BLOCK8-V1" 8 "CYTO-BRICK3" 24 "PLUS8-24-V2")
                       (sc-para "PLUS3-5-V1" "CYTO-BRICK1" 3 "CYTO-BRICK2" 5 "CYTO-BLOCK8-V1")
                       (sc-para "PLUS3-5-V9" "CYTO-BRICK4" 3 "CYTO-BRICK2" 5 "cyto-block8-v1"))
          '(32 3 5 24 3 14))
    (list "printed as both"
          (concatenate 'string (sc-one "CYTO-BLOCK8-V1" 8 "CYTO-BRICK3" 24 "PLUS8-24-V2")
                       (sc-para "PLUS3-5-V1" "CYTO-BRICK1" 3 "Cyto-Block8-V1" 9 "CYTO-X"))
          '(32 3 5 24 3 14))
    (list "printed twice, same value"
          (concatenate 'string (sc-one "CYTO-BLOCK8-V1" 8 "CYTO-BRICK3" 24 "PLUS8-24-V2")
                       (sc-para "PLUS3-5-V1" "CYTO-BRICK1" 3 "CYTO-BRICK2" 5 "CYTO-BLOCK8-V1")
                       (sc-para "PLUS3-5-V7" "CYTO-BRICK1" 3 "CYTO-BRICK2" 5 "CYTO-Y"))
          '(32 3 5 24 3 14))
    (list "not an integer" (sc-one "CYTO-BRICK1" "three" "CYTO-BRICK2" 5) *sc-p3*)
    (list "cycle"
          (concatenate 'string (sc-one "CYTO-A" 5 "CYTO-BRICK1" 0 "PLUS5-0-V1")
                       (sc-para "PLUS5-0-V2" "CYTO-TARGET" 5 "CYTO-BRICK2" 0 "CYTO-A"))
          '(5 0 0 1 2 3))
    (list "brick 6" (sc-one "CYTO-BRICK6" 3 "CYTO-BRICK2" 5) '(8 3 5 1 1 1))
    (list "brick 0" (sc-one "CYTO-BRICK0" 3 "CYTO-BRICK2" 5) '(8 3 5 1 1 1))
    (list "brick 4 of 3" (sc-one "CYTO-BRICK4" 3 "CYTO-BRICK2" 5) '(8 3 5 1))
    (list "brick value" (sc-one "CYTO-BRICK1" 4 "CYTO-BRICK2" 5) '(9 3 5 1 1 1))
    (list "brick used twice" (sc-one "CYTO-BRICK1" 3 "cyto-brick1" 3) '(6 3 5 1 1 1))
    (list "never derived" (sc-one "CYTO-BLOCK3-V1" 3 "CYTO-BRICK2" 5) '(8 3 5 1 1 1))
    (list "unknown operation" (sc-one "CYTO-BRICK1" 3 "CYTO-BRICK2" 5 "MINUS3-5-V1")
          '(8 3 5 1 1 1))
    (list "short op name" (sc-one "CYTO-BRICK1" 3 "CYTO-BRICK2" 5 "PLU") '(8 3 5 1 1 1))
    (list "cannot give" (sc-one "CYTO-BRICK1" 3 "CYTO-BRICK2" 5) '(9 3 5 1 1 1))
    (list "times cannot give" (sc-one "CYTO-BRICK1" 3 "CYTO-BRICK2" 5 "TIMES3-5-V1")
          '(16 3 5 1 1 1))
    (list "first failure is a's" (sc-one "CYTO-BRICK9" 3 "CYTO-BLOCK5-V1" 5) '(8 3 5 1 1 1)))
   ;; --- arithmetic forms ---------------------------------------------------
   (list
    (list "a + b" (sc-one "CYTO-BRICK1" 3 "CYTO-BRICK2" 5) '(8 3 5 1 1 1))
    (list "a - b" (sc-one "CYTO-BRICK2" 5 "CYTO-BRICK1" 3) '(2 3 5 1 1 1))
    (list "b - a" (sc-one "CYTO-BRICK1" 3 "CYTO-BRICK2" 5) '(2 3 5 1 1 1))
    (list "0 = a - b first" (sc-one "CYTO-BRICK1" 3 "CYTO-BRICK2" 3) '(0 3 3 1 1 1))
    (list "a + b, b = 0" (sc-one "CYTO-BRICK1" 3 "CYTO-BRICK2" 0) '(3 3 0 1 1 1))
    (list "a x b" (sc-one "CYTO-BRICK1" 3 "CYTO-BRICK2" 5 "TIMES3-5-V1") '(15 3 5 1 1 1))
    (list "a / b" (sc-one "CYTO-BRICK1" 6 "CYTO-BRICK2" 3 "TIMES2-3-V1") '(2 6 3 1 1 1))
    (list "b / a" (sc-one "CYTO-BRICK1" 3 "CYTO-BRICK2" 6 "TIMES2-3-V1") '(2 3 6 1 1 1))
    (list "a / b inexact" (sc-one "CYTO-BRICK1" 7 "CYTO-BRICK2" 3 "TIMES2-3-V1") '(2 7 3 1 1 1))
    (list "x 0" (sc-one "CYTO-BRICK1" 0 "CYTO-BRICK2" 3 "TIMES0-3-V1") '(0 0 3 1 1 1))
    (list "/ 0 skipped" (sc-one "CYTO-BRICK1" 3 "CYTO-BRICK2" 0 "TIMES3-0-V1") '(5 3 0 1 1 1))
    (list "0 / a" (sc-one "CYTO-BRICK1" 0 "CYTO-BRICK2" 3 "TIMES0-3-V1") '(5 0 3 1 1 1))
    (list "negative" (sc-one "CYTO-BRICK1" -3 "CYTO-BRICK2" 5) '(2 -3 5 1 1 1))
    (list "bignum" (sc-one "CYTO-BRICK1" 99999999999999999999 "CYTO-BRICK2" 1)
          '(100000000000000000000 99999999999999999999 1 1 1 1))
    (list "lower-case op" (sc-one "CYTO-BRICK1" 3 "CYTO-BRICK2" 5 "times3-5-v1")
          '(15 3 5 1 1 1))
    (list "root is a brick" (sc-one "CYTO-BRICK2" 5 "CYTO-BRICK3" 1 "PLUS5-1-V1" "CYTO-BRICK1")
          '(6 6 5 1 1 1))
    (list "nested parens"
          (concatenate 'string (sc-one "CYTO-BLOCK8-V1" 8 "CYTO-BLOCK4-V2" 4 "TIMES8-4-V3")
                       (sc-para "PLUS3-5-V1" "CYTO-BRICK1" 3 "CYTO-BRICK2" 5 "CYTO-BLOCK8-V1")
                       (sc-para "PLUS1-3-V2" "CYTO-BRICK4" 1 "CYTO-BRICK3" 3 "CYTO-BLOCK4-V2"))
          '(32 3 5 3 1 1))
    (sc-chain 20)
    (sc-chain 21))
   ;; --- case, spacing, noise -----------------------------------------------
   (list
    (list "lower-case words" (string-downcase (sc-one "CYTO-BRICK1" 3 "CYTO-BRICK2" 5))
          '(8 3 5 1 1 1))
    (list "mixed-case words"
          "Done : OPERATION PLUS3-5-V1 Has BEEN applied TO CYTO-BRICK1 ( 3) AND to CYTO-BRICK2 ( 5) To Get CYTO-TARGET"
          '(8 3 5 1 1 1))
    (list "done lower-case" (sc-substitute (sc-one "CYTO-BRICK1" 3 "CYTO-BRICK2" 5) "Done" "done")
          '(8 3 5 1 1 1))
    (list "Done : twice"
          (concatenate 'string "Done : junk " (sc-one "CYTO-BRICK1" 3 "CYTO-BRICK2" 5))
          '(8 3 5 1 1 1))
    (list "Done :Operation" (sc-substitute (sc-one "CYTO-BRICK1" 3 "CYTO-BRICK2" 5) "Done : " "Done :")
          '(8 3 5 1 1 1))
    (list "operation before Done"
          (concatenate 'string (sc-para "PLUS9-9-V9" "CYTO-BRICK1" 9 "CYTO-BRICK2" 9 "CYTO-Z")
                       (sc-one "CYTO-BRICK1" 3 "CYTO-BRICK2" 5))
          '(8 3 5 1 1 1))
    (list "noise between paragraphs"
          (concatenate 'string (sc-one "CYTO-BLOCK8-V1" 8 "CYTO-BRICK3" 24 "PLUS8-24-V2")
                       "Node CYTO-BLOCK9-V3 killed (whatever) to get" (string #\Tab)
                       (sc-para "PLUS3-5-V1" "CYTO-BRICK1" 3 "CYTO-BRICK2" 5 "CYTO-BLOCK8-V1"))
          '(32 3 5 24 3 14))
    (list "values without spaces"
          "Done : Operation PLUS3-5-V1 has been applied to CYTO-BRICK1 (3) and to CYTO-BRICK2 (5) to get CYTO-TARGET"
          '(8 3 5 1 1 1))
    (list "CRLF" (sc-substitute (sc-substitute (sc-one "CYTO-BRICK1" 3 "CYTO-BRICK2" 5)
                                               (string #\Newline) (coerce '(#\Return #\Newline) 'string))
                                (string #\Newline) (coerce '(#\Return #\Newline) 'string))
          '(8 3 5 1 1 1))
    (list "brick name, lower case" (sc-one "cyto-brick1" 3 "Cyto-Brick2" 5) '(8 3 5 1 1 1))
    (list "brick name with sign" (sc-one "CYTO-BRICK+1" 3 "CYTO-BRICK2" 5) '(8 3 5 1 1 1))
    (list "brick name 01" (sc-one "CYTO-BRICK01" 3 "CYTO-BRICK2" 5) '(8 3 5 1 1 1))
    (list "brick name, bare prefix" (sc-one "CYTO-BRICK" 3 "CYTO-BRICK2" 5) '(8 3 5 1 1 1))
    (list "brick name, Arabic-Indic digit"
          (sc-one (concatenate 'string "CYTO-BRICK" (string (code-char #x0661))) 3 "CYTO-BRICK2" 5)
          '(8 3 5 1 1 1)))
   ;; --- value tokens -------------------------------------------------------
   (cl:loop for tok in *sc-token-values*
            collect (list (format nil "token ~a" tok)
                          (sc-one "CYTO-BRICK1" tok "CYTO-BRICK2" 3)
                          '(5 2 3 1 1 1)))))

(cl:defparameter *sc-token-texts*
  (list *sc-seed-18* "" "   " "(a)(b) c" "a(b)c" (format nil "x~ay~az" #\Tab #\Return)
        "((( ))) Done : x"))

(cl:defun sc-json ()
  (with-output-to-string (s)
    (format s "{\"cases\": [")
    (cl:loop for (label text problem) in *sc-cases*
             for first = t then nil
             do (multiple-value-bind (valid reason expression) (check-solution text problem)
                  (format s "~:[,~;~]~% {\"label\": " first)
                  (oracle-write-json-string label s)
                  (format s ", \"text\": ")
                  (oracle-write-json-string text s)
                  (format s ", \"problem\": ")
                  (oracle-write-data problem s)
                  (format s ", \"valid\": ~:[false~;true~], \"reason\": " valid)
                  (cl:if reason (oracle-write-json-string reason s) (write-string "null" s))
                  (format s ", \"expression\": ")
                  (cl:if expression (oracle-write-json-string expression s) (write-string "null" s))
                  (format s "}")))
    (format s "],~% \"tokens\": [")
    (cl:loop for text in *sc-token-texts*
             for first = t then nil
             do (format s "~:[,~;~]~% {\"text\": " first)
                (oracle-write-json-string text s)
                (format s ", \"tokens\": [")
                (cl:loop for tok in (solution-tokens text)
                         for f2 = t then nil
                         do (format s "~:[, ~;~]" f2)
                            (oracle-write-json-string tok s))
                (format s "]}"))
    (format s "]}~%")))

(cl-user::write-fixture "solution_checker.json" (sc-json))
