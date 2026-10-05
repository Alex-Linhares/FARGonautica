;;; rule-extra-battery.scm -- rules.ss cases that tests/diff/rule-battery.scm
;;; does not pin (loop0002 item 09).
;;;
;;; Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Captured like tests/diff/*-battery.scm by python/oracle/capture.py:
;;; chez_scheme/oracle/diff-eval.ss evaluates tests/diff/helpers.scm, then this
;;; file, with the original loaded.  The Python side is the end of
;;; python/tests/test_rules.py.  Same rules as the frozen batteries: no two
;;; side-effecting subexpressions in one call or one `let'.
;;;
;;;   - transcribe-*: transcribe-to-english on hand-made rule clauses over the
;;;     strings of abc ccbbaa ijk.  A self GroupCtgy change without a BondFacet
;;;     change reaches get-change-phrase's (3rd BondFacet-change) with
;;;     BondFacet-change #f: Chez's "caddr: incorrect list structure #f", the
;;;     crash of run.ss abc ccbbaa ijk --seed 3 (anomalies: "`caddr` of `#f`
;;;     in `transcribe-to-english`").  diff-eval records it as ERROR.  The other
;;;     cases pin the phrases and the line breaking around it.
;;;   - change-phrase-*: get-change-phrase called directly, likewise.
;;; rule-battery.scm's harness does not reach the crash on abc ccbbaa ijk seed
;;; 3 (no answer and no error in 2500 codelets: it has no themes and no
;;; self-watching); the full run reaches it, and the golden-run items cover it.

(load "tests/diff/workspace-dump.scm")

(b:init-problem '(abc ccbbaa ijk) 3)

(define x:od-whole (list plato-group plato-string-position-category plato-whole))
(define x:od-rmost (list plato-letter plato-string-position-category plato-rightmost))
(define x:od-c (list plato-letter plato-letter-category plato-c))

(define x:transcribe
  (lambda (type clauses) (transcribe-to-english type clauses)))

(test transcribe-group-category-without-bond-facet
  (x:transcribe 'top
    (list (list 'intrinsic (list x:od-whole)
                (list (list 'self plato-group-category plato-predgrp))))))
(test transcribe-group-category-with-bond-facet
  (x:transcribe 'top
    (list (list 'intrinsic (list x:od-whole)
                (list (list 'self plato-group-category plato-predgrp)
                      (list 'self plato-bond-facet plato-letter-category))))))
(test transcribe-group-category-with-length-facet
  (x:transcribe 'top
    (list (list 'intrinsic (list x:od-whole)
                (list (list 'self plato-group-category plato-succgrp)
                      (list 'self plato-bond-facet plato-length))))))
(test transcribe-group-category-subobjects
  (x:transcribe 'top
    (list (list 'intrinsic (list x:od-whole)
                (list (list 'subobjects plato-group-category plato-predgrp))))))
(test transcribe-no-clauses (x:transcribe 'bottom '()))
(test transcribe-verbatim
  (x:transcribe 'top (list (list 'verbatim (list plato-x plato-y plato-z plato-a)))))
(test transcribe-letter-changes
  (x:transcribe 'top
    (list (list 'intrinsic (list x:od-rmost)
                (list (list 'self plato-letter-category plato-successor)))
          (list 'intrinsic (list x:od-c)
                (list (list 'self plato-letter-category plato-d))))))
(test transcribe-direction-and-length
  (x:transcribe 'bottom
    (list (list 'intrinsic (list x:od-whole)
                (list (list 'subobjects plato-direction-category plato-right)
                      (list 'self plato-length plato-four)
                      (list 'subobjects plato-length plato-two))))))
(test transcribe-string-position
  (x:transcribe 'top
    (list (list 'intrinsic (list x:od-rmost)
                (list (list 'self plato-string-position-category plato-leftmost))))))
(test transcribe-alphabetic-position
  (x:transcribe 'top
    (list (list 'intrinsic (list x:od-rmost)
                (list (list 'self plato-alphabetic-position-category plato-alphabetic-first))))))
(test transcribe-long-lines
  (x:transcribe 'top
    (list (list 'intrinsic (list x:od-rmost)
                (list (list 'self plato-letter-category plato-predecessor)))
          (list 'intrinsic (list x:od-whole)
                (list (list 'subobjects plato-direction-category plato-left)))
          (list 'extrinsic (list x:od-rmost x:od-c)
                (list plato-letter-category plato-string-position-category)))))

(test change-phrase-group-category-crash
  ((get-change-phrase "the whole string" #f #f)
   (list 'self plato-group-category plato-predgrp)))
(test change-phrase-group-category-subobjects
  ((get-change-phrase "the whole string" #t #f)
   (list 'subobjects plato-group-category plato-predgrp)))

;;; Written after the code (item 09's mutation checks): a rule's quality values,
;;; on hand-made rules over the same strings.  compute-rule-intrinsic-quality
;;; ("temporary" in rules.ss) is only read through get-intrinsic-quality, which
;;; nothing in the model sends, so rule-battery.scm cannot see it.

(define x:quality-values
  (lambda (type clauses)
    (let* ((rule (make-rule type clauses)))
      (tell rule 'set-quality-values)
      (list (tell rule 'get-english-transcription)
            (tell rule 'get-uniformity) (tell rule 'get-abstractness)
            (tell rule 'get-succinctness) (tell rule 'get-intrinsic-quality)
            (tell rule 'get-quality)))))

(test quality-letter-changes
  (x:quality-values 'top
    (list (list 'intrinsic (list x:od-rmost)
                (list (list 'self plato-letter-category plato-successor)))
          (list 'intrinsic (list x:od-c)
                (list (list 'self plato-letter-category plato-d))))))
(test quality-mixed-changes
  (x:quality-values 'top
    (list (list 'intrinsic (list x:od-rmost)
                (list (list 'self plato-letter-category plato-successor)
                      (list 'self plato-length plato-two)))
          (list 'intrinsic (list x:od-whole)
                (list (list 'subobjects plato-direction-category plato-left)
                      (list 'subobjects plato-letter-category plato-c))))))
(test quality-extrinsic
  (x:quality-values 'top
    (list (list 'extrinsic (list x:od-rmost x:od-c)
                (list plato-letter-category plato-string-position-category))
          (list 'intrinsic (list x:od-whole)
                (list (list 'self plato-length plato-four))))))
(test quality-verbatim
  (x:quality-values 'top (list (list 'verbatim (list plato-x plato-y plato-z)))))
