;;; gui-battery.scm -- the parts of gui.ss and demos.ss that need no widget:
;;; the command-line parser, the speed slider's settings, the figure titles,
;;; the clamp-codelets menu's patterns, and the demo problems (loop0002 item 15).
;;;
;;; Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Captured like tests/diff/*-battery.scm by python/oracle/capture.py:
;;; chez_scheme/oracle/diff-eval.ss evaluates tests/diff/helpers.scm, then this
;;; file, with the original loaded (gui.ss and demos.ss included, on the
;;; prelude's SWL stubs).  The Python side is python/tests/test_gui.py.

(define x:inputs
  (list "" "abc abd xyz" "  abc   abd xyz  " "ABC Abd xYz" "abc abd xyz 7"
        "abc abd xyz xyd" "abc abd xyz xyd 1760747975" "abc -> abd; xyz -> ?"
        "abc,abd,xyz,1" "abc abd ijk 1" "abc 12x" "12x" "abc12" "007 abc"
        "1.5" "abc abd xyz 0" "abc abd xyz 4294967295" "abc abd xyz 4294967296"
        "abc abd" "a b c d e f" "abc abd xyz wyz 3 4" "abc abd 3 xyz"
        "\x3bb;\x3bc; \x3bd;" "caf\xe9; abd xyz" "abc\tabd\nxyz" "-12" "abc abd xyz -5"
        "\xbd; abc" "abc abd xyz \x661;\x662;"))

;; tokenize-string: symbols, numbers, or the symbol error
(test tokenize-string
  (map (lambda (s) (list s (tokenize-string s))) x:inputs))

;; what the Step/Go/Reset buttons decide: an empty line resumes, else a
;; valid token list starts a problem, else "Invalid input!"
(test command-line-decisions
  (map (lambda (s)
         (if (= 0 (string-length s))
           'resume
           (let ((tokens (tokenize-string s)))
             (if (valid-token-list? tokens) (list 'new tokens) 'invalid))))
       x:inputs))

(test char-noise
  (map (lambda (c) (list c (char-noise? c)))
       (string->list "aZ09 -;>?,.\t\x3bb;\xe9;\x661;\xbd;_")))

;; the speed slider: each value's flashes and pauses
(test speed-slider
  (map (lambda (v)
         (speed-slider-action #f v)
         (list v %num-of-flashes% %flash-pause% %snag-pause% %text-scroll-pause%))
       (let loop ((i 100) (l '())) (if (< i 0) l (loop (- i 1) (cons i l))))))

(test speed-constants
  (list %max-num-of-flashes% %max-flash-pause% %max-snag-pause% %text-scroll-pause%
        %codelet-highlight-pause% %initial-speed% %gui-slider-length%
        %gui-slider-thickness%))

(test figure-titles
  (list (figure 5 4 'top) (figure 5 4 'bottom) (figure 5 7) (figure 5 10)))

(test demos
  (list run1 run2 run3 run4 run5 run6 run7 run8
        abc-xyd abc-wyz abc-dyz rst-xyu rst-wyz rst-uyz
        abc-mrrkkk abc-mrrjjjj xqc-mrrkkk xqc-mrrjjjj
        eqe-baaab eqe-aaabaaa eqe-qeeeq eqe-aaabccc
        fig5.4-top fig5.4-bottom fig5.5-top fig5.5-bottom
        fig5.7 fig5.8 fig5.10 fig5.11
        misc1 misc2 misc3 misc4 misc5))

;; the patterns clamp-codelets-menu-item clamps (gui.ss's case on the type)
(define x:named
  (lambda (pattern)
    (cons (car pattern)
          (map (lambda (entry)
                 (cons (tell (car entry) 'get-codelet-type-name) (cdr entry)))
               (cdr pattern)))))

(test clamp-codelet-patterns
  (map x:named (list %top-down-codelet-pattern% %bottom-up-codelet-pattern%
        (against-background %very-low-urgency% %group-codelet-pattern%)
        (against-background %very-low-urgency% %bridge-codelet-pattern%)
        (against-background %very-low-urgency% %rule-codelet-pattern%))))
