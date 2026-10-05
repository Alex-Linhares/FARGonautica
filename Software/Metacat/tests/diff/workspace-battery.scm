;;; workspace-battery.scm -- differential checks of the engine's workspace.ss,
;;; workspace-objects.ss, workspace-structures.ss, workspace-strings.ss,
;;; workspace-structure-formulas.ss and formulas.ss (item 06) against the
;;; original under Chez Scheme 10.
;;;
;;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Read form by form, after tests/diff/helpers.scm, by two runners:
;;;   chez_scheme/oracle/diff-eval.ss       (Chez, with the whole original loaded)
;;;   racket/tests/workspace-diff-test.rkt  (Racket: compat + utilities + engine)
;;; The rules of utilities-battery.scm apply: no two side-effecting
;;; subexpressions in one call or one `let' (b:seq runs thunks in order);
;;; lists for `map' are computed, never literal `(list ...)'.
;;;
;;; The main tests: for every problem in tests/problems.txt, the initial
;;; workspace (strings, letters, descriptions, salience, importance and
;;; unhappiness values, workspace averages), built for each of its seeds,
;;; with the generator state afterwards.  workspace-dump.scm builds and dumps
;;; it; chez_scheme/oracle/tests/workspace-init-check.ss checks there that
;;; the dump equals the one after the original's own init-mcat.
;;;
;;; Parts not ported yet are replaced, in both runners, through b:set-global!:
;;; *themespace* (themes.ss: no active themes, as at the start of a run),
;;; *EEG* (eeg-graphics.ss) and contains? (groups.ss, its own definition).

(load "tests/diff/workspace-dump.scm")

;;;---------------------------------------------------------------------------
;;; Fakes and helpers

(define b:fake-themespace
  (lambda msg
    (record-case (cdr msg)
      (get-active-themes (types) '())
      (else (error 'fake-themespace "unexpected message" msg)))))

(define b:eeg-log '())
(define b:fake-eeg
  (lambda msg
    (set! b:eeg-log (cons (cadr msg) b:eeg-log))
    'done))

(b:set-global! '*themespace* b:fake-themespace)
(b:set-global! '*EEG* b:fake-eeg)
(b:set-global! 'contains?
  (lambda (object1 object2)
    (and (group? object1) (tell object1 'nested-member? object2))))
(b:set-global! '%workspace-graphics% #f)

;; run thunks in order, collecting their values
(define b:seq
  (lambda thunks
    (let loop ((ts thunks) (acc '()))
      (if (null? ts)
          (reverse acc)
          (let* ((v ((car ts)))) (loop (cdr ts) (cons v acc)))))))

;; map in list order, for effectful procedures
(define b:map-in-order
  (lambda (f l)
    (let loop ((l l) (acc '()))
      (if (null? l)
          (reverse acc)
          (let* ((v (f (car l)))) (loop (cdr l) (cons v acc)))))))

(define b:list-tail-from
  (lambda (l i) (if (or (null? l) (= i 0)) l (b:list-tail-from (cdr l) (- i 1)))))

(define b:same? (lambda (a b) (string=? (b:canon a) (b:canon b))))

(define b:string-name (lambda (s) (if s (tell s 'print-name) #f)))
(define b:ascii (lambda (o) (if o (tell o 'ascii-name) #f)))
(define b:asciis (lambda (l) (map b:ascii l)))

;; the initial workspace of problem i under each of its seeds: the dump for
;; the first seed, then for each seed the generator state after and whether
;; the dump is the same
(define b:problem-dump
  (lambda (i)
    (let* ((p (list-ref b:problems i))
           (strings (car p))
           (seeds (cadr p)))
      (b:init-problem strings (car seeds))
      (let* ((first (b:dump-workspace))
             (acts (b:activations))
             (eeg (reverse b:eeg-log))
             (per-seed
               (b:map-in-order
                 (lambda (seed)
                   (b:init-problem strings seed)
                   (let* ((d (b:dump-workspace))
                          (state (random-seed)))
                     (list seed state (b:same? d first))))
                 seeds)))
        (set! b:eeg-log '())
        (list strings first acts eeg per-seed)))))

(define b:problem-dumps-from
  (lambda (i)
    (if (>= i (length b:problems))
        '()
        (let* ((d (b:problem-dump i))) (cons d (b:problem-dumps-from (+ i 1)))))))

;;;---------------------------------------------------------------------------
;;; The initial workspace of every problem in tests/problems.txt

(test ws-problem-count (length b:problems))
(test ws-problem-00 (b:problem-dump 0))
(test ws-problem-01 (b:problem-dump 1))
(test ws-problem-02 (b:problem-dump 2))
(test ws-problem-03 (b:problem-dump 3))
(test ws-problem-04 (b:problem-dump 4))
(test ws-problem-05 (b:problem-dump 5))
(test ws-problem-06 (b:problem-dump 6))
(test ws-problem-07 (b:problem-dump 7))
(test ws-problem-08 (b:problem-dump 8))
(test ws-problem-09 (b:problem-dump 9))
(test ws-problem-10 (b:problem-dump 10))
(test ws-problem-11 (b:problem-dump 11))
(test ws-problem-12 (b:problem-dump 12))
(test ws-problem-13 (b:problem-dump 13))
(test ws-problem-14 (b:problem-dump 14))
(test ws-problem-15 (b:problem-dump 15))
(test ws-problem-16 (b:problem-dump 16))
(test ws-problem-17 (b:problem-dump 17))
(test ws-problem-18 (b:problem-dump 18))
(test ws-problem-19 (b:problem-dump 19))
(test ws-problem-20 (b:problem-dump 20))
(test ws-problem-21 (b:problem-dump 21))
(test ws-problem-22 (b:problem-dump 22))
(test ws-problem-23 (b:problem-dump 23))
(test ws-problem-24 (b:problem-dump 24))
(test ws-problem-25 (b:problem-dump 25))
(test ws-problem-26 (b:problem-dump 26))
(test ws-problem-27 (b:problem-dump 27))
(test ws-problem-28 (b:problem-dump 28))
(test ws-problem-29 (b:problem-dump 29))
(test ws-problem-30 (b:problem-dump 30))
(test ws-problem-31 (b:problem-dump 31))
(test ws-problem-32 (b:problem-dump 32))
(test ws-problem-33 (b:problem-dump 33))
(test ws-problem-34 (b:problem-dump 34))
(test ws-problem-35 (b:problem-dump 35))
;; problems added to problems.txt later
(test ws-problem-rest (b:problem-dumps-from 36))

;;;---------------------------------------------------------------------------
;;; Queries on the initial workspace of a problem (live values: activations,
;;; neighbours, relevance, support, mapping)

(define b:types
  (list plato-object-category plato-letter-category plato-string-position-category
        plato-alphabetic-position-category plato-length plato-group-category
        plato-direction-category plato-bond-category))

(define b:object-queries
  (lambda (obj)
    (list (b:ascii obj)
          (map b:nm (tell obj 'get-all-description-types))
          (map b:nm (tell obj 'get-all-descriptors))
          (map (lambda (d) (tell d 'relevant?)) (tell obj 'get-descriptions))
          (map (lambda (d) (tell d 'print-name)) (tell obj 'get-relevant-descriptions))
          (map (lambda (d) (tell d 'print-name)) (tell obj 'get-distinguishing-descriptions))
          (map (lambda (d) (tell d 'print-name))
               (tell obj 'get-relevant-distinguishing-descriptions))
          (map (lambda (d) (tell d 'print-name)) (tell obj 'get-descriptions-for-rule))
          (b:deep-nm (tell obj 'get-concept-pattern))
          (map (lambda (t) (b:nm (tell obj 'get-descriptor-for t))) b:types)
          (map (lambda (t) (tell obj 'description-type-present? t)) b:types)
          (tell obj 'all-description-types-present? b:types)
          (map (lambda (n) (tell obj 'descriptor-present? n)) *slipnet-nodes*)
          (map (lambda (n) (tell obj 'distinguishing-descriptor? n)) *slipnet-nodes*)
          (b:asciis (tell obj 'get-all-left-neighbors))
          (b:asciis (tell obj 'get-all-right-neighbors))
          (b:ascii (tell obj 'get-ungrouped-left-neighbor))
          (b:ascii (tell obj 'get-ungrouped-right-neighbor))
          (list (tell obj 'leftmost-in-string?) (tell obj 'middle-in-string?)
                (tell obj 'rightmost-in-string?) (tell obj 'spans-whole-string?)
                (tell obj 'string-spanning-group?) (tell obj 'get-letter-span)
                (tell obj 'get-nesting-level) (tell obj 'get-num-of-incident-bonds)
                (tell obj 'get-num-of-spanning-bridges)
                (tell obj 'mapped? 'vertical) (tell obj 'mapped? 'horizontal)
                (tell obj 'mapped? 'both))
          (list (unrelated? obj) (ungrouped? obj) (unmapped? obj))
          (list (tell obj 'get-letters) (tell obj 'nested-member? obj)
                (tell obj 'singleton-group?) (eq? (tell obj 'make-flipped-version) obj)
                (b:nm (tell obj 'get-platonic-length))
                (b:nm (tell obj 'get-initial-letter-category))
                (tell obj 'in-string? (tell obj 'get-string))))))

(define b:string-queries
  (lambda (s)
    (list (tell s 'print-name)
          (list (tell s 'top-string?) (tell s 'vertical-string?) (tell s 'bottom-string?)
                (tell s 'translated?) (tell s 'string-type? 'initial))
          (map (lambda (c) (tell s 'get-bond-category-relevance c))
               (list plato-sameness plato-successor plato-predecessor))
          (map (lambda (d) (tell s 'get-direction-relevance d))
               (list plato-left plato-right))
          (spanning-group-possible? s)
          (b:asciis (tell s 'get-constituent-objects))
          (b:asciis (tell s 'get-top-level-objects))
          (tell s 'singleton-group?)
          (tell s 'whole-group?)
          (tell s 'spanning-group-exists?)
          (tell s 'get-spanning-group)
          (list (tell s 'get-left-string-pos) (tell s 'get-right-string-pos)
                (tell s 'get-nesting-level) (b:nm (tell s 'get-bond-facet))
                (tell s 'get-bridge 'vertical))
          (map (lambda (t) (description-type-support t s)) b:types)
          (map (lambda (n) (descriptor-support n s)) *slipnet-nodes*)
          (b:ascii (lowest-level-object (tell s 'get-objects)))
          (b:ascii (highest-level-object (tell s 'get-objects)))
          (b:map-in-order
            (lambda (od) (b:asciis (tell s 'get-object-description-ref-objects od)))
            (list (list 'string plato-string-position-category plato-whole)
                  (list plato-letter plato-letter-category plato-a)
                  (list plato-letter plato-string-position-category plato-leftmost)
                  (list plato-letter plato-string-position-category plato-rightmost)
                  (list plato-letter plato-string-position-category plato-middle)
                  (list plato-group plato-group-category plato-samegrp))))))

(define b:threshold-distributions
  (list %very-low-translation-temperature-threshold-distribution%
        %low-translation-temperature-threshold-distribution%
        %medium-translation-temperature-threshold-distribution%
        %high-translation-temperature-threshold-distribution%
        %very-high-translation-temperature-threshold-distribution%))

(define b:index-of
  (lambda (x l)
    (let loop ((l l) (i 0))
      (cond
        ((null? l) #f)
        ((eq? x (car l)) i)
        (else (loop (cdr l) (+ i 1)))))))

(define b:workspace-queries
  (lambda ()
    (list (b:asciis (tell *workspace* 'get-objects))
          (b:asciis (tell *workspace* 'get-all-letters))
          (list (tell *workspace* 'get-bonds) (tell *workspace* 'get-groups)
                (tell *workspace* 'get-all-groups) (tell *workspace* 'get-all-bridges)
                (tell *workspace* 'get-all-rules) (tell *workspace* 'get-clamped-rules))
          (b:asciis (tell *workspace* 'get-possible-bridge-objects 'top))
          (b:asciis (tell *workspace* 'get-possible-bridge-objects 'vertical))
          (b:map-in-order
            (lambda (s) (list (b:string-name (tell *workspace* 'get-other-string s 'horizontal))
                              (b:string-name (tell *workspace* 'get-other-string s 'vertical))))
            (list *initial-string* *target-string*))
          (tell *workspace* 'get-activity)
          (tell *workspace* 'get-youngest-structures-average-age)
          (tell *workspace* 'get-max-inter-string-unhappiness)
          (tell *workspace* 'get-min-mapping-strength)
          (map (lambda (t) (tell *workspace* 'maximal-mapping? t)) (list 'top 'vertical))
          (map (lambda (t) (tell *workspace* 'spanning-bridge-exists? t)) (list 'top 'vertical))
          (map (lambda (t) (tell *workspace* 'get-all-slippages t)) (list 'top 'vertical))
          (tell *workspace* 'get-all-vertical-CMs)
          (tell *workspace* 'get-proposed-bridges 'top)
          (tell *workspace* 'get-proposed-vertical-bridges (tell *initial-string* 'get-letter 0))
          (tell *workspace* 'get-proposed-horizontal-bridges (tell *modified-string* 'get-letter 0))
          (tell *workspace* 'object-exists? (tell *target-string* 'get-letter 0))
          (tell *workspace* 'check-if-rules-possible)
          (tell *workspace* 'get-possible-rule-types)
          (map (lambda (t) (tell *workspace* 'rule-possible? t)) (list 'top 'bottom))
          (map (lambda (t) (tell *workspace* 'supported-rule-exists? t)) (list 'top 'bottom))
          (b:index-of (current-translation-temperature-threshold-distribution)
                      b:threshold-distributions)
          (begin (update-temperature) *temperature*))))

(define b:seeded-choices
  (lambda (seed temperature)
    (b:set-global! '*temperature* temperature)
    (b:seeded seed
      (lambda ()
        (let* ((objects (tell *workspace* 'get-objects)))
          (b:seq
            (lambda () (b:ascii (tell *workspace* 'choose-object 'get-intra-string-salience)))
            (lambda () (b:ascii (tell *workspace* 'choose-object
                                  'get-inter-string-salience 'vertical)))
            (lambda () (b:ascii (tell *workspace* 'choose-object 'get-relative-importance)))
            (lambda () (b:ascii (tell *initial-string* 'choose-object 'get-average-salience)))
            (lambda () (b:ascii (tell *target-string* 'choose-object-with-description-type
                                  plato-string-position-category 'get-relative-importance)))
            (lambda () (b:ascii (tell *target-string* 'choose-object-with-description-type
                                  plato-length 'get-relative-importance)))
            (lambda () (b:ascii (tell *modified-string* 'choose-leftmost-object)))
            (lambda () (b:ascii (tell *target-string* 'get-random-letter)))
            (lambda () (b:repeat 5 (lambda () (tell *target-string* 'get-num-of-bonds-to-scan))))
            (lambda () (tell *workspace* 'get-rough-num-of-unrelated-objects))
            (lambda () (tell *workspace* 'get-rough-num-of-ungrouped-objects))
            (lambda () (tell *workspace* 'get-rough-num-of-unmapped-objects))
            (lambda ()
              (b:map-in-order
                (lambda (obj)
                  (b:seq
                    (lambda () (b:ascii (tell obj 'choose-left-neighbor)))
                    (lambda () (b:ascii (tell obj 'choose-right-neighbor)))
                    (lambda () (b:ascii (tell obj 'choose-neighbor)))
                    (lambda ()
                      (let ((d (tell obj 'choose-relevant-description-by-activation)))
                        (if d (tell d 'print-name) #f)))
                    (lambda ()
                      (let ((d (tell obj 'choose-relevant-distinguishing-description-by-depth)))
                        (if d (tell d 'print-name) #f)))))
                objects))))))))

(define b:problem-queries
  (lambda (i)
    (let* ((p (list-ref b:problems i)))
      (b:init-problem (car p) (car (cadr p)))
      (let* ((objects (b:map-in-order b:object-queries (tell *workspace* 'get-objects)))
             (strings (b:map-in-order b:string-queries
                        (if %justify-mode%
                            *all-strings*
                            (list *initial-string* *modified-string* *target-string*))))
             (ws (b:workspace-queries))
             (choices (b:map-in-order
                        (lambda (st) (b:seeded-choices (car st) (cadr st)))
                        (list (list 1 100) (list 42 100) (list 7 35) (list 3141592653 0)))))
        (list (car p) objects strings ws choices)))))

;; every distinct problem shape: 3 and 4 strings, lengths 1 to 7
(test ws-queries-00 (b:problem-queries 0))   ; abc abd mrrjjj mrrjjjj
(test ws-queries-06 (b:problem-queries 6))   ; abc abd xyz
(test ws-queries-07 (b:problem-queries 7))   ; eqe qeq abbbc
(test ws-queries-14 (b:problem-queries 14))  ; eqe qeq abbba aaabaaa
(test ws-queries-15 (b:problem-queries 15))  ; eeqee qeeq xxixx
(test ws-queries-16 (b:problem-queries 16))  ; aabc aabd ijkk ijll
(test ws-queries-21 (b:problem-queries 21))  ; abc aabbcc kkjjii
(test ws-queries-22 (b:problem-queries 22))  ; a b z
(test ws-queries-25 (b:problem-queries 25))  ; eqe qeq bxxxb bbbxbbb
(test ws-queries-28 (b:problem-queries 28))  ; abc abd iijjkk

;;;---------------------------------------------------------------------------
;;; Workspace objects with fake bonds, groups and bridges: every branch of the
;;; unhappiness and salience formulas

(define b:fake-structure
  (lambda (type strength . props)
    (lambda msg
      (let ((m (cadr msg)))
        (cond
          ((eq? m 'object-type) type)
          ((eq? m 'get-strength) strength)
          ((eq? m 'update-strength) 'done)
          ((assq m props) => (lambda (p) (apply (cdr p) (cddr msg))))
          (else (error 'fake-structure "unexpected message" m)))))))

(define b:object-values
  (lambda (obj)
    (list (b:ascii obj)
          (tell obj 'get-intra-string-unhappiness)
          (tell obj 'get-inter-string-unhappiness 'horizontal)
          (tell obj 'get-inter-string-unhappiness 'vertical)
          (tell obj 'get-average-unhappiness)
          (tell obj 'get-intra-string-salience)
          (tell obj 'get-inter-string-salience 'horizontal)
          (tell obj 'get-inter-string-salience 'vertical)
          (tell obj 'get-average-salience)
          (tell obj 'get-raw-importance)
          (tell obj 'get-relative-importance))))

(define b:all-object-values
  (lambda () (map b:object-values (tell *workspace* 'get-objects))))

;; abc abd ijk (problem index of "abc abd ijk abd" is 19; use its 4 strings)
(define b:with-structures
  (lambda (strengths)
    (b:init-problem (list 'abc 'abd 'ijk 'abd) 1)
    (let* ((i (tell *initial-string* 'get-letters))
           (m (tell *modified-string* 'get-letters))
           (t (tell *target-string* 'get-letters))
           (a (tell *answer-string* 'get-letters))
           (s1 (car strengths)) (s2 (cadr strengths)) (s3 (caddr strengths))
           (bond (b:fake-structure 'bond s1
                   (cons 'get-bond-category (lambda () plato-successor))))
           (sbond (b:fake-structure 'bond s2
                    (cons 'get-bond-category (lambda () plato-sameness))))
           (hbridge (b:fake-structure 'bridge s3))
           (vbridge (b:fake-structure 'bridge s1))
           (gbridge (b:fake-structure 'bridge s2))
           (group (b:fake-structure 'group s3
                    (cons 'get-bridge (lambda (o) (if (eq? o 'horizontal) gbridge #f)))
                    (cons 'nested-member? (lambda (o) #f))
                    (cons 'get-nesting-level (lambda () 0)))))
      ;; initial: a-b bonded (a leftmost: one bond; b middle: two bonds)
      (tell (car i) 'update-right-bond bond)
      (tell (cadr i) 'update-left-bond bond)
      (tell (cadr i) 'update-right-bond sbond)
      (tell (caddr i) 'update-left-bond sbond)
      (tell (car i) 'add-outgoing-bond bond)
      (tell (cadr i) 'add-incoming-bond sbond)
      ;; bridges
      (tell (car i) 'update-bridge 'horizontal hbridge)
      (tell (car i) 'update-bridge 'vertical vbridge)
      (tell (car t) 'update-bridge 'vertical vbridge)
      (tell (car m) 'update-bridge 'horizontal hbridge)
      (tell (car a) 'update-bridge 'horizontal gbridge)
      ;; a group around the target's b and c, and one around the answer's
      (tell (cadr t) 'update-enclosing-group group)
      (tell (caddr t) 'update-enclosing-group group)
      (tell (cadr a) 'update-enclosing-group group)
      (tell (cadr m) 'clamp-salience)
      (b:update-workspace-values)
      (let* ((v (b:all-object-values))
             (ws (list (tell *workspace* 'get-average-intra-string-unhappiness)
                       (tell *workspace* 'get-average-inter-string-unhappiness 'top)
                       (tell *workspace* 'get-average-inter-string-unhappiness 'bottom)
                       (tell *workspace* 'get-average-inter-string-unhappiness 'vertical)
                       (tell *workspace* 'get-average-unhappiness)
                       (tell *workspace* 'get-mapping-strength 'top)
                       (tell *workspace* 'get-mapping-strength 'bottom)
                       (tell *workspace* 'get-mapping-strength 'vertical)))
             (misc (list (tell (car i) 'get-num-of-incident-bonds)
                         (tell (cadr i) 'get-nesting-level)
                         (unrelated? (car i)) (unrelated? (cadr i))
                         (ungrouped? (cadr t)) (unmapped? (car i)) (unmapped? (car t))
                         (unmapped? (car m)) (unmapped? (car a))
                         (b:ascii (tell (cadr t) 'get-ungrouped-left-neighbor))
                         (b:ascii (tell (car t) 'get-ungrouped-right-neighbor))
                         (map (lambda (d) (tell d 'print-name))
                              (tell (cadr m) 'get-distinguishing-descriptions)))))
        (tell (cadr m) 'unclamp-salience)
        (list v ws misc)))))

(test ws-structures-1 (b:with-structures (list 100 50 0)))
(test ws-structures-2 (b:with-structures (list 37 81 99)))
(test ws-structures-3 (b:with-structures (list 0 0 0)))
(test ws-structures-4 (b:with-structures (list 13 100 61)))

;; descriptions: add, attach, delete
(test ws-descriptions
  (begin
    (b:init-problem (list 'abc 'abd 'xyz) 1)
    (let* ((x (tell *target-string* 'get-letter 0)))
      (b:seq
        (lambda () (tell x 'new-description plato-alphabetic-position-category plato-alphabetic-last))
        (lambda () (tell x 'attach-description plato-length plato-one))
        (lambda () (map (lambda (d) (tell d 'print-name)) (tell x 'get-descriptions)))
        (lambda () (tell x 'description-present?
                     (car (tell x 'get-descriptions))))
        (lambda () (tell x 'delete-description-type plato-letter-category))
        (lambda () (map (lambda (d) (tell d 'print-name)) (tell x 'get-descriptions)))
        (lambda () (tell x 'update-raw-importance))
        (lambda () (tell x 'get-raw-importance))
        (lambda () (tell x 'update-description-strengths))
        (lambda () (map (lambda (d) (tell d 'get-strength)) (tell x 'get-descriptions)))
        (lambda () (b:deep-nm (tell x 'get-concept-pattern)))))))

;;;---------------------------------------------------------------------------
;;; Workspace strings: bonds, groups and storage expansion with fakes

(define b:fake-bond
  (lambda (from to category)
    (b:fake-structure 'bond 50
      (cons 'get-from-object (lambda () from))
      (cons 'get-to-object (lambda () to))
      (cons 'get-left-object (lambda () from))
      (cons 'get-right-object (lambda () to))
      (cons 'get-bond-category (lambda () category)))))

(define b:fake-group
  (lambda (string left right)
    (let* ((id 0)
           (leftmost (tell string 'get-letter left))
           (rightmost (tell string 'get-letter right)))
      (b:fake-structure 'group 70
        (cons 'set-id-num (lambda (n) (set! id n) 'done))
        (cons 'get-id-num (lambda () id))
        (cons 'get-left-string-pos (lambda () left))
        (cons 'get-right-string-pos (lambda () right))
        (cons 'get-leftmost-object (lambda () leftmost))
        (cons 'get-rightmost-object (lambda () rightmost))
        (cons 'get-direction (lambda () plato-right))
        (cons 'get-enclosing-group (lambda () #f))
        (cons 'spans-whole-string?
              (lambda () (= (+ 1 (- right left)) (tell string 'get-length))))
        (cons 'descriptor-present? (lambda (d) #f))
        (cons 'nested-member? (lambda (o) #f))
        (cons 'get-raw-importance (lambda () (* 10 (+ left right))))
        (cons 'update-relative-importance (lambda (v) 'done))))))

(test ws-string-bonds
  (begin
    (b:init-problem (list 'abc 'abd 'mrrjjj) 1)
    (let* ((s *target-string*)
           (l (tell s 'get-letters))
           (b1 (b:fake-bond (car l) (cadr l) plato-successor))
           (b2 (b:fake-bond (cadr l) (caddr l) plato-sameness)))
      (b:seq
        (lambda () (tell s 'add-bond b1))
        (lambda () (tell s 'add-bond b2))
        (lambda () (length (tell s 'get-bonds)))
        (lambda () (tell s 'add-proposed-bond b1))
        (lambda () (tell s 'add-proposed-bond b2))
        (lambda () (length (tell s 'get-all-bonds)))
        (lambda () (tell s 'delete-proposed-bond b1))
        (lambda () (length (tell s 'get-all-bonds)))
        (lambda () (tell s 'delete-proposed-bonds (cadr l)))
        (lambda () (length (tell s 'get-all-bonds)))
        (lambda () (tell s 'get-bond-category-relevance plato-successor))
        (lambda () (tell s 'get-bond-category-relevance plato-sameness))
        (lambda () (tell s 'delete-bond b1))
        (lambda () (length (tell s 'get-bonds)))
        (lambda () (tell s 'delete-all-proposed-bonds))))))

(test ws-string-groups
  (begin
    (b:init-problem (list 'abc 'abd 'mrrjjj) 1)
    (let* ((s *target-string*)
           (g1 (b:fake-group s 1 2))
           (g2 (b:fake-group s 3 5))
           (g3 (b:fake-group s 0 5)))
      (b:seq
        (lambda () (tell s 'get-max-object-capacity))
        (lambda () (tell s 'add-group g1))
        (lambda () (tell s 'add-proposed-group g2))
        (lambda () (length (tell s 'get-all-groups)))
        (lambda () (length (tell s 'get-all-objects)))
        (lambda () (tell s 'add-group g2))
        (lambda () (list (tell g1 'get-id-num) (tell g2 'get-id-num)))
        (lambda () (length (tell s 'get-left-edged-groups 3)))
        (lambda () (length (tell s 'get-right-edged-groups 5)))
        (lambda () (length (tell s 'get-all-other-coincident-groups g1 1 2 plato-right)))
        (lambda () (length (tell s 'get-all-other-coincident-groups g1 3 5 plato-right)))
        (lambda () (eq? (tell s 'get-equivalent-group g1) g1))
        (lambda () (b:asciis (tell (tell s 'get-letter 4) 'get-all-left-neighbors)))
        (lambda () (tell s 'spanning-group-exists?))
        (lambda () (tell s 'add-group g3))
        (lambda () (tell s 'spanning-group-exists?))
        (lambda () (tell s 'get-max-object-capacity))
        (lambda () (tell s 'update-all-relative-importances))
        (lambda () (map (lambda (o) (tell o 'get-relative-importance))
                        (tell s 'get-letters)))
        (lambda () (tell s 'delete-group g1))
        (lambda () (length (tell s 'get-groups)))
        (lambda () (tell s 'delete-proposed-group g2))
        (lambda () (tell s 'delete-all-proposed-groups))
        (lambda () (length (tell s 'get-all-groups)))))))

;; filling a string's object capacity doubles it and reallocates the
;; workspace's bridge storage
(test ws-string-expand
  (begin
    (b:init-problem (list 'abc 'abd 'xyz) 1)
    (let* ((s *initial-string*))
      (b:seq
        (lambda () (tell s 'get-max-object-capacity))
        (lambda () (tell s 'add-group (b:fake-group s 0 1)))
        (lambda () (tell s 'add-group (b:fake-group s 1 2)))
        (lambda () (tell s 'get-max-object-capacity))
        (lambda () (tell s 'add-group (b:fake-group s 0 2)))
        (lambda () (tell s 'get-max-object-capacity))
        (lambda () (tell *workspace* 'get-proposed-bridges 'top))
        (lambda () (tell *workspace* 'get-proposed-bridges 'vertical))))))

(test ws-string-names
  (begin
    (b:init-problem (list 'abc 'abd 'xyz) 1)
    (list (tell *initial-string* 'ascii-name)
          (tell *initial-string* 'generic-name)
          (b:capture (lambda () (tell *initial-string* 'print)))
          (tell *initial-string* 'mark-as-translated)
          (tell *initial-string* 'generic-name)
          (b:capture (lambda () (tell *initial-string* 'print)))
          (tell *initial-string* 'object-type)
          (b:ascii (tell *initial-string* 'get-letter 2))
          (tell (tell *initial-string* 'get-letter 2) 'print-name))))

;;;---------------------------------------------------------------------------
;;; Workspace: bridges and rules with fakes

(define b:fake-bridge
  (lambda (type obj1 obj2 strength spanning?)
    (b:fake-structure 'bridge strength
      (cons 'get-bridge-type (lambda () type))
      (cons 'get-orientation (lambda () (if (eq? type 'vertical) 'vertical 'horizontal)))
      (cons 'get-object1 (lambda () obj1))
      (cons 'get-object2 (lambda () obj2))
      (cons 'get-original-object1 (lambda () obj1))
      (cons 'get-original-object2 (lambda () obj2))
      (cons 'spanning-bridge? (lambda () spanning?))
      (cons 'get-slippages (lambda () (list type)))
      (cons 'get-non-symmetric-slippages (lambda () '()))
      (cons 'get-all-concept-mappings (lambda () (list 'cm)))
      (cons 'get-covered-letters
            (lambda () (append (tell obj1 'get-letters) (tell obj2 'get-letters))))
      (cons 'boost-themes (lambda () (set! b:eeg-log (cons type b:eeg-log)) 'done)))))

(define b:fake-rule
  (lambda (type name supported?)
    (lambda msg
      (record-case (cdr msg)
        (get-rule-type () type)
        (supported? () supported?)
        (name () name)
        (equal? (other) (eq? name (tell other 'name)))
        (else (error 'fake-rule "unexpected message" msg))))))

(test ws-bridges
  (begin
    (b:init-problem (list 'abc 'abd 'xyz) 1)
    (set! b:eeg-log '())
    (let* ((i (tell *initial-string* 'get-letters))
           (m (tell *modified-string* 'get-letters))
           (t (tell *target-string* 'get-letters))
           (tb (b:fake-bridge 'top (car i) (car m) 80 #f))
           (tb2 (b:fake-bridge 'top (car i) (car m) 60 #f))
           (vb (b:fake-bridge 'vertical (cadr i) (cadr t) 40 #t))
           (pb (b:fake-bridge 'vertical (car i) (caddr t) 20 #f)))
      (b:seq
        (lambda () (tell *workspace* 'add-bridge tb))
        (lambda () (tell *workspace* 'add-bridge vb))
        (lambda () (tell *workspace* 'add-proposed-bridge pb))
        (lambda () (tell *workspace* 'add-proposed-bridge tb2))
        (lambda () (length (tell *workspace* 'get-all-bridges)))
        (lambda () (length (tell *workspace* 'get-all-proposed-bridges)))
        (lambda () (length (tell *workspace* 'get-proposed-bridges 'vertical)))
        (lambda () (length (tell *workspace* 'get-proposed-vertical-bridges (car i))))
        (lambda () (length (tell *workspace* 'get-proposed-vertical-bridges (caddr t))))
        (lambda () (length (tell *workspace* 'get-proposed-horizontal-bridges (car m))))
        (lambda () (length (tell *workspace* 'get-all-other-coincident-bridges
                             tb2 (car i) (car m))))
        (lambda () (tell *workspace* 'bridge-present? tb))
        (lambda () (tell *workspace* 'spanning-bridge-exists? 'vertical))
        (lambda () (eq? (tell *workspace* 'get-spanning-bridge 'vertical) vb))
        (lambda () (tell *workspace* 'get-all-slippages 'top))
        (lambda () (tell *workspace* 'get-all-vertical-CMs))
        (lambda () (tell *workspace* 'maximal-mapping? 'vertical))
        (lambda () (tell *workspace* 'spread-activation-to-themespace))
        (lambda () (reverse b:eeg-log))
        (lambda () (length (tell *workspace* 'get-structures)))
        (lambda () (b:update-workspace-values))
        (lambda () (list (tell *workspace* 'get-mapping-strength 'top)
                         (tell *workspace* 'get-mapping-strength 'vertical)
                         (tell *workspace* 'get-average-unhappiness)))
        (lambda () (tell *workspace* 'delete-proposed-bridge pb))
        (lambda () (tell *workspace* 'delete-proposed-vertical-bridges (car i)))
        (lambda () (tell *workspace* 'delete-proposed-horizontal-bridges (car m)))
        (lambda () (length (tell *workspace* 'get-all-proposed-bridges)))
        (lambda () (tell *workspace* 'delete-bridge tb))
        (lambda () (tell *workspace* 'delete-all-proposed-bridges))
        (lambda () (length (tell *workspace* 'get-all-bridges)))))))

(test ws-rules
  (begin
    (b:init-problem (list 'abc 'abd 'xyz) 1)
    (let* ((r1 (b:fake-rule 'top 'r1 #t))
           (r2 (b:fake-rule 'top 'r2 #f))
           (r3 (b:fake-rule 'top 'r1 #f)))
      (b:seq
        (lambda () (tell *workspace* 'rule-exists? 'top))
        (lambda () (tell *workspace* 'add-rule r2))
        (lambda () (tell *workspace* 'supported-rule-exists? 'top))
        (lambda () (tell *workspace* 'add-rule r1))
        (lambda () (tell *workspace* 'supported-rule-exists? 'top))
        (lambda () (map (lambda (r) (tell r 'name)) (tell *workspace* 'get-all-rules)))
        (lambda () (map (lambda (r) (tell r 'name))
                        (tell *workspace* 'get-all-supported-rules)))
        (lambda () (eq? (tell *workspace* 'get-equivalent-rule r3) r1))
        (lambda () (tell *workspace* 'rule-present? r3))
        (lambda () (tell *workspace* 'delete-rule r1))
        (lambda () (tell *workspace* 'rule-present? r3))
        (lambda () (tell *workspace* 'clamp-rule r1))
        (lambda () (length (tell *workspace* 'get-clamped-rules)))
        (lambda () (tell *workspace* 'unclamp-rules))
        (lambda () (length (tell *workspace* 'get-clamped-rules)))))))

;; update-temperature with a fake workspace
(define b:fake-temperature-workspace
  (lambda (unhappiness top-possible top-supported bottom-possible bottom-supported)
    (lambda msg
      (record-case (cdr msg)
        (get-average-unhappiness () unhappiness)
        (rule-possible? (t) (if (eq? t 'top) top-possible bottom-possible))
        (supported-rule-exists? (t) (if (eq? t 'top) top-supported bottom-supported))
        (else (error 'fake-workspace "unexpected message" msg))))))

(test update-temperature
  (let* ((saved *workspace*)
         (r (b:map-in-order
              (lambda (args)
                (b:set-global! '%justify-mode% (car args))
                (b:set-global! '*temperature-clamped?* (cadr args))
                (b:set-global! '*temperature* 55)
                (b:set-global! '*workspace* (apply b:fake-temperature-workspace (cddr args)))
                (update-temperature)
                *temperature*)
              (list (list #f #f 0 #t #t #f #f)
                    (list #f #f 0 #t #f #f #f)
                    (list #f #f 37 #f #t #f #f)
                    (list #f #f 100 #t #t #f #f)
                    (list #f #t 0 #t #t #f #f)
                    (list #t #f 42 #t #t #f #f)
                    (list #t #f 42 #t #t #t #t)
                    (list #t #f 99 #t #t #t #f)
                    (list #f #f 41 #f #f #f #f)
                    (list #f #f 43 #f #f #f #f)))))
    (b:set-global! '*workspace* saved)
    (b:set-global! '*temperature-clamped?* #f)
    (b:set-global! '%justify-mode% #f)
    r))

;;;---------------------------------------------------------------------------
;;; Workspace structures

(define b:structure
  (lambda (internal external compatibility)
    (let* ((base (make-workspace-structure)))
      (lambda msg
        (record-case (cdr msg)
          (calculate-internal-strength () internal)
          (calculate-external-strength () external)
          (get-thematic-compatibility () compatibility)
          (else (apply base msg)))))))

(define b:strength-cases
  (let loop ((cases '())
             (xs (list (list 0 0 0) (list 100 0 0) (list 0 100 0) (list 50 50 0)
                       (list 73 21 0) (list 100 100 0) (list 30 90 1/2)
                       (list 30 90 -1/2) (list 60 40 1) (list 60 40 -1)
                       (list 99 1 0.3) (list 12 88 -0.7) (list 45 45 0.05))))
    (if (null? xs) (reverse cases) (loop (cons (car xs) cases) (cdr xs)))))

(test ws-structure-strength
  (b:map-in-order
    (lambda (c)
      (let* ((s (apply b:structure c)))
        (tell s 'update-strength)
        (list c (tell s 'get-strength) (tell s 'get-weakness))))
    b:strength-cases))

(test ws-structure-misc
  (begin
    (b:set-global! '*codelet-count* 17)
    (let* ((s (b:structure 50 50 0)))
      (b:set-global! '*codelet-count* 40)
      (let* ((r (b:seq
                  (lambda () (tell s 'object-type))
                  (lambda () (tell s 'get-time-stamp))
                  (lambda () (tell s 'get-age))
                  (lambda () (tell s 'proposed?))
                  (lambda () (tell s 'update-proposal-level %evaluated%))
                  (lambda () (tell s 'get-proposal-level))
                  (lambda () (tell s 'update-proposal-level %built%))
                  (lambda () (tell s 'proposed?))
                  (lambda () (tell s 'drawn?))
                  (lambda () (tell s 'set-drawn? #t))
                  (lambda () (tell s 'drawn?))
                  (lambda () (tell s 'update-enclosing-group 'g))
                  (lambda () (tell s 'get-enclosing-group))
                  (lambda () (tell (make-workspace-structure) 'get-thematic-compatibility)))))
        (b:set-global! '*codelet-count* 0)
        r))))

(test wins-fight
  (b:map-in-order
    (lambda (temp)
      (b:set-global! '*temperature* temp)
      (b:map-in-order
        (lambda (seed)
          (b:seeded seed
            (lambda ()
              (b:repeat 6
                (lambda ()
                  (wins-fight? (b:structure 70 30 0) 1 (b:structure 40 60 0) 3/2))))))
        b:seeds))
    (list 100 50 10 0)))

(test wins-all-fights
  (begin
    (b:set-global! '*temperature* 60)
    (b:map-in-order
      (lambda (seed)
        (b:seeded seed
          (lambda ()
            (let* ((c (b:structure 60 60 0))
                   (ds (list (b:structure 50 20 0) (b:structure 90 90 0)
                             (b:structure 10 10 0)))
                   (r1 (wins-all-fights? c 1 ds 1))
                   (r2 (wins-all-fights? c 2 ds (list 1 1/2 3)))
                   (r3 (wins-all-fights? c 1 '() 1)))
              (list r1 r2 r3)))))
      b:seeds)))

;;;---------------------------------------------------------------------------
;;; formulas.ss and workspace-structure-formulas.ss

(define b:temps (list 0 1 10 25 35 50 64 75 90 99 100))
(define b:probs (list 0 0.0 0.0001 0.003 0.01 0.05 0.1 0.25 0.3333 0.5 0.5000001
                      0.6 0.75 0.9 0.99 1 1.0 1/3 1/1000 2/3))

(test temp-adjusted-probability
  (b:map-in-order
    (lambda (temp)
      (b:set-global! '*temperature* temp)
      (map temp-adjusted-probability b:probs))
    b:temps))

(test temp-adjusted-values
  (b:map-in-order
    (lambda (temp)
      (b:set-global! '*temperature* temp)
      (temp-adjusted-values (list 0 1 2 3 10 33 50 77 99 100 1/3 2.5 1000)))
    b:temps))

(define b:fake-group-for-probability
  (lambda (len supporting support)
    (lambda msg
      (record-case (cdr msg)
        (get-group-length () len)
        (get-num-of-local-supporting-groups () supporting)
        (get-local-support () support)
        (else (error 'fake-group "unexpected message" msg))))))

(test length-description-probability
  (b:map-in-order
    (lambda (act)
      (tell plato-length 'set-activation act)
      (b:map-in-order
        (lambda (temp)
          (b:set-global! '*temperature* temp)
          (map (lambda (len) (length-description-probability
                               (b:fake-group-for-probability len 0 0)))
               (b:iota 7)))
        b:temps))
    (list 0 30 77 100)))

(test single-letter-group-probability
  (b:map-in-order
    (lambda (act)
      (tell plato-length 'set-activation act)
      (b:map-in-order
        (lambda (temp)
          (b:set-global! '*temperature* temp)
          (map (lambda (n) (single-letter-group-probability
                             (b:fake-group-for-probability 1 n (* 20 n))))
               (b:iota 5)))
        b:temps))
    (list 0 30 77 100)))

(test translation-threshold-distribution
  (b:map-in-order
    (lambda (strings)
      (b:init-problem strings 1)
      (b:index-of (current-translation-temperature-threshold-distribution)
                  b:threshold-distributions))
    (list (list 'a 'b 'z) (list 'abc 'abd 'xyz) (list 'abc 'abd 'mrrjjj))))

(test ws-objects-helpers
  (begin
    (b:init-problem (list 'abc 'abd 'mrrjjj) 1)
    (let* ((t (tell *target-string* 'get-letters))
           (i (tell *initial-string* 'get-letters)))
      (list (disjoint-objects? (car t) (cadr t))
            (disjoint-objects? (car t) (car t))
            (lone-spanning-object? (car t) (cadr t))
            (both-spanning-groups? (car t) (cadr t))
            (both-spanning-objects? (car t) (cadr t))
            (rough-num-of-objects 0)
            (b:seeded 5 (lambda () (b:repeat 10 (lambda () (rough-num-of-objects 3)))))
            (b:ascii (tell (car t) 'get-instantiated-image-object))))))
;; tanh (the mapping strengths; Chez's own primitive in compat.rkt): every
;; argument the mapping strengths can give, and doubles across magnitudes
(test tanh-mapping-arguments
  (map (lambda (n) (tanh (* 1/40 (- n 101)))) (b:iota 201)))
(test tanh-doubles
  (b:seeded 11
    (lambda ()
      (b:repeat 300
        (lambda ()
          (let* ((x (random 1.0)) (e (random 12)))
            (tanh (* (- x 0.5) (expt 2.0 (- e 4))))))))))
(test tanh-special (map tanh (list 0 0.0 -0.0 1 -1 1/2 20 30.0 -400.0 1e-300)))

;; raw and relative importance with fully active description types (at the
;; start of a run they are not, so every raw importance is 0), one letter in
;; a group (raw importance 2/3)
(test ws-importance
  (b:map-in-order
    (lambda (types)
      (b:init-problem (list 'abc 'abd 'mrrjjj) 1)
      (for-each (lambda (t) (tell t 'set-activation %max-activation%)) types)
      (tell plato-c 'set-activation 37)
      (tell plato-j 'set-activation 71)
      (tell (tell *target-string* 'get-letter 2) 'update-enclosing-group
        (b:fake-structure 'group 55
          (cons 'get-bridge (lambda (o) #f))
          (cons 'nested-member? (lambda (o) #f))
          (cons 'get-nesting-level (lambda () 0))))
      (b:update-workspace-values)
      (list (b:all-object-values)
            (tell *workspace* 'get-average-unhappiness)
            (map (lambda (s) (tell s 'get-average-intra-string-unhappiness))
                 (list *initial-string* *modified-string* *target-string*))))
    (list (list plato-letter-category)
          (list plato-string-position-category)
          (list plato-letter-category plato-string-position-category
                plato-object-category))))

;; mapping strengths: a maximal top mapping with no spanning bridge (the tanh
;; branch), and a vertical one where both strings could be spanned (1/2)
(test ws-maximal-mapping
  (b:map-in-order
    (lambda (strengths)
      (b:init-problem (list 'abc 'abd 'xyz) 1)
      (let* ((i (tell *initial-string* 'get-letters))
             (m (tell *modified-string* 'get-letters))
             (t (tell *target-string* 'get-letters))
             (b1 (b:fake-bridge 'top (car i) (car m) (car strengths) #f))
             (b2 (b:fake-bridge 'top (cadr i) (cadr m) (cadr strengths) #f))
             (b3 (b:fake-bridge 'top (caddr i) (caddr m) (caddr strengths) #f))
             (v1 (b:fake-bridge 'vertical (car i) (car t) (cadr strengths) #f)))
        (b:seq
          (lambda () (tell *workspace* 'add-bridge b1))
          (lambda () (tell *workspace* 'add-bridge b2))
          (lambda () (tell *workspace* 'maximal-mapping? 'top))
          (lambda () (tell *workspace* 'add-bridge b3))
          (lambda () (tell *workspace* 'add-bridge v1))
          (lambda () (for-each (lambda (o) (tell o 'update-bridge 'horizontal b1)) i))
          (lambda () (for-each (lambda (o) (tell o 'update-bridge 'horizontal b2)) m))
          (lambda () (tell (car i) 'update-bridge 'vertical v1))
          (lambda () (tell (car t) 'update-bridge 'vertical v1))
          (lambda () (tell *workspace* 'maximal-mapping? 'top))
          (lambda () (list (spanning-group-possible? *initial-string*)
                           (spanning-group-possible? *modified-string*)
                           (spanning-group-possible? *target-string*)))
          (lambda () (b:update-workspace-values))
          (lambda () (list (tell *workspace* 'get-mapping-strength 'top)
                           (tell *workspace* 'get-mapping-strength 'vertical)
                           (tell *workspace* 'get-min-mapping-strength)
                           (tell *workspace* 'get-max-inter-string-unhappiness)
                           (tell *workspace* 'get-average-unhappiness)))
          (lambda () (b:all-object-values)))))
    (list (list 100 100 100) (list 90 70 50) (list 33 66 99) (list 1 2 3))))
