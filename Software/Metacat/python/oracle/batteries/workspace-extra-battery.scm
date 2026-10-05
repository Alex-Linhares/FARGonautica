;;; workspace-extra-battery.scm -- workspace-objects.ss, workspace-strings.ss,
;;; workspace.ss and formulas.ss cases that tests/diff/workspace-battery.scm
;;; does not reach (loop0002 item 06).
;;;
;;; Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Captured like tests/diff/*-battery.scm by python/oracle/capture.py:
;;; chez_scheme/oracle/diff-eval.ss evaluates tests/diff/helpers.scm, then this
;;; file, with the original loaded.  The Python side is the end of
;;; python/tests/test_workspace.py.  Same rules as the frozen batteries: no two
;;; side-effecting subexpressions in one call or one `let'.  It loads
;;; tests/diff/workspace-dump.scm (unedited) for b:init-problem, and sets the
;;; same stand-ins as workspace-battery.scm.

(load "tests/diff/workspace-dump.scm")

(b:set-global! '*themespace*
  (lambda msg
    (record-case (cdr msg)
      (get-active-themes (types) '())
      (else (error 'fake-themespace "unexpected message" msg)))))
(b:set-global! '*EEG* (lambda msg 'done))
(b:set-global! 'contains?
  (lambda (object1 object2)
    (and (group? object1) (tell object1 'nested-member? object2))))
(b:set-global! '%workspace-graphics% #f)

(define x:fake
  (lambda (type strength . props)
    (lambda msg
      (let ((m (cadr msg)))
        (cond
          ((eq? m 'object-type) type)
          ((eq? m 'get-strength) strength)
          ((eq? m 'update-strength) 'done)
          ((assq m props) => (lambda (p) (apply (cdr p) (cddr msg))))
          (else (error 'fake "unexpected message" m)))))))

(define x:fake-group
  (lambda (string left right salience)
    (let* ((id 0)
           (leftmost (tell string 'get-letter left))
           (rightmost (tell string 'get-letter right)))
      (x:fake 'group 70
        (cons 'set-id-num (lambda (n) (set! id n) 'done))
        (cons 'get-id-num (lambda () id))
        (cons 'ascii-name (lambda () (format "group:~a-~a" left right)))
        (cons 'get-left-string-pos (lambda () left))
        (cons 'get-right-string-pos (lambda () right))
        (cons 'get-leftmost-object (lambda () leftmost))
        (cons 'get-rightmost-object (lambda () rightmost))
        (cons 'get-enclosing-group (lambda () #f))
        (cons 'get-intra-string-salience (lambda () salience))))))

(define x:bond
  (lambda (category direction)
    (x:fake 'bond 50
      (cons 'get-bond-category (lambda () category))
      (cons 'get-direction (lambda () direction)))))

(define x:ascii (lambda (o) (if o (tell o 'ascii-name) #f)))

(define x:values
  (lambda (obj)
    (list (tell obj 'ascii-name)
          (tell obj 'get-intra-string-unhappiness)
          (tell obj 'get-inter-string-unhappiness 'horizontal)
          (tell obj 'get-inter-string-unhappiness 'vertical)
          (tell obj 'get-average-unhappiness)
          (tell obj 'get-inter-string-salience 'horizontal)
          (tell obj 'get-inter-string-salience 'vertical)
          (tell obj 'get-average-salience))))

;; letters in a group whose bridge is vertical only: the 1/2 factor on the
;; group's vertical bridge (workspace-battery.scm's fake group has a
;; horizontal one only)
(test group-vertical-bridge
  (begin
    (b:init-problem (list 'abc 'abd 'ijk) 1)
    (let* ((vbridge (x:fake 'bridge 37))
           (group (x:fake 'group 64
                    (cons 'get-bridge (lambda (o) (if (eq? o 'vertical) vbridge #f)))
                    (cons 'nested-member? (lambda (o) #f))
                    (cons 'get-nesting-level (lambda () 0))))
           (i (tell *initial-string* 'get-letters))
           (t (tell *target-string* 'get-letters)))
      (tell (car i) 'update-enclosing-group group)
      (tell (cadr t) 'update-enclosing-group group)
      (tell (caddr t) 'update-enclosing-group group)
      (b:update-workspace-values)
      (map x:values (tell *workspace* 'get-objects)))))

;; relevant descriptions (description types fully active) whose descriptors
;; have different activations and depths: choosing by activation and by depth
(test relevant-description-choices
  (begin
    (b:init-problem (list 'abc 'abd 'xyz) 1)
    (tell plato-letter-category 'set-activation 100)
    (tell plato-string-position-category 'set-activation 100)
    (tell plato-object-category 'set-activation 100)
    (tell plato-a 'set-activation 5)
    (tell plato-letter 'set-activation 40)
    (tell plato-leftmost 'set-activation 80)
    (tell plato-x 'set-activation 15)
    (b:seeded 5
      (lambda ()
        (let* ((objects (tell *workspace* 'get-objects)))
          (b:repeat 4
            (lambda ()
              (let loop ((os objects) (acc '()))
                (if (null? os)
                    (reverse acc)
                    (let* ((d1 (tell (car os) 'choose-relevant-description-by-activation))
                           (d2 (tell (car os)
                                 'choose-relevant-distinguishing-description-by-depth)))
                      (loop (cdr os)
                            (cons (list (tell d1 'print-name)
                                        (if d2 (tell d2 'print-name) #f))
                                  acc))))))))))))

;; neighbours that are letters and groups with different saliences
(test neighbours-with-groups
  (begin
    (b:init-problem (list 'abc 'abd 'mrrjjj) 1)
    (let* ((s *target-string*)
           (g1 (x:fake-group s 1 2 90))
           (g2 (x:fake-group s 3 5 10))
           (g3 (x:fake-group s 2 3 55)))
      (tell s 'add-group g1)
      (tell s 'add-group g2)
      (tell s 'add-group g3)
      (b:seeded 9
        (lambda ()
          (b:repeat 6
            (lambda ()
              (let* ((l3 (tell s 'get-letter 3))
                     (l2 (tell s 'get-letter 2))
                     (a (x:ascii (tell l3 'choose-left-neighbor)))
                     (b (x:ascii (tell l2 'choose-right-neighbor)))
                     (c (x:ascii (tell l3 'choose-neighbor)))
                     (d (x:ascii (tell l2 'choose-neighbor))))
                (list a b c d)))))))))

;; bond-category and direction relevance with letters' right bonds set
(test relevance-with-bonds
  (begin
    (b:init-problem (list 'abc 'abd 'mrrjjj) 1)
    (let* ((s *target-string*)
           (l (tell s 'get-letters)))
      (tell (list-ref l 0) 'update-right-bond (x:bond plato-successor plato-right))
      (tell (list-ref l 1) 'update-right-bond (x:bond plato-sameness #f))
      (tell (list-ref l 3) 'update-right-bond (x:bond plato-sameness #f))
      (tell (list-ref l 4) 'update-right-bond (x:bond plato-sameness #f))
      (list (tell s 'get-bond-category-relevance plato-sameness)
            (tell s 'get-bond-category-relevance plato-successor)
            (tell s 'get-bond-category-relevance plato-predecessor)
            (tell s 'get-direction-relevance plato-right)
            (tell s 'get-direction-relevance plato-left)))))

;; unrelated? with one incident bond: leftmost, middle and rightmost letters
(test unrelated-one-bond
  (begin
    (b:init-problem (list 'abc 'abd 'xyz) 1)
    (let* ((l (tell *initial-string* 'get-letters))
           (bond (x:bond plato-successor plato-right)))
      (tell (cadr l) 'update-left-bond bond)
      (tell (car l) 'update-right-bond bond)
      (list (unrelated? (car l)) (unrelated? (cadr l)) (unrelated? (caddr l))
            (tell (cadr l) 'get-num-of-incident-bonds)))))

;; the translation threshold distribution as bonds are added one by one: the
;; densities k/15 meet the flonum thresholds exactly at 3/15, 6/15, 9/15 and
;; 12/15 (exact rationals compared with the doubles nearest 0.2, 0.4, 0.6, 0.8)
(define x:index-of
  (lambda (x l)
    (let loop ((l l) (i 0))
      (cond
        ((null? l) #f)
        ((eq? x (car l)) i)
        (else (loop (cdr l) (+ i 1)))))))

(test density-boundaries
  (begin
    (b:init-problem (list 'abcdef 'abcdeg 'ijklmn) 1)
    (let* ((ds (list %very-low-translation-temperature-threshold-distribution%
                     %low-translation-temperature-threshold-distribution%
                     %medium-translation-temperature-threshold-distribution%
                     %high-translation-temperature-threshold-distribution%
                     %very-high-translation-temperature-threshold-distribution%))
           (strings (list *initial-string* *modified-string* *target-string*)))
      (let loop ((k 0) (acc '()))
        (if (= k 15)
            (reverse acc)
            (let* ((s (list-ref strings (quotient k 5)))
                   (from (tell s 'get-letter (remainder k 5)))
                   (to (tell s 'get-letter (+ 1 (remainder k 5))))
                   (bond (x:fake 'bond 50
                           (cons 'get-from-object (lambda () from))
                           (cons 'get-to-object (lambda () to))
                           (cons 'get-left-object (lambda () from))
                           (cons 'get-right-object (lambda () to))
                           (cons 'get-bond-category (lambda () plato-successor)))))
              (tell s 'add-bond bond)
              (loop (+ k 1)
                    (cons (x:index-of (current-translation-temperature-threshold-distribution) ds)
                          acc))))))))

;; the Workspace's activity as structures (fake top rules) of different ages
;; are added: the youngest three, and (min 1.0 ...) making the age ratio a
;; flonum before 100* rounds it
(test activity-and-ages
  (begin
    (b:init-problem (list 'abc 'abd 'xyz) 1)
    (let loop ((ages (list 13 12 700 1000 3 2 501 1)) (acc '()))
      (if (null? ages)
          (reverse acc)
          (let* ((age (car ages))
                 (rule (lambda msg
                         (record-case (cdr msg)
                           (get-rule-type () 'top)
                           (get-age () age)
                           (else (error 'fake-rule "unexpected message" msg))))))
            (tell *workspace* 'add-rule rule)
            (loop (cdr ages)
                  (cons (list (tell *workspace* 'get-youngest-structures-average-age)
                              (tell *workspace* 'get-activity))
                        acc)))))))

;; average ages whose 100*(age/500) is a tie only exactly: 545/2 and 575/2
;; round one way as exact rationals and the other way as flonums
(test activity-float-ties
  (map (lambda (ages)
         (b:init-problem (list 'abc 'abd 'xyz) 1)
         (for-each
           (lambda (age)
             (tell *workspace* 'add-rule
               (lambda msg
                 (record-case (cdr msg)
                   (get-rule-type () 'top)
                   (get-age () age)
                   (else (error 'fake-rule "unexpected message" msg))))))
           ages)
         (list (tell *workspace* 'get-youngest-structures-average-age)
               (tell *workspace* 'get-activity)))
       (list (list 272 273) (list 287 288) (list 1 2))))
