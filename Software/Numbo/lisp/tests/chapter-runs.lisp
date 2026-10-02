;;; chapter-runs.lisp -- item 11: run the chapter's puzzles over many seeds
;;; and print the tables in src/RESULTS.md.
;;;
;;; Run: sbcl --non-interactive --load tests/chapter-runs.lisp
;;; (about a minute).  Not part of run-tests.sh; tests/validation-tests.lisp
;;; checks a few of these numbers.
;;;
;;; Every run is (run-config puzzle :seed s :max-iterations 20000), seeds
;;; 1..*seeds*.  Outcomes:
;;;   valid    "Done :" and check-solution accepts the decomposition
;;;   invalid  "Done :" but check-solution rejects it (kill-block gap, item 10)
;;;   gave-up  the coderack was empty after the retry (start.lisp (return))
;;;   capped   still running at 20000 iterations
;;;   error    a Lisp error (the reactivate-cyto/plinks race, item 10)

(defvar cl-user::*numbo-no-autoload* t)
(unless (find-package :numbo-trace)
  (let ((*error-output* (make-broadcast-stream))
        (*standard-output* (make-broadcast-stream)))
    (handler-bind ((warning #'muffle-warning))
      (load (merge-pathnames "../src/load.lisp" *load-truename*))
      (funcall (intern "NUMBO-LOAD" "CL-USER"))
      (load (merge-pathnames "trace-tools.lisp" *load-truename*)))))

(in-package :numbo-trace)

(defparameter *seeds* 20)
(defparameter *cap* 20000)

;;; (number target bricks chapter-says)
(defparameter *puzzles*
  '((1 114 (11 20 7 1 6) "sample run (Fig. III-3) ends 20 x 6 - (7 - 1); also (20 - 1) x 6")
    (2 87 (8 3 9 10 7) "human data only: 8 x 10 + 7, 9 x 10 - 3")
    (3 31 (3 5 24 3 14) "Numbo did not solve it (Fig. III-6); four solutions exist")
    (4 25 (8 5 5 11 2) "human data only: 5 x 5")
    (5 102 (6 17 2 4 1) "human data only: 6 x 17")
    (6 146 (12 2 5 7 18) "human data only: 12 x (5 + 7) + 2")
    (7 6 (3 3 17 11 22) "Numbo immediately gives 3 + 3")
    (8 11 (2 5 1 25 23) "Numbo immediately gives 2 x 5 + 1")
    (9 116 (20 2 16 14 6) "Numbo finds 6 x 20 - 2 - (16 - 14)")
    (10 127 (6 4 22 5 7) "once in a while 6 x 5, then 4 x 30 + 7")
    (11 41 (5 16 22 25 1) "Numbo has problems: nearly random search")))

(defun median (xs)
  (when xs
    (let* ((v (sort (coerce xs 'vector) #'<)) (n (length v)))
      (if (oddp n) (aref v (floor n 2))
          (round (+ (aref v (1- (floor n 2))) (aref v (floor n 2))) 2)))))

;;; check-solution prints e.g. "114 = (20 x 6) - (7 - 1)".  Solutions that
;;; differ only in the order of + or x operands are counted together, under
;;; a canonical form (operands of + and x sorted, larger first by value).
(defun expr-tokens (s)
  (let ((tokens nil) (i 0))
    (loop while (< i (length s)) do
      (let ((c (char s i)))
        (cond ((char= c #\Space) (incf i))
              ((find c "()+-x/") (push (string c) tokens) (incf i))
              ((digit-char-p c)
               (let ((j (or (position-if-not #'digit-char-p s :start i) (length s))))
                 (push (parse-integer s :start i :end j) tokens)
                 (setq i j)))
              (t (error "expr-tokens: ~s in ~s" c s)))))
    (nreverse tokens)))

(defun parse-expr (tokens)
  "TOKENS = term op term | term.  Return (values tree rest)."
  (labels ((term (ts)
             (if (equal (car ts) "(")
                 (multiple-value-bind (e rest) (parse-expr (cdr ts))
                   (values e (cdr rest)))  ; drop ")"
                 (values (car ts) (cdr ts)))))
    (multiple-value-bind (a rest) (term tokens)
      (if (and rest (member (car rest) '("+" "-" "x" "/") :test #'equal))
          (multiple-value-bind (b rest2) (term (cdr rest))
            (values (list (car rest) a b) rest2))
          (values a rest)))))

(defun expr-value (e)
  (if (integerp e) e
      (let ((a (expr-value (second e))) (b (expr-value (third e))))
        (cond ((equal (car e) "+") (+ a b)) ((equal (car e) "-") (- a b))
              ((equal (car e) "x") (* a b)) (t (/ a b))))))

(defun expr-string (e top)
  (if (integerp e) (princ-to-string e)
      (format nil (if top "~a ~a ~a" "(~a ~a ~a)")
              (expr-string (second e) nil) (car e) (expr-string (third e) nil))))

(defun canonical (e)
  (if (integerp e) e
      (let ((a (canonical (second e))) (b (canonical (third e))))
        (when (and (member (car e) '("+" "x") :test #'equal)
                   (or (< (expr-value a) (expr-value b))
                       (and (= (expr-value a) (expr-value b))
                            (string< (expr-string a t) (expr-string b t)))))
          (rotatef a b))
        (list (car e) a b))))

(defun canonical-solution (s)
  "\"114 = (20 x 6) - (7 - 1)\" => the same with + and x operands ordered."
  (let ((p (search " = " s)))
    (format nil "~a = ~a" (subseq s 0 p)
            (expr-string (canonical (parse-expr (expr-tokens (subseq s (+ p 3))))) t))))

(defun run-puzzle (target bricks)
  "Plist of outcome counts, valid-solution iterations and expressions."
  (let ((problem (cons target bricks))
        (counts (list :valid 0 :invalid 0 :gave-up 0 :capped 0 :error 0))
        (iterations nil) (expressions nil) (valid-seeds nil))
    (loop for seed from 1 to *seeds* do
      (multiple-value-bind (r out err)
          (run-captured problem :seed seed :max-iterations *cap*)
        (let ((outcome
                (cond (err :error)
                      ((eq (getf r :outcome) :solved)
                       (multiple-value-bind (ok reason expr)
                           (numbo::check-solution out problem)
                         (declare (ignore reason))
                         (cond (ok (push (getf r :iterations) iterations)
                                   (push seed valid-seeds)
                                   (let* ((key (canonical-solution expr))
                                          (cell (assoc key expressions :test #'string=)))
                                     (if cell (incf (cdr cell)) (push (cons key 1) expressions)))
                                   :valid)
                               (t :invalid))))
                      (t (getf r :outcome)))))
          (incf (getf counts outcome)))))
    (list :counts counts :iterations (reverse iterations)
          :expressions (sort expressions #'> :key #'cdr)
          :valid-seeds (reverse valid-seeds))))

(defun report ()
  (let ((rows (loop for (num target bricks says) in *puzzles*
                    collect (list num target bricks says (run-puzzle target bricks)))))
    (format t "~&| # | Target | Bricks | valid | invalid Done | gave up | capped | error | iterations to a valid solution (median / min-max) |~%")
    (format t "|---|---|---|---|---|---|---|---|---|~%")
    (dolist (row rows)
      (destructuring-bind (num target bricks says res) row
        (declare (ignore says))
        (let ((c (getf res :counts)) (its (getf res :iterations)))
          (format t "| ~a | ~a | ~{~a~^ ~} | ~a/~a | ~a | ~a | ~a | ~a | ~:[-~;~:*~a / ~a-~a~] |~%"
                  num target bricks (getf c :valid) *seeds* (getf c :invalid)
                  (getf c :gave-up) (getf c :capped) (getf c :error)
                  (median its) (and its (reduce #'min its)) (and its (reduce #'max its))))))
    (format t "~%")
    (dolist (row rows)
      (destructuring-bind (num target bricks says res) row
        (declare (ignore bricks))
        (format t "**#~a (~a)** — chapter: ~a.~%" num target says)
        (if (getf res :expressions)
            (progn
              (format t "Valid solutions found (count): ~{~a~^; ~}.~%"
                      (mapcar (lambda (e) (format nil "`~a` (~a)" (car e) (cdr e)))
                              (getf res :expressions)))
              (format t "Seeds: ~{~a~^ ~}; iterations: ~{~a~^ ~}.~%~%"
                      (getf res :valid-seeds) (getf res :iterations)))
            (format t "No valid solution.~%~%"))))))

;;; (defvar cl-user::*chapter-runs-no-report* t) before loading = functions only.
(unless (boundp 'cl-user::*chapter-runs-no-report*)
  (let ((start (get-internal-real-time)))
    (format t "~&;; chapter runs: ~a seeds, cap ~a iterations~%~%" *seeds* *cap*)
    (report)
    (format t "~&;; ~,1f s~%" (/ (- (get-internal-real-time) start)
                                 internal-time-units-per-second))))
