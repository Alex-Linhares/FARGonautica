;;; solution-checker.lisp -- check the solution CONFIG prints after "Done :".
;;;
;;; Not part of the 1987 source.  When *problem-solved* is 1, CONFIG prints
;;; "Done : " and calls (decompose 'cyto-target), which prints one paragraph
;;; per operation node, from the target down (codelets.lisp `decompose'):
;;;
;;;   Operation PLUS24-7-V1 has been applied
;;;   to CYTO-TARGET-7-V1 ( 7) and to CYTO-BRICK3 ( 24)
;;;   to get CYTO-TARGET
;;;
;;; An operation node links three cyto-nodes (res = op1 + op2, or res = op1 x
;;; op2) and decompose prints the two that are not the node being expanded.
;;; So "to get N" means N = a + b or N = |a - b| for a PLUS node, and
;;; N = a x b or N = a / b (exact) for a TIMES node, depending on which side of
;;; the operation N is on.  The value of N is not printed: it is the target
;;; for the first paragraph, and otherwise the value printed where N appears
;;; as an operand.
;;;
;;; (check-solution output '(31 3 5 24 3 14))
;;;   => T,   nil,    "31 = 24 + ((3 x 3) - (5 - 3))"-style expression
;;;   or NIL, reason, nil
;;; The solution is valid when the operations form a tree whose root is the
;;; target, every leaf is a given brick (CYTO-BRICKi has the i-th brick's
;;; value) used at most once, every other operand is itself derived, and
;;; every step is correct arithmetic.  This rejects, e.g., a decomposition
;;; that goes through a block killed after a derived target was built on it
;;; (kill-block only cascades through "4bl" and "1t" parents; the dangling
;;; operand is printed but never expanded).

(in-package :numbo)

(cl:defun solution-tokens (text)
  "Split TEXT into whitespace-separated tokens, dropping parentheses."
  (let ((tokens nil) (start nil))
    (dotimes (i (1+ (length text)))
      (let ((c (cl:if (< i (length text)) (char text i) #\Space)))
        (cond ((cl:member c '(#\Space #\Tab #\Newline #\Return #\( #\)))
               (when start
                 (push (subseq text start i) tokens)
                 (setq start nil)))
              ((null start) (setq start i)))))
    (nreverse tokens)))

(cl:defun brick-index (name)
  "1..n for a name CYTO-BRICKn, else nil."
  (let ((prefix "CYTO-BRICK"))
    (and (> (length name) (length prefix))
         (string-equal prefix name :end2 (length prefix))
         (every #'digit-char-p (subseq name (length prefix)))
         (parse-integer name :start (length prefix)))))

(cl:defun check-solution (output problem)
  "Check the decomposition printed in OUTPUT for PROBLEM = (target b1 .. b5).
Return (values valid-p reason expression)."
  (let ((target (car problem))
        (bricks (cdr problem))
        (derivations (make-hash-table :test #'equalp)) ; name -> (op a va b vb)
        (claimed (make-hash-table :test #'equalp))     ; name -> printed value
        (used (make-hash-table :test #'equalp))
        root)
    (catch 'invalid
      (flet ((fail (fmt &rest args)
               (throw 'invalid (values nil (apply #'format nil fmt args) nil))))
        (let ((done (search "Done :" output)))
          (unless done (fail "no \"Done :\" in the output"))
          ;; Parse every "Operation ..." paragraph after "Done :".
          (let ((tokens (solution-tokens (subseq output (+ done 6)))))
            (loop while tokens do
              (cond
                ((string-equal (car tokens) "Operation")
                 (let ((p tokens))
                   (flet ((next () (cl:if p (pop p) (fail "truncated operation paragraph")))
                          (expect (word)
                            (let ((got (cl:if p (pop p) "")))
                              (unless (string-equal got word)
                                (fail "expected ~s, got ~s" word got))))
                          (int (s)
                            (let ((v (ignore-errors
                                      (let ((*read-eval* nil)) (read-from-string s)))))
                              (cl:if (integerp v) v (fail "not an integer value: ~s" s)))))
                     (expect "Operation")
                     (let ((op (next)))
                       (expect "has") (expect "been") (expect "applied") (expect "to")
                       (let* ((a (next)) (va (int (next))))
                         (expect "and") (expect "to")
                         (let* ((b (next)) (vb (int (next))))
                           (expect "to") (expect "get")
                           (let ((r (next)))
                             (when (gethash r derivations)
                               (fail "~a is derived twice" r))
                             (unless root (setq root r))
                             (setf (gethash r derivations) (list op a va b vb))
                             (loop for (n v) in (list (list a va) (list b vb)) do
                               (multiple-value-bind (old found) (gethash n claimed)
                                 (when (and found (/= old v))
                                   (fail "~a printed as both ~a and ~a" n old v))
                                 (setf (gethash n claimed) v))))))))
                   (setq tokens p)))
                (t (pop tokens)))))
          (unless root (fail "no operation after \"Done :\""))
          ;; Rebuild the expression from the root, which must be the target.
          (labels ((expand (name value depth)
                     (when (> depth 20) (fail "cycle through ~a" name))
                     (let ((i (brick-index name)))
                       (cond
                         (i
                          (unless (<= 1 i (length bricks))
                            (fail "~a: there are only ~a bricks" name (length bricks)))
                          (unless (= value (nth (1- i) bricks))
                            (fail "~a printed as ~a but brick ~a is ~a"
                                  name value i (nth (1- i) bricks)))
                          (when (gethash name used) (fail "~a is used twice" name))
                          (setf (gethash name used) t)
                          (format nil "~a" value))
                         (t
                          (let ((d (gethash name derivations)))
                            (unless d
                              (fail "~a (~a) is used but never derived, and is not a brick"
                                    name value))
                            (destructuring-bind (op a va b vb) d
                              (let* ((plus (and (>= (length op) 4)
                                                (string-equal "PLUS" op :end2 4)))
                                     (times (and (>= (length op) 5)
                                                 (string-equal "TIMES" op :end2 5)))
                                     (form
                                       (cond
                                         ((and plus (= value (+ va vb))) '(:a "+" :b))
                                         ((and plus (= value (- va vb))) '(:a "-" :b))
                                         ((and plus (= value (- vb va))) '(:b "-" :a))
                                         ((and times (= value (* va vb))) '(:a "x" :b))
                                         ((and times (/= vb 0) (= value (cl:/ va vb)))
                                          '(:a "/" :b))
                                         ((and times (/= va 0) (= value (cl:/ vb va)))
                                          '(:b "/" :a))
                                         ((not (or plus times))
                                          (fail "unknown operation ~a" op))
                                         (t (fail "~a on ~a and ~a cannot give ~a = ~a"
                                                  op va vb name value)))))
                                (let ((ea (expand a va (1+ depth)))
                                      (eb (expand b vb (1+ depth))))
                                  (flet ((side (k)
                                           (let ((e (cl:if (eq k :a) ea eb))
                                                 (n (cl:if (eq k :a) a b)))
                                             (cl:if (brick-index n) e (format nil "(~a)" e)))))
                                    (format nil "~a ~a ~a"
                                            (side (first form)) (second form)
                                            (side (third form)))))))))))))
            (values t nil (format nil "~a = ~a" target (expand root target 0)))))))))
