;;; run-tests.lisp -- seeded runs of the six classic problems finish with an
;;; answer and no Lisp error; the same seed gives the same run twice; a
;;; snag run (xyz) finishes.  Exit 0 = pass.

(in-package :cl-user)
(load (merge-pathnames "../load.lisp"
                       (make-pathname :name nil :type nil :defaults *load-truename*)))

(defvar *failures* 0)
(defun check (name ok)
  (if ok (format t "  ok   ~a~%" name)
      (progn (incf *failures*) (format t "  FAIL ~a~%" name))))

(dolist (problem '(("abc" "abd" "ijk") ("abc" "abd" "xyz") ("abc" "abd" "iijjkk")
                   ("abc" "abd" "kji") ("abc" "abd" "mrrjjj") ("abc" "abd" "xyd")))
  (dolist (seed '(1 2 3))
    (let ((r (handler-case
                 (apply #'copycat:run (append problem (list :seed seed :stream nil
                                                            :timeout 300)))
               (error (c) (list :error c)))))
      (check (format nil "~{~a~^ ~} seed ~a -> ~a (~a codelets, T=~a)"
                     problem seed (getf r :answer) (getf r :codelets)
                     (getf r :temperature))
             (and (getf r :answer)
                  (stringp (getf r :answer))
                  (plusp (length (getf r :answer)))
                  (not (getf r :timed-out))
                  (integerp (getf r :codelets))
                  (plusp (getf r :codelets))
                  (not (string= (getf r :rule) "none")))))))

;; Pinned facts about one run, so a change in the layer that alters the
;; course of a run is noticed (SBCL's RANDOM is stable across versions).
(let ((r (copycat:run "abc" "abd" "ijk" :seed 1 :stream nil)))
  (check "abc abd ijk seed 1 is ijl in 389 codelets at temperature 13"
         (and (string= (getf r :answer) "ijl")
              (= (getf r :codelets) 389)
              (= (getf r :temperature) 13))))

(let ((a (copycat:run "abc" "abd" "xyz" :seed 2 :stream nil))
      (b (copycat:run "abc" "abd" "xyz" :seed 2 :stream nil)))
  (check "the same seed reproduces a run (xyz seed 2, with snags)"
         (and (equal a b) (plusp (getf a :snags)))))

(check "the problem can be given as symbols too"
       (string= (getf (copycat:run 'abc 'abd 'ijk :seed 1 :stream nil) :answer)
                "ijl"))

(format t "~a failure~:p~%" *failures*)
(sb-ext:exit :code (if (zerop *failures*) 0 1))
