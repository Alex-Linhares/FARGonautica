;;; copystat-tests.lisp -- Mitchell's batch tool (copystat.l) runs, writes
;;; its three files, and copystat-summary reads them back; the saved random
;;; state replays a run.  Exit 0 = pass.

(in-package :cl-user)
(load (merge-pathnames "../load.lisp"
                       (make-pathname :name nil :type nil :defaults *load-truename*)))

(defvar *failures* 0)
(defun check (name ok)
  (if ok (format t "  ok   ~a~%" name)
      (progn (incf *failures*) (format t "  FAIL ~a~%" name))))

(let* ((dir (format nil "/tmp/ccat-copystat-test-~a/" (get-universal-time)))
       (*standard-output* (make-broadcast-stream)))
  (ensure-directories-exist dir)
  (copycat:copystat 'abc 'abd 'ijk 3 dir)
  (let ((*standard-output* sb-sys:*stdout*))
    (check "copystat wrote .data, .verbose-data and .random-state"
           (and (probe-file (concatenate 'string dir "ABC-ABD-IJK.data"))
                (probe-file (concatenate 'string dir "ABC-ABD-IJK.verbose-data"))
                (probe-file (concatenate 'string dir "ABC-ABD-IJK.random-state"))))
    (check "the .random-state file holds three states"
           (= 3 (length (with-open-file (s (concatenate 'string dir "ABC-ABD-IJK.random-state"))
                          (read s)))))
    (copycat:copystat-summary "ABC-ABD-IJK" dir)
    (check "copystat-summary wrote .summary"
           (probe-file (concatenate 'string dir "ABC-ABD-IJK.summary")))
    ;; Replay run 2 from its saved state, as Mitchell did with get-random-state.
    (let* ((state (copycat::get-random-state 1 "ABC-ABD-IJK" dir))
           (data (with-open-file (s (concatenate 'string dir "ABC-ABD-IJK.data"))
                   (let ((text (make-string (file-length s))))
                     (subseq text 0 (read-sequence text s)))))
           ;; the .data file is one line per run, the first preceded by the
           ;; "(" that opens the list copystat-summary closes and reads
           (run-2-line (string-right-trim (list #\Newline) (second (sb-unicode:lines data)))))
      (copycat:init-ccat 'abc 'abd 'ijk :random-state state :no-graphics t)
      (check (format nil "replaying run 2 from its saved random state gives its line again: ~a" run-2-line)
             (string= run-2-line
                      (format nil "(~a ~a ~a ~a ~a ~a ~a ~a)"
                              (copycat::send copycat:*answer-string* :pstring)
                              copycat:*temperature* copycat:*codelet-count*
                              copycat:*snag-count*
                              copycat::*single-letter-group-count*
                              copycat::*single-letter-group-at-end-count*
                              copycat::*length-description-count*
                              copycat::*length-relevant-at-end*))))
    (sb-ext:delete-directory dir :recursive t)))

(format t "~a failure~:p~%" *failures*)
(sb-ext:exit :code (if (zerop *failures*) 0 1))
