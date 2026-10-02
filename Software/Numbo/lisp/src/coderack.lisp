;;; coderack.lisp -- the Coderack.
;;;
;;; RECONSTRUCTED: none of the 1987 printout's files define the cr-
;;; functions (see ../numbo-digitized/README.md).  Everything here
;;; is rebuilt from the chapter (Defays, "Numbo: A
;;; Study in Cognition and Recognition"), and from the call sites in the
;;; ported source.
;;;
;;; What the chapter says:
;;;   p.135  "all processing is carried out by codelets, which are
;;;          probabilistically selected from the Coderack.  Each codelet,
;;;          when placed on the Coderack, is assigned an urgency, and its
;;;          likelihood of being chosen is proportional to its urgency."
;;;          A codelet stays on the Coderack "until chosen to run".
;;;   p.143  "its chance of being the next selected to run is, at any time,
;;;          proportional to its urgency (specifically, it is the ratio of
;;;          the codelet's urgency to the sum of all the urgencies of all the
;;;          codelets in the Coderack)."
;;;   p.150  Task selection is two-phase: once when a codelet is loaded onto
;;;          the Coderack, and again when it is chosen from the Coderack to
;;;          be run.
;;;
;;; What the call sites say:
;;;   codelets.lisp CREATE-CODERACK (defined there, not here):
;;;       (cr-make-coderack 'my-coderack
;;;          (list %upper-urgency% %first-urgency% %second-urgency%
;;;                %third-urgency% %fourth-urgency% %fifth-urgency% 0))
;;;       (setq *coderack* 'my-coderack)
;;;     A coderack is named by a symbol, and the cr- functions get that
;;;     symbol.  It is made with a list of urgency levels, which with
;;;     init-chiffre's values is (600 300 150 7 4 1 0).
;;;   (cr-hang *coderack* form urgency): urgency is always one of those
;;;     levels.  It is either a %...-urgency% global directly, or a value
;;;     taken from them (find-interest-in-pnet, look-for-diff, and the pnode
;;;     :codelet-urgency method, which returns its base urgency unchanged).
;;;     find-interest-in-pnet can return 0.
;;;   (eval (cr-choose *coderack*)): returns the codelet form, which the
;;;     caller evals.  start.lisp checks (cr-empty? *coderack*) before
;;;     choosing.
;;;   (cr-empty-coderack *coderack*): start.lisp clears the rack before
;;;     reading the target, and again when the temperature stays high.
;;;   start.lisp, commented out:  (cr-choose *coderack* t), next to
;;;     ";  (print rescod) (terpri)" / ";  (eval (car rescod))".  So with a
;;;     true second argument, the caller gets back a list whose car is the
;;;     form.  Here that list is (form urgency).
;;;
;;; How this works: as in the Jumbo/Copycat coderacks the chapter refers to,
;;; the rack is a list of urgency bins, one per level.  A bin is chosen
;;; with probability (level * codelets in bin) / (sum over all bins), then a
;;; codelet is chosen uniformly inside that bin.  So each codelet's chance is
;;; its urgency over the total urgency, as on p.143.  The chosen codelet is
;;; removed.
;;;
;;; Choices the sources don't settle (RECONSTRUCTED, logged in
;;; PORTING_NOTES.md):
;;;   - Urgency-0 codelets (the 0 level) are never chosen while any codelet
;;;     with positive urgency is on the rack (p.143's ratio is 0).  If only
;;;     urgency-0 codelets are left, one is chosen uniformly.  That keeps
;;;     cr-choose consistent with cr-empty?, since start.lisp only calls it
;;;     on a non-empty rack.
;;;   - An urgency that isn't one of the rack's levels is an error, not
;;;     silently rounded.  The source never does this, so if it happens it
;;;     is a porting bug.
;;;   - Randomness comes from CL RANDOM, i.e. *RANDOM-STATE*, the same
;;;     generator the codelets use with (random n).  Seed it with
;;;     (setq *random-state* (sb-ext:seed-random-state n)) for reproducible
;;;     runs.

(in-package :numbo)

(defstruct (coderack (:constructor %make-coderack (name bins)))
  name
  ;; list of (urgency . codelet-forms), in the order given to cr-make-coderack
  bins)

(defun cr-get (name)
  ;; The coderack named NAME.
  (if (coderack-p name)
         name
         (or (get name 'coderack)
                (error "No coderack named ~s" name))))

(defun cr-make-coderack (name urgencies)
  ;; Makes an empty coderack with one bin per urgency level and names it
  ;; NAME.  Returns NAME.
  (dolist (u urgencies)
    (unless (and (realp u) (>= u 0))
      (error "cr-make-coderack: bad urgency level ~s" u)))
  (setf (get name 'coderack)
        (%make-coderack name
                        (mapcar (lambda (u) (list u))
                                   (remove-duplicates urgencies :from-end t))))
  name)

(defun cr-hang (name form urgency)
  ;; Posts the codelet FORM on the coderack with URGENCY.
  (let ((bin (assoc urgency (coderack-bins (cr-get name)) :test #'eql)))
    (unless bin
      (error "cr-hang: urgency ~s is not a level of coderack ~s ~s"
             urgency name (mapcar #'car (coderack-bins (cr-get name)))))
    (push form (cdr bin))
    form))

(defun cr-count (name)
  ;; Number of codelets on the coderack.
  (loop for bin in (coderack-bins (cr-get name)) sum (length (cdr bin))))

(defun cr-empty? (name)
  (zerop (cr-count name)))

(defun cr-empty-coderack (name)
  ;; Removes every codelet from the coderack.
  (dolist (bin (coderack-bins (cr-get name)))
    (setf (cdr bin) nil))
  name)

(defun cr-choose (name &optional full)
  ;; Removes a codelet chosen at random, weighted by urgency (p.143), and
  ;; returns its form, or (form urgency) if FULL.  Returns nil if the rack
  ;; is empty.
  (let* ((bins (remove-if-not #'cdr (coderack-bins (cr-get name))))
         (total (loop for bin in bins sum (* (car bin) (length (cdr bin)))))
         bin)
    (cond ((null bins) nil)
          (t
           (if (zerop total)
                  ;; only urgency-0 codelets are left
                  (setq bin (car bins))
                  (let ((r (random total)))
                    (loop for b in bins
                          do (decf r (* (car b) (length (cdr b))))
                          when (< r 0) do (setq bin b) (return))))
           (let* ((i (random (length (cdr bin))))
                  (form (nth i (cdr bin))))
             (setf (cdr bin) (append (subseq (cdr bin) 0 i)
                                     (nthcdr (1+ i) (cdr bin))))
             (if full (list form (car bin)) form))))))

