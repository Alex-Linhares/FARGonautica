;;; pnet.lisp -- write python/fixtures/pnet.json from the oracle.
;;;
;;; Run from anywhere: sbcl --non-interactive --load tests/oracle/pnet.lisp
;;; (or python/scripts/regen_fixtures.sh).
;;;
;;; The Pnet as src/pnet-def.lisp defines it, read back from the loaded
;;; oracle (python/tests/test_pnet_def.py compares python/numbo/pnet_def.py
;;; with it, field by field and in order):
;;;
;;;   slots     the 19 pnode instance variables, in defflavor order (downcased)
;;;   holders   the 91 variables init-pnet SETQs, in init-pnet's order, each
;;;             {"holder": name, "pnode": {slot: value, ...}}, with the slots
;;;             in defflavor order and the values as init-pnet left them
;;;             (neighbors and codelets are still the quoted symbol lists)
;;;   pnet      the 88 *pnet* pnodes, in order, as the names of their holders
;;;   parameters  the init.lisp globals the Pnet's codelets and pnet-functions
;;;             read: {"defvar": {...}, "init-chiffre": {...}}, i.e. their
;;;             values after loading and after (init-chiffre)
;;;   config-start  each *pnet* pnode after (init-chiffre) and the forms of
;;;             start.lisp's config that come before (init-cytoplasm ...)
;;;             (initialize-pnet and the 5g/6g pseudo-instances): activation,
;;;             instances, activation-decay-factor, neighbors (now pnodes,
;;;             written as their holders' names), and codelets with the
;;;             threshold and urgency evaluated
;;;   print-pnet-all, print-pnet  what those two pnet-def functions print in
;;;             the config-start state, with node-5, plus2-3 and operation set
;;;             to 50.5, 24.0 and 7 (print-pnet's threshold is 24, so plus2-3
;;;             is on the boundary of its >)
;;;
;;; Values use the trace's Lisp-data encoding (src/oracle.lisp): a symbol is
;;; its name, a string is {"str": ...}, nil is null.
;;;
;;; This file is READ with SBCL's default float format (single), so every
;;; float literal below is written with d0.

(load (merge-pathnames "common.lisp" *load-truename*))

(in-package :numbo)

(cl:defun pnet-fixture-source-forms (file)
  "Every top-level form of src/FILE, read as numbo-load reads it."
  (let ((*package* (cl:find-package :numbo))
        (*read-default-float-format* 'double-float))
    (with-open-file (in (merge-pathnames (concatenate 'string "src/" file)
                                         cl-user::*oracle-repo*))
      (cl:loop for form = (read in nil in)
               until (eq form in)
               collect form))))

(cl:defun pnet-fixture-defun-body (file name)
  (cddr (cdr (cl:find-if (cl:lambda (f) (and (consp f) (eq (car f) 'defun)
                                             (eq (cadr f) name)))
                         (pnet-fixture-source-forms file)))))

(cl:defparameter *pnet-fixture-holders*
  (cl:loop for form in (pnet-fixture-defun-body "pnet-def.lisp" 'init-pnet)
           do (assert (and (consp form) (eq (car form) 'setq) (= (length form) 3)))
           collect (cadr form))
  "The holders init-pnet SETQs, in its order.")

(assert (= (length *pnet-fixture-holders*) 91))
(assert (= (length (symbol-value '*pnet*)) 88))

(cl:defparameter *pnet-fixture-slots*
  (cl:mapcar #'sb-mop:slot-definition-name
             (sb-mop:class-direct-slots (cl:find-class 'pnode))))

(assert (= (length *pnet-fixture-slots*) 19))

(cl:defun pnet-fixture-holder-of (pnode)
  "The holder whose value is PNODE (eq)."
  (or (cl:find-if (cl:lambda (h) (eq (symbol-value h) pnode)) *pnet-fixture-holders*)
      (error "no holder for ~s" pnode)))

(cl:defun pnet-fixture-key (x)
  (with-output-to-string (s) (oracle-write-json-string x s)))

(cl:defun pnet-fixture-object (pairs)
  "PAIRS ((key . json-text) ...) as a JSON object."
  (format nil "{~{~a~^,~}}"
          (cl:loop for (k . v) in pairs
                   collect (format nil "~a:~a" (pnet-fixture-key k) v))))

(cl:defun pnet-fixture-pnode-slots (pnode)
  (pnet-fixture-object
   (cl:loop for slot in *pnet-fixture-slots*
            collect (cons (string-downcase (symbol-name slot))
                          (oracle-json-string (slot-value pnode slot))))))

(cl:defparameter *pnet-fixture-holders-json*
  (cl:loop for h in *pnet-fixture-holders*
           collect (pnet-fixture-object
                    (list (cons "holder" (oracle-json-string h))
                          (cons "pnode" (pnet-fixture-pnode-slots (symbol-value h)))))))

(cl:defparameter *pnet-fixture-pnet-json*
  (oracle-json-string (cl:mapcar #'pnet-fixture-holder-of (symbol-value '*pnet*))))

(cl:defparameter *pnet-fixture-parameters*
  '(%initial-activation% %min-activation-to-be-added%
    %max-activation-to-be-transmitted% %k% %length%
    %first-decay-rate% %second-decay-rate% %third-decay-rate%
    %fourth-decay-rate% %fifth-decay-rate% %sixth-decay-rate%
    %upper-threshold% %first-threshold%
    %upper-urgency% %first-urgency% %second-urgency% %third-urgency%
    %fourth-urgency% %fifth-urgency%))

(cl:defun pnet-fixture-parameters-json ()
  (pnet-fixture-object
   (cl:loop for p in *pnet-fixture-parameters*
            collect (cons (symbol-name p) (oracle-json-string (symbol-value p))))))

(cl:defparameter *pnet-fixture-defvar-json* (pnet-fixture-parameters-json))

;;; From here on the Pnet is changed: init-chiffre runs initialize-pnet-2.
(let ((*standard-output* (make-broadcast-stream)))
  (init-chiffre))

(cl:defparameter *pnet-fixture-init-chiffre-json* (pnet-fixture-parameters-json))

(cl:defparameter *pnet-fixture-config-forms*
  (cl:loop for form in (pnet-fixture-defun-body "start.lisp" 'config)
           until (and (consp form) (eq (car form) 'init-cytoplasm))
           collect form)
  "config's forms before (init-cytoplasm ...).")

(assert (= (length *pnet-fixture-config-forms*) 10))
(cl:dolist (form *pnet-fixture-config-forms*) (eval form))

(cl:defun pnet-fixture-config-start (pnode)
  (pnet-fixture-object
   (list (cons "holder" (oracle-json-string (pnet-fixture-holder-of pnode)))
         (cons "activation" (oracle-json-string (send pnode :activation)))
         (cons "instances" (oracle-json-string (send pnode :instances)))
         (cons "activation-decay-factor"
               (oracle-json-string (send pnode :activation-decay-factor)))
         (cons "neighbors"
               (oracle-json-string
                (cl:loop for (n l) in (send pnode :neighbors)
                         collect (list (pnet-fixture-holder-of n)
                                       (pnet-fixture-holder-of l)))))
         (cons "codelets"
               (oracle-json-string
                (cl:loop for (fn threshold urgency args) in (send pnode :codelets)
                         collect (list fn (eval threshold) (eval urgency) args)))))))

(cl:defparameter *pnet-fixture-config-start-json*
  (cl:mapcar #'pnet-fixture-config-start (symbol-value '*pnet*)))

(set-up-activations '(node-5 50.5d0 plus2-3 24.0d0 operation 7))

(cl:defparameter *pnet-fixture-print-pnet-all*
  (with-output-to-string (*standard-output*) (print-pnet-all)))

(cl:defparameter *pnet-fixture-print-pnet*
  (with-output-to-string (*standard-output*) (print-pnet 24)))

(cl-user::write-fixture
 "pnet.json"
 (format nil "{~a:~a,~%~a:[~%~{~a~^,~%~}~%],~%~a:~a,~%~a:{~a:~a,~%~a:~a},~%~a:[~%~{~a~^,~%~}~%],~%~a:~a,~%~a:~a}~%"
         (pnet-fixture-key "slots")
         (format nil "[~{~a~^,~}]"
                 (cl:mapcar (cl:lambda (s) (pnet-fixture-key (string-downcase (symbol-name s))))
                            *pnet-fixture-slots*))
         (pnet-fixture-key "holders") *pnet-fixture-holders-json*
         (pnet-fixture-key "pnet") *pnet-fixture-pnet-json*
         (pnet-fixture-key "parameters")
         (pnet-fixture-key "defvar") *pnet-fixture-defvar-json*
         (pnet-fixture-key "init-chiffre") *pnet-fixture-init-chiffre-json*
         (pnet-fixture-key "config-start") *pnet-fixture-config-start-json*
         (pnet-fixture-key "print-pnet-all")
         (pnet-fixture-key *pnet-fixture-print-pnet-all*)
         (pnet-fixture-key "print-pnet")
         (pnet-fixture-key *pnet-fixture-print-pnet*)))
