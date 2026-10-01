;; Jun 24 08:56 1987  init.l Page 1

(defvar *print-array*)
(defvar *coderack*)
(defvar *current-target*)
(defvar %initial-activation% 0.0)
(defvar %min-activation-to-be-added% 24.0)
(defvar %max-activation-to-be-transmitted% 35.0)
(defvar %k% 0.001)
(defvar %length% 1000)
(defvar %target-activation% 180)
(defvar %brick-activation% 60)
(defvar %dtarget-activation% 100)
(defvar %block-activation% 30)
(defvar %target-plus% 75)
(defvar %brick-plus% 25)
(defvar %dtarget-plus% 50)
(defvar %node-minus% -25)
(defvar %similar% 50)
(defvar %operation% 200)
(defvar %instance% 200)
(defvar %verbose% nil)
(defvar %graphics% nil)
(defvar %first-decay-rate% 0.50)
(defvar %second-decay-rate% 0.50)
(defvar %third-decay-rate% 0.70)
(defvar %fourth-decay-rate% 0.90)
(defvar %fifth-decay-rate% 0.90)
(defvar %sixth-decay-rate% 0.0)
(defvar %upper-threshold% 90)
(defvar %first-threshold% 30)
(defvar %upper-urgency% 600)
(defvar %first-urgency% 300)
(defvar %second-urgency% 70)
(defvar %third-urgency% 7)
(defvar %fourth-urgency% 4)
(defvar %fifth-urgency% 1)
(defvar %temperature-threshold% 80)
(defvar *name-counter* 1)

;Initialization of the pnet
(defun init-chiffre ()
    (setq *print-array* nil) ; don't print circular vectors
    (setq %initial-activation% 0.0)
    (setq %min-activation-to-be-added% 24.0)
    (setq %max-activation-to-be-transmitted% 35.0)
    (setq %k% 0.001)
    (setq %length% 1000)
    (setq %target-activation% 170)
    (setq %brick-activation% 60)
    (setq %dtarget-activation% 130)
    (setq %block-activation% 30)
    (setq %target-plus% 75)
    (setq %brick-plus% 40)
    (setq %dtarget-plus% 30)
    (setq %node-minus% -25)
    (setq %similar% 50)
    (setq %operation% 200)
;;
;; Jun 24 08:56 1987  init.l Page 2
;;
    (setq %instance% 200)
    (setq %verbose% nil)
    (if (> (string-length (getenv "WINDOW_GFX")) 0)
        (setq %graphics% t)
        (setq %graphics% nil))
    (if %graphics%
        (format t "~&Graphics is ON.~%")
        (format t "~&Graphics is OFF.~%"))
    (setq %first-decay-rate% 0.50)
    (setq %second-decay-rate% 0.50)
    (setq %third-decay-rate% 0.70)
    (setq %fourth-decay-rate% 0.90)
    (setq %fifth-decay-rate% 0.90)
    (setq %sixth-decay-rate% 0.0)
    (setq %upper-threshold% 60)
    (setq %first-threshold% 30)
    (setq %temperature-threshold% 200)
    (initialize-pnet-2)
    (compile-flavor-methods pnode)
;Here the nodes which represent the link-types have received a pseudo
;instantiation in the cyto. Those nodes are not going to spread their
;activity. But they are not going to lose their level of activity as
;well because their decay-rate will be 0.0. This means that they will
;keep at least the activity they have at the beginning during the whole
;game
    (setq %upper-urgency% 600)
    (setq %first-urgency% 300)
    (setq %second-urgency% 150)
    (setq %third-urgency% 7)
    (setq %fourth-urgency% 4)
    (setq %fifth-urgency% 1)
    (create-coderack)
    (setq *name-counter* 1))

(defun quick ()
  (let (r)
    (setq r (cr-choose *coderack*))
    (print r) (terpri)
    (eval r)))

;REACTIVATE-CYTO FUNCTION
(defun reactivate-cyto ()
  (declare (special cyto-target cyto-brick1 cyto-brick2 cyto-brick3
                    cyto-brick4 cyto-brick5 *current-target*))
  (let (pnode)
    (setq pnode (car (send cyto-target :plinks)))
    (send (eval pnode) :add-activation %target-plus%)
    (setq pnode (car (send cyto-brick1 :plinks)))
    (send (eval pnode) :set-activation %brick-plus%)
    (setq pnode (car (send cyto-brick2 :plinks)))
    (send (eval pnode) :set-activation %brick-plus%)
    (setq pnode (car (send cyto-brick3 :plinks)))
    (send (eval pnode) :set-activation %brick-plus%)
    (setq pnode (car (send cyto-brick4 :plinks)))
    (send (eval pnode) :set-activation %brick-plus%)
    (setq pnode (car (send cyto-brick5 :plinks)))
;;
;; Jun 24 08:56 1987  init.l Page 3
;;
    (send (eval pnode) :set-activation %brick-plus%)
    (setq pnode (car (send (eval (send *current-target* :name))
                          :plinks)))
    (if pnode (send (eval pnode) :add-activation %dtarget-plus%))
    (repump)))

;REFRESH-EVERYTHING FUNCTION
(defun refresh-everything ()
    (declare (special *iteration* *coderack* %third-urgency% %graphics% *pnet*))
;Loads new codelets on the coderack (random associations)
;Updates the interest of the blocks in the cytoplasm and
;checks the temperature
;Updates the pnet by spreading the activation
;Loads codelets coming from the Pnet
  (let (nn)
    (cr-hang *coderack* '(look-for-new-block) %fourth-urgency%)
    (decrease-interest) (check-temperature)
    (spread-activation-in-pnet)
    (when %graphics%
       (update-pnet-display *pnet*)
       (setq nn (get-pname (concat 'gr *iteration*)))
       (dump-window nn))
    (populate-coderack)))
