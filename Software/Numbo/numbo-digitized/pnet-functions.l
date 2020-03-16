;; May  5 14:02 1987  pnet-functions.l Page 1

  (declare (macros t))

;ACTIVATION-DECAY METHOD
(defmethod (pnode :activation-decay) ()
; Computes and returns the activation decay of the pnode.
   (times (my :activation-decay-factor) (my :activation)))

;ACTIVATION-DECAY-FACTOR METHOD
(defmethod (pnode :activation-decay-factor) ()
; The decay is linked to the type of instance in the cyto
  (let ((s (caar (sortcar (my :instances) ()))))
   (cond ((equal s "1t") %first-decay-rate%)
         ((equal s "2b") %second-decay-rate%)
         ((equal s "3dt") %third-decay-rate%)
         ((equal s "5g") %fifth-decay-rate%)
         ((equal s "6g") %sixth-decay-rate%)
         (t %fourth-decay-rate%))))

;ADD-ACTIVATION METHOD
;This method adds the given amount of activation to the pnode
; (if the amount to be added is above the %min-activation-to-be-added%
; threshold).
;The amount can be negative. In that case, the activation is
;decreased, but can never be negative.
(defmethod (pnode :add-activation) (act)
    (if (> (abs act) %min-activation-to-be-added%)
     then (setq activation (max 0(plus activation act)))))

;ADD-TEMP-ACTIVATION-HOLDER METHOD
(defmethod (pnode :add-temp-activation-holder) (act)
    (send self :set-temp-activation-holder
               (plus (my :temp-activation-holder) act)))

;CODELET-URGENCY METHOD
(defmethod (pnode :codelet-urgency) (base-urgency)
; Calculates what the urgency of this codelet should be, given the
; base urgency of the codelet.
   base-urgency)

;HOTTER-NEIGHBOR-ACTIVATION METHOD
(defmethod (pnode :hotter-neighbor-activation) ()
;this method returns the activation of the hottest neighbor which
;is not a brick
    (let ((maxi 0) act)
         (loop for link in (my :neighbors) do
;Here link is of the form (node link-type)
  (setq node (car link))
  (cond
       ((eq (cadr link)  similar) nil)
       (t
          (setq act (send (car link) :activation))
          (if (> act maxi) then (setq maxi act)))))
         maxi))

;INITIALIZE-CODELET FUNCTION
;;
;; May  5 14:02 1987  pnet-functions.l Page 2
;;
(defun initialize-codelet ()
; Loops through the nodes in the pnet, examining codelets of
; all the nodes. If a codelets variable contains a codelet with a
; corresponding threshold nequal to %first-threshold% this threshold
; is replaced by %first-threshold%.
   (let ((x 0))
    (loop for pnode in *pnet* do
          (loop for codelet in (send pnode :codelets) do
                (let ((codelet-call (car codelet))
                      (threshold (cadr codelet))
                      (base-urgency (caddr codelet))
                      (arguments (cadddr codelet)))
                (if (nequal (eval threshold)  (eval %first-threshold%))
                 then
                     (send pnode :modify-threshold codelet-call
                              '%first-threshold%)))))))

;INITIALIZE-PNET FUNCTION
(defun initialize-pnet ()
  (loop for pnode in *pnet* do
     (send pnode :set-activation %initial-activation%)
     (send pnode :set-temp-activation-holder 0.0)
     (send pnode :set-spreadable-activation 0.0)
     (send pnode :set-instances nil))
  (initialize-codelet))

;INITIALIZE-PNET-2 FUNCTION
; Now evaluate all the atoms in the :neighbors
(defun initialize-pnet-2 ()
  (loop for pnode in *pnet* do
    (send pnode :set-neighbors
          (loop for pair
                in (send pnode :neighbors)
                collect (list (eval (car pair)) (eval (cadr pair)))))))

;LINK-LENGTH METHOD
(defmethod (pnode :link-length) (&optional (k O.1))
; Computes and returns the length of the links associated with this pnode.
; The length of the link is inversely proportional to the activation.
; When activation is 0, then the link-length is %length%, when activation
; is infinite, it is 1. The minimum link-length is 1 because we
; are going to calculate the amount of "radiated energy" passed
; from one node to its neighbors as an inverse square law, i.e.
; the activation passed will be the decayed activation divided
; by the square of the link-length between the nodes. The function
; used is f[x] = 1 + 1/[kx + 1/%length%] where %k% is a constant which
; depends on the link.
    (plus 1 (quotient (float 1) (plus (times %k% (my :activation))
                                      (quotient (float 1) %length%)))))

;MODIFY-THRESHOLD METHOD
(defmethod (pnode :modify-threshold) (codelet-call &optional (new nil))
;Replaces the threshold of a given codelet in the codelets variable
;of a pnode by a new threshold which is a function.
;This new threshold starts with a value equal to %upper-threshold%
;and diminishes with time.
;;
;; May  5 14:02 1987  pnet-functions.l Page 3
;;
;Every basic iteration (x counter) the value is decreased by 1.
 (declare (special *iteration*))
 (let ((res nil) (function nil))
      (if (null new)
      (setq new (list 'max %first-threshold% (list 'add *iteration*
                                                   '(minus *iteration*)
                                                   %upper-threshold%))))
      (loop for codelet in codelets do
           (setq function (car codelet))
           (if (equal codelet-call function)
                then
            (setq codelet (cons function (cons new (cddr codelet)))))
           (setq res (cons codelet res)))
      (setq codelets res)))

;POPULATE-CODERACK FUNCTION
(defun populate-coderack ()
; Loops through the nodes in the pnet, examining activations of
; all the nodes. Whichever nodes exceed the threshold activation get
; to post their codelet(s), whose urgency is a function of the activation
; of the node. Note: the value of the pnode slot :CODELETS is a list
; of triples of the form (function-call threshold-activation base-urgency)
; where the threshold-activation value is the one used in determining if
; that particular codelet should be posted. The urgency is calculated using
; base-urgency.
    (loop for pnode in *pnet* do
          (loop for codelet in (send pnode :codelets) do
                (let ((codelet-call (car codelet))
                      (threshold (cadr codelet))
                      (base-urgency (caddr codelet))
                      (arguments (cadddr codelet)))
                (if (>= (send pnode :activation) (eval threshold))
                 then (if %verbose%
                       then (format t "About to post codelet ~a ~a~&"
                                    codelet-call arguments))
                      (cr-hang *coderack*
                        (append (list codelet-call) arguments)
                        (send pnode :codelet-urgency (eval base-urgency)))
                        (send pnode :modify-threshold codelet-call
                              ))))))

;PRINT METHOD
(defmethod (pnode :print) ()
  (format t "I am a pnode.~&")
  (format t "name: ~a~&" (my :name))
  (format t "instances: ~a~&" (my :instances))
  (format t "activation: ~a~&" (my :activation))
  (format t "spreadable-activation: ~a~&" (my :spreadable-activation))
  (format t "temp-activation-holder: ~a~&" (my :temp-activation-holder))
  (format t "neighbors: ~a~&"
                (my :neighbors)))

;SET-UP-ACTIVATIONS FUNCTION
(defun set-up-activations (list-of-pnodes-and-activations)
; Sets up activations as in the list given in the argument. This is a list
;;
;; May  5 14:02 1987  pnet-functions.l Page 4
;;
; of the form (node1 act1 node2 act2 . . .)
    (loop until (null list-of-pnodes-and-activations) do
        (send (eval (car list-of-pnodes-and-activations))
              :set-activation (cadr list-of-pnodes-and-activations))
        (setq list-of-pnodes-and-activations
              (cddr list-of-pnodes-and-activations))))

;SPREAD-ACTIVATION METHOD
(defmethod (pnode :spread-activation) ()
; Spreads activation to all nodes the given node is linked to.
; The amount of activation spread along each link is
;               :spreadable-activation / (:link-length ^ 2)
; where :spreadable-activation is d * (activation of node doing the
; spreading).
; where d is the decay factor of the node doing the spreading.

   (loop for link in (my :neighbors) do
       ; here, link is of the form (node link-type)
       (let (linked-node link-type link-length)
          (setq linked-node (car link))
          (setq link-type (cadr link))
          (setq link-length (send link-type :link-length))
          ; give linked-node new activation (store in temporary holder)
          (send linked-node :add-temp-activation-holder
                            (quotient (my :spreadable-activation)
                                      (sqrt link-length))))))

;SPREAD-ACTIVATION-IN-PNET METHOD
(defun spread-activation-in-pnet ()
; Causes each node to decay and then
; Spreads activation throughout the pnet. All the
; nodes spread activation in "parallel" and the spread
; of activation is "instantaneous". This is simulated as
; follows: For each node, the amount of spreadable activation is computed
; and put in the :spreadable-activation instance variable.
; This is also the amount that the node will have "decayed" during this
; cycle, and it will be subtracted later from the node's activation.
; Next each node in turn spreads activation to other nodes, the spread
; activation being put in the :temp-activation-holder variable in each node.
; When the entire pnet has been gone over, each node's activation is
; then set to its old activation plus :temp-activation-holder minus
; :spreadable-activation. Thus the new activation of a node is
;          new-activation (node) = old-activation (node) - decay +
;                                  activation spread from other nodes.
;
;
   ; Compute and store :spreadable-activation for each node.
   (loop for pnode in *pnet* do
       (send pnode :set-spreadable-activation
                      (send pnode :activation-decay)))

   ; Spread activation from each node to its neighbors
   (loop for pnode in *pnet* do
       (send pnode :spread-activation))

   ; Update activation for each node.
;;
;; May  5 14:02 1987  pnet-functions.l Page 5
;;
   (loop for pnode in *pnet* do
       (send pnode :update-activation)))

;SUBTRACT-ACTIVATION METHOD
(defmethod (pnode :subtract-activation) (act)
    (if (> act %min-activation-to-be-added%)
     then (setq activation (difference activation act))))

;SUPPRESS-INSTANCES METHOD
(defmethod (pnode :suppress-instances) (l)
; Suppress a given cyto-node from the list of instances.
     (setq res nil)
     (do ((x instances (cdr x)))
         ((null x) (setq instances res))
         (cond
             ((eq (cadar x) l) nil)
             (t (setq res (cons (car x) res))))))

;UPDATE-ACTIVATION METHOD
(defmethod (pnode :update-activation) ()
; Sets the activation of the node to old-activation - decay
; (= :spreadable-activation) + :temp-activation-holder, and then
; sets :spreadable-activation and :temp-activation-holder to zero.
;If the activation to be added is not high enough (less than
;%min-activation-to-be-added% ),nothing is added (if the node is
;not a numerical one)
(let ((activation-to-add temp-activation-holder) new-activation)
   (if
      (and (null value)
            (< activation-to-add %min-activation-to-be-added%))
       (setq activation-to-add 0))
   (setq activation-to-add (min %max-activation-to-be-transmitted%
                                activation-to-add))
   (setq new-activation (plus (difference (my :activation)
         (my :spreadable-activation)) activation-to-add))
   (send self :set-activation new-activation)
   (send self :set-spreadable-activation 0.0)
   (send self :set-temp-activation-holder 0.0)))

;UPDATE-INSTANCES METHOD
(defmethod (pnode :update-instances) (l)
; Makes it possible to link a cyto-node to a pnode by updating instances
; Instances is a list of pairs. Each pair specify the type of the
; cyto-node (1t 2b 3dt 4bl 5g) and its name.
     (setq instances (cons l instances)))

(compile-flavor-methods pnode)
