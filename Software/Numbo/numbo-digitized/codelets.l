;; May 21 09:31 1987  codelets.l Page 1

 ;ACTIVATE CODELET
(defun activate (activation pnode repump-yes-or-no)
;Activates a given pnode to a given level
;If the pnode is linked to a target, the add, subtract and multiply
;pnodes are activated as a function of the value of pnode
(let (val val- valx)
     (send (eval pnode) :add-activation activation)
     (cond
          (repump-yes-or-no
              (setq val (send pnode :value))
              (setq val- (plus 5 (times 50 (expt 0.9 val))))
              (setq valx (times 4 (sqrt val)))
              (send node-add :set-activation (diff 60 val- valx))
              (send node-subtract :set-activation val-)
              (send node-multiply :set-activation valx)
              (repump))
          (t nil))))

;CHECK-TEMPERATURE FUNCTION
(defun check-temperature function ()
;This functions checks if the temperature of the cytoplasm is not too
;high. If it is the case, a node to be killed is randomly choosen
;according to the misfortunes. A "kill-node" codelet is loaded.
(let ((rvictim 0) (victim nil))
    (cond
        ((< %temperature-threshold%  (temperature))
;        (terpri) (terpri) (format t " TEMP ~a" (temperature))
         (setq reserve (send *cytoplasm* :secondary-cyto-nodes))
;the probability of being killed is a function of the activation
;of the node (proport. to 300 - activation)
         (setq weights (mapcar 'diff300 (find-activations reserve)))
         (setq rvictim (randlist weights))
        (cond
         (rvictim
         (setq victim (nth rvictim reserve))
         (cr-hang *coderack* (list 'kill-node victim) %first-urgency%)))))))

;COLLECT-MISFORTUNE FUNCTION
(defun collect-misfortune ()
;This function creates a list of the misfortunes of the secondary-
;cyto-nodes.
(let ((list nil) (cyto-nodes nil) (hap nil))
    (setq current-target (send *current-target* :name))
    (setq cyto-nodes (send *cytoplasm* :secondary-cyto-nodes))
    (loop for node in cyto-nodes do
          (setq hap (misfortune (send node :activation)
                               (send node :level)
                               (send node :status)))
          (setq list (cons hap list)))
    (reverse list)))

;COMPARE FUNCTION
(defun compare (element list)
;returns t if it exists in the list one number similar
;to element
(do
;;
;; May 21 09:31 1987  codelets.l Page 2
;;
    ((x list (cdr x)))
    ((null x) nil)
    (if (< (sim element (car x)) 4) (return t))))

;COMPARE-B-TO-T CODELET
(defun compare-b-to-t (cyto-block)
;Compares a given block to the current target.
;If equality then it loads a replace-target codelet and frees the block
;if it is linked.
;If similar (see sim function) then it loads a decomp+ codelet.
;If digits in common (see digits-in-common function) then it loads a decompi
;codelet.
;If multiple (see multiple function) then it loads a decompx codelet.
(let (cyto-current-target current-target block stat target)
(setq target (send cyto-target :value))
(setq cyto-current-target (send *current-target* :name))
(setq current-target (send (eval cyto-current-target) :value))
(setq block (send cyto-block :value))
(setq stat (send cyto-block :status))
 (cond
  ((or (= 0 (sim block current-target)) (= O (sim block target)))
   (cond
        ((is-linked-to-target cyto-block) nil)
        (t
          (if (equal "linked" stat)
           (kill-block (send (send cyto-block :upper-neighbor)
                              :upper-neighbor)))
          (cr-hang *coderack* (list 'replace-target
           cyto-block cyto-current-target) %upper-urgency%))))
 ((nequal "free" stat) nil)
 (t
    (cond
       ((= 1 (sim block current-target))
(cr-hang *coderack* (list 'decomp+ cyto-block cyto-current-target)
                                           %first-urgency%))
       ((= 2 (sim block current-target))
(cr-hang *coderack* (list 'decomp+ cyto-block cyto-current-target)
                                           %second-urgency%))
       ((= 3 (sim block current-target))
(cr-hang *coderack* (list 'decomp+ cyto-block Cyto-current-target)
                                           %fourth-urgency%))))
    (cond
       ((null (digits-in-common block current-target)) nil)
       (t
(cr-hang *coderack* (list 'decompi cyto-block cyto-current-target)
(list 'quote (digits-in-common block current-target))) %second-urgency%)))))

;CONST-BL+ CODELET
(defun const-bl+ (cyto-block1 cyto-block2 sum)
;Creates a new block in the cytoplasm.
;Checks if the nodes are still free.
;Changes their status to "linked".
;Computes how to combine the blocks to get sum.
;Creates a new block in the cytoplasm.
;Creates a node x in the cytoplasm. Increments count to distinguish
;possible different versions of versions and targets.
;;
;; May 21 09:31 1987  codelets.l Page 3
;;
(let (stblock1 stblock2 block1 block2 lev1 lev2 lev (activation 200)
         cyto-res cyto-op1 cyto-op2 cyto-op-node cyto-block-sum)
    (setq stblock1 (send cyto-block1 :status))
    (setq stblock2 (send cyto-block2 :status))
    (setq block1 (send cyto-block1 :value))
    (setq block2 (send cyto-block2 :value))
    (setq lev1 (send cyto-block1 :level))
    (setq lev2 (send cyto-block2 :level))
    (setq lev (max lev1 lev2))
;Checks if the blocks are still free
    (cond
         ((or (nequal "free" stblock1) (nequal "free" stblock2)) nil)
;Changes their status to "linked"
         (t
           (send cyto-block1 :set-status "linked")
           (send cyto-block2 :set-status "linked")
;Creates a new block in the cytoplasm
           (setq cyto-block-sum (concat 'cyto-block sum '-v *name-counter*))
           (if (= 0 (mod sum 5)) (setq activation 250))
           (if (= 0 (mod sum 10)) (setq activation 300))
           (create-cyto-node activation cyto-block-sum sum "free"
               (+ lev 2) "4bl" 1)
;Creates a node x in the cytoplasm
         (cond
           ((equal sum (+ block1 block2))
            (setq cyto-res (eval cyto-block-sum))
            (setq cyto-op1 cyto-block1)
            (setq cyto-op2 cyto-block2))
           ((equal sum (- block1 block2))
            (setq cyto-res cyto-block1)
            (setq cyto-op1 cyto-block2)
            (setq cyto-op2 (eval cyto-block-sum)))
           ((equal sum (- block2 block1))
            (setq cyto-res cyto-block2)
            (setq cyto-op1 cyto-block1)
            (setq cyto-op2 (eval cyto-block-sum))))
           (setq cyto-op-node (concat 'plus
                (min (send cyto-op1 :value) (send cyto-op2 :value)) '-
                (max (send cyto-op1 :value) (send cyto-op2 :value)) '-v *name-counter*))
           (create-op-node cyto-op-node cyto-res cyto-op1 cyto-op2
                50 (+ lev 1))
;Increments count.
           (setq *name-counter* (+ 1 *name-counter*))))))

;CONST-BLX CODELET
(defun const-blx (cyto-block1 cyto-block2 prod)
;Creates a new block in the cytoplasm.
;Checks if the nodes are still free.
;Changes their status to "linked".
;Creates a new block in the cytoplasm.
;Creates a node x in the cytoplasm. Increments count to distinguish
;possible different versions of versions and targets.
(let (stblock1 stblock2 block1 block2 lev1 lev2 lev cyto-block-prod
         (activation 200) cyto-op-node)
    (setq stblock1 (send cyto-block1 :status))
    (setq stblock2 (send cyto-block2 :status))
;;
;; May 21 09:31 1987  codelets.l Page 4
;;
    (setq block1 (send cyto-block1 :value))
    (setq block2 (send cyto-block2 :value))
    (setq lev1 (send cyto-block1 :level))
    (setq lev2 (send cyto-block2 :level))
    (setq lev (max lev1 lev2))
;Checks if the blocks are still free
    (cond
         ((or (nequal "free" stblock1) (nequal "free" stblock2)) nil)
;Changes their status to "linked"
         (t
           (send cyto-block1 :set-status "linked")
           (send cyto-block2 :set-status "linked")
;Creates a new block in the cytoplasm
           (setq cyto-block-prod (concat 'cyto-block prod '-v *name-counter*))
           (if (= 0 (mod prod 5)) (setq activation 250))
           (if (= 0 (mod prod 10)) (setq activation 300))
           (create-cyto-node activation cyto-block-prod prod "free"
               (+ lev 2) "4bl" 1)
;Creates a node x in the cytoplasm
           (setq cyto-op-node (concat 'times block1 '- block2 '-v *name-counter*))
           (create-op-node cyto-op-node (eval cyto-block-prod)
                cyto-block1 cyto-block2 50 (+ lev 1))
;Increments count.
           (setq *name-counter* (add1 *name-counter*))))))

;CREATE-CODERACK FUNCTION
;Initialize the coderack
(defun create-coderack ()
(cr-make-coderack 'my-coderack
     (list %upper-urgency% %first-urgency% %second-urgency%
            %third-urgency% %fourth-urgency% %fifth-urgency% 0))
(setq *coderack* 'my-coderack))
;CREATE-CYTO-NODE CODELET
(defun create-cyto-node (activation name value status
                            level type success)
;Codelet which creates a new node in the cytoplasm
;Loads a link-to-pnet codelet
;Name is the name of the variable which contains the address of the node
;Loads a compare-b-to-t codelet if the node type is "2b" or "4bl".
;Updates the context if the node type is "1t" or "3dt".
    (set name (make-instance 'cyto-node
            :activation activation
            :name name
            :value value
            :status status
            :level level
            :type type
            :success success))
    (format t "Node ~a created" name) (terpri)
    (send *cytoplasm* :set-nodes (cons (eval name) (send *cytoplasm* :nodes)))
    (cr-hang *coderack* (list 'link-to-pnet name type value)
                               %upper-urgency%)
    (if (or (equal type "2b") (equal type "4bl")) then
      (cr-hang *coderack* (list 'compare-b-to-t name) %upper-urgency%))
    (if (or (equal type "1t") (equal type "3dt")) then
      (update-current-target (eval name) 100)))

;;
;; May 21 09:31 1987  codelets.l Page 5
;;
;CREATE-OP-NODE CODELET
(defun create-op-node (name res op1 op2 activation level)
;Codelet which creates an operation node in the cytoplasm
;Creates links with the cyto-nodes res, op1, op2
;Updates the neighbors variable of res, op1, op2
;Updates the cytoplasm nodes variable.
     (setq n1 (list res 'result))
     (setq n2 (list op1 'operand))
     (setq n3 (list op2 'operand))
     (set name (make-instance 'cyto-node
                   :name name
                   :type "5g"
                   :level level
                   :neighbors (list n1 n2 n3)))
     (format t "Node ~a created" name)(terpri)
     (send *cytoplasm* :set-nodes (cons (eval name)(send *cytoplasm* :nodes)))
     (send res :update-neighbors (list (list (eval name) 'result)))
     (send op1 :update-neighbors (list (list (eval name) 'operand)))
     (send op2 :update-neighbors (list (list (eval name) 'operand))))

;DECOMP+ CODELET
(defun decomp+ (cyto-block cyto-current-target)
;Checks if the nodes are still free
;Creates a derived target in the cytoplasm
;Changes the status of the current-target to "linked"
;Changes the status of the block to "linked"
;Changes the current-target
;Create a node + in the cytoplasm. Increments count to distinguish
;possible different versions of operations and targets.
;Loads compare codelets for all the free blocks and bricks in the cyto.
(let (stblock sttarget lev block current-target
         res cyto-oper cyto-res cyto-derived-target diff cyto-op-node)
(setq stblock (send cyto-block :status))
(setq sttarget (send cyto-current-target :status))
(setq lev (send cyto-current-target :level))
(setq block (send cyto-block :value))
(setq current-target (send cyto-current-target :value))
(cond
    ((or (nequal "free" stblock) (nequal "free" sttarget)) nil)
    ((= block current-target)
         (cr-hang *coderack* (list 'replace-target cyto-block
                cyto-current-target) %upper-urgency%))
         (t
         (cond
             ((> block current-target)
              (setq res block) (setq cyto-res cyto-block) (setq oper current-target)
              (setq cyto-oper cyto-current-target))
             ((<= block current-target)
              (setq oper block) (setq cyto-oper cyto-block) (setq res current-target)
              (setq cyto-res cyto-current-target)))
         (setq diff (- res oper))
         (setq cyto-derived-target (concat 'cyto-target- diff '-v *name-counter*))
;Creates a derived target in the cytoplasm
    (create-cyto-node 200 cyto-derived-target diff "free"
;;
;; May 21 09:31 1987  codelets.l Page 6
;;
        (- lev 2) "3dt" 0)
;Changes the status of the current-target to "linked"
    (send cyto-current-target :set-status "linked")
;Changes the status of the block to "linked"
    (send cyto-block :set-status "linked")
;Changes the current-target
    (update-current-target (eval cyto-derived-target) 50)
;Creates a node + in the cytoplasm. Increments count to distinguish
;possible different versions of operations and targets.
    (setq cyto-op-node (concat 'plus oper '- diff '-v *name-counter*))
    (setq *name-counter* (+ 1  *name-counter*))
    (create-op-node   cyto-op-node cyto-res cyto-oper
        (eval cyto-derived-target) 50 (- lev 1))
;Loads compare codelets for all the free blocks and bricks in the cyto.
    (loop for nn in (send *cytoplasm* :cyto-brick-block-nodes) do
          (cond
               ((is-linked-to-target nn) nil)
               (t
    (cr-hang *coderack* (list 'compare-b-to-t nn) %first-urgency%))))))))

;DECOMPx CODELET
(defun decompx (cyto-block cyto-current-target)
;Checks if the nodes are still free
;Creates a derived target in the cytoplasm
;Changes the status of the current-target block to "linked"
;Changes the status of the block to "linked"
;Changes the current-target
;Create a node + in the cytoplasm. Increments count to distinguish
;possible different versions of operations and targets.
;Loads compare codelets for all the free blocks and bricks in the cyto.
(let (stblock sttarget lev oper res cyto-oper
         cyto-res quot cyto-derived-target cyto-block cyto-op-node)
(setq stblock (send cyto-block :status))
(setq sttarget (send cyto-current-target :status))
(setq lev (send cyto-current-target :level))
(cond
    ( (or (nequal "free" stblock) (nequal "free" sttarget)) nil)
    (t
    (setq oper (send cyto-block :value))
    (setq res (send cyto-current-target :value))
    (setq cyto-oper cyto-block)
    (setq cyto-res cyto-current-target)
    (setq quot (/ res oper))
    (setq cyto-derived-target (concat 'cyto-target- quot '-v *name-counter*))
;Creates a derived target in the cytoplasm
    (create-cyto-node 200 cyto-derived-target quot "free"
        (- lev 2) "3dt" 0)
;Changes the status of the current-target to "linked"
    (send cyto-current-target :set-status "linked")
;Changes the status of the block to "linked"
    (send cyto-block :set-status "linked")
;Changes thte current-target
    (update-current-target (eval cyto-derived-target) 50)
;Creates a node x in the cytoplasm. Increments count to distinguish
;possible different versions of operations and targets.
    (setq cyto-op-node (concat 'times oper '- quot '-v *name-counter*))
;;
;; May 21 09:31 1987  codelets.l Page 7
;;
    (setq *name-counter* (add1 *name-counter*))
    (create-op-node   cyto-op-node cyto-res cyto-oper
        (eval cyto-derived-target) 50 (- lev 1))
;Loads compare codelets for all the free blocks and bricks in the cyto.
    (loop for nn in (send *cytoplasm* :cyto-brick-block-nodes) do
          (cond
               ((is-linked-to-target nn) nil)
               (t
    (cr-hang *coderack* (list 'compare-b-to-t nn) %first-urgency%))))))))

;DECOMPI CODELET
(defun decompi (cyto-block cyto-current-target rank-list)
;Loads a Look-for-blx codelet if the rank-list has a 1 in the first or
;second position, that is if the target is equal to 10 or 100 times
;the block plus something less than 10 or 100.
;The activation variable of the block is increased. This will make it
;more probably chosen when a new block will be built.
(let ((target (send cyto-current-target :value))
      (block (send cyto-block :value))
      (op2 nil))
    (cond
         ((= 1 block) nil)
         ((= 1 (cadr rank-list))
          (setq op2 10) (send cyto-block :set-activation 200))
         ((= 1 (car rank-list))
          (setq op2 100) (send cyto-block :set-activation 100)))
    (if op2 then
        (cr-hang *coderack* (list 'look-for-blx (times block op2)
                                  block op2) %second-urgency%))))

;DECOMPOSE FUNCTION
(defun decompose (node)
;This function expands a given node using its neighbors variable.
;Used in the decoding of the result.
;The operation node is first extracted and from it the relevant
;information is searched for and printed. The function is recursived.
;Once a operation node has been listed, its listed variable is put to t.
(let ((op) (opn) (close) (operands) (op1v) (op2v) (op1n) (op2n))
    (setq op (send (eval node) :neighbors))
;Considers all the neighbors (operation nodes) of the node.
(do ((x op (cdr x)))
    ((null x))
;Controls if the operation has not been already considered
(cond
    ((send (caar x) :listed) nil)
;If not yet considered it is decomposed
    (t
;The listed variable of the op node is put to t.
    (send (caar x) :set-listed t)
    (setq opn (send (caar x) :name))
    (setq close (send (caar x) :neighbors))
    (setq operands (sup close (eval node)))
    (setq op1v (send (car operands) :value))
    (setq op2v (send (cadr operands) :value))
    (setq op1n (send (car operands) :name))
    (setq op2n (send (cadr operands) :name))
;;
;; May 21 09:31 1987  codelets.l Page 8
    (format t "Operation ~a has been applied " opn)
    (terpri) (format t "to ~a ( ~a) and to ~a ( ~a)"  op1n op1v op2n op2v)
    (terpri) (format t "to get ~a" (send (eval node) :name))(terpri)
    (if (nequal "2b" (send (eval op1n) :type)) then (decompose op1n))
    (if (nequal "2b" (send (eval op2n) :type)) then (decompose op2n)))))))

;DECREASE-INTEREST FUNCTION
(defun decrease-interest ()
;This function decreases (by 40%) the level of interest
;(activation variable) of the cyto-nodes which are free
;and secondary nodes.
(let ((list nil) (cyto-nodes nil))
    (setq cyto-nodes (send *cytoplasm* :free-secondary-cyto-nodes))
    (loop for node in cyto-nodes do
          (setq new (times (send node :activation) 0.6))
          (send node :set-activation new))))

;DIFF300 FUNCTION
(defun diff300 (val)
;used in check-temperature
(diff 300 val))

;DIGITS-IN-COMMON FUNCTION
;This function examines if two given values start with the same
;digit(s) or end with the same digit(s)
;Returns nil or a 3 dimensional vector.
;Examples: (digits-in-common 7 71) gives (0 1 0)
;Examples: (digits-in-common 7 607) gives (0 0 1)
;Examples: (digits-in-common 7 707) gives (1 0 1)
;Examples: (digits-in-common 22 222) gives (0 1 1)
(defun digits-in-common (val1 val2)
(let ((res '(0 0 0)))
  (cond
        ((> val1 val2) (setq res nil))
        (t
              (if (zerop (mod (- val2 val1) 10)) then (setq res '(0 0 1)))
              (if (equal val1 (*quo val2 10))
                              then (setq res (list 0 1 (caddr res))))
              (if (equal val1 (*quo val2 100))
                              then (setq res (cons 1 (cdr res)))
                              else res)))
  (cond
       ((equal res '(0 0 0)) (setq res nil))
       (t res))))

;DISCONNECT FUNCTION
(defun disconnect (cyto-node op)
;Suppress a given block/derived-target in the cytoplasm.
;Takes the neighbors of its op-neighbor and frees them.
;Updates all the neighbors variable.
;Updates the node variable of the cytoplasm.
;Suppresses the link in the pnet by loading an "free-from-pnet"
;codelet.
(let ((close) (operands) (op1n) (op2n) (opn) (cyton))
;Frees the component blocks.
          (setq close (send op :neighbors))
;;
;; May 21 09:31 1987  codelets.1 Page 9
;;
          (setq operands (sup close cyto-node))
          (setq op1n (send (car operands) :name))
          (setq op2n (send (cadr operands) :name))
          (if (equal "linked" (send (eval op1n) :status))
          (send (eval op1n) :set-status "free"))
          (if (equal "linked" (send (eval op2n) :status))
          (send (eval op2n) :set-status "free"))
;Updates the neighbors variable.
          (send (eval op1n) :suppress-neighbors op)
          (send  (eval op2n) :suppress-neighbors op)
          (send cyto-node :suppress-neighbors op)
;Updates the node variable of the cytoplasm.
          (send *cytoplasm* :suppress-node op)
          (setq opn (send op :name))
          (format t "Node ~a killed" opn) (terpri)
          (send *cytoplasm* :suppress-node cyto-node)
          (send cyto-node :set-status "killed")
          (setq cyton (send cyto-node :name))
          (format t "Node ~a killed" cyton) (terpri)
;Suppresses the link in the pnet by loading an "free-from-pnet"
;codelet.
          (cr-hang *coderack* (list 'free-from-pnet cyto-node)
              %first-urgency%)))

;ELIMINATE FUNCTION
(defun eliminate (element list)
;Eliminates from a given list the member which is the closest from
;element. For instance (eliminate 8 '(4 6 7) ) should give (4 6).
;In case of ties, the first best is eliminated.
     (let ((diffmin 999))
     (do
         ((x list (cdr x)))
         ((null x))
         (setq diff (abs (- element (car x))))
         (cond
              ((< diff diffmin) (setq diffmin diff) (setq min (car x)))
              (t nil)))
     (remove-dd min list)))

;FIND-ACTIVATIONS FUNCTION
(defun find-activations (list)
;Finds the activations of a list of cyto-nodes.
(let ((res nil)(x nil))
    (do ((v list (cdr v)))
        ((null v) (reverse res))
        (setq x (send (car v) :activation))
        (setq res (cons x res)))))

;FIND-IN FUNCTION
(defun find-in (element list &optional (level 4))
;Finds in a list of nodes a node with a value similar to a given value.
;The level of similarity is optionally specified in level. Its meaning
;is relative (see sim function) , 1 being a perfect match.
;A node is randomly chosen (the weights are the activations of the nodes)
;in the list and its value is compared with
;element. If not too different the node is returned.
;;
;; May 21 09:31 1987  codelets.l Page 10
;;
    (let((res1 list) (res2 (find-activations list)) (rep nil))
    (do
         ((x (randlist res2) (randlist res2)))
         ((equal nil x) rep)
         (setq rep (nth x res1))
         (cond
          ((> level (sim (send rep :value) element))
              (setq res2 nil))
          (t (setq res1 (remove-dd rep res1))
             (setq res2 (remove-dd (nth x res2) res2))
             (setq rep nil))))))

;FIND-INTEREST-IN-PNET FUNCTION
(defun find-interest-in-pnet (val)
;Finds the activation of the corresponding pnode in the pnet
;and returns an urgency. If the pnode is a target the urgency
;is maximal. If it is a brick the urgency is medium. In the other
;cases, the urgency is an increasing fonction of the activation.
(let (node type urgency priority)
 (cond
  ((< val 200)
   (setq node (find-node val))
   (setq priority (send node :activation))
   (setq type (caar (sortcar (send node :instances) nil)))
   (cond
        ((= val (send (send *current-target* :name) :value))
            (setq urgency %upper-urgency%))
        ((or (equal type "1t") (equal type "3dt"))
            (setq urgency %first-urgency%))
        ((and (nequal type "2b") (nequal type "4bl"))
            (cond
               ((< priority 10) (setq urgency %fifth-urgency%))
               ((< priority 14) (setq urgency %fourth-urgency%))
               ((< priority 50) (setq urgency %third-urgency%))
               (t (setq urgency %second-urgency%))))
        (t (setq urgency %third-urgency%))))
  ((< val 500) (setq urgency %fifth-urgency%))
  (t (setq urgency 0)))))

;FIND-NODE FUNCTION
(defun find-node (val)
;This function finds the pnode which should be associated to a given
;value (generally, the closest one)
;Similar to the code used in LINK-TO-PNET
(setq address (concat 'node- val))
(cond
    ((boundp address) t)
    ((= 0 val) (setq address 'node-1))
    ((< (round val) 200) (setq address (concat 'node- (round val))))
    (t (setq address 'node-150)))
(eval address))

;FREE-FROM-PNET CODELET
(defun free-from-pnet (name)
;Suppresses in the pnet the link to name.
;Decreases its activation.
;;
;; May 21 09:31 1987  codelets.l Page 11
;;
(let ((pnode nil))
    (setq pnode (car (send name :plinks)))
    (cond
         (pnode (send pnode :suppress-instances name)
                (send pnode :add-activation %node-minus%)))))

;IS-LINKED-TO FUNCTION
(defun is-linked-to (cyto-block1 cyto-block2)
;Checks if two cyto-nodes are linked in the cytoplasm.
(let (lev1 lev2 st1 st2 node)
(setq lev1 (send cyto-block1 :level))
(setq lev2 (send cyto-block2 :level))
(setq st1 (send cyto-block1 :status))
(setq st2 (send cyto-block2 :status))
(cond
;If the levels are equal, they cannot be linked.
    ((= lev1 lev2)  nil)
;If they are both free, they cannot be free neither.
    ((and (equal "free" st1) (equal "free" st2)) nil)
;The level of cyto-block1 will be considered the lower.
;If it is not the case they are interchanged.
    (t
      (when (> lev1 lev2)
        (setq node cyto-block2)
        (setq cyto-block2 cyto-block1)
        (setq cyto-block1 node))
;If the lower cyto-block (that is cyto-block1) is free,
;once again, they cannot be linked.
      (cond
          ((nequal "linked" (send cyto-block1 :status)) nil)
          (t
;The upper neighbors of cyto-block1 are examined. If one
;of them is equal to cyto-block2, then the function will
;return "true", if not, it will return "nil".
           (do
              ((node (send (send cyto-block1 :upper-neighbor)
                           :upper-neighbor)
                     (send (send node :upper-neighbor)
                           :upper-neighbor)))
              ((null node) nil)
              (cond
                   ((eq node cyto-block2)
                    (setq node nil) (return t))
                   ((equal "free" (send node :status))
                    (return nil))
                   ((equal "1t" (send node :type))
                    (return nil))
                   (t nil)))))))))

;IS-LINKED-TO-TARGET FUNCTION
(defun is-linked-to-target (cyto-block)
;This function checks if a given block/brick is linked in some way
;to the target.
(cond
    ((equal "linked" (send cyto-block :status))
 (do
;;
;; May 21 09:31 1987  codelets.l Page 12
;;
   ((node cyto-block
          (send (send node :upper-neighbor) :upper-neighbor)))
   ((null node) nil)
   (cond
         ((equal "free" (send node :status)) (return nil))
         ((equal "1t" (send node :type)) (return t))
         (t nil))))))

;KILL-BLOCK FUNCTION
(defun kill-block (cyto-block)
;This function is able to suppress a cyto-block in the tree structure of
;the cytoplasm. Two different cases have to be considered. First the node
;can be a free block. Then, we just have to disconnect the two blocks or
;bricks which are under it in the tree (they are linked through an
;operation node of course). If the node is not free, two different
;manipulations are needed. First, as in the first case, disconnection of
;the children but also determination of the parent and suppression of
;that parent by recursion. If the parent happens to be the target, a call
;to kill-dtarget is needed. It will proceed by going down in the tree,
;from parent to children (one of whose is always a derived-target).
;
;Disconnects the cyto-block from the nodes which are under it.
(let (op cyto-node)
(disconnect cyto-block (send cyto-block :lower-neighbor))
(cond
;If the block was free (thus no more neighbors), it is finished.
    ((null (send cyto-block :neighbors)) nil)
;If the block was not free, it calls op the upper operation-node.
    (t
        (setq op (caar (send cyto-block :neighbors)))
;It determines the upper block and if it is not the target, applies
;recursively kill-block. If it is the target, kill-dtarget is applied
;to the first derived-target (child of the target).
        (setq cyto-node (send op :upper-neighbor))
        (cond
            ((equal "4bl" (send cyto-node :type)) (kill-block cyto-node))
            ((equal "1t" (send cyto-node :type))
             (setq cyto-node (send op :lower-dtarget-neighbor))
             (kill-dtarget cyto-node)))))))

;KILL-DTARGET FUNCTION
(defun kill-dtarget (cyto-dtarget)
;This function is able to suppress a cyto-derived-target in the tree structure of
;the cytoplasm. Two different cases have to be considered. First the node
;can be a free derived-target. Then, we just have to disconnect the two nodes
;(block/brick and dtarget/target) which are above and next to it in the tree
;(they are linked through an operation node of course).
;If the node is not free, two different manipulations are needed.
;First, as in the first case, disconnection of the parent and the brother
;but also determination of the dtarget-child and suppression of it by
;recursion
;
;Disconnects the cyto-dtarget
(disconnect cyto-dtarget (send cyto-dtarget :upper-neighbor))
(let (op cyto-node)
(cond
;;
;; May 21 09:31 1987  codelets.l Page 13
;;
;If the derived-target was free (no more neighbors), it is finished.
    ((null (send cyto-dtarget :neighbors)) nil)
;If the derived-target was not free, it calls op the lower operation-node.
    (t
        (setq op (caar (send cyto-dtarget :neighbors)))
;It determines the dtarget-child and applies recursively kill-dtarget.
        (setq cyto-node (send op :lower-dtarget-neighbor))
        (kill-dtarget cyto-node)))))

;KILL-NODE CODELET
(defun kill-node (cyto-node)
;This codelet suppresses a block or a derived-target in the cytoplasm
;and all the dependent nodes.
;Depending on the type of the node, it uses kill-block or kill-dtarget.
;It updates the current-target and reactivates the corresponding
;pnode in the pnet (and the result pnode).
(let ((type) (pnode nil) (new-target nil) old-block)
  (cond
   ((memq cyto-node (send *cytoplasm* :nodes))
    (setq type (send cyto-node :type))
    (cond
         ((equal type "4bl") (kill-block cyto-node))
         ((equal type "3dt")
;If the node to be killed is a target, its old block neighbor is
;saved in order to avoid to recreate the same association directly
;after.
          (setq old-block (send cyto-node :block-neighbor))
          (kill-dtarget cyto-node))
         (t nil))
    (setq new-target (send *cytoplasm* :find-new-target))
    (update-current-target new-target 100)
    (if new-target
    (setq pnode (car (send (send *current-target* :name) :plinks))))
    (if pnode
      (cr-hang *coderack* (list 'activate %dtarget-plus% pnode t)
          %upper-urgency%))
;Loads compare codelets for all the free blocks and bricks in the cyto.
    (loop for nn in (send *cytoplasm* :cyto-brick-block-nodes) do
          (cond
               ((is-linked-to-target nn) nil)
               ((eq old-block nn) nil)
               (t
   (cr-hang *coderack* (list 'compare-b-to-t nn) %second-urgency%))))))))

;LINK-TO-PNET CODELET
(defun link-to-pnet ( name type value)
;Determines the level of activation depending on the type of node.
;If the node is a derived-target, the result node in the pnet is
;reactivated via the loading of an activate codelet.
;Updates instances in the closest node of the pnet
;Loads an activate codelet
    (let ((repump-yes-or-no nil) (address nil) (transact nil))
(cond ((equal "killed" (send (eval name) :status)) nil)
   (t
    (cond
         ((equal type "1t") (setq activation %target-activation%)
;;
;; May 21 09:31 1987  codelets.l Page 14
;;
          (setq repump-yes-or-no t))
         ((equal type "2b") (setq activation %brick-activation%))
         ((equal type "3dt") (setq activation %dtarget-activation%)
          (setq repump-yes-or-no t))
         ((equal type "4bl") (setq activation %block-activation%)))
    (setq address (concat 'node- value))
    (cond
        ((boundp address) (setq transact activation))
        ((= 0 value) (setq address 'node-1)
                     (setq transact 0))
        ((< (round value) 200)
           (setq address (concat 'node- (round value)))
           (setq transact (times activation (ratio value))))
        (t (setq address 'node-150)
           (setq transact (times activation 0.50))))
    (send (eval address) :update-instances (list type name))
    (send name :update-plinks (eval address))
    (cr-hang *coderack* (list 'activate transact
                          address repump-yes-or-no) %upper-urgency%)))))

;LOOK-FOR-APPROX-BL+ CODELET
(defun look-for-approx-bl+ (res op1 op2)
;Given 3 numbers which represent an operation (res = op1 + op2)
;checks if this type of operation could be instantiated in the
;cytoplasm.
;Checks first if one of the 3 arguments is similar to ther
;current-target.
;Then eliminates in (res op1 op2) the value closest to the current-
;target.
(let (current-target values-to-find blocks rn first-value
         cyto-block1 cyto-block2 n-blocks second-value v1 v2
         sum)
   (setq current-target (send (eval (send *current-target* :name)) :value))
(cond
    ((compare current-target (list res op1 op2))
   (setq values-to-find (eliminate current-target (list res op1 op2)))
;Tries to find randomly a block not too far from one of the given
;value. In order to do that, chooses randomly a value and a block and
;compares them.
   (setq blocks (send *cytoplasm* :cyto-brick-block-nodes))
   (setq rn (random 2))
   (setq first-value (nth rn values-to-find))
   (setq cyto-block1 (find-in first-value blocks))
;Tries to find another block not too far from the second value.
   (setq n-blocks (remove-dd cyto-block1 blocks))
   (setq second-value (nth (- 1 rn) values-to-find))
   (setq cyto-block2 (find-in second-value n-blocks))
;Checks if the operation should be implemented in the cytoplasm or if
;a derived target has to be defined.
;If one of the cyto-block is absent, a decomp+ codelet is loaded.
   (cond
        ((and (null cyto-block1) (null cyto-block2)) nil)
        ((null cyto-block1)
         (cr-hang *coderack* (list 'decomp+ cyto-block2
            (send *current-target* :name)) %second-urgency%))
        ((null cyto-block2)
;;
;; May 21 09:31 1987  codelets.l Page 15
;;
         (cr-hang *coderack* (list 'decomp+ cyto-block1
            (send *current-target* :name)) %second-urgency%))
;If none is absent, a constant-bl+ codelet is loaded.
        (t
         (setq v1 (send cyto-block1 :value))
         (setq v2 (send cyto-block2 :value))
;Determines the exact operation to implement
         (cond
           ((member res values-to-find) (setq sum (abs (- v1 v2))))
           (t (setq sum (+ v1 v2))))
;Determines the interest and the feasability of creating the new block
;by loading a "test-if-possible-and-desirable codelet".
            (cr-hang *coderack* (list 'test-if-possible-and-desirable
             cyto-block1 cyto-block2 "const-bl+" sum) %second-urgency%)))))))
;old method to load the codelet
;         (cond
;               ((< (sim sum current-target) 4)
;                        (cr-hang *coderack* (list 'const-bl+ cyto-block1 cyto-block2
;                         sum) %second-urgency%))
;               (t nil)))

;LOOK-FOR-APPROX-BLX CODELET
(defun look-for-approx-blx (res op1 op2)
;Given 3 numbers which represent an operation (res = op1 x op2)
;checks if this type of operation could be instantiated in the
;cytoplasm.
;First eliminates in (res op1 op2) the value closest to the current-
;target.
(let (current-target values-to-find blocks rn first-value cyto-block1
         cyto-block2 n-blocks second-value v1 v2 prod)
(setq current-target (send
        (eval (send *current-target* :name)) :value))
(cond
   ((compare current-target (list res op1 op2))

   (setq values-to-find (eliminate current-target (list res op1 op2)))
;Tries to find randomly a block not too far from one of the given
;value. In order to do that, chooses randomly a value and a block and
;compares them.
   (setq blocks (send *cytoplasm* :cyto-brick-block-nodes))
   (setq rn (random 2))
   (setq first-value (nth rn values-to-find))
   (setq cyto-block1 (find-in first-value blocks))
;Tries to find another  block not too far from the second value.
   (setq n-blocks (remove-dd cyto-block1 blocks))
   (setq second-value (nth (- 1 rn) values-to-find))
   (setq cyto-block2 (find-in second-value n-blocks))
;Checks if the operation should be implemented in the cytoplasm.
;If it is the case loads a const-blx codelet.
   (cond
        ((or (null cyto-block1) (null cyto-block2)) nil)
        (t
         (setq v1 (send cyto-block1 :value))
         (setq v2 (send cyto-block2 :value))
         (setq prod (times v1 v2))
;Determines the interest and the feasability of creating the new block
;;
;; May 21 09:31 1987  codelets.l Page 16
;;
;by loading a "test-if-possible-and-desirable codelet".
          (cr-hang *coderack* (list 'test-if-possible-and-desirable
           cyto-block1 cyto-block2 "const-blx" prod) %second-urgency%)))))))
;Old method
;         (cond
;               ((< (sim prod current-target) 4)
;                        (cr-hang *coderack* (list 'const-blx cyto-block1 cyto-block2
;                           prod) %second-urgency%))
;               (t nil)))))

;LOOK-FOR-BL+ CODELET
(defun look-for-bl+ (res op1 op2)
;Given 3 numbers which represent an operation (res = op1 + op2)
;checks if this operation could be exactly instantiated in the
;cytoplasm.
;First eliminates in (res op1 op2) the value closest to the current-
;target.
(let (current-target values-to-find node1 node2 cyto-block1
         cyto-block2 v1 v2 sum st1 st2)
   (setq current-target (send (eval (send *current-target* :name)) :value))
   (setq values-to-find (eliminate current-target (list res op1 op2)))
;Tries to reach the target with the cyto-nodes linked to the
;pnodes corresponding to values-to-find (here op1 and op2 but could
;be more general)
   (setq node1 (find-node (car values-to-find)))
   (setq node2 (find-node (cadr values-to-find)))
   (setq cyto-block1 (cadar (send (eval node1) :instances)))
;Here I use a kludge to take into account the fact that we can have
;op1 = op2
   (setq cyto-block2 (cadar (reverse (send (eval node2) :instances))))
;Checks if cyto-block1 and cyto-block2 exist
(cond
    ((or (null cyto-block1)(null cyto-block2) (eq cyto-block1 cyto-block2))
     (cr-hang *coderack* (list 'look-for-approx-bl+ res op1 op2)
         %first-urgency%))
    ((or (equal "1t" (send cyto-block1 :type))
         (equal "3dt" (send cyto-block1 :type)))
     (cr-hang *coderack* (list 'look-for-approx-bl+ res op1 op2)
         %first-urgency%))
    ((or (equal "1t" (send cyto-block2 :type))
         (equal "3dt" (send cyto-block2 :type)))
     (cr-hang *coderack* (list 'look-for-approx-bl+ res op1 op2)
         %first-urgency%))
    (t
      (setq v1 (send cyto-block1 :value))
      (setq v2 (send cyto-block2 :value))
;Determines the exact operation to implement
      (cond
          ((= res (car values-to-find)) (setq sum (- v1 v2)))
          ((= res (cadr values-to-find)) (setq sum (- v2 v1)))
          (t (setq sum (+ v1 v2))))
      (cond
          ((nequal current-target sum)
     (cr-hang *coderack* (list 'look-for-approx-bl+ res op1 op2)
         %first-urgency%))
          (t
;;
;; May 21 09:31 1987  codelets.l Page 17
;;
;Loads a "test-if-possible-and-desirable codelet" with a very high
;urgency, since the block is equal to the target.
            (cr-hang *coderack*
                (list 'test-if-possible-and-desirable
                       cyto-block1 cyto-block2 "const-bl+" sum)
           %upper-urgency%)))))))

;LOOK-FOR-BLX CODELET
(defun look-for-blx (res op1 op2)
;Given 3 numbers which represent an operation (res = op1 + op2)
;checks if this operation could be exactly instantiated in the
;cytoplasm.
;First eliminates in (res op1 op2) the value closest to the current-
;target.
   (setq current-target (send (eval (send *current-target* :name)) :value))
   (setq values-to-find (eliminate current-target (list res op1 op2)))
;Tries to reach the target with the cyto-nodes linked to the
;pnodes corresponding to values-to-find (here op1 and op2 but could
;be more general)
   (setq node1 (find-node (car values-to-find)))
   (setq node2 (find-node (cadr values-to-find)))
   (setq cyto-block1 (cadar (send (eval node1) :instances)))
;Here I use a kludge to take into account the fact that we can have
;op1 = op2
   (setq cyto-block2 (cadar (reverse (send (eval node2) :instances))))
;Checks if cyto-block1 and cyto-block2 exist
(cond
    ((or (null cyto-block1)(null cyto-block2) (eq cyto-block1 cyto-block2))
     (cr-hang *coderack* (list 'look-for-approx-blx res op1 op2)
         %first-urgency%))
    ((or (equal "1t" (send cyto-block1 :type))
         (equal "3dt" (send cyto-block1 :type)))
     (cr-hang *coderack* (list 'look-for-approx-blx res op1 op2)
         %first-urgency%))
    ((or (equal "1t" (send cyto-block2 :type))
         (equal "3dt" (send cyto-block2 :type)))
     (cr-hang *coderack* (list 'look-for-approx-blx res op1 op2)
         %first-urgency%))
    ((nequal current-target
         (times (send cyto-block1 :value) (send cyto-block2 :value)))
     (cr-hang *coderack* (list 'look-for-approx-blx res op1 op2)
         %first-urgency%))
    (t
;Loads a "test-if-possible-and-desirable codelet" with a very high
;urgency, since the block is equal to the target.
            (cr-hang *coderack*
                (list 'test-if-possible-and-desirable
                       cyto-block1 cyto-block2 "const-blx" current-target)
           %upper-urgency%))))

;LOOK-FOR-DIFF CODELET
(defun look-for-diff (trial)
;Tries to combine similar bricks to get small numbers. Trial is
;the number of trials already made.
(let (current-target blocks rn cyto-block1 value1 n-blocks
;;
;; May 21 09:31 1987  codelets.l Page 18
;;
        (cyto-block2 nil) node urgency value2)
   (setq current-target (send (eval (send *current-target* :name))
                              :value))
;Chooses randomly a  block which will be called cyto-block1
;If trial = 0, just the free blocks are considered. If trial > 0
;all the blocks are taken into account
   (if (= 0 trial)
   (setq blocks (send *cytoplasm* :free-blocks))
   (setq blocks (send *cytoplasm* :cyto-brick-block-nodes)))
(cond ((null blocks) nil)
  (t
   (setq rn (random (length blocks)))
   (setq cyto-block1 (nth rn blocks))
   (setq value1 (send cyto-block1 :value))
;Updates blocks by removing cyto-block1
   (setq n-blocks (remove-dd cyto-block1 blocks))
;Tries to find another free block similar to cyto-block1 (cyto-block2)
   (do
      ((x n-blocks n-blocks))
      ((null x) cyto-block2)
      (setq node (nth (random (length n-blocks)) n-blocks))
      (setq similarity (sim value1 (send node :value)))
      (cond
            ((< 3 similarity) (setq n-blocks
                                    (remove-dd node n-blocks)))
            (t (setq n-blocks nil) (setq cyto-block2 node))))
;Checks if cyto-block2 exists. If it is not the case, a new
;look-for-diff codelets is loaded, if trial is less than 5.
;The urgency of that codelet is function of trial.
   (cond
        ((and (null cyto-block2) (< trial 4))
         (cond
               ((> 2 trial) (setq urgency %first-urgency%))
               ((> 5 trial) (setq urgency %second-urgency%)))
         (setq trial (add1 trial))
         (cr-hang *coderack* (list 'look-for-diff trial) urgency))
;If cyto-block2 exists, a look-for-bl+ codelets is loaded
        (cyto-block2
            (setq value2 (send cyto-block2 :value))
            (cr-hang *coderack* (list 'test-if-possible-and-desirable
            cyto-block1 cyto-block2 "const-bl+" (abs (- value1 value2)))
                     %first-urgency%)))))))

;LOOK-FOR-NEW-BLOCK CODELET
(defun look-for-new-block ()
;This codelet tries to build a new block by randomly choosing two
;bricks or blocks in the cytoplasm and combining them. The operation
;used to combine them depends on the activitions of the nodes "add"
;"subtract" and "multiply" in the Pnet. A random choice is performed
;with probabilities proportional to those activations.
;Once a new block has been formed, it is tested by
;"test-if-possible-and-desirable".
(let (blocks list rank cyto-block1 cyto-block2 res  node activation
             type  w+ w- wx fun)
;Random choice of two blocks according to their level of interest
;measured by their activation stored in liste.
;;
;; May 21 09:31 1987  codelets.l Page 19
;;
    (setq blocks (send *cytoplasm* :cyto-brick-block-nodes))
    (setq liste (find-activation blocks))
    (setq rank (randlist liste))
    (setq cyto-block1 (nth rank blocks))
    (setq blocks (remove-dd cyto-block1 blocks))
    (setq liste (remove-dd (nth rank liste) liste))
    (setq rank (randlist liste))
    (setq cyto-block2 (nth rank blocks))
;Random choice of an operation according to the level of activation
;of the corresponding pnodes.
    (setq w+ (send node-add :activation))
    (setq w- (send node-subtract :activation))
    (setq wx (send node-multiply :activation))
    (setq rank (randlist (list w+ w- wx)))
    (cond
      ((= 0 rank)
       (setq res (+ (send cyto-block1 :value) (send cyto-block2 :value)))
       (setq fun  "const-bl+"))
      ((= 1 rank)
       (setq res (abs (- (send cyto-block1 :value)
                         (send cyto-block2 :value))))
       (setq fun "const-bl+"))
      (t (setq res (* (send cyto-block1 :value)
                      (send cyto-block2 :value)))
         (setq fun "const-blx")
;If one of the blocks is equal to 1 the operation must be
;abandoned. This is achieved by setting res to 0 .
         (if (or (= 1 (send cyto-block1 :value))
                 (= 1 (send cyto-block2 :value))) (setq res 0))))
;Loading of the "test-if-possible-and-desirable codelet"
          (cr-hang *coderack*
          (list 'test-if-possible-and-desirable cyto-block1 cyto-block2
            fun res) %first-urgency%)))

;MEAN FUNCTION
(defun mean (list)
;this function computes the mean of a list of numbers.
(let ((sum 0))
  (cond
    ((null list) 0)
    (t
       (setq sum (apply 'add list))
       (quotient sum (length list))))))

;MISFORTUNE FUNCTION
(defun misfortune (interest level status)
;This function computes the misfortune (of a cyto-node) given
;the level of interest (maximum = 300)
;the level in the tree structure (for blocks the level is 3,5 or more
;and for a dtarget the level is 97,95 or less)
;the status (free or linked).
;The function used is (/ 20 interest)
;                     + 10 (if status is "free")
(let ((st 0))
    (if (equal "free" status) then (setq st 10))
;;
;; May 21  09:31 1987  codelets.l Page 20
;;
    (plus (quotient 20 interest) st)))

;MULTIPLE FUNCTION
;This function tests if val2 is a multiple of val1.
;Val1 must be different from 1.
(defun multiple (val1 val2)
    (cond
        ((>= val1 val2) nil)
        ((equal 1 val1) nil)
        ((zerop (mod val2 val1)) t)
        (t nil)))

;PROPAGATE-SUCCESS CODELET
(defun propagate-success ()
;This codelet seraches in the cytoplasm for operation nodes.
;If an operation node has two neighbors with success = 1, the success
;variable of the third one is put to 1. It loops until changes are not
;possible any more.
     (setq nn (send *cytoplasm* :node))
     (setq cont 1)
     (do ((x 1 (add1 x)))
         ((equal 0 cont))
         (setq cont 0)
         (loop for node in nn do
               (cond
                    ((equal "5g" (send node :type))
                     (setq n (update-success (send node :neighbors))))
                    (t (setq n nil)))
               (cond
                    ((null n) nil)
                    ((equal "1t" (send n :type))
                     (setq *problem-solved* 1) (setq cont 0))
                    (t (send n :set-success 1) (setq cont 1))))))

;RANDLIST FUNCTION
(defun randlist (list)
;Chooses randomly a rank between 0 and length list - 1 according to weights
;given in list. For instance (randlist '(100 50)) should give 0 with a
;probability 2/3 and 1 with a probability 1/3.
      (cond
       ((null list) nil)
       ((< 0 (fix (apply 'add list)))
        (let ((r (random (fix (apply 'add list)))) (sum 0))
           (do ((v list (cdr v))
                (i -1 (add1 i)))
               ((null v) i)
               (setq sum (add sum (car v)))
               (cond
                     ((< r sum) (setq v nil))
                     (t nil)))))
       (t nil)))

;RATIO FUNCTION
(defun ratio (num)
;Gives the level to which the activation has to be decreased
;    if no node exists in the pnet
;;
;; May 21 09:31 1987  codelets.l Page 21
;;
;    (cond
;       ((< (abs (- num (round num))) 5) 0.90)
;       ((< (abs (- num (round num))) 10) 0.80)
;       (t 0.70)))
    (quotient (float (min num (round num))) (float (max num (round num)))))

;READ-BRICK CODELET
(defun read-brick (i)
;Reads the brick i in the cytoplasm
;Loads a create-cyto-node codelet
;Loads a compare-b-to-t codelet
    (setq bricki  (concat 'brick i))
    (setq a  (intern (uconcat "brick" i) (find-package "keyword")))
    (setq brick (send *cytoplasm* a))
    (setq activation 50)
    (if (= 0 (mod brick 10)) (setq activation 300))
    (update-context "2b" (concat 'cyto-brick i) brick 50 "cyto")
    (setq cyto-bricki (concat 'cyto-brick i))
    (cr-hang *coderack* (list 'create-cyto-node activation
     (list 'quote cyto-bricki) brick "free" 1 "2b" 1) %first-urgency%))

;READ-TARGET CODELET
(defun read-target ()
;Read the target in the cytoplasm
;Update the context
;Load a create-cyto-node codelet
     (setq target (send *cytoplasm* :target))
     (update-current-target 'cyto-target 100)
;     (update-context "1t" 'cyto-target target 100 "cyto")
     (cr-hang *coderack* (list 'create-cyto-node 150 ''cyto-target target "free" 99
                               "1t" 0)
                               %first-urgency%))

;REMOVE-DD FUNCTION
(defun remove-dd (l1 l)
;Variant of the remove function which, in case of ex-aequo, remove just
;one element. (remove-dd 3 '(2 3 4 3)) should give (2 4 3). If nothing is
;found in the list the list is reversed.
    (let ((res nil))
    (do ((x l (cdr x)))
        ((null x) res)
        (cond
             ((eq (car x) l1)
              (setq res (append (reverse res) (cdr x))) (setq x nil))
             (t (setq res (cons (car x) res)))))))

;REPLACE-TARGET CODELET
(defun replace-target (cyto-block cyto-current-target)
;This codelet checks if the current-target is the target. If it is the
;case the problem is solved and the variable *problem-solved* is given the
;value 1.
;The cyto-block status is put to "linked".
;If it is not the case, the cyto-node corresponding to the current-target
;is replaced everywhere by th cyto-node corresponding to the given block.
;In order to do that, the neighbors of cyto-block must be updated by
;;
;; May 21 09:31 1987  codelets.l Page 22
;;
;the neighbors of the current target to suppress . The neighbors of
;the neighbors of the current target must be updated as well.Idem for the
;pnodes which were linked to the current-target.
;The current-target is changed to the final target in the current-target flav.
;The current-target must be suppressed from the list of nodes in the
;*cytoplasm* flavour.
;Propagate the success.
   (cond
       ((equal (send cyto-target :value)
               (send cyto-block :value)) (setq *problem-solved* 1)
                                         (setq cyto-target cyto-block)
                                         (format t "Obvious. "))
       ((eq cyto-current-target (send *current-target* :name))
;The cyto-block status is put to "linked".
          (send cyto-block :set-status "linked")
;Update of the neighbors of the block
          (setq nn (send cyto-current-target :neighbors))
          (send  cyto-block :update-neighbors nn)
;Update of the neighbors of the neighbors
          (loop for pair in nn do
                (send (car pair) :replace-neighbors
                       cyto-current-target cyto-block))
;Update of the instances of the pnodes which are in the plinks of the cur-targ.
          (setq pp (send cyto-current-target :plinks))
          (loop for pnode in pp do
                (send pnode :suppress-instances cyto-current-target))
;Update of the cyto-current-target
          (update-current-target 'cyto-target 100)
;Update of the nodes variable in the *cytoplasm*
          (send *cytoplasm* :suppress-node cyto-current-target)
;Loading of the propagate codelet
          (propagate-success))))

;REPUMP FUNCTION
(defun repump ()
;Reactivates the operand, similar and result pnodes.
;Computes the values of %result+%, %resultx% and %operand% as
;a function of the activations of node-multiply and node-add +
;node-subtract.
  (let (resx res+)
       (setq resx (send node-multiply :activation))
       (setq res+ (diff 60 resx))
       (setq %resultx% (times 300 (quotient resx 60.0)))
       (setq %result+% (times 100 (quotient res+ 60.0)))
       (setq %operand% (quotient %resultx% 1.5))
       (set-up-activations (list 'result+ %result+%
                        'resultx %resultx% 'operand %operand%
 'similar %similar% 'operation %operation% 'instance %instance%))))

;ROUND FUNCTION
(defun round (num)
;Gives the multiple of 5 which is closest to num if num < 20
;Gives the multiple of 10 which is closest to num if num < 100
;Otherwise gives the closest multiple of 50
       (cond
           ((< num 20) (setq div 5))
;;
;; May 21 09:31 1987  codelets.l Page 23
;;
           ((< num 100) (setq div 10))
           (t (setq div 50)))
       (cond
           ((< (*mod num div) 0) (times div (+ 1 (/ num div))))
           (t (times div (/ num div)))))

;SIM FUNCTION
;This functions computes the similarity between two values
;It will have to be replaced by something more general based
;on common descriptive characteristics.
(defun sim (val1 val2)
    (setq diff (abs (- val1 val2)))
    (setq diffrel (quotient (float diff) (float val2)))
    (cond
         ((= 0 diff) 0)
         ((<= diffrel 0.1) 1)
         ((<= diffrel 0.2) 2)
         ((<= diffrel 0.3) 3)
         (t 4)))

;SUP FUNCTION
(defun sup (l l1)
;l is a list of pairs. The pairs which start with l1 are suppressed.
;The result is the list of the first elements of the pairs not suppressed.
;Used in the decoding of the result.
    (let ((res nil))
    (do ((x l (cdr x)))
        ((null x) res)
        (cond
             ((eq (caar x) l1) nil)
             (t (setq res (cons (caar x) res)))))))

;TEMPERATURE FUNCTION
(defun temperature ()
;This functions computes the temperature of the cytoplasm.
;The temperature is based on the number of available free
;nodes (nfree), on the degree of elaboration of the current-target,
;on the average degree of misfortune of the nodes
;and on the maximum misfortune.
;The function used is
; (max misfortune) + 2 * (mean misfortune) if the number of free nodes
;plus the level of the current target ((99 - level)/2, in fact) is > 4
;if not, the function is
; (6 - nfree - lev) * (max misfortune) + 2 * (mean misfortune).
(let ((misfort nil) (nfree 0) (lev 0))
    (setq misfort (collect-misfortune))
    (setq nfree (length (send *cytoplasm* :free-cyto-nodes)))
    (setq lev (/ (- 99 (send (send *current-target* :name) :level)) 2))
    (cond
         ((> (add nfree lev) 4)
          (add (apply 'max misfort) (times 2 (mean misfort))))
         (t
          (add (times (- 6 nfree lev) (apply 'max misfort))
                (times 2 (mean misfort)))))))

;TEST-IF-POSSIBLE-AND-DESIRABLE CODELET
;;
;; May 21 09:31 1987  codelets.l Page 24
;;
(defun test-if-possible-and-desirable (cyto-block1 cyto-block2 fun res)
;Checks if it is possible to agregate cyto-block1 and cyto-block2
;Aren't they linked to the target or to each other?
;If not, the interest of creating a new block (with the value res) is
;examined with the function "find-interest-in-pnet". If the interest
;is very high, the two blocks are freed and the fun codelet is loaded.
;If the interest is not very high, the fun codelet is tried.
;The value of fun is 'const+' or 'constx'.
(let (urgency (const 'const-blx) node)
;Checks if the blocks are not linked (one to the other)
;and not linked to the target.
 (cond
      ((is-linked-to cyto-block1 cyto-block2) (setq urgency nil))
      ((= res (send cyto-target :value)) (setq urgency %upper-urgency%))
      ((is-linked-to-target cyto-block1) (setq urgency nil))
      ((is-linked-to-target cyto-block2) (setq urgency nil))
;If res = 0 the operation is without any interest.
      ((= 0 res) (setq urgency nil))
;Computes the interest of the new possible block
      (t (setq urgency (find-interest-in-pnet res))))
;If the interest is high, cleans what must be cleaned.
;If a block is not free,
;this block is freed by killing its upper-neighbor, if its
;value is not equal to res (to avoid to kill and then recreate
;the same block).
  (cond
; If the urgency is nil, nothing must be done.
        ((null urgency) nil)
        (t
             (when
                  (>= urgency %first-urgency%)
               (cond ((equal "linked" (send cyto-block1 :status))
                      (setq node
                         (send (send cyto-block1 :upper-neighbor)
                                  :upper-neighbor))
                      (if (nequal res (send node :value))
                         (kill-node node))))
               (cond ((equal "linked" (send cyto-block2 :status))
                      (setq node
                         (send (send cyto-block2 :upper-neighbor)
                                  :upper-neighbor))
                      (if (nequal res (send node :value))
                         (kill-node node)))))
;A fun codelet is loaded to construct the new block
            (if (equal fun "const-bl+") (setq const 'const-bl+))
            (cr-hang *coderack*
                (list const cyto-block1 cyto-block2 res)
           urgency)))))

;UPDATE-SUCCESS FUNCTION
(defun update-success (l)
;returns in a list of the three neighbors of an operation node (type "5g")
;the node with success = 0 if for the two other ones success = 1
;Used in the propagate-success codelet.
(let (n1 n2 n3 s1 s2 s3)
;;
;; May 21 09:31 1987  codelets.l Page 25
;;
    (setq n1 (caar l))
    (setq n2 (caadr l))
    (setq n3 (caaddr l))
    (setq s1 (send n1 :success))
    (setq s2 (send n2 :success))
    (setq s3 (send n3 :success))
    (cond
         ((nequal 2 (plus s1 s2 s3)) nil)
         ((equal 0 s1) n1)
         ((equal 0 s2) n2)
         ((equal 0 s3) n3))))
