;; May 19 14:00 1987  cyto-def.l Page 1

; CYTOPLASM FLAVOR
; The cytoplasm is represented as a flavor with instance variable
; nodes ,current-target and context
(defflavor cytoplasm
           (target
            brick1
            brick2
            brick3
            brick4
            brick5
            nodes
            current-target
            context)
           ()
; Cytoplasm      - target and bricks are the parameters of the game.
;                 - nodes are the nodes which constitute the cyto.
;                   They are flavors as well (cyto-node).
;                - current-target is a flavor which the target of current
;                       interest at a given time.
;                - context is a flavor with the current entities of interest
;                                           (very short term storage)
            :settable-instance-variables
            :inittable-instance-variables
            :gettable-instance-variables)

; CYTO-NODE FLAVOR
(defflavor cyto-node
           (activation
            neighbors
            name
            type
            value
            success
            status
            level
            listed
            plinks)
           ()
; Cyto-node      - activation is the level of activation
;                - neighbors are the neighbors and the corresponding type of link
;                - name is the full name of the node.It is also the name of
;                    the variable which contains the node address.
;                - type is the type of the node(1t 2b 3dt 4bl 5g).
;                - value is the possible value associated with the node.
;                - success tells us if a target is hit or not (1 0).
;                - status tells us if the node is still free or is already or is
;                - level is the level of the node in the tree. By definition
;                   the level of the bricks is 1 and of the target 99.
;                   For blocks the level is increasing, for derived targets
;                   it is decreasing.
;                               implied in some operation (free linked).
;                - listed is used in listing the solution . Its value is yes
;                     for a given operation when it has been listed in the
;                                    final result.
;                - plinks are the links with the pnodes (list of pnodes),
;                              but the address is not compiled.
;;
;; May 19 14:00 1987  cyto-def.l Page 2
;;
            :settable-instance-variables
            :inittable-instance-variables
            :gettable-instance-variables)

; CYTO-CURRENT-TARGET
(defflavor cyto-current-target
           (interest
            name)
            ()
; Cyto-current-target   - interest is the current interest in the target
;                       - name is the name of the node which represents the
;                                                current target
            :settable-instance-variables
            :inittable-instance-variables
            :gettable-instance-variables)

; CYTO-CONTEXT
(defflavor cyto-context
           (type
            name
            value
            interest
            location)
           ()
; Cyto-context-node    - type is the type of the node if relevant
;                      - name is the name of the node which represents the
;                                               current target
;                      - value is the possible value associated with the node
;                      - interest is the current interest in the node
;                      - location is Pnet or Cytoplasm
            :settable-instance-variables
            :inittable-instance-variables
            :gettable-instance-variables)

;CYTO-BRICK-BLOCK-NODES METHOD
(defmethod (cytoplasm :cyto-brick-block-nodes) ()
;Sends the list of cyto-bricks and cyto-blocks
;in the cytoplasm nodes variable
       (let ((res nil))
       (do  ((x nodes (cdr x)))
            ((null x) res)
            (setq type (send (car x) :type))
            (cond
                 ((equal "1t" type) nil)
                 ((equal "3dt" type) nil)
                 ((equal "5g" type) nil)
                 (t (setq res (cons (car x) res)))))))

;FIND-NEW-TARGET METHOD
(defmethod (cytoplasm :find-new-target) ()
;Sends the free target from the nodes variable of the cytoplasm.
       (let ((res nil))
       (do  ((x nodes (cdr x)))
            ((null x) res)
            (setq type (send (car x) :type))
            (setq status (send (car x) :status))
;;
;; May 19 14:00 1987  cyto-def.l Page 3
;;
            (cond
                 ((equal "linked" status) nil)
                 ((equal "2b" type) nil)
                 ((equal "4bl" type) nil)
                 ((equal "5g" type) nil)
                 (t (setq res (car x) ))))))

;FREE-BLOCKS METHOD
(defmethod (cytoplasm :free-blocks) ()
;Sends the list of blocks in the cytoplasm-nodes variable which are
;still free.
       (let ((res nil))
       (do  ((x nodes (cdr x)))
            ((null x) res)
            (setq type (send (car x) :type))
            (setq status (send (car x) :status))
            (cond
                 ((equal "linked" status) nil)
                 ((equal "1t" type) nil)
                 ((equal "3dt" type) nil)
                 ((equal "5g" type) nil)
                 (t (setq res (cons (car x) res)))))))

;FREE-CYTO-NODES METHOD
(defmethod (cytoplasm :free-cyto-nodes) ()
;Sends the list of cyto-nodes in the cytoplasm nodes variable
;which are still free.
       (let ((res nil))
       (do  ((x nodes (cdr x)))
            ((null x) res)
            (setq status (send (car x) :status))
            (setq type (send (car x) :type))
            (cond
                 ((equal "linked" status) nil)
                 ((equal "5g" type) nil)
                 (t (setq res (cons (car x) res)))))))

;FREE-SECONDARY-CYTO-NODES METHOD
(defmethod (cytoplasm :free-secondary-cyto-nodes) ()
;Sends the list of secondary-cyto-nodes in the cytoplasm nodes variable
;which are still free. Bricks,target and operations are thus excluded.
       (let ((res nil))
       (do  ((x nodes (cdr x)))
            ((null x) res)
            (setq status (send (car x) :status))
            (setq type (send (car x) :type))
            (cond
                 ((equal "linked" status) nil)
                 ((equal "1t" type) nil)
                 ((equal "2b" type) nil)
                 ((equal "5g" type) nil)
                 (t (setq res (cons (car x) res)))))))

;INIT-CYTOPLASM FUNCTION
;Initialize the cytoplasm
(defun init-cytoplasm (target brick1 brick2 brick3 brick4 brick5)
;;
;; May 19 14:00 1987  cyto-def.l Page 4
;;
    (setq *temperature* 100)
    (setq *context* (make-instance 'cyto-context))
    (setq *current-target* (make-instance 'cyto-current-target))
    (setq *cytoplasm* (make-instance 'cytoplasm
                          :target target
                          :brick1 brick1
                          :brick2 brick2
                          :brick3 brick3
                          :brick4 brick4
                          :brick5 brick5
                          :current-target *current-target*
                          :context *context*)))

;LOWER-DTARGET-NEIGHBOR METHOD
(defmethod (cyto-node :lower-dtarget-neighbor) ()
;This method gives the lower neighbor which has the type "3dt".
(let ((res nil) (lev 1000))
    (loop for pair in neighbors do
          (setq lv (send (car pair) :level))
          (cond
               ((and (equal "3dt" (send (car pair) :type))
                     (< lv lev)) (setq lev lv)
                                (setq res (car pair)))
               (t nil)))
    res))

;LOWER-NEIGHBOR METHOD
(defmethod (cyto-node :lower-neighbor) ()
;This method gives the lower neighbor if the node is not
;an operation node
(let ((res nil) (lev 1000))
    (cond
         ((equal "5g" type) nil)
         (t
          (loop for pair in neighbors do
           (setq lv (send (car pair) :level))
           (cond
                ((< lv lev) (setq lev lv)
                            (setq res (car pair)))
                (t nil)))))
    res))

;BLOCK-NEIGHBOR METHOD
(defmethod (cyto-node :block-neighbor) ()
;The method determines for a derived target, its block neighbor
;In fact, the block neighbor of its neighbor. Used to avoid to
;repeat associations of a target with the same bock.
(let (node l (res nil))
  (cond
      ((nequal "3dt" type) nil)
      ((nequal "free" status) nil)
      (t
;l will contain the neighbors of the operation node linked to
;cyto-node
        (setq l (send (send self :upper-neighbor) :neighbors))
;The node of l which is a block or a brick is put in res
;;
;; May 19 14:00 1987  cyto-def.l Page 5
;;
        (loop for pair in l do
              (setq node (car pair))
              (cond
                  ((equal "2b" (send node :type)) (setq res node))
                  ((equal "4bl" (send node :type)) (setq res node))
                  (t nil)))))
  res))

;REPLACE FUNCTION
(defun replace-function (l l1 l2) ·
;This functions replace in l which is a list of couples, all the couples
;which start by l1 by a corresponding couple starting with l2
(let ((res nil) pair)
    (do ((x l (cdr x)))
        ((null x) res)
        (cond
             ((eq (caar x) l1) (setq pair (cons l2 (cdar x))))
             (t (setq pair (car x))))
        (setq res (cons pair res)))))

;REPLACE-NEIGHBORS METHOD
(defmethod (cyto-node :replace-neighbors) (l1 l2)
;This method replaces in the list of neighbors of a cyto-node
;the node l1 by the node l2. It uses the function
;replace in mylist l1 by l2 (replace mylist l1 l2).
      (setq neighbors  (replace-function neighbors l1 l2)))

;SECONDARY-CYTO-NODES METHOD
(defmethod (cytoplasm :secondary-cyto-nodes) ()
;Sends the list of secondary-cyto-nodes (blocks and dtargets)
;in the cytoplasm nodes variable
       (let ((res nil))
       (do  ((x nodes (cdr x)))
            ((null x) res)
            (setq type (send (car x) :type))
            (cond
                 ((equal "1t" type) nil)
                 ((equal "2b" type) nil)
                 ((equal "5g" type) nil)
                 (t (setq res (cons (car x) res)))))))

;SUPPRESS-NEIGHBORS METHOD
(defmethod (cyto-node :suppress-neighbors) (l1)
;This methods suppresses the node l1 (in fact the couple
;which starts with l1) in the list of neighbors
;of a cyto-node.
(let ((res nil))
    (do ((x neighbors (cdr x)))
        ((null x) (setq neighbors res))
        (cond
             ((eq (caar x) l1) nil)
             (t (setq res (cons (car x) res)))))))

;SUPPRESS-NODE METHOD
(defmethod (cytoplasm :suppress-node) (l)
;Suppress a given node in the nodes variable of the cytoplasm
;;
;; May 19 14:00 1987  cyto-def.l Page 6
;;
(let ((res nil))
      (do ((x nodes (cdr x)))
          ((null x) (setq nodes res))
          (cond
                ((eq (carx) l) nil)
                (t (setq res (cons (car x) res)))))))

;UPDATE-CONTEXT FUNCTION
(defun update-context (type name value interest location)
;Updates the variables of the cyto-context node
    (send *context* :set-type type)
    (send *context* :set-name name)
    (send *context* :set-value value)
    (send *context* :set-interest interest)
    (send *context* :set-location location))

;UPDATE-CURRENT-TARGET FUNCTION
(defun update-current-target (name interest)
;Updates the variables of the cyto-current-target node
    (send *current-target* :set-name name)
    (send *current-target* :set-interest interest))

;UPDATE-NEIGHBORS METHOD
(defmethod (cyto-node :update-neighbors) (l)
;Appends a list l of neighbors to the current list
        (setq neighbors (append l Neighbors)))

;UPDATE-PLINKS METHOD
(defmethod (cyto-node :update-plinks) (l)
;This method makes it possible to add one pnode on the list of plinks.
    (setq plinks (cons l plinks)))

;UPPER-NEIGHBOR METHOD
(defmethod (cyto-node :upper-neighbor) ()
;This method gives the upper neighbor
(let ((res nil) (lev level))
     (loop for pair in neighbors do
           (setq lv (send (car pair) :level))
           (cond
                ((> lv lev) (setq lev lv)
                            (setq res (car pair)))
                (t nil)))
    res))
