;; Feb 27 15:35 1987  pnet-graphics.l Page 1


 ; Note: some of the stuff in here was done in a kludgey way so it would
; work fast for my purposes. If anyone else is using these routines, they
; might want to make things more general.

(declare (special
          max-activation
          text-size
          text-offset
          origin-x origin-y
          wheight
          wwidth
          num-of-chars
          region-side
          diminished-region-side
          new-boxsize
          new-box-coordinates
          boxsizes
         )
)


(defun init-pnet-graphics (node-list)
; Set up global variables
    (open-window)
    (clear-window)
    (setq max-activation 256) ; maximum activation a node can have
    (setq text-size 5) ; pixel size of text (approximate)
    (setq wheight (window-height))
    (setq wwidth (window-width))
    (setq origin-x 0) ; x-coordinate of origin of display
    (setq origin-y 0) ; y-coordinate of origin of display
    (setq num-of-chars 3) ; number of characters allowed in title
    (setup-regions node-list) ; set up the regions in which the nodes will
                              ; be displayed
    (make-boxsizes) ; sets up a global vector of the boxsize for each
                    ; possible activation
    (loop for node in node-list do (send node :set-old-boxsize 0)))


(defun display-pnet (node-list)
    (outline-regions node-list)
    (update-pnet-display node-list))


(defun setup-regions (node-list)
; Sets up the display for the pnodes in the given list.
; This function first calculates the area of the graphics window,
; WINDOW-AREA. Then it calculates the area each square pnode
; region will get (REGION-AREA = WINDOW-AREA / NUM-OF-NODES),
; and proceeds to assign coordinates for the upper-left-hand
; corners of each pnode region.

    (let ((num-of-nodes (length node-list))
          (dummy-list node-list)
          window-area region-area
          num-of-regions-in-width num-of-regions-in-height)
;;
;; Feb 27 15:35 1987  pnet-graphics.l Page 2
;;
    (setq window-area (* wwidth wheight))
    (setq region-area (/ window-area num-of-nodes))
    (setq region-side  (fix (sqrt region-area)))


; Here I assume height > width, since I'm going to use windows where
; that's true. (I.e., this is a kludge.)
    (setq num-of-regions-in-height (/ wheight region-side))
    (setq num-of-regions-in-width (1+ (/ wwidth region-side)))
; Make sure there are enough regions
   (if (< (* num-of-regions-in-height num-of-regions-in-width)
          num-of-nodes)
    then  ; this is a kludge, but it works
        (if (or (= num-of-regions-in-height
                   (1+ num-of-regions-in-width))
                (= num-of-regions-in-height
                   num-of-regions-in-width))
         then (incf num-of-regions-in-height)
         else
             (setq num-of-regions-in-height
                   (1- num-of-regions-in-height))
             (setq num-of-regions-in-width
                   (1+ num-of-regions-in-width))))
; Make sure all the regions will fit on the screen
    (if (< wwidth (* num-of-regions-in-width region-side))
     then (setq region-side (/ wwidth num-of-regions-in-width)))
; the diminished region size is the max size box for
; displaying a node
    (setq diminished-region-side (fix (times region-side .70)))
    ; offset for drawing in text
    (setq text-offset (fix (times diminished-region-side .02)))
    (loop for i from 0 to (1- num-of-regions-in-width) do
       (loop for j from 0 to (1- num-of-regions-in-height) do
          (let ((node (car dummy-list)))
             (send node :set-x-region (+ origin-x (* i region-side)))
             (send node :set-y-region (+ origin-y (* j region-side)))

             (send node :set-x-act (+ diminished-region-side
                                      (minus text-size)
                                      (send node :x-region)))
             (send node :set-y-act (+ (* 3 text-size)
                                      (send node :y-region)))
             (setq dummy-list (cdr dummy-list)))

        until (null dummy-list)) until (null dummy-list))))


(defun make-boxsizes ()
; Sets up a global vector containing the boxsizes, indexed by activation.
   (setq boxsizes (new-vector (1+ max-activation)))
   (loop for activation from 0 to max-activation do
      (vset boxsizes activation
           (round-off (times (float diminished-region-side)
                       (quotient (float activation)
                                 (float max-activation)))))))

;;
;; Feb 27 15:35 1987  pnet-graphics.l Page 3
;;

(defun outline-regions (node-list)
; Outlines the regions for displaying the pnet
    (loop for node in node-list do
       (let* ((box-x (send node :x-region))
              (box-y (send node :y-region)))
         (draw-unfilled-rect box-x box-y
                                 (+ box-x region-side)
                                 (+ box-y region-side))
          (draw-text (+ box-x text-offset)
                             (- (+ box-y region-side) text-offset)
                             (send node :short-name)))))


(defmethod (pnode :new-boxsize) ()
; Computes the box size for the node (a function of its current activation).
; The function for box size of a node is
;  region-side * (activation / max-activation)
; If activation > max-activation, then max-activation is used.

      (cond ((> activation max-activation)
             diminished-region-side)  ; max size
            ((< activation 0)
             0)
            (t (vref boxsizes (fix activation)))))


(defmethod (pnode :new-box-coordinates) ()
; Returns a dotted pair of the x and y coordinates of the upper-left-hand
; corner of the box that is to be drawn.
  (cons (+ x-region
           (/ (- region-side (send self :new-boxsize)) 2))
        (+ y-region
           (/ (- region-side (send self :new-boxsize)) 2))))


(defmethod (pnode :draw-box) ()
; Draws the box corresponding to the current pnode

 (let ((old-boxsize old-boxsize)
       (new-boxsize (send self :new-boxsize))
       (new-x-box (car (send self :new-box-coordinates)))
       (new-y-box (cdr (send self :new-box-coordinates))))
 ; If old box size > new-boxsize then shrink old box
         (if (> old-boxsize new-boxsize)
          then (if (< (- old-boxsize new-boxsize) 10)
                then (erasebox old-boxsize x-box y-box)
                     (drawbox new-boxsize new-x-box new-y-box)
                else (shrink-box old-boxsize new-boxsize
                                 x-box y-box
                                 new-x-box new-y-box))
          else  ; expand old box
            (if (> activation 0)
             then (drawbox new-boxsize new-x-box new-y-box)))
;;
;; Feb 27 15:35 1987  pnet-graphics.l Page 4
;;
; erase old-activation and display new-activation numbers
        (let ((x-coord x-act)
              (y-coord y-act))
         (erase-rect x-coord
                     (- y-coord 10)
                     (+ x-coord 30)
                     y-coord)

         (draw-number x-coord y-coord (fix activation)))

; Save size of box and coordinates
         (send self :set-old-boxsize (send self :new-boxsize))
         (send self :set-x-box new-x-box)
         (send self :set-y-box new-y-box)))


(defun update-pnet-display (node-list)
; Displays boxes corresponding to the nodes in the node-list
      (loop for node in node-list do
          (if (> (send node :activation) 0)
           then (send node :draw-box))))


(defun shrink-box (oldboxsize newboxsize oldx oldy newx newy)
; Shrinks box from oldboxsize to newboxsize.
   (let ((var1 (+ oldx oldboxsize))
         (var2 (+ oldy oldboxsize))
         (var3 (+ newx newboxsize))
         (var4 (+ newy newboxsize)))
     (erase-rect oldx oldy var1 newy)
     (erase-rect var3 newy var1 var2)
     (erase-rect oldx var4 var1 var2)
     (erase-rect oldx oldy newx var4)))

(defun drawbox (boxsize x y)
; Draws a filled box with upper-left-hand corner (x y) and side boxsize.
   (draw-rect x y (+ x boxsize) (+ y boxsize)))


(defun erasebox (boxsize x y)
; Erases a box with upper-left-hand corner (x y) and side boxsize.
   (erase-rect x y (+ x boxsize) (+ y boxsize)))


(defun round-off (realnum)
; Rounds off a real number (to an integer).
  (fix (plus realnum 0.5)))
