;---------------------------------------------
; DESCRIPTION GRAPHICS:  This file contains graphics functions for descriptions.
;-------------------------------------------

(in-package 'user)

(defflavor description-graphics-obj 
    (x y obj name parent (previous-font %irrelevant-description-font%))
    ()    
    :gettable-instance-variables
    :settable-instance-variables
    :initable-instance-variables)

;---------------------------------------------

(defmethod (workspace-object :init-description-graphics) (d)
  (send d :init-graphics self)
  (push d *description-graphics-obj-list*)
  (if* %description-graphics% then (send d :draw)))

;---------------------------------------------

(defmethod (description :init-graphics) (obj)
  (send self :set-graphics-obj
	(make-instance 'description-graphics-obj 
	    :x (send (send obj :graphics-obj) :description-x)
	    :y (- (send (send obj :graphics-obj) :description-y) 
	          (* %space-between-descriptions% (1+ description-number)))
	    :name (send (send self :descriptor) :cm-name)
            :parent self
	    :obj obj)))

;---------------------------------------------

(defmethod (description-graphics-obj :draw) ()
  (if* (not (eq (send parent :description-type) plato-length))
   then (send self :erase)
        (if* (send parent :relevant?) 
         then (set-font %relevant-description-font%)
              (send self :set-previous-font %relevant-description-font%)
         else (set-font %irrelevant-description-font%)
              (send self :set-previous-font %irrelevant-description-font%))
        (draw-text x y name)
        (set-font %workspace-font%)))

;---------------------------------------------

(defmethod (description-graphics-obj :erase) ()
  (if* (not (eq (send parent :description-type) plato-length))
   then (set-font previous-font)
        (erase-text x y name)
        (set-font %workspace-font%)))

;---------------------------------------------

(defmethod (description :draw) ()
  (send graphics-obj :draw))

;---------------------------------------------

(defmethod (description :erase) ()
  (send graphics-obj :erase))

;---------------------------------------------

(defun display-descriptions ()
  (loop for graphics-obj in *description-graphics-obj-list* do
        (send graphics-obj :draw)))

;---------------------------------------------

(defun redisplay-descriptions ()
  (loop for graphics-obj in *description-graphics-obj-list* do
        (send graphics-obj :draw)))

;---------------------------------------------

(defun erase-descriptions ()
  (loop for graphics-obj in *description-graphics-obj-list* do
        (send graphics-obj :erase)))

;---------------------------------------------

