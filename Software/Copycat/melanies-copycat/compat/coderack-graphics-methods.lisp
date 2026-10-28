;;; coderack-graphics-methods.lisp -- two methods of the coderack flavor that
;;; coderack.l calls for every codelet posted or removed, graphics or not,
;;; and the two helpers they use.  The methods keep the per-type, per-bin
;;; counts in the coderack's codelet-urgency-array, which only the bar-graph
;;; display reads.
;;;
;;; Copied as written from src/graphics-files/coderack-graphics.l, lines
;;; 81-110 (Melanie Mitchell).  This file is loaded after coderack.l, since
;;; the methods need the flavor to exist; see load.lisp.

(cl:in-package :copycat)

(defun get-codelet-number (codelet-name)
  (- (length %codelet-types%) (length (memq codelet-name %codelet-types%))))

;---------------------------------------------

(defun get-codelet-name (codelet-number)
  (nth codelet-number %codelet-types%))

;---------------------------------------------

(defmethod (coderack :add-codelet-to-graphics) 
           (codelet &aux codelet-number urgency-bin)

  (setq codelet-number (get-codelet-number (send codelet :codelet-type)))
  (setq urgency-bin (send codelet :urgency-bin))
  (aset codelet-urgency-array 
	codelet-number (send urgency-bin :urgency-code)
        (1+ (aref codelet-urgency-array codelet-number 
		  (send urgency-bin :urgency-code)))))
                               
;---------------------------------------------

(defmethod (coderack :delete-codelet-from-graphics) 
           (codelet &aux codelet-number urgency-bin)  
  (setq codelet-number (get-codelet-number (send codelet :codelet-type)))
  (setq urgency-bin (send codelet :urgency-bin))
  (aset codelet-urgency-array 
	codelet-number (send urgency-bin :urgency-code)
        (1- (aref codelet-urgency-array codelet-number 
		  (send urgency-bin :urgency-code)))))
