;---------------------------------------------
; LETTER GRAPHICS: This file contains graphics functions for letters.
;-------------------------------------------

(in-package 'user)

(defflavor letter-graphics-obj 
    (x y  ; Coordinates for letter.
     parent ; Letter this graphics object represents
     bond-left-x bond-right-x bond-y  ; Coordinates for bonds 
                                                  ; attached to this letter.
     replacement-x replacement-y ; Coordinates for replacements attached to
                                 ; this letter.

     correspondence-x correspondence-y ; Coordinates for correspondences
                                       ; attached to this letter.

     description-x description-y) ; Coordinates for descriptions attached to
                                  ; this letter.
    ()    
    :gettable-instance-variables
    :settable-instance-variables
    :initable-instance-variables)

;---------------------------------------------

(defmethod (letter-graphics-obj :draw) ()
  (draw-text x y (send parent :pname)))

(defmethod (letter :x) ()
  (send graphics-obj :x))

(defmethod (letter :y) ()
  (send graphics-obj :y))

(defmethod (letter :draw) ()
  (send graphics-obj :draw))


	
  