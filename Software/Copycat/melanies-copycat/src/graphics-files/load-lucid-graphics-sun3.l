(in-package 'user)

(proclaim '(special *pixwin* %graphics-viewport-width% %graphics-viewport-height% 
                    %graphics-viewport-x% %graphics-viewport-y%))

(def-foreign-function (init-screen (:name "_init_screen") (:language :c))
                      (newpixwin :fixnum) 
          	      (new-screen-width :fixnum) 
  		      (new-screen-height :fixnum)
		      (new-origin-x :fixnum)
		      (new-origin-y :fixnum))

;---------------------------------------------

; (def-foreign-function (window-width (:name "_window_width") (:language :c)))

;---------------------------------------------

;(def-foreign-function (window-height (:name "_window_height") (:language :c)))

;---------------------------------------------

(def-foreign-function (clear-window (:name "_clear_window") (:language :c)))

;---------------------------------------------

(def-foreign-function (draw-line (:name "_draw_line") (:language :c))
                      (x1 :fixnum) 
		      (y1 :fixnum) 
  		      (x2 :fixnum)
		      (y2 :fixnum))

;---------------------------------------------

(def-foreign-function (erase-line (:name "_erase_line") (:language :c))
                      (x1 :fixnum) 
		      (y1 :fixnum) 
  		      (x2 :fixnum)
		      (y2 :fixnum))

;---------------------------------------------

(def-foreign-function (xor-line (:name "_xor_line") (:language :c))
                      (x1 :fixnum) 
		      (y1 :fixnum) 
  		      (x2 :fixnum)
		      (y2 :fixnum))

;---------------------------------------------

(def-foreign-function (draw-dashed-line (:name "_draw_dashed_line") (:language :c))
                      (x1 :fixnum) 
		      (y1 :fixnum) 
  		      (x2 :fixnum)
		      (y2 :fixnum)
		      (dash-length :fixnum)
		      (space-length :fixnum))

;---------------------------------------------

(def-foreign-function (erase-dashed-line (:name "_erase_dashed_line") (:language :c))
                      (x1 :fixnum) 
		      (y1 :fixnum) 
  		      (x2 :fixnum)
		      (y2 :fixnum)
		      (dash-length :fixnum)
		      (space-length :fixnum))

;---------------------------------------------

(def-foreign-function (xor-dashed-line (:name "_xor_dashed_line") (:language :c))
                      (x1 :fixnum) 
                      (y1 :fixnum) 
                      (x2 :fixnum)
                      (y2 :fixnum)
                      (dash-length :fixnum)
                      (space-length :fixnum))

;---------------------------------------------

(def-foreign-function (c-draw-jagged-line (:name "_draw_jagged_line") (:language :c))
                      (x1 :fixnum) 
                      (y1 :fixnum) 
                      (x2 :fixnum)
                      (y2 :fixnum)
                      (jag-length :fixnum)
                      (line-length :fixnum))

;---------------------------------------------

(def-foreign-function (c-erase-jagged-line (:name "_erase_jagged_line") (:language :c))
                      (x1 :fixnum) 
                      (y1 :fixnum) 
                      (x2 :fixnum)
                      (y2 :fixnum)
                      (jag-length :fixnum)
                      (line-length :fixnum))

;---------------------------------------------

(def-foreign-function (c-xor-jagged-line (:name "_xor_jagged_line") (:language :c))
                      (x1 :fixnum) 
                      (y1 :fixnum) 
                      (x2 :fixnum)
                      (y2 :fixnum)
                      (jag-length :fixnum)
                      (line-length :fixnum))

;---------------------------------------------

(def-foreign-function (draw-circle-arc (:name "_draw_circle_arc") (:language :c))
                      (ctr-x :fixnum) 
                      (ctr-y :fixnum) 
                      (radius :fixnum)
                      (begin-angle :fixnum)
                      (end-angle :fixnum))

;---------------------------------------------

(def-foreign-function (erase-circle-arc (:name "_erase_circle_arc") (:language :c))
                      (ctr-x :fixnum) 
                      (ctr-y :fixnum) 
                      (radius :fixnum)
                      (begin-angle :fixnum)
                      (end-angle :fixnum))

;---------------------------------------------

(def-foreign-function (set-font (:name "_set_font") (:language :c))
                      (font :simple-string))
  
;---------------------------------------------

(def-foreign-function (default-font (:name "_default_font") (:language :c)))
  
;---------------------------------------------

(def-foreign-function (text-length (:name "_text_length") (:language :c))
                      (text :simple-string))
  
;---------------------------------------------

(def-foreign-function (number-length (:name "_number_length") (:language :c))
                      (number :fixnum))
  
;---------------------------------------------

(def-foreign-function (draw-number (:name "_draw_number") (:language :c))
                      (x :fixnum)
                      (y :fixnum)
                      (number :fixnum))
  
;---------------------------------------------

(def-foreign-function (erase-number (:name "_erase_number") (:language :c))
                      (x :fixnum)
                      (y :fixnum)
                      (number :fixnum))
  
;---------------------------------------------

(def-foreign-function (xor-number (:name "_xor_number") (:language :c))
                      (x :fixnum)
                      (y :fixnum)
                      (number :fixnum))
  
;---------------------------------------------

(def-foreign-function (draw-text (:name "_draw_text") (:language :c))
                      (x :fixnum)
                      (y :fixnum)
                      (text :simple-string))
  
;---------------------------------------------

(def-foreign-function (erase-text (:name "_erase_text") (:language :c))
                      (x :fixnum)
                      (y :fixnum)
                      (text :simple-string))
  
;---------------------------------------------

(def-foreign-function (xor-text (:name "_xor_text") (:language :c))
                      (x :fixnum)
                      (y :fixnum)
                      (text :simple-string))
  
;---------------------------------------------

(def-foreign-function (draw-centered-number (:name "_draw_centered_number") (:language :c))
                      (x :fixnum)
                      (y :fixnum)
                      (number :fixnum)
                      (limit :fixnum))
  

;---------------------------------------------

(def-foreign-function (erase-centered-number (:name "_erase_centered_number") (:language :c))
                      (x :fixnum)
                      (y :fixnum)
                      (number :fixnum)
                      (limit :fixnum))
  
;---------------------------------------------

(def-foreign-function (xor-centered-number (:name "_xor_centered_number") (:language :c))
                      (x :fixnum)
                      (y :fixnum)
                      (number :fixnum)
                      (limit :fixnum))
  

;---------------------------------------------

(def-foreign-function (draw-centered-text (:name "_draw_centered_text") (:language :c))
                      (x :fixnum)
                      (y :fixnum)
                      (text :simple-string)
                      (limit :fixnum))
  
;---------------------------------------------

(def-foreign-function (erase-centered-text (:name "_erase_centered_text") (:language :c))
                      (x :fixnum)
                      (y :fixnum)
                      (text :simple-string)
                      (limit :fixnum))
  
;---------------------------------------------

(def-foreign-function (xor-centered-text (:name "_xor_centered_text") (:language :c))
                      (x :fixnum)
                      (y :fixnum)
                      (text :simple-string)
                      (limit :fixnum))
  

;---------------------------------------------

(def-foreign-function (draw-solid-rectangle (:name "_draw_solid_rectangle") (:language :c))
                      (x1 :fixnum)
                      (y1 :fixnum)
                      (x2 :fixnum)
                      (y2 :fixnum))
  
;---------------------------------------------

(def-foreign-function (erase-solid-rectangle (:name "_erase_solid_rectangle") (:language :c))
                      (x1 :fixnum)
                      (y1 :fixnum)
                      (x2 :fixnum)
                      (y2 :fixnum))
  
 
;---------------------------------------------

(def-foreign-function (xor-solid-rectangle (:name "_xor_solid_rectangle") (:language :c))
                      (x1 :fixnum)
                      (y1 :fixnum)
                      (x2 :fixnum)
                      (y2 :fixnum))
  
;---------------------------------------------

(def-foreign-function (draw-point (:name "_draw_point") (:language :c))
                      (x :fixnum)
                      (y :fixnum))

;---------------------------------------------

(def-foreign-function (erase-point (:name "_erase_point") (:language :c))
                      (x :fixnum)
                      (y :fixnum))

;---------------------------------------------

(def-foreign-function (xor-point (:name "_xor_point") (:language :c))
                      (x :fixnum)
                      (y :fixnum))

;---------------------------------------------

(def-foreign-function (draw-unfilled-rectangle (:name "_draw_unfilled_rectangle") (:language :c))
                      (x1 :fixnum)
                      (y1 :fixnum)
                      (x2 :fixnum)
                      (y2 :fixnum))
  
;---------------------------------------------

(def-foreign-function (erase-unfilled-rectangle (:name "_erase_unfilled_rectangle") (:language :c))
                      (x1 :fixnum)
                      (y1 :fixnum)
                      (x2 :fixnum)
                      (y2 :fixnum))
  
;---------------------------------------------

(def-foreign-function (xor-unfilled-rectangle (:name "_xor_unfilled_rectangle") (:language :c))
                      (x1 :fixnum)
                      (y1 :fixnum)
                      (x2 :fixnum)
                      (y2 :fixnum))
  
;---------------------------------------------

(def-foreign-function (solve-parabola (:name "_solve_parabola") (:language :c))
                      (x1 :fixnum)
                      (y1 :fixnum)
                      (x2 :fixnum)
                      (y2 :fixnum))
  
;---------------------------------------------

(def-foreign-function (c-draw-parabola (:name "_c_draw_parabola") (:language :c))
                      (x1 :fixnum)
                      (x2 :fixnum)
                      (width :fixnum))
  
;---------------------------------------------

(def-foreign-function (c-erase-parabola (:name "_c_erase_parabola") (:language :c))
                      (x1 :fixnum)
                      (x2 :fixnum)
                      (width :fixnum))
  
;---------------------------------------------

(def-foreign-function (c-xor-parabola (:name "_c_xor_parabola") (:language :c))
                      (x1 :fixnum)
                      (x2 :fixnum)
                      (width :fixnum))
  
;---------------------------------------------

(def-foreign-function (c-draw-dashed-parabola (:name "_c_draw_dashed_parabola") (:language :c))
                      (x1 :fixnum)
                      (x2 :fixnum)
                      (width :fixnum)
                      (dash-length :fixnum)
                      (space-length :fixnum))

;---------------------------------------------

(def-foreign-function (c-erase-dashed-parabola (:name "_c_erase_dashed_parabola") (:language :c))
                      (x1 :fixnum)
                      (x2 :fixnum)
                      (width :fixnum)
                      (dash-length :fixnum)
                      (space-length :fixnum))
  
;---------------------------------------------

(def-foreign-function (c-xor-dashed-parabola (:name "_c_xor_dashed_parabola") (:language :c))
                      (x1 :fixnum)
                      (x2 :fixnum)
                      (width :fixnum)
                      (dash-length :fixnum)
                      (space-length :fixnum))

;---------------------------------------------

(load-foreign-files '("lucid-graphics.o") '("-lsuntool" "-lsunwindow" "-lpixrect" "-lc" "-lm"))

;---------------------------------------------

(defun open-window (window-width window-height screen-x screen-y)
; Opens a graphics viewport called *graphics-viewport*.
  (declare (special *graphics-viewport*)
           (special *current-font*))
  (setq *graphics-viewport* 
        (make-viewport 
	    :width window-width :height window-height
	    :screen-x screen-x :screen-y screen-y))
  (setq *current-font* *default-font*)
  (setq *pixwin* (windows::current-pixwin))

  (init-screen *pixwin* %graphics-viewport-width% %graphics-viewport-height%
                        %graphics-viewport-x% %graphics-viewport-y%))

;---------------------------------------------

(defun draw-jagged-line (x1 y1 x2 y2 jag-length &optional line-length)
  (if* (null line-length)
   then (setq line-length (round (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1)))))))
  (c-draw-jagged-line x1 y1 x2 y2 jag-length line-length))

;---------------------------------------------

(defun erase-jagged-line (x1 y1 x2 y2 jag-length &optional line-length)
  (if* (null line-length)
   then (setq line-length (round (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1)))))))
  (c-erase-jagged-line x1 y1 x2 y2 jag-length line-length))

;---------------------------------------------

(defun xor-jagged-line (x1 y1 x2 y2 jag-length &optional line-length)
  (if* (null line-length)
   then (setq line-length (round (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1)))))))
  (c-xor-jagged-line x1 y1 x2 y2 jag-length line-length))

;---------------------------------------------

(defun draw-dashed-rectangle (x1 y1 x2 y2 dash-length space-length)
  (draw-dashed-line x1 y1 x2 y1 dash-length space-length)
  (draw-dashed-line x2 y1 x2 y2 dash-length space-length)
  (draw-dashed-line x2 y2 x1 y2 dash-length space-length)
  (draw-dashed-line x1 y2 x1 y1 dash-length space-length))

;---------------------------------------------

(defun erase-dashed-rectangle (x1 y1 x2 y2 dash-length space-length)
  (erase-dashed-line x1 y1 x2 y1 dash-length space-length)
  (erase-dashed-line x2 y1 x2 y2 dash-length space-length)
  (erase-dashed-line x2 y2 x1 y2 dash-length space-length)
  (erase-dashed-line x1 y2 x1 y1 dash-length space-length))

;---------------------------------------------

(defun xor-dashed-rectangle (x1 y1 x2 y2 dash-length space-length)
  (xor-dashed-line x1 y1 x2 y1 dash-length space-length)
  (xor-dashed-line x2 y1 x2 y2 dash-length space-length)
  (xor-dashed-line x2 y2 x1 y2 dash-length space-length)
  (xor-dashed-line x1 y2 x1 y1 dash-length space-length))

;---------------------------------------------

(defun op-parabola (x1 y1 x2 y2 x3 y3 width op)
; General function for parabolas.  (X1, Y1), (X2, Y2), and (X3, Y3)
; respectively give the x and y coordinates for the starting point, 
; the middle point, and the ending point of the parabola.  The WIDTH 
; parameter gives the width of the lines making up the parabola.

  (solve-parabola x1 y1 x2 y2)
  (case op
    (draw (c-draw-parabola x1 x2 width))
    (erase (c-erase-parabola x1 x2 width))
    (xor (c-xor-parabola x1 x2 width)))
  (solve-parabola x3 y3 x2 y2)
  (case op
    (draw (c-draw-parabola x2 x3 width))
    (erase (c-erase-parabola x2 x3 width))
    (xor (c-xor-parabola x2 x3 width))))

;---------------------------------------------

(defun draw-parabola (x1 y1 x2 y2 x3 y3 width)
  (op-parabola x1 y1 x2 y2 x3 y3 width 'draw))

;---------------------------------------------

(defun erase-parabola (x1 y1 x2 y2 x3 y3 width)
  (op-parabola x1 y1 x2 y2 x3 y3 width 'erase))

;---------------------------------------------

(defun xor-parabola (x1 y1 x2 y2 x3 y3 width)
  (op-parabola x1 y1 x2 y2 x3 y3 width 'xor))


;---------------------------------------------

(defun op-dashed-parabola (x1 y1 x2 y2 x3 y3 width dash-length space-length op)
  (solve-parabola x1 y1 x2 y2)
  (case op
    (draw (c-draw-dashed-parabola x1 x2 width dash-length space-length))
    (erase (c-erase-dashed-parabola x1 x2 width dash-length space-length))
    (xor (c-xor-dashed-parabola x1 x2 width dash-length space-length)))
  (solve-parabola x3 y3 x2 y2)
  (case op
    (draw (c-draw-dashed-parabola x2 x3 width dash-length space-length))
    (erase (c-erase-dashed-parabola x2 x3 width dash-length space-length))
    (xor (c-xor-dashed-parabola x2 x3 width dash-length space-length))))

;---------------------------------------------

(defun draw-dashed-parabola (x1 y1 x2 y2 x3 y3 width dash-length space-length)
  (op-dashed-parabola x1 y1 x2 y2 x3 y3 width dash-length space-length 'draw))

;---------------------------------------------

(defun erase-dashed-parabola (x1 y1 x2 y2 x3 y3 width dash-length space-length)
  (op-dashed-parabola x1 y1 x2 y2 x3 y3 width dash-length space-length 'erase))

;---------------------------------------------

(defun xor-dashed-parabola (x1 y1 x2 y2 x3 y3 width dash-length space-length)
  (op-dashed-parabola x1 y1 x2 y2 x3 y3 width dash-length space-length 'xor))

;---------------------------------------------

(defun op-circle* (op x y radius &optional (width 1))
  (declare (special *graphics-viewport*))
  (draw-circle *graphics-viewport* (make-position x y) radius 
               :width width :operation op))

(defun draw-circle* (x y radius &optional (width 1))
  (op-circle* boole-1 x y radius width))

(defun erase-circle* (x y radius &optional (width 1))
  (op-circle* boole-clr x y radius width))

(defun xor-circle* (x y radius &optional (width 1))
  (op-circle* boole-xor x y radius width))

;---------------------------------------------


;---------------------------------------------

; The following is a list of some of the fonts that
; are available.  More can be loaded.  There are also
; some preloaded into Lucid Common Lisp that don't appear
; in this list.

(let ((font-list
       (list "/usr/lib/fonts/fixedwidthfonts/apl.r.10" 
             "/usr/lib/fonts/fixedwidthfonts/cmr.b.14"
             "/usr/lib/fonts/fixedwidthfonts/cmr.r.14"
             "/usr/lib/fonts/fixedwidthfonts/cour.b.10"
             "/usr/lib/fonts/fixedwidthfonts/cour.b.12"
             "/usr/lib/fonts/fixedwidthfonts/cour.b.14"
             "/usr/lib/fonts/fixedwidthfonts/cour.b.16"
             "/usr/lib/fonts/fixedwidthfonts/cour.b.18"
             "/usr/lib/fonts/fixedwidthfonts/cour.b.24"
             "/usr/lib/fonts/fixedwidthfonts/cour.r.10"
             "/usr/lib/fonts/fixedwidthfonts/cour.r.12"
             "/usr/lib/fonts/fixedwidthfonts/cour.r.14"
             "/usr/lib/fonts/fixedwidthfonts/cour.r.16"
             "/usr/lib/fonts/fixedwidthfonts/cour.r.18"
             "/usr/lib/fonts/fixedwidthfonts/cour.r.24"
             "/usr/lib/fonts/fixedwidthfonts/gallant.r.19"
             "/usr/lib/fonts/fixedwidthfonts/screen.b.12"
             "/usr/lib/fonts/fixedwidthfonts/screen.b.14"
             "/usr/lib/fonts/fixedwidthfonts/screen.b.16"
             "/usr/lib/fonts/fixedwidthfonts/screen.r.11"
             "/usr/lib/fonts/fixedwidthfonts/screen.r.12"
             "/usr/lib/fonts/fixedwidthfonts/screen.r.14"
             "/usr/lib/fonts/fixedwidthfonts/screen.r.16"
             "/usr/lib/fonts/fixedwidthfonts/screen.r.7"
             "/usr/lib/fonts/fixedwidthfonts/serif.r.10"
             "/usr/lib/fonts/fixedwidthfonts/serif.r.12"
             "/usr/lib/fonts/fixedwidthfonts/serif.r.14"
             "/usr/lib/fonts/fixedwidthfonts/serif.r.16")))

  (loop for font in font-list do 
        (load-font font :foreign-font-type 'vfont)))
