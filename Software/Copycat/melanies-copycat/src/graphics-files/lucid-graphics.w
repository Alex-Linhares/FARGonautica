Graphics Routines for Lucid Common Lisp
---------------------------------------
Note: These routines assume that only one window, called 
*graphics-viewport*, will be used. Someone should eventually 
generalize this.

Also note:  These routines are mostly written in C, and take the place
of graphics routines built into Lucid Common Lisp.  The C routines are
much faster.

Also note:  You have to open a window before calling any of the drawing 
routines.
-------------------------------------------------------------------

1. Window Routines

OPEN-WINDOW window-width window-height screen-x screen-y   

Opens a graphics window called *graphics-viewport*.  The 
parameters SCREEN-X and SCREEN-Y give the coordinates of 
the upper-left-corner with respect to the root-viewport.  
This function will work only after a call to INITIALIZE-WINDOWS,
which sets up the root viewport.
Example:  (open-window 800 800 0 0)

CLEAR-WINDOW  

Clears the window.
Example: (clear-window)

-------------------------------------------------------------------

2. Line-drawing Routines

DRAW-LINE x1 y1 x2 y2

Draws a line from integer coordinates (x1, y1) to (x2, y2) (these 
coordinates are with respect to the upper-left-corner of 
*graphics-viewport*).  
Example: (draw-line 100 100 200 200)

ERASE-LINE x1 y1 x2 y2  

XOR-LINE x1 y1 x2 y2  

DRAW-DASHED-LINE x1 y1 x2 y2 dash-length space-length  

Draws a dashed line from integer coordinates (x1, y1) to (x2, y2) 
(these coordinates are with respect to the upper-left-corner of 
*graphics-viewport*).  DASH-LENGTH is the length of the dashes and 
SPACE-LENGTH is the length of the spaces between dashes.
Example: (draw-dashed-line 100 100 200 200 4 8)

ERASE-DASHED-LINE x1 y1 x2 y2 dash-length space-length  

XOR-DASHED-LINE x1 y1 x2 y2 dash-length space-length  

DRAW-JAGGED-LINE x1 y1 x2 y2 jag-length &optional jagged-line-length  

Draws a jagged line from integer coordinates (x1, y1) to (x2, y2) 
(these coordinates are with respect to the upper-left-corner of 
*graphics-viewport*).  JAG-LENGTH gives the length of the jags in the line.  
If the optional argument JAGGED-LINE-LENGTH is given, then the line will 
start at (x1, y1) and will be that length; otherwise the line will go 
from (x1, y1) to (x2, y2).
Examples: (draw-jagged-line 100 100 200 200 8)
          (draw-jagged-line 100 100 200 200 8 50)  ; Draws a jagged line of length 50.

ERASE-JAGGED-LINE x1 y1 x2 y2 jag-length &optional jagged-line-length  

XOR-JAGGED-LINE x1 y1 x2 y2 jag-length &optional jagged-line-length  

-------------------------------------------------------------------

3.  Rectangle-drawing routines

DRAW-SOLID-RECTANGLE x1 y1 x2 y2  

Draws a solid rectangle with upper-left corner (x1, y1) and 
lower-right corner (x2, y2).
Example: (draw-solid-rectangle 100 100 200 200) 

ERASE-SOLID-RECTANGLE x1 y1 x2 y2  

XOR-SOLID-RECTANGLE x1 y1 x2 y2  

DRAW-UNFILLED-RECTANGLE x1 y1 x2 y2

Draws an unfilled rectangle with opposite corners (x1, y1) and (x2, y2).
Examples: (draw-unfilled-rectangle 100 100 200 200) 
          (draw-unfilled-rectangle 100 100 200 200 2) 

ERASE-UNFILLED-RECTANGLE x1 y1 x2 y2

XOR-UNFILLED-RECTANGLE x1 y1 x2 y2

DRAW-DASHED-RECTANGLE x1 y1 x2 y2 dash-length space-length  

Draws a dashed rectangle with opposite corners (x1, y1) and (x2, y2).
The parameters DASH-LENGTH and SPACE-LENGTH respectively give the length 
of the dashes and the length of the spaces between dashes.
Example: (draw-dashed-rectangle 100 100 200 200 4 8) 

ERASE-DASHED-RECTANGLE x1 y1 x2 y2 dash-length space-length  

XOR-DASHED-RECTANGLE x1 y1 x2 y2 dash-length space-length  

-------------------------------------------------------------------

4. Circle-drawing routines

; Note that an asterisk is appended to each of these function names 
; to avoid conflicts with existing common-lisp functions.

DRAW-CIRCLE x y radius &optional width  

Draws a circle with the given center and radius.  The optional parameter 
WIDTH gives the width of the circle's outer line.  If WIDTH is not given, 
the circle's outer line is drawn with width 1.
Examples: (draw-circle 100 100 20)
          (draw-circle 100 100 20 5)

ERASE-CIRCLE x y radius &optional width  

XOR-CIRCLE x y radius &optional width  

-------------------------------------------------------------------

5. Point-drawing routines

DRAW-POINT x y  

Draws a point at the given position.
Example: (draw-point 100 100)

ERASE-POINT x y  

XOR-POINT x y  

-------------------------------------------------------------------

6. Parabola-drawing routines

DRAW-PARABOLA x1 y1 x2 y2 x3 y3 width 

The points (x1, y1), (x2, y2), (x3, y3) respectively contain the x and y 
coordinates of the leftmost, middle, and rightmost points on the parabola.  
WIDTH parameter gives the width of the lines making up the parabola.
Examples: (draw-parabola 100 100 250 100 400 100 1)

ERASE-PARABOLA x1 y1 x2 y2 x3 y3 width 

XOR-PARABOLA x1 y1 x2 y2 x3 y3 width 

DRAW-DASHED-PARABOLA x1 y1 x2 y2 x3 y3 width dash-length space-length

This is like DRAW-PARABOLA, but it draws a dashed parabola with
the length of the dashes given by DASH-LENGTH and the length of the 
spaces between dashes given by SPACE-LENGTH

ERASE-DASHED-PARABOLA x1 y1 x2 y2 x3 y3 width dash-length space-length

XOR-DASHED-PARABOLA x1 y1 x2 y2 x3 y3 width dash-length space-length

-------------------------------------------------------------------

7. Text-drawing routines

DRAW-TEXT x y text  

Draws the given string of text at the given position in the font given 
in the global variable *current-font*.  To change fonts, use the function 
SET-FONT (see below).
Examples: (draw-text 100 100 "Here is a string of text")
          (draw-text 100 100 (format nil "The value of var1 is ~a" var1))

ERASE-TEXT x y text  

DRAW-CENTERED-TEXT x y text width  

Draws the given string of text centered in the given width, starting 
at the given position.  Uses the font given in the global variable 
*current-font*.  To change fonts, use the function SET-FONT (see below).
Examples: (draw-centered-text 100 100 "Here is a string of text" 100)
          (draw-text 100 100 (format nil "The value of var1 is ~a" var1) 200)

ERASE-CENTERED-TEXT x y text width  

SET-FONT font-name 

Sets the varible *current-font* to the specified font.  After a call to 
this function, all printing of text in the window *graphics-viewport* 
will be in the given font. 
Example: (set-font "/usr/lib/fonts/fixedwidthfonts/cour.r.12")

The available fonts are:
/usr/lib/fonts/fixedwidthfonts/apl.r.10 
/usr/lib/fonts/fixedwidthfonts/cmr.b.14
/usr/lib/fonts/fixedwidthfonts/cmr.r.14
/usr/lib/fonts/fixedwidthfonts/cour.b.10
/usr/lib/fonts/fixedwidthfonts/cour.b.12
/usr/lib/fonts/fixedwidthfonts/cour.b.14
/usr/lib/fonts/fixedwidthfonts/cour.b.16
/usr/lib/fonts/fixedwidthfonts/cour.b.18
/usr/lib/fonts/fixedwidthfonts/cour.b.24
/usr/lib/fonts/fixedwidthfonts/cour.r.10
/usr/lib/fonts/fixedwidthfonts/cour.r.12
/usr/lib/fonts/fixedwidthfonts/cour.r.14
/usr/lib/fonts/fixedwidthfonts/cour.r.16
/usr/lib/fonts/fixedwidthfonts/cour.r.18
/usr/lib/fonts/fixedwidthfonts/cour.r.24
/usr/lib/fonts/fixedwidthfonts/gallant.r.19
/usr/lib/fonts/fixedwidthfonts/screen.b.12
/usr/lib/fonts/fixedwidthfonts/screen.b.14
/usr/lib/fonts/fixedwidthfonts/screen.b.16
/usr/lib/fonts/fixedwidthfonts/screen.r.11
/usr/lib/fonts/fixedwidthfonts/screen.r.12
/usr/lib/fonts/fixedwidthfonts/screen.r.14
/usr/lib/fonts/fixedwidthfonts/screen.r.16
/usr/lib/fonts/fixedwidthfonts/screen.r.7
/usr/lib/fonts/fixedwidthfonts/serif.r.10
/usr/lib/fonts/fixedwidthfonts/serif.r.12
/usr/lib/fonts/fixedwidthfonts/serif.r.14
/usr/lib/fonts/fixedwidthfonts/serif.r.16

