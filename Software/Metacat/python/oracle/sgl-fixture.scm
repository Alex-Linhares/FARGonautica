;;; sgl-fixture.scm -- a fixture of every SGL form, as data (item 13).
;;;
;;; Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Read by python/oracle/sgl-tcl.ss (Chez: the original's sgl-interpreter.ss
;;; and fonts.ss, recording every Tcl command they send) and by
;;; python/tests/test_sgl.py (Python: metacat/gui/sgl.py).  The pictures are
;;; racket/tests/sgl-fixture.rkt's, so the rendering can be compared with
;;; racket/tests/snapshots/sgl-fixture.png.  Both readers interpret the same
;;; three kinds of form:
;;;
;;;   (fonts (NAME FACE SIZE STYLE) ...)   NAME = (make-mfont FACE SIZE STYLE),
;;;                                        FACE one of serif, sans-serif, fancy
;;;   (viewport NAME W H XMIN YMIN XMAX YMAX)
;;;                                        a <viewport> of W x H pixels showing
;;;                                        [XMIN, XMAX] x [YMIN, YMAX], with the
;;;                                        transforms of racket/tests/sgl-fixture.rkt
;;;   (ops OP ...)                         run on every viewport, in order:
;;;     (draw PEXP)  (draw PEXP TAG)       draw!
;;;     (erase PEXP)                       erase!
;;;     (send METHOD ARG ...)              a viewport method; an argument
;;;                                        (color "name") is (swl-color "name")
;;;
;;; In a PEXP, (cell X Y LABEL PEXP ...) stands for
;;;   (let-sgl ((origin (X Y)))
;;;     (let-sgl ((font label-font) (foreground-color "grey40")) (text (4 92) LABEL))
;;;     (let-sgl ((foreground-color "grey80")) (rectangle (0 0) (155 105)))
;;;     PEXP ...)
;;; and a binding (font NAME) is the font NAME of the fonts form.

(fonts (title-font sans-serif 18 (bold italic))
       (label-font sans-serif -11 ())
       (letter-font serif -22 (bold italic))
       (small-font fancy -12 (italic)))

(viewport v1 640 480 0 0 640 480)
(viewport v2 320 240 0 0 640 480)

(ops
  (draw
    (let-sgl ()
      ;; drawn, then cleared: none of it may show
      (filled-rectangle (0 0) (640 480))
      (let-sgl ((font label-font)) (text (100 100) "cleared"))
      (clear "ivory")
      (let-sgl ((font title-font) (text-justification center))
        (text (320 452) "SGL fixture"))
      (cell 0 335 "rectangle"
        (rectangle (10 10) (60 60))
        (let-sgl ((line-width 3) (foreground-color "red"))
          (rectangle (70 10) (110 50)))
        (let-sgl ((line-style dashed) (foreground-color "blue"))
          (rectangle (10 65) (145 85)))
        (let-sgl ((line-style dotted))
          (rectangle (118 10) (148 55))))
      (cell 160 335 "filled-rectangle"
        (filled-rectangle (10 10) (50 50))
        (let-sgl ((foreground-color "forest green"))
          (filled-rectangle (60 20) (100 80)))
        (let-sgl ((foreground-color "gold"))
          (filled-rectangle (110 10) (150 40))))
      (cell 320 335 "arc, filled-arc"
        (arc (40 40) (60 60) 0 180)
        (let-sgl ((foreground-color "red") (line-width 2))
          (arc (40 40) (40 40) 45 400))
        (let-sgl ((line-style dashed) (foreground-color "blue"))
          (arc (40 15) (60 20) 180 180))
        (let-sgl ((foreground-color "dark orange"))
          (filled-arc (115 45) (60 60) 30 300))
        (let-sgl ((foreground-color "purple"))
          (filled-arc (115 45) (16 16) 0 360)))
      (cell 480 335 "line, polyline"
        (line (10 10) (60 60) (10 60) (60 10))
        (let-sgl ((line-width 4) (foreground-color "dark green"))
          (polyline (75 10) (95 60) (115 10) (135 60)))
        (let-sgl ((line-style dashed) (foreground-color "blue"))
          (polyline (10 75) (80 85) (150 75)))
        (let-sgl ((line-style dotted)) (line (70 70) (150 70))))
      (cell 0 225 "polygon, filled-polygon"
        (let-sgl ((background-color "light blue"))
          (polygon (10 10) (70 10) (40 70)))
        (let-sgl ((foreground-color "brown"))
          (filled-polygon (80 10) (150 10) (150 60) (115 80) (80 60)))
        (let-sgl ((line-style dashed) (line-width 2) (background-color "ivory"))
          (polygon (20 20) (60 20) (40 50))))
      (cell 160 225 "polypoints, dashed"
        (polypoints (10 20) (14 20) (18 20) (22 20) (26 24) (30 28) (34 32))
        (let-sgl ((foreground-color "red"))
          (dashed-polypoints (10 50) (18 50) (26 50) (34 50) (42 50) (50 50)))
        (let-sgl ((foreground-color "blue"))
          (polypoints (70 10) (70 12) (70 14) (70 16) (70 18) (70 20))))
      (cell 320 225 "ring"
        (ring (40 45) 60 40)
        (let-sgl ((foreground-color "steel blue") (background-color "ivory"))
          (ring (115 45) 60 30 90 225))
        (let-sgl ((foreground-color "red") (background-color "ivory"))
          (ring (115 45) 20 10)))
      (cell 480 225 "text justification"
        (let-sgl ((foreground-color "red"))
          (line (80 0) (80 85))
          (line (0 20) (155 20) (0 45) (155 45) (0 70) (155 70)))
        (let-sgl ((origin (80 70)) (font letter-font))
          (let-sgl ((text-justification left)) (text "left")))
        (let-sgl ((origin (80 45)) (font letter-font))
          (let-sgl ((text-justification center)) (text "center")))
        (let-sgl ((origin (80 20)) (font letter-font))
          (let-sgl ((text-justification right)) (text "right"))))
      (cell 0 115 "text: at, relative, image"
        (let-sgl ((font letter-font))
          (text (10 60) "abc")
          (text (10 30) "x")
          (let-sgl ((origin (10 30)))
            (text (text-relative (1 0)) "y")
            (text (text-relative (2 1)) "z")
            (text (text-relative (3 -1)) "w")))
        (let-sgl ((foreground-color "light grey"))
          (filled-rectangle (90 10) (150 80)))
        (let-sgl ((font letter-font) (text-mode image)
                  (background-color "light yellow") (foreground-color "dark blue"))
          (text (95 50) "img"))
        (let-sgl ((font small-font) (foreground-color "dark red"))
          (text (95 15) "fancy")))
      (cell 160 115 "erase"
        (let-sgl ((foreground-color "orange"))
          (filled-rectangle (10 10) (145 80)))
        (erase "ivory" (filled-rectangle (30 30) (60 60)))
        (erase "white" (let-sgl ((line-width 5)) (line (70 20) (140 70))))
        (let-sgl ((foreground-color "black"))
          (rectangle (10 10) (145 80))
          (erase "orange" (rectangle (100 15) (140 40)))))
      (cell 320 115 "rule"
        (rule top ((verbatim abd))
          (let-sgl ((origin (78 45)) (line-style dashed))
            (rectangle (-70 -18) (70 18))
            (let-sgl ((font small-font) (text-justification center))
              (text (0 -4) "Change c to d")))))
      (cell 480 115 "nested origins"
        (let-sgl ((origin (10 10)))
          (rectangle (0 0) (40 40))
          (let-sgl ((origin (45 0)) (foreground-color "red"))
            (rectangle (0 0) (40 40))
            (let-sgl ((origin (45 0)) (foreground-color "blue"))
              (rectangle (0 0) (40 40))
              (let-sgl ((origin (-60 20)) (foreground-color "dark green"))
                (rectangle (0 0) (40 40))))))
        (let-sgl ((origin (1/2 0)))
          (let-sgl ((origin (1/2 0)))
            (line (10 80) (150 80))))
        (let-sgl ((origin (0.25 0)))
          (line (10 84) (150 84)))
        ())))
  (draw
    (let-sgl ()
      (cell 0 5 "move")
      (cell 160 5 "raise")
      (cell 320 5 "delete")
      (cell 480 5 "hidden, unhide")))
  ;; move: the grey square stays, the black one moves right and up,
  ;; the red one moves by pixels
  (draw (let-sgl ((origin (0 5)) (foreground-color "grey70"))
          (filled-rectangle (10 10) (40 40))))
  (draw (let-sgl ((origin (0 5))) (filled-rectangle (10 10) (40 40))) mover)
  (send move 50 30 mover)
  (draw (let-sgl ((origin (0 5)) (foreground-color "red"))
          (rectangle (100 10) (130 40)))
        pixel-mover)
  (send move-pixels 20 -40 pixel-mover)
  ;; raise: blue was drawn first, then raised above green
  (draw (let-sgl ((origin (160 5)) (foreground-color "blue"))
          (filled-rectangle (20 10) (80 70))
          (filled-arc (110 40) (40 40) 0 360))
        blue)
  (draw (let-sgl ((origin (160 5)) (foreground-color "light green"))
          (filled-rectangle (50 30) (120 85))))
  (send raise blue)
  ;; delete: of three discs only the outer two remain; retagged items go too
  (draw (let-sgl ((origin (320 5))) (filled-arc (30 40) (30 30) 0 360)))
  (draw (let-sgl ((origin (320 5)) (foreground-color "red"))
          (filled-arc (78 40) (30 30) 0 360))
        doomed)
  (draw (let-sgl ((origin (320 5)) (foreground-color "orange") (font letter-font))
          (text (60 70) "gone"))
        renamed)
  (send retag renamed doomed)
  (draw (let-sgl ((origin (320 5))) (filled-arc (126 40) (30 30) 0 360)))
  (send delete doomed)
  ;; hidden: two hidden rectangles, only the second unhidden
  (send draw-hidden-filled-rectangle "dark violet" 490 20 540 80 hidden-1)
  (send draw-hidden-filled-rectangle (color "dark violet") 570 20 620 80 hidden-2)
  (send unhide hidden-2)
  ;; beyond racket/tests/sgl-fixture.rkt: these leave the picture unchanged
  (erase (let-sgl ((foreground-color "red")) (rectangle (700 700) (710 710))))
  (send rescale no-such-tag 2 1/2)
  (draw (let-sgl ((origin (1/3 2/3))) (line (700 700) (701 701))) no-such-tag)
  ;; degenerate shapes (the viewport's guards compare SGL coordinates with =) and a
  ;; colour given as a symbol (lookup converts only strings), all off the canvas
  (draw (let-sgl ((origin (700 700)))
          (rectangle (5 5) (5 9)) (rectangle (5 5) (9 5.0))
          (filled-rectangle (5 5) (5.0 9)) (filled-rectangle (5 5) (9 5))
          (arc (5 5) (0 4) 0 90) (arc (5 5) (4 0) 0 360)
          (filled-arc (5 5) (0 0) 0 360) (filled-arc (5 5) (4 0) 0 90)
          (ring (5 5) 4 0) (ring (5 5) 0 4) (ring (5 5) 4 0 0 90) (ring (5 5) 0 4 0 90)
          (let-sgl ((foreground-color red) (background-color "red"))
            (line (0 0) (1 1))
            (polygon (0 0) (1 0) (0 1))))))
