#lang racket/base
;; Item 12: a fixture of every SGL form, rendered offscreen by
;; racket/gui/sgl.rkt.  Used by racket/tests/sgl-test.rkt (pixel snapshot in
;; racket/tests/snapshots/) and by hand:
;;   racket racket/tests/sgl-fixture.rkt OUT.png
;; writes the rendering for inspection.
;;
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
(require racket/class
         racket/draw
         "../gui/sgl.rkt"
         "../gui/fonts.rkt"
         "../gui/colors.rkt")

(provide make-test-viewport fixture-pexp tags-pexp render-viewport
         fixture-width fixture-height draw-fixture)

;; the coordinate transforms of general-graphics.ss's make-graphics-window,
;; for a canvas of w x h pixels showing x in [xmin, xmax], y in [ymin, ymax]
(define (make-test-viewport w h [xmin 0] [ymin 0] [xmax w] [ymax h] #:bg [bg =white=])
  (let* ([width-per-pixel (/ (- xmax xmin) w)]
         [height-per-pixel (/ (- ymax ymin) h)]
         [pixel->x (lambda (i) (+ xmin (* width-per-pixel i)))]
         [pixel->y (lambda (j) (- ymax (* height-per-pixel j)))]
         [x->pixel (lambda (x offset)
                     (inexact->exact (floor (/ (- (+ x offset) xmin) width-per-pixel))))]
         [y->pixel (lambda (y offset)
                     (inexact->exact (floor (/ (- ymax (+ y offset)) height-per-pixel))))])
    (new viewport% [pixel->x pixel->x] [pixel->y pixel->y]
         [x->pixel x->pixel] [y->pixel y->pixel]
         [width w] [height h] [background-color bg])))

(define fixture-width 640)
(define fixture-height 480)

(define title-font (make-mfont sans-serif 18 '(bold italic)))
(define label-font (make-mfont sans-serif -11 '()))
(define letter-font (make-mfont serif -22 '(bold italic)))
(define small-font (make-mfont fancy -12 '(italic)))

;; a cell: label at the top, contents with the cell's lower left as origin
(define (cell x y label . pexps)
  `(let-sgl ((origin (,x ,y)))
     (let-sgl ((font ,label-font) (foreground-color "grey40"))
       (text (4 92) ,label))
     (let-sgl ((foreground-color "grey80")) (rectangle (0 0) (155 105)))
     ,@pexps))

;; y up: the canvas is 640 x 480, cells are 160 x 110 from the bottom; the
;; bottom row is drawn by draw-fixture, which also works the tags
(define fixture-pexp
  `(let-sgl ()
     ;; drawn, then cleared: none of it may show
     (filled-rectangle (0 0) (640 480))
     (let-sgl ((font ,label-font)) (text (100 100) "cleared"))
     (clear "ivory")
     (let-sgl ((font ,title-font) (text-justification center))
       (text (320 452) "SGL fixture"))
     ,(cell 0 335 "rectangle"
            '(rectangle (10 10) (60 60))
            '(let-sgl ((line-width 3) (foreground-color "red"))
               (rectangle (70 10) (110 50)))
            '(let-sgl ((line-style dashed) (foreground-color "blue"))
               (rectangle (10 65) (145 85)))
            '(let-sgl ((line-style dotted))
               (rectangle (118 10) (148 55))))
     ,(cell 160 335 "filled-rectangle"
            '(filled-rectangle (10 10) (50 50))
            '(let-sgl ((foreground-color "forest green"))
               (filled-rectangle (60 20) (100 80)))
            '(let-sgl ((foreground-color "gold"))
               (filled-rectangle (110 10) (150 40))))
     ,(cell 320 335 "arc, filled-arc"
            '(arc (40 40) (60 60) 0 180)
            '(let-sgl ((foreground-color "red") (line-width 2))
               (arc (40 40) (40 40) 45 400))
            '(let-sgl ((line-style dashed) (foreground-color "blue"))
               (arc (40 15) (60 20) 180 180))
            '(let-sgl ((foreground-color "dark orange"))
               (filled-arc (115 45) (60 60) 30 300))
            '(let-sgl ((foreground-color "purple"))
               (filled-arc (115 45) (16 16) 0 360)))
     ,(cell 480 335 "line, polyline"
            '(line (10 10) (60 60) (10 60) (60 10))
            '(let-sgl ((line-width 4) (foreground-color "dark green"))
               (polyline (75 10) (95 60) (115 10) (135 60)))
            '(let-sgl ((line-style dashed) (foreground-color "blue"))
               (polyline (10 75) (80 85) (150 75)))
            '(let-sgl ((line-style dotted)) (line (70 70) (150 70))))
     ,(cell 0 225 "polygon, filled-polygon"
            '(let-sgl ((background-color "light blue"))
               (polygon (10 10) (70 10) (40 70)))
            '(let-sgl ((foreground-color "brown"))
               (filled-polygon (80 10) (150 10) (150 60) (115 80) (80 60)))
            '(let-sgl ((line-style dashed) (line-width 2) (background-color "ivory"))
               (polygon (20 20) (60 20) (40 50))))
     ,(cell 160 225 "polypoints, dashed"
            '(polypoints (10 20) (14 20) (18 20) (22 20) (26 24) (30 28) (34 32))
            '(let-sgl ((foreground-color "red"))
               (dashed-polypoints (10 50) (18 50) (26 50) (34 50) (42 50) (50 50)))
            '(let-sgl ((foreground-color "blue"))
               (polypoints (70 10) (70 12) (70 14) (70 16) (70 18) (70 20))))
     ,(cell 320 225 "ring"
            '(ring (40 45) 60 40)
            '(let-sgl ((foreground-color "steel blue") (background-color "ivory"))
               (ring (115 45) 60 30 90 225))
            '(let-sgl ((foreground-color "red") (background-color "ivory"))
               (ring (115 45) 20 10)))
     ,(cell 480 225 "text justification"
            '(let-sgl ((foreground-color "red"))
               (line (80 0) (80 85))
               (line (0 20) (155 20) (0 45) (155 45) (0 70) (155 70)))
            `(let-sgl ((origin (80 70)) (font ,letter-font))
               (let-sgl ((text-justification left)) (text "left")))
            `(let-sgl ((origin (80 45)) (font ,letter-font))
               (let-sgl ((text-justification center)) (text "center")))
            `(let-sgl ((origin (80 20)) (font ,letter-font))
               (let-sgl ((text-justification right)) (text "right"))))
     ,(cell 0 115 "text: at, relative, image"
            `(let-sgl ((font ,letter-font))
               (text (10 60) "abc")
               (text (10 30) "x")
               (let-sgl ((origin (10 30)))
                 (text (text-relative (1 0)) "y")
                 (text (text-relative (2 1)) "z")
                 (text (text-relative (3 -1)) "w")))
            '(let-sgl ((foreground-color "light grey"))
               (filled-rectangle (90 10) (150 80)))
            `(let-sgl ((font ,letter-font) (text-mode image)
                       (background-color "light yellow") (foreground-color "dark blue"))
               (text (95 50) "img"))
            `(let-sgl ((font ,small-font) (foreground-color "dark red"))
               (text (95 15) "fancy")))
     ,(cell 160 115 "erase"
            '(let-sgl ((foreground-color "orange"))
               (filled-rectangle (10 10) (145 80)))
            '(erase "ivory" (filled-rectangle (30 30) (60 60)))
            '(erase "white" (let-sgl ((line-width 5)) (line (70 20) (140 70))))
            '(let-sgl ((foreground-color "black"))
               (rectangle (10 10) (145 80))
               (erase "orange" (rectangle (100 15) (140 40)))))
     ,(cell 320 115 "rule"
            `(rule top ((verbatim abd))
               (let-sgl ((origin (78 45)) (line-style dashed))
                 (rectangle (-70 -18) (70 18))
                 (let-sgl ((font ,small-font) (text-justification center))
                   (text (0 -4) "Change c to d")))))
     ,(cell 480 115 "nested origins"
            '(let-sgl ((origin (10 10)))
               (rectangle (0 0) (40 40))
               (let-sgl ((origin (45 0)) (foreground-color "red"))
                 (rectangle (0 0) (40 40))
                 (let-sgl ((origin (45 0)) (foreground-color "blue"))
                   (rectangle (0 0) (40 40))
                   (let-sgl ((origin (-60 20)) (foreground-color "dark green"))
                     (rectangle (0 0) (40 40))))))
            '(let-sgl ((origin (1/2 0)))
               (let-sgl ((origin (1/2 0)))
                 (line (10 80) (150 80))))
            '(let-sgl ((origin (0.25 0)))
               (line (10 84) (150 84)))
            '())))

(define tags-pexp
  `(let-sgl ()
     ,(cell 0 5 "move")
     ,(cell 160 5 "raise")
     ,(cell 320 5 "delete")
     ,(cell 480 5 "hidden, unhide")))

;; the viewport operations the graphics windows use on tags (general-graphics.ss:
;; move, move-pixels, raise, delete, unhide, retag)
(define (draw-fixture vp)
  (draw! vp fixture-pexp)
  (draw! vp tags-pexp)
  ;; move: the grey square stays, the black one moves right and up,
  ;; the red one moves by pixels
  (draw! vp '(let-sgl ((origin (0 5)) (foreground-color "grey70"))
               (filled-rectangle (10 10) (40 40))))
  (draw! vp '(let-sgl ((origin (0 5))) (filled-rectangle (10 10) (40 40))) 'mover)
  (send vp move 50 30 'mover)
  (draw! vp '(let-sgl ((origin (0 5)) (foreground-color "red"))
               (rectangle (100 10) (130 40))) 'pixel-mover)
  (send vp move-pixels 20 -40 'pixel-mover)
  ;; raise: blue was drawn first, then raised above green
  (draw! vp '(let-sgl ((origin (160 5)) (foreground-color "blue"))
               (filled-rectangle (20 10) (80 70))
               (filled-arc (110 40) (40 40) 0 360)) 'blue)
  (draw! vp '(let-sgl ((origin (160 5)) (foreground-color "light green"))
               (filled-rectangle (50 30) (120 85))))
  (send vp raise 'blue)
  ;; delete: of three discs only the outer two remain; retagged items go too
  (draw! vp '(let-sgl ((origin (320 5))) (filled-arc (30 40) (30 30) 0 360)))
  (draw! vp '(let-sgl ((origin (320 5)) (foreground-color "red"))
               (filled-arc (78 40) (30 30) 0 360)) 'doomed)
  (draw! vp `(let-sgl ((origin (320 5)) (foreground-color "orange") (font ,letter-font))
               (text (60 70) "gone")) 'renamed)
  (send vp retag 'renamed 'doomed)
  (draw! vp '(let-sgl ((origin (320 5))) (filled-arc (126 40) (30 30) 0 360)))
  (send vp delete 'doomed)
  ;; hidden: two hidden rectangles, only the second unhidden
  (send vp draw-hidden-filled-rectangle "dark violet" 490 20 540 80 'hidden-1)
  (send vp draw-hidden-filled-rectangle (swl-color "dark violet") 570 20 620 80 'hidden-2)
  (send vp unhide 'hidden-2))

(define (render-viewport vp)
  (define bm (make-bitmap (send vp get-width) (send vp get-height) #f))
  (define dc (new bitmap-dc% [bitmap bm]))
  (send vp render dc)
  bm)

(module+ main
  (define out (vector-ref (current-command-line-arguments) 0))
  (define vp (make-test-viewport fixture-width fixture-height))
  (draw-fixture vp)
  (send (render-viewport vp) save-file out 'png))
