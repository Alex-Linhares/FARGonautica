;;; sgl-battery.scm -- differential checks of racket/gui/sgl.rkt's SGL
;;; interpreter (item 12) against the original's sgl-interpreter.ss.
;;;
;;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Read form by form, after tests/diff/helpers.scm, by two runners:
;;;   chez_scheme/oracle/diff-eval.ss with tests/diff/sgl-chez-setup.ss
;;;   racket/tests/sgl-diff-test.rkt (compat + racket/tests/sgl-recorder.rkt)
;;; Both define b:vp, a viewport that records every message the interpreter
;;; sends it (one per Tk canvas item the original creates), and
;;; (b:record thunk) -> (value calls).  Each test checks the messages, with
;;; their arguments (colours as (rgb r g b), fonts as 'obj), for SGL
;;; expressions covering every form, every let-sgl binding, nested origins,
;;; erasing and tags.

(define b:draw
  (lambda (pexp . tag)
    (b:record (lambda () (if (null? tag) (draw! b:vp pexp) (draw! b:vp pexp (car tag)))))))

;;;---------------------------------------------------------------------------
;;; The environment

(test env-init
  (map (lambda (s) (b:canon-arg (lookup init-env s)))
       '(foreground-color background-color font text-justification text-mode
         line-width line-style origin unknown)))

(test env-line-styles
  (map (lambda (style)
         (lookup (extend init-env 'line-style style) 'line-style))
       '(dotted dashed solid other)))

(test env-colors
  (list (b:canon-arg (lookup (extend init-env 'foreground-color "red") 'foreground-color))
        (b:canon-arg (lookup (extend init-env 'background-color "navy blue") 'background-color))
        (b:canon-arg (lookup (extend init-env 'erase-color "pink") 'erase-color))
        (b:canon-arg (lookup (extend init-env 'foreground-color b:bg) 'foreground-color))))

(test env-origin-not-bound
  (list ((extend init-env 'origin '(5 5)) 'origin)
        ((extend* init-env '((origin (1 2)) (line-width 3))) 'line-width)
        ((extend* init-env '((line-width 3) (line-width 4))) 'line-width)
        (empty-env 'anything)))

(test dash-pattern (graphics-dash-pattern))

(test polyline-coords
  (generate-polyline-coords '((0 0) (1 2) (3 4)) 10 20
    (lambda (x o) (* 10 (+ x o))) (lambda (y o) (- 100 (+ y o)))))

;;;---------------------------------------------------------------------------
;;; Every form, at the top level

(test empty (b:draw '()))
(test rectangle (b:draw '(rectangle (1 2) (30 40))))
(test rectangle-rational (b:draw '(rectangle (1/3 2.5) (30 -40))))
(test filled-rectangle (b:draw '(filled-rectangle (10 10) (20 30))))
(test arc (b:draw '(arc (50 50) (20 10) 30 120)))
(test arc-full (b:draw '(arc (50 50) (21 11) 0 360)))
(test arc-over (b:draw '(arc (50 50) (20 20) 90 400)))
(test filled-arc (b:draw '(filled-arc (50 50) (20 10) -45 90)))
(test filled-arc-full (b:draw '(filled-arc (5 5) (3 3) 0 360)))
(test line-one (b:draw '(line (0 0) (10 10))))
(test line-many (b:draw '(line (0 0) (10 10) (20 0) (30 30))))
(test polyline (b:draw '(polyline (0 0) (10 10) (20 0) (30 30) (40 0))))
(test polygon (b:draw '(polygon (0 0) (10 10) (20 0))))
(test filled-polygon (b:draw '(filled-polygon (0 0) (10 10) (20 0) (5 -5))))
(test polypoints (b:draw '(polypoints (0 0) (2 2) (4 4))))
(test dashed-polypoints (b:draw '(dashed-polypoints (0 0) (8 0) (16 0))))
(test ring (b:draw '(ring (50 50) 20 10)))
(test ring-arc (b:draw '(ring (50 50) 21 9 45 270)))
(test text-plain (b:draw '(text "abc")))
(test text-at (b:draw '(text (10 20) "abc")))
(test text-relative (b:draw '(text (text-relative (2 -1)) "abc")))
(test clear (b:draw '(clear)))
(test clear-color (b:draw '(clear "white")))
(test rule (b:draw '(rule top ((a b) (c d)) (rectangle (0 0) (5 5)))))
(test invalid (b:draw '(squiggle (0 0))))
(test invalid-nested (b:draw '(let-sgl () (rectangle (0 0) (1 1)) (squiggle))))

;;;---------------------------------------------------------------------------
;;; let-sgl: bindings, origins, nesting

(test let-sgl-empty
  (b:draw '(let-sgl () (rectangle (0 0) (1 1)) (line (0 0) (1 1)))))

(test let-sgl-all-bindings
  (b:draw '(let-sgl ((origin (10 20))
                     (line-width 3)
                     (foreground-color "red")
                     (background-color "blue")
                     (line-style dashed)
                     (font big-font)
                     (text-justification center)
                     (text-mode image))
             (rectangle (0 0) (5 5))
             (filled-rectangle (0 0) (5 5))
             (arc (0 0) (4 4) 0 90)
             (filled-arc (0 0) (4 4) 0 90)
             (line (0 0) (1 1))
             (polyline (0 0) (1 1) (2 0))
             (polygon (0 0) (1 1) (2 0))
             (filled-polygon (0 0) (1 1) (2 0))
             (ring (0 0) 4 2)
             (ring (0 0) 4 2 0 180)
             (polypoints (0 0))
             (dashed-polypoints (0 0))
             (text "x")
             (text (1 1) "y")
             (text (text-relative (1 1)) "z"))))

(test let-sgl-nested-origins
  (b:draw '(let-sgl ((origin (10 20)))
             (rectangle (0 0) (1 1))
             (let-sgl ((origin (1/2 -3)) (line-style dotted))
               (rectangle (0 0) (1 1))
               (let-sgl ((origin (0.25 0.5)))
                 (text (1 1) "deep")
                 (line (0 0) (1 1))))
             (rectangle (0 0) (1 1)))))

(test let-sgl-shadowing
  (b:draw '(let-sgl ((foreground-color "red") (line-width 2))
             (line (0 0) (1 1))
             (let-sgl ((foreground-color "green"))
               (line (0 0) (1 1)))
             (line (0 0) (1 1)))))

(test let-sgl-justifications
  (b:draw '(let-sgl ()
             (let-sgl ((text-justification left)) (text "l"))
             (let-sgl ((text-justification center)) (text "c"))
             (let-sgl ((text-justification right)) (text "r")))))

(test let-sgl-solid
  (b:draw '(let-sgl ((line-style solid)) (polyline (0 0) (1 1)))))

;;;---------------------------------------------------------------------------
;;; Erasing and tags

(test erase-string
  (b:draw '(erase "white"
             (let-sgl ((foreground-color "red") (background-color "blue"))
               (rectangle (0 0) (1 1))
               (filled-rectangle (0 0) (1 1))
               (arc (0 0) (1 1) 0 360)
               (filled-arc (0 0) (1 1) 0 10)
               (line (0 0) (1 1))
               (polyline (0 0) (1 1))
               (polygon (0 0) (1 1) (2 2))
               (filled-polygon (0 0) (1 1) (2 2))
               (ring (0 0) 4 2)
               (ring (0 0) 4 2 0 90)
               (polypoints (0 0))
               (dashed-polypoints (0 0))
               (text "gone")))))

(test erase-inside-let-sgl
  (b:draw '(let-sgl ((origin (5 5)) (line-width 2))
             (rectangle (0 0) (1 1))
             (erase "yellow" (let-sgl ((origin (1 1))) (rectangle (0 0) (1 1))))
             (rectangle (0 0) (1 1)))))

(test erase-nested
  (b:draw '(erase "white" (erase "red" (line (0 0) (1 1))))))

(test erase-bang
  (b:record (lambda () (erase! b:vp '(let-sgl () (rectangle (0 0) (2 2)) (text "t"))))))

(test erase-color-object
  (b:record (lambda () (draw-exp b:vp '(erase (rgb 1 2 3) (line (0 0) (1 1))) init-env 0 0 #f 'all))))

(test tag-given (b:draw '(let-sgl () (rectangle (0 0) (1 1)) (text "a")) 'flash))
(test tag-eraser (b:draw '(rectangle (0 0) (1 1)) 'eraser))

(test draw-exp-direct
  (b:record (lambda ()
              (draw-exp b:vp '(let-sgl ((origin (1 1))) (rectangle (0 0) (1 1)))
                        (extend init-env 'foreground-color "orange") 7 8 "x" 'mytag))))

(test draw-exps-direct
  (b:record (lambda ()
              (draw-exps b:vp '((line (0 0) (1 1)) (text "q") ())
                         init-env 2 3 "x" 'tags))))

(test fg-color-object
  (b:draw (list 'let-sgl (list (list 'foreground-color b:bg)) '(rectangle (0 0) (1 1)))))
