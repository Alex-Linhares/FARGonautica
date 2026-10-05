#lang racket/base
;; The Racket side of tests/diff/sgl-battery.scm (item 12): the definitions
;; tests/diff/sgl-chez-setup.ss gives the Chez side.  b:vp is a viewport that
;; records every message racket/gui/sgl.rkt's interpreter sends it, with
;; colours (color%) as (rgb r g b) lists and other objects (fonts) as 'obj;
;; b:record runs a thunk and returns (value calls).  The interpreter's own
;; definitions are re-exported for the battery.
;;
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
(require racket/class
         (only-in racket/draw color%)
         "../gui/colors.rkt"
         "../gui/sgl.rkt")

(provide b:vp b:bg b:record b:canon-arg
         draw! erase! draw-exp draw-exps lookup extend extend* empty-env init-env
         graphics-dash-pattern generate-polyline-coords)

(define (b:canon-arg x)
  (cond
    [(or (number? x) (string? x) (symbol? x) (boolean? x) (null? x)) x]
    [(pair? x) (map b:canon-arg x)]
    [(is-a? x color%) (cons 'rgb (swl-color-rgb x))]
    [else 'obj]))

(define b:bg (swl-color "light grey"))

(define calls '())

(define (b:record thunk)
  (set! calls '())
  (let ([r (thunk)])
    (list r (reverse calls))))

(define-syntax-rule (recording-methods name ...)
  (begin
    (define/public (name . args)
      (set! calls (cons (cons 'name (map b:canon-arg args)) calls))
      'ok)
    ...))

(define recorder%
  (class object%
    (super-new)
    (define/public (get-background-color) b:bg)
    (recording-methods draw-open-rectangle draw-filled-rectangle
                       draw-hidden-filled-rectangle draw-line-segments draw-polyline
                       draw-open-oval draw-filled-oval draw-open-arc draw-filled-arc
                       draw-ring draw-arc-ring draw-open-polygon draw-filled-polygon
                       draw-polypoints draw-text delete set-background-color!)))

(define b:vp (new recorder%))
