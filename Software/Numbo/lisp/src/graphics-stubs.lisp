;;; graphics-stubs.lisp -- no-op stand-ins for the 1987 window primitives.
;;;
;;; Graphics are out of scope (TASK.md).  pnet-graphics.lisp draws the Pnet
;;; with primitives from a window library that is not part of the printout
;;; (it was presumably a local library on the 1987 Unix machine).  start.lisp
;;; and init.lisp (refresh-everything) also call DUMP-WINDOW to save a
;;; snapshot of the window named "gr<iteration>".  None of this code runs
;;; unless %graphics% is true.  init-chiffre sets %graphics% from the
;;; WINDOW_GFX environment variable, so it stays nil when WINDOW_GFX is unset
;;; or empty.
;;;
;;; The stubs below cover every function that is called in the source but
;;; defined in no source file and is a graphics primitive.  They were found
;;; with a census (load everything, then list the undefined functions; see
;;; tests/graphics-tests.lisp).  Argument lists come from the call sites:
;;;   open-window ()                     pnet-graphics init-pnet-graphics
;;;   clear-window ()                    pnet-graphics init-pnet-graphics
;;;   window-height ()  window-width ()  pnet-graphics init-pnet-graphics
;;;   draw-rect (x1 y1 x2 y2)            pnet-graphics drawbox (filled)
;;;   draw-unfilled-rect (x1 y1 x2 y2)   pnet-graphics outline-regions
;;;   erase-rect (x1 y1 x2 y2)           pnet-graphics erasebox, shrink-box,
;;;                                        :draw-box
;;;   draw-text (x y string)             pnet-graphics outline-regions
;;;   draw-number (x y n)                pnet-graphics :draw-box
;;;   dump-window (name)                 start.lisp config, init.lisp
;;;                                        refresh-everything
;;;
;;; The drawing stubs do nothing and return nil.  WINDOW-HEIGHT and
;;; WINDOW-WIDTH must return numbers, because setup-regions divides by them.
;;; They return a nominal 800 x 512 window: setup-regions says it assumes
;;; "height > width".  With these stubs, the whole graphics path runs if
;;; %graphics% is turned on, and draws nothing.
;;;
;;; Each call is counted in *GRAPHICS-STUB-CALLS* (primitive name -> count),
;;; so the tests can check that the graphics code reached the stubs.

(in-package :numbo)

(defvar *graphics-stub-calls* (make-hash-table :test #'eq)
  "Primitive name -> number of calls to its stub.")

(defparameter *stub-window-height* 800)
(defparameter *stub-window-width* 512)

(defmacro define-graphics-stub (name lambda-list &body body)
  ;; NAME counts its calls and then runs BODY (nil if BODY is empty).
  `(cl:defun ,name ,lambda-list
     (declare (ignorable ,@(remove-if (lambda (s) (member s lambda-list-keywords))
                                      lambda-list)))
     (incf (gethash ',name *graphics-stub-calls* 0))
     ,@(or body '(nil))))

(define-graphics-stub open-window ())
(define-graphics-stub clear-window ())
(define-graphics-stub window-height () *stub-window-height*)
(define-graphics-stub window-width () *stub-window-width*)
(define-graphics-stub draw-rect (x1 y1 x2 y2))
(define-graphics-stub draw-unfilled-rect (x1 y1 x2 y2))
(define-graphics-stub erase-rect (x1 y1 x2 y2))
(define-graphics-stub draw-text (x y string))
(define-graphics-stub draw-number (x y n))
(define-graphics-stub dump-window (name))

(defparameter *graphics-stubs*
  '(open-window clear-window window-height window-width
    draw-rect draw-unfilled-rect erase-rect draw-text draw-number
    dump-window)
  "Every graphics primitive stubbed in this file.")
