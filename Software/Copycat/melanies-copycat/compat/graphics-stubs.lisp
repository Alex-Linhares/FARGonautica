;;; graphics-stubs.lisp -- what the engine needs from the Lucid graphics code.
;;;
;;; Mitchell's display code (src/graphics-files/, 5,500 lines) draws on a
;;; SunView pixwin through Lucid foreign functions (lucid-graphics.c, loaded
;;; by load-lucid-graphics-sun{3,4}.l) and is not loaded here.  The engine
;;; is written to run without it: init-ccat's :no-graphics switches every
;;; %...-graphics% flag off and all drawing is behind those flags.  What is
;;; left is:
;;;
;;;   1. functions the engine calls only when graphics are on, which the
;;;      compiler must still see defined (no-op stubs below);
;;;   2. two coderack methods and two helpers that the engine calls on every
;;;      codelet, graphics or not, because they keep the per-type counts that
;;;      the bar graph reads.  They are in coderack-graphics-methods.lisp,
;;;      copied as written from graphics-files/coderack-graphics.l so the
;;;      coderack's bookkeeping stays identical;
;;;   3. the window variables that load-lucid-graphics-sun4.l proclaims.
;;;
;;; What a GUI port would need is described in README.md.

(cl:in-package :copycat)

;;; 3. Proclaimed in graphics-files/load-lucid-graphics-sun4.l, read by
;;;    init-ccat's open-window call (never reached with :no-graphics).
(defvar *pixwin* nil)
(defvar %graphics-viewport-width% nil)
(defvar %graphics-viewport-height% nil)
(defvar %graphics-viewport-x% nil)
(defvar %graphics-viewport-y% nil)

;;; 1. Called behind %...-graphics% flags.  Each is a no-op here.
(defmacro define-graphics-stubs (&rest specs)
  `(progn
     ,@(cl:loop for (name . args) in specs
                collect `(defun ,name (&rest args)
                           (declare (ignore args))
                           nil))
     '(,@(mapcar #'car specs))))

(define-graphics-stubs
  ;; windows and menus (initialization.l, run.l)
  (open-window w h x y) (clear-window)
  (init-init-time-menu) (get-init-time-menu-command target-string-input)
  (init-begin-run-time-menu) (get-begin-run-time-menu-command)
  ;; whole displays
  (display-ccat) (redraw-graphics) (init-workspace-graphics)
  (init-answer-graphics)
  (display-descriptions) (init-coderack-graphics) (display-coderack)
  (update-coderack-display) (init-minimal-coderack-display)
  (display-minimal-coderack) (update-minimal-coderack-display)
  (init-slipnet-graphics) (display-slipnet) (update-slipnet-display)
  (display-temperature) (update-temperature-display)
  ;; the "groping" animations of scout codelets (bonds.l, groups.l,
  ;; correspondences.l)
  (draw-bond-grope from-obj to-obj) (draw-group-grope left-obj right-obj)
  (draw-correspondence-grope obj1 obj2))
