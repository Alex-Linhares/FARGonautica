;;; sgl-chez-setup.ss -- Chez side of the SGL battery (item 12).
;;;
;;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Evaluated by chez_scheme/oracle/diff-eval.ss after tests/diff/helpers.scm
;;; and before tests/diff/sgl-battery.scm.  The prelude's `send' throws its
;;; arguments away, so this file redefines `send' to call the receiver with
;;; the message and its arguments, defines a recording viewport b:vp, and
;;; reloads the original's sgl-interpreter.ss (unmodified) so that draw-exp
;;; and friends are compiled against the recording `send'.  Colours are
;;; (rgb r g b) lists here; racket/tests/sgl-recorder.rkt is the Racket side
;;; and turns its color% objects into the same lists.

(define swl-color
  (lambda (name)
    (let ((e (assoc name *color-names*)))
      (list 'rgb (cadr e) (caddr e) (cadddr e)))))
(set! =black= (swl-color "black"))

(define b:bg (swl-color "light grey"))

;; viewport arguments as data: colours stay (rgb r g b), fonts and other
;; objects become 'obj
(define b:canon-arg
  (lambda (x)
    (cond
      ((or (number? x) (string? x) (symbol? x) (boolean? x) (null? x)) x)
      ((pair? x) (map b:canon-arg x))
      (else 'obj))))

(define b:calls '())

(define b:vp
  (lambda (msg args)
    (case msg
      ((get-background-color) b:bg)
      (else (set! b:calls (cons (cons msg (map b:canon-arg args)) b:calls))
            'ok))))

(define b:record
  (lambda (thunk)
    (set! b:calls '())
    (let ((r (thunk)))
      (list r (reverse b:calls)))))

(define-syntax send
  (syntax-rules ()
    ((_ obj msg arg ...) (obj 'msg (list arg ...)))))

(load "chez_scheme/original/sgl-interpreter.ss")
