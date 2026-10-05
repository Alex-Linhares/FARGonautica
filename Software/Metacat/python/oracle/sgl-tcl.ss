;;; sgl-tcl.ss -- the Tcl command stream of the original's SGL interpreter (item 13).
;;;
;;; Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;;   scheme --script python/oracle/sgl-tcl.ss python/oracle/sgl-fixture.scm
;;;
;;; Run from the repository root by python/oracle/capture_sgl_tcl.py.  Loads
;;; the unedited original through chez_scheme/oracle/prelude.ss, as diff-eval.ss
;;; does, then gives it what SWL gave it and the prelude leaves out:
;;;   - swl:tcl-eval records every command it gets, with its arguments
;;;     (windows by name, colours as (rgb r g b), SWL fonts as
;;;     (font FACE SIZE STYLE)), and answers the hidden canvas's text bbox
;;;     from a fixed metric (below) instead of Tk's;
;;;   - swl-color makes (rgb r g b) lists, as tests/diff/sgl-chez-setup.ss does;
;;;   - define-class makes a <viewport> a closure over its ivars that answers
;;;     its public methods, with a <canvas> base for get-background-color and
;;;     set-background-color! (recorded as (swl WINDOW METHOD ARG ...));
;;;   - send calls the receiver.
;;; Then it reloads the unedited sgl-interpreter.ss against these, and draws
;;; sgl-fixture.scm's operations on each of its viewports, printing
;;;   ;; viewport NAME
;;; followed by one written datum per command, (tcl WINDOW ARG ...) or
;;; (swl WINDOW METHOD ARG ...), in the order the original sends them.
;;;
;;; The fixed metric (python/tests/test_sgl.py has the same): a font of SIZE
;;; (points if positive, pixels if negative) is px = |SIZE| pixels, or
;;; round(4/3 SIZE) for points; each character is (quotient 3px 5) + 1 pixels
;;; wide, one more if bold; the text is px + (quotient px 4) + 2 high; the bbox
;;; is (1 2 1+width 2+height).

(define $oracle-directory (string-append (current-directory) "/chez_scheme/oracle"))
(load (string-append $oracle-directory "/prelude.ss"))
(load-metacat)

(define $windows '())
(define $hidden-text #f)

(define $canon
  (lambda (x)
    (cond
      ((and (swl-stub? x) (eq? (swl-stub-class x) '<font>))
       (cons 'font (map $canon (swl-stub-args x))))
      ((procedure? x)
       (let ((e (assq x $windows))) (if e (cdr e) 'procedure)))
      ((pair? x) (cons ($canon (car x)) ($canon (cdr x))))
      (else x))))

(define $emit
  (lambda (datum)
    (write ($canon datum) $stdout)
    (put-string $stdout "\n")))

(define $metric
  (lambda (string font)
    (let* ((args (swl-stub-args font))
           (size (cadr args))
           (style (caddr args))
           (px (if (< size 0) (- size) (round (* size 4/3))))
           (cw (+ (quotient (* 3 px) 5) 1 (if (memq 'bold style) 1 0)))
           (w (* cw (string-length string)))
           (h (+ px (quotient px 4) 2)))
      (list 1 2 (+ 1 w) (+ 2 h)))))

(define $text-option
  (lambda (args key)
    (cond ((null? args) #f)
          ((eq? (car args) key) (cadr args))
          (else ($text-option (cdr args) key)))))

(set! swl:tcl-eval
  (lambda args
    ($emit (cons 'tcl args))
    (if (eq? (car args) 'hidden)
        (case (cadr args)
          ((create)
           (set! $hidden-text (cons ($text-option args '-text) ($text-option args '-font)))
           "1")
          ((bbox) ($metric (car $hidden-text) (cdr $hidden-text)))
          (else ""))
        "")))
(set! swl:tcl->scheme (lambda (x) x))
(set! *hidden-canvas* 'hidden)

(define swl-color
  (lambda (name)
    (let ((e (assoc name *color-names*)))
      (list 'rgb (cadr e) (caddr e) (cadddr e)))))
(set! =black= (swl-color "black"))
(set! =white= (swl-color "white"))

(define $make-canvas-base
  (lambda ()
    (let ((bg (swl-color "white")))
      (lambda (self msg args)
        (case msg
          ((get-background-color) bg)
          ((set-background-color!)
           ($emit (cons* 'swl self msg args))
           (set! bg (car args)))
          (else (error 'canvas "no method ~s" msg)))))))

(define-syntax event-case (syntax-rules () ((_ . any) (void))))

(define-syntax define-class
  (lambda (x)
    (syntax-case x ()
      ((_ (name formal ...) (base base-arg ...) (ivars (iv init) ...) other ...
          (public (m (a ...) b ...) ...))
       (with-syntax ((self (datum->syntax #'name 'self)))
         #'(define name
             (lambda (formal ...)
               (let* ((iv init) ... ($base ($make-canvas-base)))
                 (letrec ((self (lambda (msg . args)
                                  (case msg
                                    ((m) (apply (lambda (a ...) b ...) args)) ...
                                    (else ($base self msg args))))))
                   self)))))))))

(define-syntax send
  (syntax-rules ()
    ((_ obj msg arg ...) (obj 'msg arg ...))))

(load "chez_scheme/original/sgl-interpreter.ss")

;;; the fixture

(define $fixture
  (call-with-input-file (cadr (command-line))
    (lambda (in)
      (let loop ((acc '()))
        (let ((form (read in)))
          (if (eof-object? form) (reverse acc) (loop (cons form acc))))))))

(define $section
  (lambda (key) (cdr (assq key $fixture))))

(define $fonts
  (map (lambda (spec)
         (cons (car spec)
               (make-mfont (top-level-value (cadr spec)) (caddr spec) (cadddr spec))))
       ($section 'fonts)))

(define $expand
  (lambda (pexp)
    (cond
      ((not (pair? pexp)) pexp)
      ((eq? (car pexp) 'cell)
       (let ((x (cadr pexp)) (y (caddr pexp)) (label (cadddr pexp)))
         ($expand
           `(let-sgl ((origin (,x ,y)))
              (let-sgl ((font label-font) (foreground-color "grey40")) (text (4 92) ,label))
              (let-sgl ((foreground-color "grey80")) (rectangle (0 0) (155 105)))
              ,@(cddddr pexp)))))
      ((and (eq? (car pexp) 'font) (pair? (cdr pexp)) (null? (cddr pexp))
            (assq (cadr pexp) $fonts))
       => (lambda (e) (list 'font (cdr e))))
      (else (map $expand pexp)))))

(define $make-viewport
  (lambda (w h xmin ymin xmax ymax)
    (let* ((wpp (/ (- xmax xmin) w))
           (hpp (/ (- ymax ymin) h))
           (pixel->x (lambda (i) (+ xmin (* wpp i))))
           (pixel->y (lambda (j) (- ymax (* hpp j))))
           (x->pixel (lambda (x offset) (floor (/ (- (+ x offset) xmin) wpp))))
           (y->pixel (lambda (y offset) (floor (/ (- ymax (+ y offset)) hpp)))))
      (<viewport> 'parent pixel->x pixel->y x->pixel y->pixel))))

(define $arg
  (lambda (a)
    (if (and (pair? a) (eq? (car a) 'color)) (swl-color (cadr a)) a)))

(define $run-op
  (lambda (vp op)
    (case (car op)
      ((draw) (apply draw! vp ($expand (cadr op)) (cddr op)))
      ((erase) (erase! vp ($expand (cadr op))))
      ((send) (apply vp (cadr op) (map $arg (cddr op))))
      (else (error 'sgl-tcl "bad op ~s" op)))))

(for-each
  (lambda (spec)
    (let ((vp (apply $make-viewport (cddr spec))))
      (set! $windows (list (cons vp (cadr spec))))
      (put-string $stdout (format ";; viewport ~a\n" (cadr spec)))
      (for-each (lambda (op) ($run-op vp op)) ($section 'ops))))
  (filter (lambda (f) (eq? (car f) 'viewport)) $fixture))
(flush-output-port $stdout)
