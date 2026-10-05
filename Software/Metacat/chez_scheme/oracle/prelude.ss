;;; prelude.ss -- make the unmodified Metacat 1.2 source in chez_scheme/original/
;;; load and run headless under Chez Scheme 10.
;;;
;;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;; Nothing here changes the model: it supplies what the 1999-2017 environment
;;; (Chez Scheme 6-8 + SWL 0.9 + Tcl/Tk) provided and Chez 10 lacks.
;;;
;;;  1. extend-syntax, written with syntax-case (fenders and `with' supported);
;;;  2. empty modules for the swl:* imports in metacat.ss;
;;;  3. SWL stand-ins: `make'/`create'/`send' on inert stub objects,
;;;     swl:tcl-eval, swl:font-families, screen size, threads, ...;
;;;  4. the configuration variables metacat.ss expects;
;;;  5. a muteable stdout, so the load-time chatter can be hidden while
;;;     syntactic-sugar.ss's printf (which captures the port at load time)
;;;     still reaches the real stdout afterwards;
;;;  6. null windows for a headless run (install-headless-windows!);
;;;  7. an error handler that prints a backtrace.
;;;
;;; Randomness: none.  The oracle uses Chez 10's own `random'/`random-seed',
;;; which the port reproduces exactly (see docs/trace-format.md).

;;;---------------------------------------------------------------------------
;;; 1. extend-syntax
;;;
;;; (extend-syntax (name key ...) (pattern [fender] template) ...)
;;; Fenders and `with' expressions refer to pattern variables as quoted data
;;; ('x, '(x ...)); they are evaluated at expansion time, after substitution,
;;; in the interaction environment.  A template of the form
;;; (with ((id exp) ...) template) binds each id to the datum value of exp,
;;; given the lexical context of the macro use (these produce top-level names
;;; such as plato-a or a-b-link).

(define $es-eval
  (lambda (stx)
    (eval (syntax->datum stx) (interaction-environment))))

(define $es-subst
  (lambda (stx alist)
    (syntax-case stx ()
      (id (identifier? #'id)
       (let ((a (assp (lambda (k) (bound-identifier=? k #'id)) alist)))
         (if a (cdr a) #'id)))
      ((a . d) (cons ($es-subst #'a alist) ($es-subst #'d alist)))
      (#(e ...) (list->vector (map (lambda (e) ($es-subst e alist)) #'(e ...))))
      (_ stx))))

(define $es-template
  (lambda (ctx tmpl)
    (syntax-case tmpl ()
      ((w ((id exp) ...) body)
       (and (identifier? #'w) (eq? (syntax->datum #'w) 'with))
       ($es-subst #'body
         (map (lambda (id exp) (cons id (datum->syntax ctx ($es-eval exp))))
              #'(id ...) #'(exp ...))))
      (_ tmpl))))

(define-syntax extend-syntax
  (lambda (x)
    (syntax-case x ()
      ((_ (name key ...) clause ...)
       (andmap identifier? #'(name key ...))
       (with-syntax ((((pat fender tmpl) ...)
                      (map (lambda (c)
                             (syntax-case c ()
                               ((p t) #'(p #t t))
                               ((p f t) #'(p f t))))
                           #'(clause ...))))
         #'(define-syntax name
             (lambda (y)
               (syntax-case y (key ...)
                 (pat ($es-eval #'fender)
                      ($es-template (car (syntax->list y)) #'tmpl))
                 ...))))))))

;;;---------------------------------------------------------------------------
;;; 2. The SWL modules imported by metacat.ss

(module swl:oop ())
(module swl:macros ())
(module swl:generics ())
(module swl:option ())
(module swl:threads ())

;;;---------------------------------------------------------------------------
;;; 3. SWL stand-ins.  Widgets are inert records; every message to them is
;;; ignored.  The headless oracle never creates the Metacat windows (it does
;;; not call `setup'), so these only serve the load-time definitions in
;;; fonts.ss, constants.ss and the *-graphics.ss files (fonts and colours).

(define-record-type swl-stub (fields class args))

(define-syntax make
  (syntax-rules ()
    ((_ class arg ...) (make-swl-stub 'class (list arg ...)))))

;; (create <class> arg ... [with (keyword: value ...) ...]); the
;; positional args are evaluated, the keyword options ignored.
(define-syntax create
  (lambda (x)
    (define with?
      (lambda (a) (and (identifier? a) (eq? (syntax->datum a) 'with))))
    (syntax-case x ()
      ((_ class arg ...)
       (let loop ((args #'(arg ...)) (acc '()))
         (if (or (null? args) (with? (car args)))
             (with-syntax (((a ...) (reverse acc)))
               #'(make-swl-stub 'class (list a ...)))
             (loop (cdr args) (cons (car args) acc))))))))

;; SWL class definitions (sgl-interpreter.ss: <viewport>, ...) are skipped.
(define-syntax define-class
  (syntax-rules ()
    ((_ (name formal ...) (base base-arg ...) clause ...) (define name 'swl-class))))

(define-syntax send
  (syntax-rules ()
    ((_ obj msg arg ...) ($swl-send obj 'msg))))

(define $swl-send
  (lambda (obj msg)
    (case msg
      ((get-actual-values)
       (if (swl-stub? obj) (apply values (swl-stub-args obj)) (values 'times 12 '())))
      ((get-width get-height) 0)
      (else (void)))))

(define swl:version "0.9x")
(define swl:font-families (lambda args '(times helvetica courier)))
(define swl:screen-width (lambda () 1280))
(define swl:screen-height (lambda () 1024))
(define swl:tcl-eval (lambda args ""))
(define swl:tcl->scheme (lambda (x) x))
(define swl:sync-display (lambda () (void)))
(define swl:file-dialog (lambda args #f))

(define thread-self (lambda () 'repl-thread))
(define thread-sleep (lambda (ms) (void)))
(define thread-break (lambda args (void)))
(define thread-kill
  (lambda args
    (printf "prelude: thread-kill called (configuration error)~%")
    (exit 2)))
(define thread-fork (lambda args (void)))
(define thread-make-msg-queue (lambda args 'msg-queue))
(define thread-msg-waiting? (lambda args #f))
(define thread-send-msg (lambda args (void)))
(define thread-receive-msg (lambda args (void)))

;;;---------------------------------------------------------------------------
;;; 4. Configuration variables (commented out in metacat.ss)

;; The directory of this file; run.ss defines $oracle-directory before
;; loading the prelude, otherwise it is chez_scheme/oracle under the
;; current directory (the repository root).
(define $oracle-directory
  (if (top-level-bound? '$oracle-directory)
      (top-level-value '$oracle-directory)
      (string-append (current-directory) "/chez_scheme/oracle")))

(define *platform* 'linux)
(define *metacat-directory*
  (string-append (path-parent $oracle-directory) "/original/"))
(define *file-dialog-directory* "/tmp/")

;;;---------------------------------------------------------------------------
;;; 5. Muteable stdout

(define $stdout (current-output-port))
(define $stdout-muted? #f)
(define $muteable-stdout
  (make-custom-textual-output-port "muteable-stdout"
    (lambda (str start count)
      (unless $stdout-muted? (put-string $stdout str start count))
      count)
    #f #f
    (lambda () (flush-output-port $stdout))))
(set-textual-port-output-size! $muteable-stdout 0)  ; unbuffered
(current-output-port $muteable-stdout)

;;; Load Metacat: metacat.ss unmodified, with load-time chatter muted.
(define load-metacat
  (lambda ()
    (let ((cwd (current-directory)))
      (set! $stdout-muted? #t)
      (load (string-append *metacat-directory* "metacat.ss"))
      (flush-output-port $muteable-stdout)
      (set! $stdout-muted? #f)
      (current-directory cwd))))

;;;---------------------------------------------------------------------------
;;; 6. Headless windows
;;;
;;; The oracle never calls (setup), so the window globals of setup.ss stay #f.
;;; The model still sends a few messages to them outside the graphics
;;; switches (%workspace-graphics% etc., which the oracle turns off).  Each
;;; window is replaced by a null object that accepts exactly the messages a
;;; headless run sends and does nothing; any other message is an error, so a
;;; display call that could feed a value back into the model is noticed.
;;; The Commentary window is the original one (make-comment-window from
;;; commentary-graphics.ss) drawing on a recording text window, so the
;;; eliza/non-eliza paragraph choice is the original's.

(define make-null-window
  (lambda (name messages)
    (lambda (self . msg)
      (if (memq (car msg) messages)
          'done
          (errorf 'headless-window "~s received unexpected message ~s"
                  name msg)))))

;; Paragraphs drawn in the Commentary window, oldest first, and a hook
;; called with each new paragraph.
(define $commentary '())
(define $commentary-hook (lambda (paragraph) (void)))

(define make-recording-text-window
  (lambda ()
    (lambda (self . msg)
      (case (car msg)
        ((draw-paragraph)
         (set! $commentary (append $commentary (list (cadr msg))))
         ($commentary-hook (cadr msg))
         'done)
        ((clear) (set! $commentary '()) 'done)
        ((get-y-max get-visible-y-min) 0)
        ((set-icon-label set-icon-image set-window-title new-font
          centering-off draw set-paragraphs redraw)
         'done)
        (else (errorf 'headless-window
                      "text window received unexpected message ~s" msg))))))

;; Verbose mode (gui.ss's Options > Verbose mode checkbox): when #t, the
;; model's vprintf/vprint output is printed (run.ss --verbose).
(define $verbose? #f)

(define install-headless-windows!
  (lambda ()
    (set! %workspace-graphics% #f)
    (set! %slipnet-graphics% #f)
    (set! %coderack-graphics% #f)
    (set! *workspace-window* (make-null-window 'workspace '(garbage-collect caching-on flush)))
    (set! *slipnet-window* (make-null-window 'slipnet '(clear)))
    (set! *coderack-window* (make-null-window 'coderack '(clear)))
    (set! *themespace-window* (make-null-window 'themespace
        '(erase-all-themes update-thematic-pressure update-graphics
          set-theme-graphics-parameters-and-draw garbage-collect)))
    (set! *top-themes-window* (make-null-window 'top-themes '()))
    (set! *bottom-themes-window* (make-null-window 'bottom-themes '()))
    (set! *vertical-themes-window* (make-null-window 'vertical-themes '()))
    (set! *memory-window*
      ;; add-memory-icon gives each answer or snag description its icon
      ;; drawing procedures (memory-graphics.ss), which memory.ss calls even
      ;; when nothing is displayed; here they draw nothing.
      (let ((null-window (make-null-window 'memory '(draw))))
        (lambda (self . msg)
          (case (car msg)
            ((add-memory-icon)
             (tell (cadr msg) 'set-graphics-info (lambda (activation) 'no-icon) 'no-icon)
             'done)
            (else (apply null-window self msg))))))
    (set! *trace-window* (make-null-window 'trace '(initialize add-event)))
    (set! *temperature-window*
      (make-null-window 'temperature '(initialize update-graphics)))
    (set! *EEG-window* (make-null-window 'EEG '(initialize)))
    ;; Each codelet type keeps its own reference to the Coderack window,
    ;; set by the window (coderack-graphics.ss); a codelet's 'run tells it
    ;; 'set-last-codelet-type whatever the graphics switches say.
    (let ((coderack-graphics (make-null-window 'coderack-graphics '(set-last-codelet-type))))
      (for-each
        (lambda (type)
          (tell type 'set-graphics-parameters coderack-graphics #f #f #f #f #f #f #f #f))
        *codelet-types*))
    (set! *control-panel*
      (lambda (self . msg)
        (case (car msg)
          ;; as in gui.ss, with the verbose checkbox off unless run.ss's
          ;; --verbose set $verbose?
          ((set-verbose-step-mode) (set! %verbose% (or (cadr msg) $verbose?)) 'done)
          (else (errorf 'headless-window
                        "control panel received unexpected message ~s" msg)))))
    (set! *comment-window*
      (let ((original make-scrollable-text-window))
        (set! make-scrollable-text-window
          (lambda args (make-recording-text-window)))
        (let ((w (make-comment-window)))
          (set! make-scrollable-text-window original)
          w)))))

;;;---------------------------------------------------------------------------
;;; 7. Errors: print the condition and the procedure names on the stack
;;; (Chez's script mode prints only the message), then exit 1.

(define $print-backtrace
  (lambda (k)
    (let loop ((o (inspect/object k)) (n 0))
      (when (and (< n 40) (eq? (o 'type) 'continuation))
        (let* ((code (o 'code))
               (name (and code (code 'name)))
               (src (guard (e (#t #f))
                      (call-with-values (lambda () (o 'source-path)) list))))
          (fprintf (current-error-port) "  ~a ~a~a~%" n (or name "?")
                   (if (and (pair? src) (pair? (cdr src)))
                       (format " ~a" src)
                       "")))
        (loop (o 'link) (+ n 1))))))

(define $install-error-handler!
  (lambda ()
    (base-exception-handler
      (lambda (c)
        (base-exception-handler (lambda (c) (exit 1)))  ; no recursion
        (flush-output-port (current-output-port))
        (fprintf (current-error-port) "Error: ")
        (display-condition c (current-error-port))
        (newline (current-error-port))
        (when (continuation-condition? c)
          ($print-backtrace (condition-continuation c)))
        (exit 1)))))
