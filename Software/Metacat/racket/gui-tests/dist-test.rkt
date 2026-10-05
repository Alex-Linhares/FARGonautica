#lang racket/base
;; Item 16: the standalone program.  make-dist.sh builds it (raco exe +
;; raco distribute) into a fresh directory; then, from another empty
;; directory:
;;
;;  - `metacat abc abd xyz --seed 3852097033 --trace FILE` (Run 7) prints what
;;    racket/cli.rkt prints and writes tests/golden/abc-abd-xyz_3852097033.jsonl
;;    byte for byte; `metacat abc abd xyz` (a clock seed) answers;
;;  - `metacat` with no arguments opens the control panel and every window
;;    on the (virtual) display.
;;
;; When bubblewrap (bwrap) is installed, the program runs in a sandbox where
;; the Racket installation, the home directories (so this repository) and
;; /tmp are empty, which shows that the distribution needs none of them.
;;
;; Needs a display: tests/run-tests.sh runs racket/gui-tests under xvfb-run
;; (never on the owner's screen; see control-panel-test.rkt).  Also needs
;; xwininfo (x11-utils) to list the windows.
;;
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
(require rackunit
         racket/file
         racket/list
         racket/port
         racket/string
         racket/system
         racket/path
         racket/runtime-path)

(define-runtime-path repo "../..")
(define-runtime-path make-dist "../../make-dist.sh")
(define-runtime-path golden "../../tests/golden/abc-abd-xyz_3852097033.jsonl")
(define-runtime-path cli "../cli.rkt")

(when (getenv "WAYLAND_DISPLAY")
  (error 'dist-test "WAYLAND_DISPLAY is set: run under env -u WAYLAND_DISPLAY GDK_BACKEND=x11 xvfb-run"))

(define bash (find-executable-path "bash"))
(define bwrap (find-executable-path "bwrap"))
(define xwininfo (find-executable-path "xwininfo"))

(define work (make-temporary-directory "metacat-dist-test-~a"))
(define dest (build-path work "metacat"))   ; the distribution
(define clean (build-path work "clean"))     ; an empty working directory
(make-directory clean)

;; (values status stdout stderr) of a program run to completion
(define (run-program program args #:cwd [cwd (current-directory)] #:env [env #f])
  (define-values (sp out in err)
    (parameterize ([current-directory cwd]
                   [current-environment-variables
                    (or env (current-environment-variables))])
      (apply subprocess #f #f #f program args)))
  (close-output-port in)
  (define o (open-output-string))
  (define e (open-output-string))
  (define t1 (thread (lambda () (copy-port out o))))
  (define t2 (thread (lambda () (copy-port err e))))
  (subprocess-wait sp)
  (thread-wait t1) (thread-wait t2)
  (close-input-port out) (close-input-port err)
  (values (subprocess-status sp) (get-output-string o) (get-output-string e)))

;; a minimal environment: no Racket variables, only the display
(define (minimal-env)
  (define env (make-environment-variables
               #"PATH" #"/usr/bin:/bin"
               #"HOME" (path->bytes clean)
               #"GDK_BACKEND" #"x11"))
  (for ([v '(#"DISPLAY" #"XAUTHORITY")])
    (define x (environment-variables-ref (current-environment-variables) v))
    (when x (environment-variables-set! env v x)))
  env)

;; program + arguments that run the distributed metacat with ARGS: in the
;; sandbox, the distribution is /opt/metacat and the working directory /work
(define (metacat-command args)
  (if bwrap
      (let ([xauth (getenv "XAUTHORITY")])
        (values bwrap
                (append
                 (list "--die-with-parent" "--unshare-pid"
                       "--ro-bind" "/usr" "/usr" "--ro-bind" "/etc" "/etc"
                       "--symlink" "usr/bin" "/bin" "--symlink" "usr/lib" "/lib"
                       "--symlink" "usr/lib64" "/lib64"
                       "--tmpfs" "/usr/share/racket"
                       "--tmpfs" "/usr/lib/x86_64-linux-gnu/racket"
                       "--tmpfs" "/home" "--tmpfs" "/tmp"
                       "--proc" "/proc" "--dev" "/dev"
                       "--ro-bind" (path->string dest) "/opt/metacat"
                       "--bind" (path->string clean) "/work"
                       "--setenv" "HOME" "/work"
                       "--chdir" "/work")
                 (if (directory-exists? "/tmp/.X11-unix")
                     (list "--ro-bind" "/tmp/.X11-unix" "/tmp/.X11-unix")
                     '())
                 (if (and xauth (file-exists? xauth)) (list "--ro-bind" xauth xauth) '())
                 (list "/opt/metacat/bin/metacat")
                 args)))
      (values (build-path dest "bin" "metacat") args)))

;; the path of FILE in the clean directory, as the program sees it
(define (in-clean file) (if bwrap (string-append "/work/" file) (path->string (build-path clean file))))

(test-case "make-dist.sh builds the distribution"
  (define-values (status out err) (run-program bash (list (path->string make-dist) (path->string dest))))
  (check-equal? status 0 err)
  (check-true (file-exists? (build-path dest "bin" "metacat")))
  (check-true (file-exists? (build-path dest "LICENSE")))
  (check-true (file-exists? (build-path dest "README.md")))
  ;; gui.ss's help text travels with it (define-runtime-path)
  (check-true (for/or ([f (in-directory dest)])
                (equal? (file-name-from-path f) (string->path "help.txt")))))

(unless bwrap
  (printf "dist-test: bwrap not installed; running the distribution without a sandbox\n"))

(test-case "the distributed program runs abc abd xyz from a clean directory"
  (define-values (program args)
    (metacat-command (list "abc" "abd" "xyz" "--seed" "3852097033" "--max-codelets" "10000" "--trace" (in-clean "run7.jsonl"))))
  (define-values (status out err) (run-program program args #:cwd clean #:env (minimal-env)))
  (check-equal? status 0)
  (check-equal? err "")
  (define-values (s2 expected e2)
    (run-program (find-system-path 'exec-file)
                 (list (path->string cli) "abc" "abd" "xyz" "--seed" "3852097033" "--max-codelets" "10000")))
  (check-equal? out expected "the output is racket/cli.rkt's")
  (check-regexp-match #rx"Answers: \\(wyz\\)" out)
  (check-equal? (file->bytes (build-path clean "run7.jsonl")) (file->bytes golden)
                "the trace is the golden's")
  ;; and without a seed (the clock's)
  (define-values (p3 a3) (metacat-command (list "abc" "abd" "xyz")))
  (define-values (s3 o3 e3) (run-program p3 a3 #:cwd clean #:env (minimal-env)))
  (check-equal? s3 0 e3)
  (check-regexp-match #rx"(?m:^Answers: )" o3)
  ;; bad arguments: the CLI's exit code
  (define-values (p4 a4) (metacat-command (list "abc" "abd")))
  (define-values (s4 o4 e4) (run-program p4 a4 #:cwd clean #:env (minimal-env)))
  (check-equal? s4 2)
  (check-regexp-match #rx"usage" e4))

;; the titles of the top-level windows on the display
(define (window-titles)
  (define-values (s out err) (run-program xwininfo '("-root" "-tree")))
  (remove-duplicates
   (for/list ([m (regexp-match* #rx"0x[0-9a-f]+ \"([^\"]*)\": \\(\"metacat\"" out #:match-select cadr)])
     m)))

(define expected-windows
  '("Metacat Control Panel" "Workspace" "Slipnet" "Coderack" "Temperature"
    "Temporal Trace" "Commentary" "Episodic Memory" "Top Themes" "Bottom Themes"
    "Vertical Themes"))

(test-case "the distributed program opens the GUI"
  (check-not-false xwininfo "xwininfo (x11-utils) is installed")
  (check-not-false (getenv "DISPLAY") "a (virtual) display")
  (define-values (program args) (metacat-command '()))
  (define-values (sp out in err)
    (parameterize ([current-directory clean]
                   [current-environment-variables (minimal-env)])
      (apply subprocess #f #f #f program args)))
  (close-output-port in)
  (define o (open-output-string))
  (define e (open-output-string))
  (define t1 (thread (lambda () (copy-port out o))))
  (define t2 (thread (lambda () (copy-port err e))))
  (define titles
    (let loop ([n 0])
      (define ts (window-titles))
      (cond [(or (andmap (lambda (w) (member w ts)) expected-windows)
                 (> n 300)
                 (not (eq? (subprocess-status sp) 'running)))
             ts]
            [else (sleep 0.1) (loop (add1 n))])))
  (check-equal? (subprocess-status sp) 'running "the GUI is still up")
  (for ([w expected-windows])
    (check-not-false (member w titles) (format "window ~s is open" w)))
  ;; the GUI keeps running until its windows are closed: stop it
  (subprocess-kill sp #t)
  (subprocess-wait sp)
  (thread-wait t1) (thread-wait t2)
  ;; (its stdout, "Initializing windows...done", is block-buffered and lost
  ;; to the kill)
  (check-equal? (get-output-string e) "" "nothing on stderr before it was stopped"))

(delete-directory/files work)
