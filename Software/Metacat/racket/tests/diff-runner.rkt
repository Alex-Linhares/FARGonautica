#lang racket/base
;; The Racket runner for the differential batteries in tests/diff/ (items 03,
;; 04).  A battery is evaluated twice, after tests/diff/helpers.scm: by Chez
;; Scheme 10 with chez_scheme/original/ loaded (chez_scheme/oracle/diff-eval.ss),
;; and here, in a namespace made of racket/base plus the given modules.  Every
;; line of output (one per test form) must be identical.
;;
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
(require rackunit
         compiler/cm
         racket/runtime-path
         racket/string
         racket/system)

(provide check-battery chez-output racket-output)

(define-runtime-path repo "../..")
(define helpers (build-path repo "tests" "diff" "helpers.scm"))
(define diff-eval (build-path repo "chez_scheme" "oracle" "diff-eval.ss"))

(define (chez-output battery [setup '()])
  (define scheme (or (find-executable-path "scheme") (find-executable-path "chezscheme")))
  (unless scheme (error 'diff-runner "Chez Scheme not found"))
  (define out (open-output-string))
  (define ok?
    (parameterize ([current-output-port out]
                   [current-directory repo])
      (apply system* scheme "--script" diff-eval helpers (append setup (list battery)))))
  (unless ok? (error 'diff-runner "diff-eval.ss failed:\n~a" (get-output-string out)))
  (get-output-string out))

;; modules: module paths required into the namespace, in order (later ones
;; shadow earlier ones); set-global!: the name, in the namespace, of the
;; procedure to use as b:set-global! (the namespace has its own instances
;; of the modules), or #f
(define (racket-output battery modules set-global!)
  (define ns (make-base-empty-namespace))
  (define result (open-output-string))
  ;; the repository as current directory, as for Chez: batteries may load
  ;; files by relative path (workspace-battery.scm does)
  (parameterize ([current-namespace ns]
                 [current-directory repo])
    (namespace-require 'racket/base)
    ;; the compilation manager recompiles a module whose included files
    ;; changed (engine.rkt includes engine/*.rktl); the default load handler
    ;; would load a stale .zo, since it only compares engine.rkt's own date.
    ;; The handler must be made in the namespace it serves (it skips
    ;; modules of other module registries).
    (parameterize ([current-load/use-compiled
                    (make-compilation-manager-load/use-compiled-handler)])
      (for ([m (in-list modules)]) (namespace-require m)))
    (namespace-set-variable-value!
     'b:capture
     (lambda (thunk)
       (let ([o (open-output-string)])
         (parameterize ([current-output-port o]) (thunk))
         (get-output-string o))))
    (namespace-set-variable-value!
     'b:set-global!
     (if set-global!
         (eval set-global!)
         (lambda (name v) (error 'b:set-global! "no engine globals here: ~s" name))))
    (for ([file (list helpers battery)])
      (call-with-input-file file
        (lambda (in)
          (let loop ()
            (define form (read in))
            (unless (eof-object? form)
              (if (and (pair? form) (eq? (car form) 'test))
                  (let* ([name (cadr form)]
                         [text (with-handlers ([(lambda (e) #t)
                                                (lambda (e)
                                                  ;; METACAT_DIFF_DEBUG=1 shows why
                                                  (when (getenv "METACAT_DIFF_DEBUG")
                                                    (eprintf "~a: ~a\n" name
                                                             (if (exn? e) (exn-message e) e)))
                                                  "ERROR")])
                                 ((eval 'b:canon) (eval (caddr form))))])
                    (fprintf result "~a => ~a\n" name text))
                  (eval form))
              (loop)))))))
  (get-output-string result))

;; tests are separated at lines starting "name => "; values may span lines
(define (split-tests text)
  (let loop ([lines (string-split text "\n" #:trim? #f)] [acc '()])
    (cond
      [(null? lines) (reverse acc)]
      [(and (pair? acc) (not (regexp-match? #rx"^[^ ]+ => " (car lines))))
       (loop (cdr lines) (cons (string-append (car acc) "\n" (car lines)) (cdr acc)))]
      [else (loop (cdr lines) (cons (car lines) acc))])))

;; chez-setup: Chez-only files evaluated between helpers.scm and the battery
;; (the Racket side gets the same definitions from its modules)
(define (check-battery battery modules #:set-global! [set-global! #f]
                       #:chez-setup [chez-setup '()])
  (define chez (split-tests (chez-output battery chez-setup)))
  (define rkt (split-tests (racket-output battery modules set-global!)))
  (define test-count
    (with-input-from-file battery
      (lambda ()
        (for/sum ([form (in-port read)])
          (if (and (pair? form) (eq? (car form) 'test)) 1 0)))))
  (check-equal? (length (filter (lambda (s) (regexp-match? #rx"^[^ ]+ => " s)) chez))
                test-count
                "Chez printed one result per test")
  (check-equal? (length rkt) (length chez) "same number of results")
  (for ([c (in-list chez)] [r (in-list rkt)])
    (define name (car (string-split c " => ")))
    (check-equal? r c (format "battery test ~a" name))))
