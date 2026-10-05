#lang racket/base
;; Item 11: run.ss's own break, go and step mode, as the GUI will use them.
;; The headless drivers (racket/headless.rkt) replace break; here the
;; engine's break stops at the breakpoint (*break-time*), (reset) returns to
;; the caller, and (go) resumes the run where it stopped, through the
;; continuation break captured.  A run stopped and resumed must be the same
;; run as one never stopped: same codelet count, generator state,
;; temperature and output.
;;
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
(require rackunit
         racket/port
         racket/runtime-path)

(define-runtime-path engine-path "../engine.rkt")
(define-runtime-path compat-path "../compat.rkt")
(define-runtime-path headless-path "../headless.rkt")

;; a fresh engine with headless windows, and a control panel that accepts
;; the messages break and go send
(define (fresh-engine)
  (define ns (make-base-namespace))
  (define (get name) (parameterize ([current-namespace ns]) (dynamic-require engine-path name)))
  (define (compat name) (parameterize ([current-namespace ns]) (dynamic-require compat-path name)))
  ((parameterize ([current-namespace ns])
     (dynamic-require headless-path 'install-headless-windows!)))
  (define set-global! (get 'set-global!))
  (define modes '())
  (set-global! '*control-panel*
               (lambda (self . msg)
                 (case (car msg)
                   [(set-verbose-step-mode) (set-global! '%verbose% (cadr msg)) 'done]
                   [(switch-to-input-mode switch-to-run-mode)
                    (set! modes (cons (car msg) modes)) 'done]
                   [else (error 'control-panel "unexpected message ~s" msg)])))
  (values get compat (lambda () (reverse modes))))

;; runs thunk until the engine's (reset); returns its output
(define (until-reset compat thunk)
  (define reset-handler (compat 'reset-handler))
  (with-output-to-string
    (lambda ()
      (let/ec k
        (define old (reset-handler))
        (reset-handler (lambda () (reset-handler old) (k 'reset)))
        (call-with-continuation-prompt thunk)))))

(define (state get compat)
  (list ((compat 'random-seed)) (get '*codelet-count*) (get '*temperature*)))

(module+ test
  ;; one run stopped at 150, 300 and 450
  (define-values (get compat modes) (fresh-engine))
  (define set-global! (get 'set-global!))
  (void (with-output-to-string (lambda () ((get 'init-mcat) 'abc 'abd 'xyz #f 7))))
  (set-global! '*break-time* 150)
  (define out1 (until-reset compat (get 'run-mcat)))
  (check-equal? out1 "Codelets run: 150\nstopped\n")
  (check-equal? (get '*codelet-count*) 150)
  (check-false (get '*running?*))
  (check-true (procedure? (get '*breakpoint-continuation*)))
  (set-global! '*break-time* 300)
  (define out2 (until-reset compat (get 'go)))
  (check-equal? (get '*codelet-count*) 300)
  (set-global! '*break-time* 450)
  (define out3 (until-reset compat (get 'go)))
  (check-equal? (get '*codelet-count*) 450)
  (check-equal? (modes) '(switch-to-input-mode switch-to-run-mode switch-to-input-mode
                          switch-to-run-mode switch-to-input-mode))
  (define stopped (state get compat))

  ;; the same run, never stopped before 450
  (define-values (get2 compat2 modes2) (fresh-engine))
  (void (with-output-to-string (lambda () ((get2 'init-mcat) 'abc 'abd 'xyz #f 7))))
  ((get2 'set-global!) '*break-time* 450)
  (define out-straight (until-reset compat2 (get2 'run-mcat)))
  (check-equal? (state get2 compat2) stopped "a stopped and resumed run is the same run")
  (check-equal? out-straight "Codelets run: 450\nstopped\n")
  (check-equal? (list out2 out3) '("Codelets run: 300\nstopped\n" "Codelets run: 450\nstopped\n"))

  ;; step mode (ss): a break every n codelets
  (define-values (get3 compat3 modes3) (fresh-engine))
  (void (with-output-to-string (lambda () ((get3 'init-mcat) 'abc 'abd 'xyz #f 7))))
  (void (with-output-to-string (lambda () ((get3 'ss) 40))))
  (check-true (get3 '*step-mode?*))
  (void (until-reset compat3 (get3 'run-mcat)))
  (check-equal? (get3 '*codelet-count*) 40)
  (void (until-reset compat3 (get3 'go)))
  (check-equal? (get3 '*codelet-count*) 80)
  ;; go without a break
  (define-values (get4 compat4 modes4) (fresh-engine))
  (check-equal? (with-output-to-string (lambda () ((get4 'go)))) "No previous break.\n"))
