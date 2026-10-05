#lang racket/base
;; Item 17 (final audit): no engine module requires racket/gui.
;;
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;
;; For every module of the port outside racket/gui/ (and the views modules in
;; racket/gui/ that must render offscreen), walks the transitive imports of
;; its compiled form, at every phase, and checks that racket/gui (any
;; racket/gui/... module, or mred, its implementation) is not among them.
;; The engine and the headless driver must not reach racket/draw either; the
;; views modules may (they render on racket/draw bitmaps).  main.rkt and
;; metacat.rkt reach the GUI only through lazy-require, which is not an
;; import.  As a control, the walk does find racket/gui from gui/gui.rkt.
(require rackunit
         racket/runtime-path
         (only-in racket/string string-prefix?)
         syntax/modresolve)

(define-runtime-path here ".")
(define (port-file f) (simplify-path (build-path here 'up f)))

;; every module reachable from file by require, as resolved paths (or
;; symbols for primitive modules, (submod ...) lists for submodules)
(define (transitive-imports file)
  (define seen (make-hash))
  (define (visit mod)
    (unless (hash-ref seen mod #f)
      (hash-set! seen mod #t)
      (define imports
        (with-handlers ([exn:fail? (lambda (e) '())])
          ;; declares the module (and its imports) without instantiating it
          ((current-module-name-resolver) mod #f #f #t)
          (module->imports mod)))
      (for* ([phase+mods imports] [mpi (cdr phase+mods)])
        (visit (resolve-module-path-index mpi (if (pair? mod) (cadr mod) mod))))))
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (visit (resolve-module-path file #f)))
  (hash-keys seen))

(define (port->string* in)
  (let loop ([acc '()])
    (define s (read-string 65536 in))
    (if (eof-object? s) (apply string-append (reverse acc)) (loop (cons s acc)))))

(define (mentions? names collection-dir)
  ;; names under a collection directory, e.g. ".../collects/racket/gui/"
  (for/or ([n names])
    (define p (if (pair? n) (cadr n) n))
    ;; the port's own racket/gui/ directory is not the collection
    (and (path? p)
         (not (string-prefix? (path->string p) port-root))
         ;; racket/draw's PostScript dc imports racket/gui/dynamic, a small
         ;; base module that only asks whether racket/gui is loaded
         (not (regexp-match? #rx"/racket/gui/dynamic\\.rkt$" (path->string p)))
         (regexp-match? collection-dir (path->string p)))))

(define port-root (path->string (port-file (quote same))))

(define gui-rx #rx"/(racket/gui|mred|mrlib)/")
(define draw-rx #rx"/racket/draw(/|\\.rkt$)")

(define engine-modules
  '("compat.rkt" "utilities.rkt" "engine-lang.rkt" "engine.rkt"
    "headless.rkt" "cli.rkt" "main.rkt" "metacat.rkt" "one-window.rkt"))
(define view-modules
  '("gui/sgl.rkt" "gui/fonts.rkt" "gui/colors.rkt" "gui/engine-route.rkt"
    "gui/views.rkt"))

(for ([f engine-modules])
  (define names (transitive-imports (port-file f)))
  (check-true (> (length names) 3) (format "~a: the walk found its imports" f))
  (check-false (mentions? names gui-rx) (format "~a reaches racket/gui" f))
  (check-false (mentions? names draw-rx) (format "~a reaches racket/draw" f)))

(for ([f view-modules])
  (define names (transitive-imports (port-file f)))
  (check-false (mentions? names gui-rx) (format "~a reaches racket/gui" f)))

;; the views do reach racket/draw, and gui.rkt racket/gui: the walk sees them
(check-true (mentions? (transitive-imports (port-file "gui/views.rkt")) draw-rx))
(check-true (mentions? (transitive-imports (port-file "gui/gui.rkt")) gui-rx))

;; the engine's included files have no require of their own
(for ([f (directory-list (port-file "engine") #:build? #t)]
      #:when (regexp-match? #rx"\\.rktl$" (path->string f)))
  (check-false (regexp-match? #px"(?m:^\\s*\\((require|lazy-require|dynamic-require)\\b)"
                              (call-with-input-file f port->string*))
               (format "~a has a require" f)))
