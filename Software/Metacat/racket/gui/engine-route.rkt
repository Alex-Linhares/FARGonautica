#lang racket/base
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;
;; define and set! for racket/gui/views.rkt, which includes the graphics
;; files (racket/gui/*.rktl).  In the original those files are loaded into
;; the same top level as the model, so their definitions and set!s reach
;; variables the model reads (colours, fonts, *fg-color*, *display-mode?*,
;; *workspace-window*, ...).  A Racket module cannot define or set! a
;; variable it imports, so a module-level define, or any set!, of a name
;; imported from racket/engine.rkt becomes (set-global! 'name value); every
;; other define and set! is racket/base's.  The included files stay verbatim.
;; See docs/porting-notes.md, item 13.

(require (for-syntax racket/base))

(provide (rename-out [view-define define] [view-set! set!]))

(begin-for-syntax
  ;; is id imported from racket/engine.rkt?
  (define (engine-binding? id)
    (let ([b (identifier-binding id)])
      (and (list? b)
           (let ([name (resolved-module-path-name (module-path-index-resolve (car b)))])
             (and (path? name)
                  (regexp-match? #rx"[/\\\\]racket[/\\\\]engine[.]rkt$" (path->string name))))))))

;; is id imported from racket/gui/views.rkt?  (gui.rktl, the control panel,
;; set!s a few variables of the graphics files: item 15)
(begin-for-syntax
  (define (views-binding? id)
    (let ([b (identifier-binding id)])
      (and (list? b)
           (let ([name (resolved-module-path-name (module-path-index-resolve (car b)))])
             (and (path? name)
                  (regexp-match? #rx"[/\\\\]racket[/\\\\]gui[/\\\\]views[.]rkt$"
                                 (path->string name))))))))

(define-syntax (view-define stx)
  (syntax-case stx ()
    [(_ (name . formals) body ...)
     #'(view-define name (lambda formals body ...))]
    [(_ name value)
     (if (and (eq? (syntax-local-context) 'module) (engine-binding? #'name))
         (with-syntax ([set-global! (datum->syntax #'name 'set-global!)])
           #'(set-global! 'name value))
         #'(define name value))]))

(define-syntax (view-set! stx)
  (syntax-case stx ()
    [(_ name value)
     (cond
       [(engine-binding? #'name)
        (with-syntax ([set-global! (datum->syntax #'name 'set-global!)])
          #'(set-global! 'name value))]
       [(views-binding? #'name)
        (with-syntax ([set-view-global! (datum->syntax #'name 'set-view-global!)])
          #'(set-view-global! 'name value))]
       [else #'(set! name value)])]))
