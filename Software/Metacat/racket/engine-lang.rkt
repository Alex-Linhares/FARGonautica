#lang racket/base
;; The module language of racket/engine.rkt.
;;
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;
;; racket/base, except that the values of module-level expressions are
;; discarded instead of printed.  The original's files are loaded into Chez's
;; top level, where the value of an expression such as
;; (define-codelet-procedure* ...) or (category-link* ...) is just dropped;
;; racket/base's #%module-begin would print each one.  Each module-level
;; form is partially expanded; definitions, requires, provides and the like
;; are kept as they are, `begin' (from `include' and from macros) is spliced
;; and handled form by form, and everything else is wrapped in `void'.
(require (for-syntax racket/base syntax/kerncase))

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [engine-module-begin #%module-begin]))

(define-syntax (engine-module-begin stx)
  (syntax-case stx ()
    [(_ form ...)
     #'(#%plain-module-begin (discard-value form) ...)]))

(define-syntax (discard-value stx)
  (syntax-case stx ()
    [(_ form)
     (let ([e (local-expand #'form 'module (kernel-form-identifier-list))])
       (kernel-syntax-case e #f
         [(begin sub ...) #'(begin (discard-value sub) ...)]
         [(define-values . _) e]
         [(define-syntaxes . _) e]
         [(begin-for-syntax . _) e]
         [(#%require . _) e]
         [(#%provide . _) e]
         [(#%declare . _) e]
         [(module . _) e]
         [(module* . _) e]
         [_ #`(#%app void #,e)]))]))
