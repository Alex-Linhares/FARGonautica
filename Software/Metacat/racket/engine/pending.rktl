;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;
;; Names that the original refers to but never defines.  Until item 15 this
;; file also held stand-ins for names defined in files not ported yet; each
;; item deleted its names as it ported a file, and since item 15 every file
;; is ported (docs/porting-notes.md, item 04 and item 17).  What is left is
;; not pending on any item: Chez's top level tolerates these names (set! of
;; an unbound variable, or a reference that is never evaluated), Racket's
;; module system does not.
;;
;; Nothing here runs at load time.

;; *temperature-clamped?* and *initial-slipnode-unclamp-time* have no
;; definition in the original: init-mcat (run.ss) creates them by set! on
;; the top level (formulas.ss reads the first, run-mcat the second).  Not
;; pending on any item.
(define *temperature-clamped?* #f)
(define *initial-slipnode-unclamp-time* #f)
;; Never defined in the original: bonds.ss's bonds-equal? (itself never
;; called) refers to it.  Under Chez a call would raise "variable
;; same-direction? is not bound"; this raises too.  Not pending on any item.
(define (same-direction? . args)
  (error 'same-direction? "variable same-direction? is not bound"))
;; Never defined in the original: the Temporal Trace's
;; get-complement-codelet-pattern message (trace.ss), never sent, returns
;; it.  Not pending on any item.
(define-syntax complement-codelet-pattern
  (syntax-id-rules ()
    [_ (error 'complement-codelet-pattern
              "variable complement-codelet-pattern is not bound")]))


