;;; globals.lisp -- special proclamations for the Franz free globals.
;;;
;;; Franz Lisp treats a SETQ of an undeclared variable as an assignment to
;;; its global value.  SBCL does the same at run time, but `compile-file`
;;; reports every such reference as an "undefined variable" WARNING.  This
;;; file proclaims the source's true globals special with value-less DEFVARs.
;;; A value-less DEFVAR binds nothing and changes no value: the source still
;;; assigns every value itself (init-pnet, the DEFVARs in init.lisp, ...).
;;;
;;; Only names that no source file ever binds lexically (LET, DO, PROG,
;;; lambda lists, LOOP FOR) are listed, and none of them is a flavor
;;; instance variable.  Proclaiming one of those special would make a LET in
;;; one function visible to a free SETQ in a function it calls, which would
;;; change behavior.  tests/pnet-compile-tests.lisp checks both conditions.
;;;
;;; Some free globals are deliberately NOT listed, e.g. NODE (pnode
;;; :hotter-neighbor-activation) and RES (pnode :suppress-instances).  The
;;; original methods SETQ them without binding them, but many other functions
;;; bind NODE and RES as locals with LET.  They stay undeclared, which means
;;; global assignment from the methods and lexical LETs everywhere else, the
;;; same as compiled Franz.  They are the only undefined-variable warnings
;;; left when the Pnet files compile; see PORTING_NOTES.md, item 7.  The
;;; cytoplasm and codelets files add more names of the same kind (TYPE,
;;; STATUS, NN, CYTO-BLOCK1, ...): see PORTING_NOTES.md, item 8.

;;; --- Globals that init.lisp DEFVARs (forward declarations) ----------------
;;; init.lisp is loaded after the Pnet files, but pnet-functions already
;;; refers to these.  init.lisp's own DEFVARs still assign the initial values,
;;; because the variables are unbound until then.  *print-array* is left out:
;;; it is a CL symbol (item 9).
(defvar *coderack*)
(defvar *current-target*)
(defvar %initial-activation%)
(defvar %min-activation-to-be-added%)
(defvar %max-activation-to-be-transmitted%)
(defvar %k%)
(defvar %length%)
(defvar %target-activation%)
(defvar %brick-activation%)
(defvar %dtarget-activation%)
(defvar %block-activation%)
(defvar %target-plus%)
(defvar %brick-plus%)
(defvar %dtarget-plus%)
(defvar %node-minus%)
(defvar %similar%)
(defvar %operation%)
(defvar %instance%)
(defvar %verbose%)
(defvar %graphics%)                     ; also DEFVARed (nil) in graphics-stubs
(defvar %first-decay-rate%)
(defvar %second-decay-rate%)
(defvar %third-decay-rate%)
(defvar %fourth-decay-rate%)
(defvar %fifth-decay-rate%)
(defvar %sixth-decay-rate%)
(defvar %upper-threshold%)
(defvar %first-threshold%)
(defvar %upper-urgency%)
(defvar %first-urgency%)
(defvar %second-urgency%)
(defvar %third-urgency%)
(defvar %fourth-urgency%)
(defvar %fifth-urgency%)
(defvar %temperature-threshold%)
(defvar *name-counter*)

;;; --- pnet-def.lisp --------------------------------------------------------
;;; *pnet* is SETQed at top level in pnet-def.lisp.
(defvar *pnet*)

;;; The 91 pnode holders that init-pnet SETQs, in init-pnet order.  *pnet*
;;; lists 88 of them: PLUS, MINUS and TIMES are created but are not in *pnet*.
;;; initialize-pnet-2 EVALs the neighbor names, so these must be global
;;; values.
(defvar node-1) (defvar node-2) (defvar node-3) (defvar node-4)
(defvar node-5) (defvar node-6) (defvar node-7) (defvar node-8)
(defvar node-9) (defvar node-10) (defvar node-12) (defvar node-15)
(defvar node-16) (defvar node-20) (defvar node-25) (defvar node-30)
(defvar node-40) (defvar node-50) (defvar node-60) (defvar node-70)
(defvar node-80) (defvar node-81) (defvar node-90) (defvar node-100)
(defvar node-150)
(defvar node-mu10) (defvar node-multiply) (defvar node-add)
(defvar node-subtract)
(defvar plus1-1) (defvar plus1-2) (defvar plus1-3) (defvar plus1-4)
(defvar plus1-5) (defvar plus1-6) (defvar plus1-7) (defvar plus1-8)
(defvar plus1-9)
(defvar plus2-2) (defvar plus2-3) (defvar plus2-4) (defvar plus2-5)
(defvar plus2-6) (defvar plus2-7) (defvar plus2-8)
(defvar plus3-3) (defvar plus3-4) (defvar plus3-5) (defvar plus3-6)
(defvar plus3-7)
(defvar plus4-4) (defvar plus4-5) (defvar plus4-6)
(defvar plus5-5) (defvar plus7-8) (defvar plus5-10)
(defvar times2-2) (defvar times2-3) (defvar times2-4) (defvar times2-5)
(defvar times2-7) (defvar times2-10) (defvar times2-12) (defvar times2-20)
(defvar times3-3) (defvar times3-10) (defvar times3-20)
(defvar times4-4) (defvar times4-10) (defvar times4-20)
(defvar times5-5) (defvar times5-6) (defvar times5-10) (defvar times5-20)
(defvar times6-10) (defvar times7-7) (defvar times7-10) (defvar times8-10)
(defvar times9-9) (defvar times9-10) (defvar times10-10) (defvar times10-15)
(defvar operand) (defvar result+) (defvar resultx) (defvar similar)
(defvar operation) (defvar instance)
(defvar plus) (defvar minus) (defvar times)

;;; --- cyto-def.lisp, codelets.lisp, init.lisp, start.lisp (item 8) ---------
;;; Working-memory globals: SETQed by init-cytoplasm (cyto-def), read-target
;;; / replace-target / repump (codelets) and config (start).
(defvar *cytoplasm*) (defvar *context*) (defvar *temperature*)
(defvar *problem-solved*) (defvar cyto-target)
(defvar %operand%) (defvar %result+%) (defvar %resultx%)
;;; Scratch variables the original functions SETQ without binding them
;;; (Franz globals).  Checked against the scan: they are free in the
;;; printout too (e.g. look-for-new-block binds LIST but uses LISTE,
;;; PDF p.55).  Each is set before it is read inside the same function.
(defvar a) (defvar brick) (defvar bricki) (defvar cyto-bricki) ; read-brick
(defvar cont) (defvar n)                ; propagate-success
(defvar pp)                             ; replace-target
(defvar diffrel)                        ; sim
(defvar div)                            ; round
(defvar liste)                          ; look-for-new-block
(defvar similarity)                     ; look-for-diff
(defvar reserve) (defvar weights)       ; check-temperature
(defvar min)                            ; eliminate (NUMBO::MIN, shadowed)
(defvar lv)                             ; cyto-node :upper-/:lower-neighbor ...
