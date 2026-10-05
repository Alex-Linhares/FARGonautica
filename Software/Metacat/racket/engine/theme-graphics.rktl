;;=============================================================================
;; Copyright (c) 1999, 2003 by James B. Marshall
;;
;; This file is part of Metacat.
;;
;; Metacat is based on Copycat, which was originally written in Common
;; Lisp by Melanie Mitchell.
;;
;; Metacat is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.
;;
;; Metacat is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;=============================================================================
;; Ported to Racket, 2026: relation-name from theme-graphics.ss, verbatim.
;; Part of the engine (racket/engine.rkt), since trace.ss's print-pattern
;; uses it; the rest of theme-graphics.ss (the Themespace windows) is in
;; racket/gui/theme-graphics.rktl (docs/porting-notes.md, item 14).

(define relation-name
  (lambda (relation)
    (cond
      ((eq? relation #f) "diff")
      ((eq? relation plato-identity) "iden")
      ((eq? relation plato-opposite) "opp")
      ((eq? relation plato-successor) "succ")
      ((eq? relation plato-predecessor) "pred")
      (else #f))))
