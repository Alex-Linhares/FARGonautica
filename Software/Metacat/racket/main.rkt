#lang racket/base
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
;; Ported to Racket, 2026.

;; Entry point: `racket racket/main.rkt [SCALE]` opens Metacat's windows and
;; its control panel, as typing (setup) after loading metacat.ss did.  Type a
;; problem such as "abc abd xyz" (optionally an answer and a seed: "abc abd
;; xyz 7") and press Enter, then Go or Step.  racket/gui is loaded only when
;; main runs (lazy-require), so requiring this module stays headless.
;; lazy-require also tells raco exe to embed gui/gui.rkt (the standalone
;; executable, racket/metacat.rkt).

(require racket/lazy-require)

(lazy-require ["gui/gui.rkt" (setup)])

(provide metacat-version main)

(define metacat-version "1.2")

(define (main . args)
  (let ((scale (if (null? args) 1 (or (string->number (car args)) 1))))
    (setup scale)))

(module+ main
  (apply main (vector->list (current-command-line-arguments))))
