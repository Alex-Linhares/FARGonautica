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

;; The standalone executable's entry point (tests/make-dist.sh builds it with
;; raco exe + raco distribute):
;;
;;   metacat                      the GUI, as racket/main.rkt
;;   metacat SCALE                the GUI with windows scaled by SCALE
;;   metacat abc abd xyz [...]    a headless run, as racket/cli.rkt (same
;;                                arguments, output and exit codes)
;;   metacat --help               this summary
;;
;; Both halves are loaded lazily, so a headless run never instantiates
;; racket/gui (and needs no display).  lazy-require registers the modules
;; with raco exe, which embeds them.

(require racket/lazy-require)

(lazy-require ["main.rkt" (main)]
              ["cli.rkt" (cli-main)])

(provide run)

(define (usage-text)
  (string-append
   "Metacat 1.2 (James B. Marshall), ported to Racket.\n"
   "usage: metacat [SCALE]                 open the GUI\n"
   "       metacat INITIAL MODIFIED TARGET [ANSWER] [--seed N] [--max-codelets K]\n"
   "               [--keep-going] [--trace FILE] [--verbose]   run headless\n"))

(define (run args)
  (cond
    ((and (pair? args) (member (car args) '("--help" "-h")))
     (display (usage-text)))
    ((or (null? args)
         (and (null? (cdr args)) (string->number (car args))))
     (apply main args))
    (else (cli-main args))))

(module+ main
  (void (run (vector->list (current-command-line-arguments)))))
