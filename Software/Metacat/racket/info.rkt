#lang info
;; Metacat 1.2 (James B. Marshall, GPL v2 or later), ported to Racket.
(define collection "metacat")
(define deps '("base" "gui-lib" "rackunit-lib"))
(define pkg-desc "Faithful Racket port of Metacat 1.2 with a racket/gui interface")
(define license 'GPL-2.0-or-later)
;; racket/gui-tests opens windows: tests/run-tests.sh runs it on a virtual
;; display (xvfb-run, without WAYLAND_DISPLAY), never plain `raco test racket/`
(define test-omit-paths '("gui-tests"))
