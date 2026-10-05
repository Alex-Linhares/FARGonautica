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

;; Release 1.0 (December 2003)
;; Version 1.1: Updated for Petite Chez Scheme 8.4 (January 2014)
;; Version 1.2: Minor bug fixes: graphics window resizing, menus (August 2016, January 2017)

;; EDIT THESE SETTINGS APPROPRIATELY FOR YOUR SYSTEM:

;; Uncomment one of the following lines:
;; (define *platform* 'linux)
;; (define *platform* 'macintosh)
;; (define *platform* 'windows)

;; Pathname of the directory containing the Metacat source code files
;; Uncomment and edit one of the following lines:
;; (define *metacat-directory* "~/Desktop/Metacat/") ;; Linux or Mac OS X
;; (define *metacat-directory* "C:\\Documents and Settings\\Your Username\\Desktop\\Metacat") ;; Windows

;; Default directory used by the "Save commentary to file" menu option
;; Uncomment and edit one of the following lines:
;; (define *file-dialog-directory* "~/Desktop/") ;; Linux or Mac OS X
;; (define *file-dialog-directory* "C:\\Documents and Settings\\Your Username\\Desktop") ;; Windows

;; Version number of Tcl/Tk installed on your system
(define *tcl/tk-version* 8.5)

;;----------------------------------------------------------------------------

;; required by SWL 0.9x
(import swl:oop)
(import swl:macros)
(import swl:generics)
(import swl:option)
(import swl:threads)

;; do some basic error checking first

(when (or (not (top-level-bound? '*platform*))
	  (not (memq *platform* '(linux windows macintosh))))
  (printf "Error: *platform* is not set properly!~%")
  (printf "Check the configuration settings in the file metacat.ss~%")
  (thread-kill))

(when (or (not (top-level-bound? '*metacat-directory*))
	  (not (file-exists? *metacat-directory*)))
  (printf "Error: *metacat-directory* is not set properly!~%")
  (printf "Check the configuration settings in the file metacat.ss~%")
  (thread-kill))

(when (or (not (top-level-bound? '*file-dialog-directory*))
	  (not (file-exists? *file-dialog-directory*)))
  (printf "Error: *file-dialog-directory* is not set properly!~%")
  (printf "Check the configuration settings in the file metacat.ss~%")
  (thread-kill))

(when (or (not (top-level-bound? '*tcl/tk-version*))
	  (not (number? *tcl/tk-version*))
	  (< *tcl/tk-version* 8.3))
  (printf "Error: Tcl/Tk version is not 8.3 or later!~%")
  (printf "Check the configuration settings in the file metacat.ss~%")
  (thread-kill))

(define *tcl/tk-version-8_3?* (>= *tcl/tk-version* 8.3))

(current-directory *metacat-directory*)

(load "syntactic-sugar.ss")
(load "utilities.ss")
(load "fonts.ss")
(load "constants.ss")
(load "setup.ss")
(load "coderack.ss")
(load "descriptions.ss")
(load "bonds.ss")
(load "groups.ss")
(load "bridges.ss")
(load "breakers.ss")
(load "workspace.ss")
(load "workspace-objects.ss")
(load "workspace-structures.ss")
(load "workspace-strings.ss")
(load "concept-mappings.ss")
(load "workspace-structure-formulas.ss")
(load "run.ss")
(load "formulas.ss")
(load "slipnet.ss")
(load "images.ss")
(load "rules.ss")
(load "answers.ss")
(load "themes.ss")
(load "justify.ss")
(load "trace.ss")
(load "jootsing.ss")
(load "memory.ss")
(load "sgl-interpreter.ss")
(load "general-graphics.ss")
(load "slipnet-graphics.ss")
(load "workspace-graphics.ss")
(load "temperature-graphics.ss")
(load "group-graphics.ss")
(load "bridge-graphics.ss")
(load "rule-graphics.ss")
(load "coderack-graphics.ss")
(load "theme-graphics.ss")
(load "trace-graphics.ss")
(load "memory-graphics.ss")
(load "commentary-graphics.ss")
(load "eeg-graphics.ss")
(load "demos.ss")
(load "gui.ss")

(printf "Metacat loaded.~%Type (setup) at the > prompt in the repl window to begin.~%")
