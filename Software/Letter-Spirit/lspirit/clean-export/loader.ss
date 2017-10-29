;;;
;;; $Header: /usr4/u/gem/lspirit/RCS/loader.ss,v 2.2 1995/05/02 20:59:16 gem Exp gem $
;;;
;;;
;;; $Log: loader.ss,v $
;;; Revision 2.2  1995/05/02  20:59:16  gem
;;; Code is now copletely commented.  Added a parameter tweaking front end.
;;;
;;; Revision 2.1  1995/03/13  02:05:10  gem
;;; Success at last.  This version of the code recognizes most letters
;;; of NORMALS very well.  Exceptions are K, W, S and C.  All other
;;; letters seem to work.  Good point for a new tree in RCS.
;;;
;;; Revision 1.6  1994/09/16  19:46:32  gem
;;; Post IRST version 9/16/94
;;;
;;; Revision 1.5  1993/11/16  11:06:57  gem
;;; IRST starting point.
;;;
;;; Revision 1.4  1992/10/14  16:17:12  gem
;;; sxm graphics complete through labeling (pc code not updated)
;;;
;;; Revision 1.3  1992/08/12  22:17:21  gem
;;; new type of labeling.  all but curve implemented.
;;;
;;; Revision 1.2  1992/07/15  22:08:39  gem
;;; scheme4.0a version (not clear it runs on the PC yet)
;;;
;;; Revision 1.1  1991/10/31  19:49:15  gem
;;; Initial revision
;;;
;;;
;===========================================================================
; loader.ss : smart loader code
;===========================================================================
; Smart loader for Chez.  Uses the extensions ss and so
; Lives by itself so that it can be loaded "dumbly" all alone

; check if the compiled file exists.  If so then load it else load source
; This does not check for the latest version of the compiled code.  You
; must make sure that's all up to date yourself!  (Use make)
; Unix smart loader
(define sload
  (lambda (sym)
    (let* ((sfilename (fspec->string sym 'ss))
	   (filename (fspec->string sym 'so))
	   (ans (file-exists? filename)))
      (if ans
	  (load filename)
	  (load sfilename)))))
      
; make a filename string with two symbols <file> and <ext>
(define fspec->string
 (lambda (name-symbol extension-symbol)
  (string-append (symbol->string name-symbol)
                 "."
                 (symbol->string extension-symbol))))

