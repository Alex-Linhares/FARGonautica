; File System routines for Lucid Common Lisp.  Written by Melanie Mitchell, May, 1989.
; Note that this file works in Sun Common Lisp only for Lisp files, not
; for C files.  Someone should modify it to work for C files as well.

(in-package 'user)

(defflavor file-system
    (list-of-files)
    :gettable-instance-variables
    :settable-instance-variables
    :initable-instance-variables)

(defflavor lisp-file
  (l-version sbin-version last-l-load-time last-sbin-load-time)
    :gettable-instance-variables
    :settable-instance-variables
    :initable-instance-variables)


; When calling defsys, the list of files argument should be as follows:
; an entry in the list of files is simply the filename if the file is a Lisp file.  
; For example, a call to DEFSYS might look like:
;
;  (defsys 'ccat-sys '("cytoplasm" 
;		       "correspondences"
;                       "slipnet.def"
;                       "cyto-graphics")) 

;---------------------------------------------

(defun defsys (name list-of-filenames &aux list-of-files)
  (declare (special *current-file-system*))
  (setq list-of-files
	(loop for filename in list-of-filenames 
	      collect (make-instance 'lisp-file 
			  :l-version (string-append filename ".l")
			  :sbin-version (string-append filename ".sbin"))))
	     
  (set name (make-instance 'file-system :list-of-files list-of-files))

  (loop for file in list-of-files do
        (send file :set-last-l-load-time 0)
	(send file :set-last-sbin-load-time 0))

  (setq *current-file-system* (eval name)))

;---------------------------------------------

(defmethod (file-system :reload) ()
; Loads any files that have been modified since the last reload,
; or if this is the first call to reload, loads all the files in
; the file system, loading binaries if they are more recent than
; sources.
  (loop for file in list-of-files do (send file :reload)))

;---------------------------------------------

(defmethod (lisp-file :reload) (&aux file-to-load last-load-time)
  ; First see whether to consider the .l version or the .sbin version.  
  ; If both exist, load most recent.
  (cond ((and (probe-file l-version) (probe-file sbin-version))
	 (if* (< (file-write-date l-version)
		 (file-write-date sbin-version)) ; .sbin version is more recent
	  then (setq file-to-load sbin-version)
	       (setq last-load-time last-sbin-load-time)
	  else (setq file-to-load l-version)
      	       (setq last-load-time last-l-load-time)))
	((probe-file l-version) 
	 (setq file-to-load l-version)
         (setq last-load-time last-l-load-time))
	((probe-file sbin-version) 
	 (setq file-to-load sbin-version)
         (setq last-load-time last-sbin-load-time))
	(t (error "File ~a does not exist." l-version)))
  ; Now determine whether or not to load the file.  Load it if necessary.
  ; Add on 15 seconds to the last load time because the write-time
  ; seems to be about 10 seconds ahead of what it should be.  I don't
  ; know why this is.
  (if* (< (+ last-load-time 15) (file-write-date file-to-load))
   then (if* (eq file-to-load sbin-version) 
	 then (send self :set-last-sbin-load-time (get-universal-time))
	 else (send self :set-last-l-load-time (get-universal-time)))
        (load file-to-load)))

;---------------------------------------------
	  
(defmethod (file-system :update) ()
; Compiles and loads any files whose .l version is more recent than
; the .sbin version.
  (loop for file in list-of-files do 
        (send file :update)))

;---------------------------------------------

(defmethod (lisp-file :update) (&aux compile?)
; If the .l version is more recent than the .sbin version or if the .sbin 
; version doesn't exist, then compile the file and load the compiled version.
; Otherwise, load the .sbin version.
  (cond ((and (probe-file l-version) (probe-file sbin-version))
	 (if* (< (file-write-date sbin-version)
		(file-write-date l-version)) ; .l version is more recent
	  then (setq compile? t)))
	((probe-file l-version) 
	 (setq compile? t))
	(t (error "File ~a does not exist." l-version)))

  (if* compile? 
   then (compile-file l-version)  
        (load sbin-version)
        (send self :set-last-sbin-load-time (get-universal-time))
   else ; Load the .sbin file if it hasn't been loaded since its 
        ; last modification.  Add on 15 seconds to the load time because 
        ; the write-time seems to be about 10 seconds ahead of what it should 
        ; be.  I don't know why this is.
        (if* (< (+ last-sbin-load-time 15) (file-write-date sbin-version))
         then (load sbin-version)
              (send self :set-last-sbin-load-time (get-universal-time)))))

;---------------------------------------------
	  
(defun reload (&optional (file-system *current-file-system*))
  (declare (special *current-file-system*))
  (if* (eq file-system *current-file-system*)
   then (send file-system :reload)
   else (send (eval file-system) :reload)))


(defun update (&optional (file-system *current-file-system*))
  (declare (special *current-file-system*))
  (if* (eq file-system *current-file-system*)
   then (send file-system :update)
   else (send (eval file-system) :update)))

(defun select-file-system (file-system)
  (declare (special *current-file-system*))
  (setq *current-file-system* (eval file-system)))


