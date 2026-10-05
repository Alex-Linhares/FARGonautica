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

;; Images are part of the machinery of rule application.  Each letter or group
;; in a string, as well as the string itself, has an "image" that represents
;; what the object currently looks like.  Normally, an image just looks like
;; the object itself.  However, when a rule is applied to the string, the
;; appearance of these images may change.  For example, if the rule "Increase
;; length of all objects in string" is applied to the string abc, the
;; resulting appearance of the string (i.e., its image) is aabbcc.  The string
;; still contains only three letters, but the images of these letters are now
;; aa, bb, and cc, respectively.  In contrast, the rule "Swap positions of
;; leftmost and rightmost letter" would transform the image of the string into
;; cba.  Images can be reset to their original (unchanged) appearance in order
;; to try out a different rule.


(define make-letter-image
  (lambda (letter-category)
    (make-image letter-category #f #f #f #f '())))


(define make-string-image
  (lambda (string direction)
    (let ((verbatim-images #f)
	  (verbatim? #f))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'string-image)
	    (get-sub-images ()
	      (if verbatim?
		verbatim-images
		(tell-all (tell string 'get-constituent-objects) 'get-image)))
	    (get-ordered-sub-images ()
	      (if (eq? direction plato-right)
		(tell self 'get-sub-images)
		(reverse (tell self 'get-sub-images))))
	    (generate ()
	      (tell-all (tell self 'get-ordered-sub-images) 'generate))
	    (reset ()
	      (set! verbatim? #f)
	      (set! direction plato-right)
	      (for* each image in (tell self 'get-sub-images) do
		(tell image 'reset))
	      'done)
	    (do-walk (walk-method action)
	      (for* each image in (tell self 'get-ordered-sub-images) do
		(tell image walk-method action))
	      'done)
	    (get-length ()
	      (number->platonic-number (length (tell self 'get-sub-images))))
	    (new-start-letter (arg fail)
	      (for* each i in (tell self 'get-sub-images) do
		(tell i 'new-start-letter arg fail))
	      'done)
	    (new-alpha-position-category (arg fail)
	      (for* each i in (tell self 'get-sub-images) do
		(tell i 'new-start-letter arg fail))
	      'done)
	    (new-length (arg fail) (fail))
	    (new-appearance (letter-categories)
	      (set! verbatim-images (map make-letter-image letter-categories))
	      (set! direction plato-right)
	      (set! verbatim? #t)
	      'done)
	    (reverse-direction (fail)
	      (set! direction (inverse direction))
	      'done)
	    (reverse-medium (medium fail)
	      (cond
		((eq? medium plato-letter-category)
		 (let ((letters (tell-all (tell self 'get-sub-images) 'get-letter)))
		   (tell self 'replace-all 'new-start-letter (reverse letters) fail)))
		((eq? medium plato-length)
		 (let ((lengths (tell-all (tell self 'get-sub-images) 'get-length)))
		   (tell self 'replace-all 'new-length (reverse lengths) fail)))))
	    (replace-all (method-name new-args fail)
	      (map (lambda (image arg)
		     (tell image method-name arg fail))
		(tell self 'get-sub-images)
		new-args)
	      'done)
	    (letter (fail) (fail))
	    (group (fail) (fail))
	    (else (delegate msg base-object))))))))


(define make-image
  (lambda (start-letter bond-facet letter-relation length-relation direction sub-images)
    (let ((original-state (list '*state*
			    start-letter bond-facet letter-relation
			    length-relation direction sub-images))
	  (swapped-image #f)
	  (instantiated-object #f))
      (lambda msg
	(let ((self (1st msg)))
	  (record-case (rest msg)
	    (object-type () 'image)
	    (print ()
	      (printf "swapped-image? = ~a~%" (exists? swapped-image))
	      (cond
		((tell self 'letter-image?)
		 (printf "start-letter: ~a~%" (tell start-letter 'get-lowercase-name)))
		(else
		  (printf "start-letter: ~a~%direction: ~a~%~aletters: ~a ~a~%"
		    (tell start-letter 'get-lowercase-name)
		    (tell direction 'get-lowercase-name)
		    (if (eq? bond-facet plato-letter-category) ">" " ")
		    (if (exists? letter-relation)
		      (tell letter-relation 'get-lowercase-name)
		      "none")
		    (map string->symbol
		      (tell-all (tell-all sub-images 'get-letter) 'get-lowercase-name)))
		  (printf "~alengths: ~a ~a~%"
		    (if (eq? bond-facet plato-length) ">" " ")
		    (if (exists? length-relation)
		      (tell length-relation 'get-lowercase-name)
		      "none")
		    (map (lambda (len)
			   (if (exists? len)
			     (platonic-number->number len)
			     '?))
		      (tell-all sub-images 'get-length))))))
	    (get-swapped-image () swapped-image)
	    (update-swapped-image (image) (set! swapped-image image) 'done)
	    (get-instantiated-object () instantiated-object)
	    (get-letter () start-letter)
	    (get-bond-facet () bond-facet)
	    (get-letter-relation () letter-relation)
	    (get-length-relation () length-relation)
	    (get-direction () direction)
	    (get-sub-images () sub-images)
	    (get-length ()
	      (if (tell self 'letter-image?)
		plato-one
		(number->platonic-number (length sub-images))))
	    (letter-image? () (null? sub-images))
	    (get-state ()
	      (list '*state*
		start-letter bond-facet letter-relation
		length-relation direction sub-images))
	    (new-state (state)
	      (record-case state
		(*state* (v1 v2 v3 v4 v5 v6)
		  (set! start-letter v1)
		  (set! bond-facet v2)
		  (set! letter-relation v3)
		  (set! length-relation v4)
		  (set! direction v5)
		  (set! sub-images v6)))
	      'done)
	    (reset ()
	      (tell self 'new-state original-state)
	      (set! swapped-image #f)
	      (set! instantiated-object #f)
	      (tell-all sub-images 'reset)
	      'done)
	    (instantiate-as-letter (string position)
	      (set! instantiated-object
		(make-letter string start-letter position))
	      (tell string 'add-letter instantiated-object position))
	    (instantiate-as-group (string)
	      (let* ((ordered-objects
		       (if (eq? direction plato-left)
			 (tell-all (reverse sub-images) 'get-instantiated-object)
			 (tell-all sub-images 'get-instantiated-object)))
		     (left-object (1st ordered-objects))
		     (right-object (last ordered-objects))
		     (bond-category
		       (if (eq? bond-facet plato-letter-category)
			 (if (eq? letter-relation plato-identity)
			   plato-sameness
			   letter-relation)
			 (if (eq? length-relation plato-identity)
			   plato-sameness
			   length-relation)))
		     (group-category
		       (tell bond-category 'get-related-node plato-group-category))
		     (group-direction
		       (if (eq? group-category plato-samegrp) #f direction)))
		(set! instantiated-object
		  (make-group string group-category bond-facet group-direction
		    left-object right-object ordered-objects '()))
		(tell string 'add-group instantiated-object)
		(for* each object in ordered-objects do
		  (tell object 'update-enclosing-group instantiated-object))
		(if* %workspace-graphics%
		  (tell instantiated-object 'update-proposal-level %built%)
		  (tell instantiated-object 'set-graphics-pexp
		    (make-group-pexp instantiated-object %built%)))))
	    (leaf-walk (action)
	      (cond
		((tell self 'letter-image?) (action self))
		((eq? direction plato-left)
		 (for* each i in (reverse sub-images) do
		   (tell i 'leaf-walk action)))
		(else (for* each i in sub-images do
			(tell i 'leaf-walk action)))))
	    (postorder-interior-walk (action)
	      (cond
		((tell self 'letter-image?) 'done)
		((eq? direction plato-left)
		 (for* each i in (reverse sub-images) do
		   (tell i 'postorder-interior-walk action))
		 (action self))
		(else
		  (for* each i in sub-images do
		    (tell i 'postorder-interior-walk action))
		  (action self))))
	    (generate ()
	      (cond
		((tell self 'letter-image?) start-letter)
		((eq? direction plato-left) (tell-all (reverse sub-images) 'generate))
		(else (tell-all sub-images 'generate))))
	    (copy ()
	      (make-image start-letter bond-facet letter-relation
		length-relation direction (tell-all sub-images 'copy)))
	    (reverse-direction (fail)
	      (set! direction (inverse direction))
	      'done)
	    ;; medium in {plato-letter-category plato-length}:
	    (reverse-medium (medium fail)
	      (cond
		((tell self 'letter-image?) 'done)
		((eq? medium plato-letter-category)
		 (let ((letters (tell-all sub-images 'get-letter)))
		   (tell self 'replace-all 'new-start-letter (reverse letters) fail)
		   (set! letter-relation (inverse letter-relation))
		   (set! start-letter (last letters))
		   'done))
		((eq? medium plato-length)
		 (let ((lengths (tell-all sub-images 'get-length)))
		   (tell self 'replace-all 'new-length (reverse lengths) fail)
		   (set! length-relation (inverse length-relation))
		   'done))))
	    (replace-all (method-name new-args fail)
	      (map (lambda (image arg)
		     (tell image method-name arg fail))
		sub-images new-args)
	      'done)
	    ;; arg in {opp alphabetic-first alphabetic-last}:
	    (new-alpha-position-category (arg fail)
	      (cond
		((or (eq? arg plato-alphabetic-first)
		     (and (eq? arg plato-opposite) (eq? start-letter plato-z)))
		 (tell self 'new-start-letter plato-a fail))
		((or (eq? arg plato-alphabetic-last)
		     (and (eq? arg plato-opposite) (eq? start-letter plato-a)))
		 (tell self 'new-start-letter plato-z fail))
		(else (fail))))
	    ;; arg in {pred succ iden} U {a ... z}:
	    (new-start-letter (arg fail)
	      (cond
		((not (exists? arg)) (fail))
		((tell self 'letter-image?)
		 (if (platonic-relation? arg)
		   (let ((new-letter (tell start-letter 'get-related-node arg)))
		     (if (not (exists? new-letter))
		       (fail)
		       (set! start-letter new-letter)))
		   (set! start-letter arg)))
		((platonic-relation? arg)
		 (tell-all sub-images 'new-start-letter arg fail)
		 (set! start-letter (tell start-letter 'get-related-node arg)))
		((platonic-letter? arg)
		 (let ((new-letters
			 (enumerate-letter arg letter-relation (length sub-images) fail)))
		   (tell self 'replace-all 'new-start-letter new-letters fail)
		   (set! start-letter arg))))
	      'done)
	    ;; arg in {pred succ iden} U {one ... five}:
	    (new-length (arg fail)
	      (cond
		((not (exists? arg)) (fail))
		((or (eq? arg plato-identity)
		     (and (platonic-number? arg) (eq? (tell self 'get-length) arg)))
		 'done)
		((tell self 'letter-image?)
		 (tell self 'letter->singleton-group fail)
		 (tell self 'new-length arg fail))
		((eq? arg plato-predecessor) (tell self 'shorten fail))
		((eq? arg plato-successor)
		 (tell self 'extend letter-relation length-relation fail))
		((platonic-number? arg)
		 (let ((n (platonic-number->number arg))
		       (len (length sub-images)))
		   (cond
		     ((<= n len)
		      (repeat* (- len n) times (tell self 'shorten fail)))
		     ((> n len)
		      (repeat* (- n len) times
			(tell self 'extend letter-relation length-relation fail))))
		   'done))
		(else (fail))))
	    (shorten (fail)
	      (if* (< (length sub-images) 2)
		(fail))
	      (set! sub-images (all-but-last 1 sub-images))
	      (if* (> (length sub-images) 1)
		(if* (not (exists? letter-relation))
		  (set! letter-relation
		    (relationship-between (tell-all sub-images 'get-letter))))
		(if* (not (exists? length-relation))
		  (set! length-relation
		    (relationship-between (tell-all sub-images 'get-length)))))
	      'done)
	    ;; letter-arg in {pred succ iden} U {a ... z}
	    ;; length-arg in {pred succ iden} U {one ... five}:
	    (extend (letter-arg length-arg fail)
	      (cond
		((tell self 'letter-image?)
		 (tell self 'letter->singleton-group fail)
		 (tell self 'extend letter-arg length-arg fail))
		(else
		  (let ((new-image (tell (last sub-images) 'copy)))
		    (cond
		      ((change-length-first? length-arg (tell new-image 'get-length))
		       (tell new-image 'new-length length-arg fail)
		       (tell new-image 'new-start-letter letter-arg fail))
		      (else
			(tell new-image 'new-start-letter letter-arg fail)
			(tell new-image 'new-length length-arg fail)))
		    (set! sub-images (append sub-images (list new-image)))
		    (set! letter-relation
		      (relationship-between (tell-all sub-images 'get-letter)))
		    (set! length-relation
		      (relationship-between (tell-all sub-images 'get-length)))
		    'done))))
	    (letter (fail)
	      (cond
		((tell self 'letter-image?) 'done)
		((eq? bond-facet plato-length) (fail))
		(else (set! bond-facet #f)
		  (set! letter-relation #f)
		  (set! length-relation #f)
		  (set! direction #f)
		  (set! sub-images '())
		  'done)))
	    (group (fail)
	      (cond
		((tell self 'letter-image?) (tell self 'letter->singleton-group fail))
		(else 'done)))
	    (letter->singleton-group (fail)
	      (let ((sub-image (tell self 'copy)))
		(set! bond-facet plato-letter-category)
		(set! letter-relation plato-identity)
		(set! length-relation plato-identity)
		(set! direction plato-right)
		(set! sub-images (list sub-image))
		(tell self 'new-length plato-one fail)))
	    (else (delegate msg base-object))))))))


(define change-length-first?
  (lambda (length-arg current-length)
    (or (eq? length-arg plato-predecessor)
	(and (platonic-number? length-arg)
	     (or (not (exists? current-length))
		 (< (platonic-number->number length-arg)
		    (platonic-number->number current-length)))))))


(define enumerate-letter
  (lambda (start relation n fail)
    (if (and (> n 1) (not (exists? relation)))
	(fail)
	(letrec
	    ((enum (lambda (n start)
		     (if (= n 1)
			 (list start)
			 (let ((next (tell start 'get-related-node relation)))
			   (if (not (exists? next))
			       (fail)
			       (cons start (enum (sub1 n) next))))))))
	  (enum n start)))))

