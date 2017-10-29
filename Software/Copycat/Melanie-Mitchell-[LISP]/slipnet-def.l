;---------------------------------------------
; SLIPNET-DEF: This file contains definitions and methods for nodes
;              in the Slipnet.  
;---------------------------------------------

(in-package 'user)
	
;---------------------------------------------

(defflavor slipnode 
    (activation 
     activation-buffer ; A buffer for storing activation between updates.
     (clamp nil) ; If this is t, then the activation of the node is clamped
                 ; to 100.

     (intrinsic-link-length nil) ; The intrinsic link-length of this links
                                 ; labeled by this node.
     (shrunk-link-length nil)  ; For now this is .4 of the intrinsic 
                               ; link-length
     conceptual-depth

     pname  ; A string giving the name of this node.
     symbol-name ; A symbol giving the name of this node.
     short-name ; A string to use for slipnet graphics.
     cm-name ; A string to use for concept-mapping graphics.

     (category-links nil)  
     (instance-links nil)
     (has-property-links nil)
     (lateral-slip-links nil)
     (lateral-nonslip-links nil)
     (incoming-links nil)

     (codelets nil)  ; A list of codelets attached to this node.

     (description-tester nil) ; A function for testing if this node
                              ; can be used as a descriptor for some object.

     (iterate-group nil) ; For nodes representing groups, a function used to 
                         ; iterate the group (e.g., if succgrp is given "a", it
                         ; will return "b").

     graphics-obj ; The graphics object representing this node.
    )
					   
  ()
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables)

;---------------------------------------------

(defmethod (slipnode :print) ()
  (format t "I am the node ~a~&" pname)
  (format t "My activation is ~a~&" activation)
  (format t "My conceptual-depth is ~a~&" conceptual-depth)
  (format t "My incoming links are: ")
  (send-method-to-list incoming-links :print)
  (format t "~%")
  (format t "My outgoing links are: ")
  (send-method-to-list (send self :outgoing-links) :print)
  (format t "~%"))

;---------------------------------------------

(defmethod (slipnode :outgoing-links) ()
; Returns a list of the links emanating from this node.
  (append category-links instance-links has-property-links lateral-slip-links 
	  lateral-nonslip-links))

;---------------------------------------------

(defmethod (slipnode :active?) ()
  (= activation 100))

;---------------------------------------------

(defmethod (slipnode :directed?) ()
; Returns t if the slipnode represents a directed bond or group.
  (or (eq self plato-predecessor) (eq self plato-successor) 
      (eq self plato-predgrp) (eq self plato-succgrp)))

;---------------------------------------------

(defmethod (slipnode :category) ()
; Returns the category that this node belongs to (e.g., "leftmost"
; belongs to "string-position-category").  For now this assumes that
; each node belongs to at most one cateogry.  
; For the time being this doesn't affect anything, but it eventually should be
; fixed.
  (if* category-links then (send (car category-links) :to-node) else nil))

;---------------------------------------------

(defflavor slipnet-link 
    (from-node to-node (label nil) (fixed-length nil))
    ; If a link has no label, then it is assigned a fixed length.
    ()
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables)

;---------------------------------------------

(defmethod (slipnet-link :print) ()
  (format t "From ~a to ~a~&" (send from-node :pname) 
	                      (send to-node :pname)))

;---------------------------------------------

(defmethod (slipnet-link :intrinsic-degree-of-association) ()
  (if* fixed-length
   then (fake-reciprocal fixed-length)
   else (send label :intrinsic-degree-of-association)))

;---------------------------------------------

(defmethod (slipnode :intrinsic-degree-of-association) ()
  (fake-reciprocal intrinsic-link-length))

;---------------------------------------------

(defmethod (slipnet-link :degree-of-association) ()
  (if* fixed-length
   then (fake-reciprocal fixed-length)
   else (send label :degree-of-association)))

;---------------------------------------------

(defmethod (slipnode :degree-of-association) ()
; Returns the degree-of-association encoded in the links this node labels.
  (if* (send self :active?) 
   then (fake-reciprocal shrunk-link-length)
   else (fake-reciprocal intrinsic-link-length)))
   
;---------------------------------------------

(defmethod (slipnode :similar-has-property-links) ()
  (loop for link in has-property-links 
	when (eq (flip-coin (get-temperature-adjusted-probability
				(/ (send link :degree-of-association) 100))) 
		 'heads)
	collect link))

;---------------------------------------------


(defun init-slipnet ()
; This function initializes all the nodes in the slipnet.  The links are 
; defined in the file "sliplinks.l".

  ; LETTERS
   
  (setq plato-a 
	(make-instance 'slipnode  
          :conceptual-depth 10
          :pname "A"
          :symbol-name 'a
          :short-name '("A")
	  :cm-name "A"
          :incoming-links '(b-a-link letter-category-a-link)
	  :category-links '(a-letter-category-link)
          :has-property-links '(a-first-link)
	  :lateral-nonslip-links '(a-b-link)))

  (setq plato-b 
	(make-instance 'slipnode 
	  :conceptual-depth 10
    	  :pname "B"
          :symbol-name 'b
          :short-name '("B")
	  :cm-name "B"
          :incoming-links '(a-b-link c-b-link letter-category-b-link)
	  :category-links '(b-letter-category-link)
          :lateral-nonslip-links '(b-a-link b-c-link)))

  (setq plato-c 
	(make-instance 'slipnode 
          :conceptual-depth 10
          :pname "C"
          :symbol-name 'c
          :short-name '("C")
	  :cm-name "C"
          :incoming-links '(b-c-link d-c-link letter-category-c-link)
	  :category-links '(c-letter-category-link)
          :lateral-nonslip-links '(c-b-link c-d-link)))

  (setq plato-d 
	(make-instance 'slipnode 
          :conceptual-depth 10
    	  :pname "D"
          :symbol-name 'd
          :short-name '("D")
	  :cm-name "D"
          :incoming-links '(c-d-link e-d-link letter-category-d-link)
	  :category-links '(d-letter-category-link)
	  :lateral-nonslip-links '(d-c-link d-e-link)))

  (setq plato-e 
	(make-instance 'slipnode 
	  :conceptual-depth 10
    	  :pname "E"
          :symbol-name 'e
          :short-name '("E")
	  :cm-name "E"
          :incoming-links '(d-e-link f-e-link letter-category-e-link)
	  :category-links '(e-letter-category-link)
          :lateral-nonslip-links '(e-d-link e-f-link)))
 
  (setq plato-f 
	(make-instance 'slipnode 
	  :conceptual-depth 10
    	  :pname "F"
          :symbol-name 'f
          :short-name '("F")
	  :cm-name "F"
          :incoming-links '(e-f-link g-f-link letter-category-f-link)
	  :category-links '(f-letter-category-link)
	  :lateral-nonslip-links '(f-e-link f-g-link)))

  (setq plato-g 
	(make-instance 'slipnode 
	  :conceptual-depth 10
    	  :pname "G"
          :symbol-name 'g
          :short-name '("G")
	  :cm-name "G"
          :incoming-links '(f-g-link h-g-link letter-category-g-link)
	  :category-links '(g-letter-category-link)
          :lateral-nonslip-links '(g-f-link g-h-link)))

  (setq plato-h 
	(make-instance 'slipnode 
	  :conceptual-depth 10
    	  :pname "H"
          :symbol-name 'h
          :short-name '("H")
	  :cm-name "H"
          :incoming-links '(g-h-link i-h-link letter-category-h-link)
	  :category-links '(h-letter-category-link)
	  :lateral-nonslip-links '(h-g-link h-i-link)))

  (setq plato-i 
	(make-instance 'slipnode 
	  :conceptual-depth 10
    	  :pname "I"
          :symbol-name 'i
          :short-name '("I")
	  :cm-name "I"
          :incoming-links '(h-i-link j-i-link letter-category-i-link)
	  :category-links '(i-letter-category-link)
	  :lateral-nonslip-links '(i-h-link i-j-link)))

  (setq plato-j 
	(make-instance 'slipnode 
	  :conceptual-depth 10
    	  :pname "J"
          :symbol-name 'j
          :short-name '("J")
	  :cm-name "J"
          :incoming-links '(i-j-link k-j-link letter-category-j-link)
	  :category-links '(j-letter-category-link)
	  :lateral-nonslip-links '(j-i-link j-k-link)))

  (setq plato-k 
	(make-instance 'slipnode 
	  :conceptual-depth 10
    	  :pname "K"
          :symbol-name 'k
          :short-name '("K")
	  :cm-name "K"
          :incoming-links '(j-k-link l-k-link letter-category-k-link)
	  :category-links '(k-letter-category-link)
          :lateral-nonslip-links '(k-j-link k-l-link)))

  (setq plato-l 
	(make-instance 'slipnode 
	  :conceptual-depth 10
    	  :pname "L"
          :symbol-name 'l
          :short-name '("L")
	  :cm-name "L"
          :incoming-links '(k-l-link m-l-link letter-category-l-link)
	  :category-links '(l-letter-category-link)
          :lateral-nonslip-links '(l-k-link l-m-link)))
			           
  (setq plato-m 
	(make-instance 'slipnode 
	  :conceptual-depth 10
    	  :pname "M"
          :symbol-name 'm
          :short-name '("M")
	  :cm-name "M"
          :incoming-links '(l-m-link n-m-link letter-category-m-link)
	  :category-links '(m-letter-category-link)
          :lateral-nonslip-links '(m-l-link m-n-link)))

  (setq plato-n 
	(make-instance 'slipnode 
	  :conceptual-depth 10
    	  :pname "N"
          :symbol-name 'n
          :short-name '("N")
	  :cm-name "N"
          :incoming-links '(m-n-link o-n-link letter-category-n-link)
	  :category-links '(n-letter-category-link)
	  :lateral-nonslip-links '(n-m-link n-o-link)))

  (setq plato-o 
	(make-instance 'slipnode 
	  :conceptual-depth 10
    	  :pname "O"
          :symbol-name 'o
          :short-name '("O")
	  :cm-name "O"
          :incoming-links '(n-o-link p-o-link letter-category-o-link)
	  :category-links '(o-letter-category-link)
	  :lateral-nonslip-links '(o-n-link o-p-link)))

  (setq plato-p 
	(make-instance 'slipnode 
	  :conceptual-depth 10
    	  :pname "P"
          :symbol-name 'p
          :short-name '("P")
	  :cm-name "P"
          :incoming-links '(o-p-link q-p-link letter-category-p-link)
	  :category-links '(p-letter-category-link)
	  :lateral-nonslip-links '(p-o-link p-q-link)))

  (setq plato-q 
	(make-instance 'slipnode 
	  :conceptual-depth 10
    	  :pname "Q"
          :symbol-name 'q
          :short-name '("Q")
	  :cm-name "Q"
          :incoming-links '(p-q-link r-q-link letter-category-q-link)
	  :category-links '(q-letter-category-link)
	  :lateral-nonslip-links '(q-p-link q-r-link)))

  (setq plato-r 
	(make-instance 'slipnode 
	  :conceptual-depth 10
    	  :pname "R"
          :symbol-name 'r
          :short-name '("R")
	  :cm-name "R"
          :incoming-links '(q-r-link s-r-link letter-category-r-link)
	  :category-links '(r-letter-category-link)
	  :lateral-nonslip-links '(r-q-link r-s-link)))

  (setq plato-s 
	(make-instance 'slipnode 
	  :conceptual-depth 10
    	  :pname "S"
          :symbol-name 's
          :short-name '("S")
	  :cm-name "S"
          :incoming-links '(r-s-link t-s-link letter-category-s-link)
	  :category-links '(s-letter-category-link)
	  :lateral-nonslip-links '(s-r-link s-t-link)))

  (setq plato-t 
	(make-instance 'slipnode 
	  :conceptual-depth 10
    	  :pname "T"
          :symbol-name 't
          :short-name '("T")
	  :cm-name "T"
          :incoming-links '(s-t-link u-t-link letter-category-t-link)
	  :category-links '(t-letter-category-link)
	  :lateral-nonslip-links '(t-s-link t-u-link)))

  (setq plato-u 
	(make-instance 'slipnode 
	  :conceptual-depth 10
    	  :pname "U"
          :symbol-name 'u
          :short-name '("U")
	  :cm-name "U"
          :incoming-links '(t-u-link v-u-link letter-category-u-link)
	  :category-links '(u-letter-category-link)
	  :lateral-nonslip-links '(u-t-link u-v-link)))

  (setq plato-v 
	(make-instance 'slipnode 
	  :conceptual-depth 10
    	  :pname "V"
          :symbol-name 'v
          :short-name '("V")
	  :cm-name "V"
          :incoming-links '(u-v-link w-v-link letter-category-v-link)
	  :category-links '(v-letter-category-link)
	  :lateral-nonslip-links '(v-u-link v-w-link)))
			        
  (setq plato-w 
	(make-instance 'slipnode 
	  :conceptual-depth 10
    	  :pname "W"
          :symbol-name 'w
          :short-name '("W")
	  :cm-name "W"
          :incoming-links '(v-w-link x-w-link letter-category-w-link)
	  :category-links '(w-letter-category-link)
	  :lateral-nonslip-links '(w-v-link w-x-link)))
			        
  (setq plato-x 
	(make-instance 'slipnode 
	  :conceptual-depth 10
    	  :pname "X"
          :symbol-name 'x
          :short-name '("X")
	  :cm-name "X"
          :incoming-links '(w-x-link y-x-link letter-category-x-link)
	  :category-links '(x-letter-category-link)
	  :lateral-nonslip-links '(x-w-link x-y-link)))

  (setq plato-y 
	(make-instance 'slipnode 
    	  :conceptual-depth 10
    	  :pname "Y"
          :symbol-name 'y
          :short-name '("Y")
	  :cm-name "Y"
          :incoming-links '(x-y-link z-y-link letter-category-y-link)
	  :category-links '(y-letter-category-link)
	  :lateral-nonslip-links '(y-x-link y-z-link)))
		    
  (setq plato-z 
	(make-instance 'slipnode 
	  :conceptual-depth 10
    	  :pname "Z"
          :symbol-name 'z
          :short-name '("Z")
	  :cm-name "Z"
          :incoming-links '(y-z-link letter-category-z-link)
	  :category-links '(z-letter-category-link)
	  :has-property-links '(z-last-link)
	  :lateral-nonslip-links '(z-y-link)))

  (setq *slipnet-letters*
	(list plato-a plato-b plato-c plato-d
              plato-e plato-f plato-g plato-h
              plato-i plato-j plato-k plato-l 
              plato-m plato-n plato-o plato-p
              plato-q plato-r plato-s plato-t
              plato-u plato-v plato-w plato-x 
              plato-y plato-z))


  ; NUMBERS

  (setq plato-one 
	(make-instance 'slipnode  
	  :conceptual-depth 30
          :pname "1"
          :short-name '("1")
	  :cm-name "1"
          :incoming-links '(length-1-link 2-1-link)
	  :category-links '(1-length-link)
	  :lateral-nonslip-links '(1-2-link)
	  :description-tester 
	  '(lambda (object) 
             (and (typep object 'group) 
  	          (= (send  object :length) 1)))))
   
  (setq plato-two 
	(make-instance 'slipnode 
	  :conceptual-depth 30
    	  :pname "2"
          :short-name '("2")
	  :cm-name "2"
          :incoming-links '(length-2-link 1-2-link)
	  :category-links '(2-length-link)
          :lateral-nonslip-links '(2-3-link 2-1-link)
	  :description-tester 
	  '(lambda (object) 
             (and (typep object 'group) 
  	          (= (send object :length) 2)))))
   
  (setq plato-three
	(make-instance 'slipnode 
	  :conceptual-depth 30
    	  :pname "3"
          :short-name '("3")
	  :cm-name "3"
          :incoming-links '(length-3-link 2-3-link 4-3-link)
	  :category-links '(3-length-link)
          :lateral-nonslip-links '(3-4-link 3-2-link)
	  :description-tester 
	  '(lambda (object) 
             (and (typep object 'group) 
  	          (= (send object :length) 3)))))
   
  (setq plato-four
	(make-instance 'slipnode 
	  :conceptual-depth 30
    	  :pname "4"
          :short-name '("4")
	  :cm-name "4"
          :incoming-links '(length-4-link 3-4-link 5-4-link)
	  :category-links '(4-length-link)
          :lateral-nonslip-links '(4-5-link 4-3-link)
	  :description-tester 
	  '(lambda (object) 
             (and (typep object 'group) 
  	          (= (send object :length) 4)))))
   
  (setq plato-five
	(make-instance 'slipnode 
	  :conceptual-depth 30
    	  :pname "5"
          :short-name '("5")
	  :cm-name "5"
          :incoming-links '(length-5-link 4-5-link)
	  :category-links '(5-length-link)
          :lateral-nonslip-links '(5-4-link)
	  :description-tester 
	  '(lambda (object) 
             (and (typep object 'group) 
  	          (= (send object :length) 5)))))
   
  (setq *slipnet-numbers* 
	(list plato-one plato-two plato-three plato-four plato-five))

  ; STRING-POSITIONS

  (setq plato-leftmost 
	(make-instance 'slipnode 
	  :conceptual-depth 40
    	  :pname "lmost"
          :short-name (case %slipnet-display-level%
			    (low '("leftmost"))
			    (medium '("lmost"))
			    (high '("lmost")))
	  :cm-name "lmost"
          :incoming-links '(string-position-category-leftmost-link 
			    rightmost-leftmost-link 
			    left-leftmost-link 
			    right-leftmost-link
			    first-leftmost-link
			    last-leftmost-link)
          :category-links '(leftmost-string-position-category-link)
	  :lateral-slip-links '(leftmost-rightmost-link)
	  :lateral-nonslip-links '(leftmost-left-link leftmost-right-link
			 leftmost-first-link leftmost-last-link)
	  :description-tester 
	  '(lambda (object) 
	     (if* (and (not (send object :spans-whole-string?))
		       (send object :leftmost-in-string?))
	      then t else nil))))
			 
  (setq plato-rightmost 
        (make-instance 'slipnode 
	  :conceptual-depth 40
    	  :pname "rmost"
          :short-name (case %slipnet-display-level%
			    (low '("rightmost"))
			    (medium '("rmost"))
			    (high '("rmost")))
	  :cm-name "rmost"
          :incoming-links '(string-position-category-rightmost-link 
			    leftmost-rightmost-link 
			    right-rightmost-link 
			    left-rightmost-link
			    first-rightmost-link last-rightmost-link)
          :category-links '(rightmost-string-position-category-link)
	  :lateral-slip-links '(rightmost-leftmost-link)
	  :lateral-nonslip-links '(rightmost-right-link rightmost-left-link
			 rightmost-first-link rightmost-last-link)
	  :description-tester 
	  '(lambda (object) 
             (if* (and (not (send object :spans-whole-string?))
		       (send object :rightmost-in-string?))
              then t else nil))))
								    
  (setq plato-middle 
        (make-instance 'slipnode 
	  :conceptual-depth 40
    	  :pname "middle"
          :short-name '("middle")
	  :cm-name "mid"
          :incoming-links '(string-position-category-middle-link)
          :category-links '(middle-string-position-category-link)
	  :description-tester 
	  '(lambda (object) 
 	     (let ((left-neighbor 
		       (send object :ungrouped-left-neighbor))
 	           (right-neighbor 
		       (send object :ungrouped-right-neighbor)))
               (and left-neighbor right-neighbor 
		    (send left-neighbor :leftmost-in-string?)
		    (send right-neighbor :rightmost-in-string?))))))

  (setq plato-single 
        (make-instance 'slipnode 
	  :conceptual-depth 40
    	  :pname "single"
          :short-name '("single")
	  :cm-name "single"
          :incoming-links '(string-position-category-single-link
			    whole-single-link)
          :category-links '(single-string-position-category-link)
	  :lateral-slip-links '(single-whole-link)
	  :description-tester 
	  '(lambda (object) 
		   (and (typep object 'letter)
			(send object :spans-whole-string?)))))

  (setq plato-whole 
        (make-instance 'slipnode 
	  :conceptual-depth 40
    	  :pname "whole"
          :short-name '("whole")
	  :cm-name "whole"
          :incoming-links '(string-position-category-whole-link
			    single-whole-link)
          :category-links '(whole-string-position-category-link)
	  :lateral-slip-links '(whole-single-link)
	  :description-tester 
	  '(lambda (object) 
		   (and (typep object 'group) 
			(send object :spans-whole-string?)))))

  ; ALPHABETIC-POSITION NODES

  (setq plato-first 
	(make-instance 'slipnode  
	  :conceptual-depth 60
          :pname "first"
          :short-name '("first")
	  :cm-name "first"
          :incoming-links '(a-first-link last-first-link 
			    alphabetic-position-category-first-link
			    leftmost-first-link rightmost-first-link)
          :category-links '(first-alphabetic-position-category-link)
	  :lateral-slip-links '(first-last-link)
	  :lateral-nonslip-links '(first-leftmost-link first-rightmost-link)
	  :description-tester
	  '(lambda (object) 
                   (eq (send object :get-descriptor 
			                  plato-letter-category) plato-a))))

  (setq plato-last
	(make-instance 'slipnode  
	  :conceptual-depth 60
          :pname "last"
          :short-name '("last")
	  :cm-name "last"
          :incoming-links '(z-last-link first-last-link 
			    alphabetic-position-category-last-link
			    leftmost-last-link rightmost-last-link)
          :category-links '(last-alphabetic-position-category-link)
	  :lateral-slip-links '(last-first-link)
	  :lateral-nonslip-links '(last-leftmost-link last-rightmost-link)
	  :description-tester
	  '(lambda (object) 
                   (eq (send object :get-descriptor 
			                  plato-letter-category) plato-z))))


  ; DIRECTIONS

  (setq plato-left 
        (make-instance 'slipnode 
	  :conceptual-depth 40
          :pname "left"
          :short-name '("left")
	  :cm-name "left"
          :incoming-links '(direction-category-left-link right-left-link 
			    leftmost-left-link rightmost-left-link)
          :category-links '(left-direction-category-link)
          :lateral-slip-links '(left-right-link)
	  :lateral-nonslip-links '(left-leftmost-link left-rightmost-link)))

  (setq plato-right 
	(make-instance 'slipnode 
   	  :conceptual-depth 40
    	  :pname "right"
          :short-name '("right")
	  :cm-name "right"
          :incoming-links '(direction-category-right-link left-right-link 
			    rightmost-right-link leftmost-right-link)
          :category-links '(right-direction-category-link)
	  :lateral-slip-links '(right-left-link)
	  :lateral-nonslip-links '(right-rightmost-link right-leftmost-link)))
			        
  ; BONDS

  (setq plato-predecessor 
        (make-instance 'slipnode 
	  :conceptual-depth 50
          :intrinsic-link-length 60
	  :pname "predecessor"
          :short-name (case %slipnet-display-level%
			    (low '("predecessor"))
			    (medium '("pred"))
			    (high '("pred")))
	  :cm-name "pred"
          :incoming-links '(bond-category-predecessor-link 
			    successor-predecessor-link 
			    predgrp-predecessor-link)
	  :category-links '(predecessor-bond-category-link)
          :lateral-slip-links '(predecessor-successor-link)
          :lateral-nonslip-links '(predecessor-predgrp-link)))
                            		            
  (setq plato-successor 
        (make-instance 'slipnode 
	  :conceptual-depth 50
          :intrinsic-link-length 60
	  :pname "successor"
          :short-name (case %slipnet-display-level%
			    (low '("successor"))
			    (medium '("succ"))
			    (high '("succ")))
	  :cm-name "succ"
          :incoming-links '(bond-category-successor-link 
			    predecessor-successor-link 
			    succgrp-successor-link)
	  :category-links '(successor-bond-category-link)
          :lateral-slip-links '(successor-predecessor-link)
          :lateral-nonslip-links '(successor-succgrp-link)))
			   
  (setq plato-sameness      
  ; "Sameness" refers to letter-sameness (or length sameness); it is a 
  ; possible bond-category for a group.  "Identity" on the other hand is, 
  ; like "opposite", a type of concept-mapping.
	(make-instance 'slipnode 
	  :conceptual-depth 80
          :intrinsic-link-length 0
    	  :pname "sameness"
          :short-name '("same")
	  :cm-name "same"
	  :incoming-links '(bond-category-sameness-link 
			    samegrp-sameness-link)
	  :category-links '(sameness-bond-category-link)
          :lateral-nonslip-links '(sameness-samegrp-link)))	  

  ; GROUPS

  (setq plato-predgrp
        (make-instance 'slipnode 
	  :conceptual-depth 50
	  :pname "predgrp"
          :short-name (case %slipnet-display-level%
			    (low '("predecessor" "group"))
			    (medium '("pred group"))
			    (high '("predgrp")))
	  :cm-name "predgrp"
          :incoming-links '(group-category-predgrp-link succgrp-predgrp-link 
			    predecessor-predgrp-link)
	  :category-links '(predgrp-group-category-link)
	  :lateral-slip-links '(predgrp-succgrp-link)
	  :lateral-nonslip-links '(predgrp-predecessor-link 
			 predgrp-length-link)
	  :iterate-group
	  '(lambda (letter-category) 
		   (send letter-category :get-related-node 
			 plato-predecessor))))


  (setq plato-succgrp
        (make-instance 'slipnode 
          :conceptual-depth 50
          :pname "succgrp"
          :short-name (case %slipnet-display-level%
			    (low '("successor" "group"))
			    (medium '("succ group"))
			    (high '("succgrp")))
	  :cm-name "succgrp"
          :incoming-links '(group-category-succgrp-link predgrp-succgrp-link 
			    successor-succgrp-link)
	  :category-links '(succgrp-group-category-link)
	  :lateral-slip-links '(succgrp-predgrp-link)
	  :lateral-nonslip-links '(succgrp-successor-link
			 succgrp-length-link)
 	  :iterate-group
	  '(lambda (letter-category) 
		   (send letter-category :get-related-node 
			 plato-successor))))

  (setq plato-samegrp 
        (make-instance 'slipnode 
	  :conceptual-depth 80
	  :pname "samegrp"
          :short-name (case %slipnet-display-level%
			    (low '("sameness" "group"))
			    (medium '("same group"))
			    (high '("samegrp")))
	  :cm-name "samegrp"
	  :incoming-links '(group-category-samegrp-link sameness-samegrp-link)
	  :category-links '(samegrp-group-category-link)
	  :lateral-nonslip-links '(samegrp-sameness-link samegrp-length-link)
  	  :iterate-group
	  '(lambda (letter-category) letter-category)))


  ; OTHER RELATIONS

  (setq plato-identity 
	(make-instance 'slipnode 
	  :conceptual-depth 90
          :intrinsic-link-length 0
    	  :pname "identity"
          :short-name (case %slipnet-display-level%
			    (low '("identity"))
			    (medium '("iden"))
			    (high '("iden")))))

  (setq plato-opposite 
	(make-instance 'slipnode 
	  :conceptual-depth 90
          :intrinsic-link-length 80
          :pname "opposite"
          :short-name (case %slipnet-display-level%
			    (low '("opposite"))
			    (medium '("opp"))
			    (high '("opp")))))

			        
  ; OBJECTS

  (setq plato-letter 
  ; This is the platonic type for the workspace structure
  ; LETTER, not to be confused with the node plato-letter-category, which
  ; is the category of all the nodes representing individual letters.
        (make-instance 'slipnode 
	  :conceptual-depth 20
	  :pname "letter"
          :short-name '("letter")
	  :cm-name "let"
	  :incoming-links '(object-category-letter-link 
			    group-letter-link)
	  :category-links '(letter-object-category-link)
          :lateral-slip-links '(letter-group-link)
	  :description-tester '(lambda (object) (typep object 'letter))))

  (setq plato-group 
        (make-instance 'slipnode 
	  :conceptual-depth 80
	  :pname "group"
	  :symbol-name 'group
          :short-name '("group")
	  :cm-name "group"
          :incoming-links '(object-category-group-link 
			    letter-group-link)
          :category-links '(group-object-category-link)
          :lateral-slip-links '(group-letter-link)
	  :description-tester '(lambda (object) (typep object 'group))))
	  

  ; CATEGORIES

  (setq plato-letter-category
        (make-instance 'slipnode 
	  :conceptual-depth 30
	  :pname "letter-category"
          :short-name (case %slipnet-display-level%
			    (low '("letter" "category"))
			    (medium '("letter cat"))
			    (high '("letter" "cat")))
          :cm-name "letcat"
          :incoming-links 
	  (append (list 'length-letter-category-link 
			'bond-facet-letter-category-link
			'samegrp-letter-category-link)
 	          (loop for l in *slipnet-letters*
		        collect 
		        (append-symbols (send l :symbol-name) 
			                '-letter-category-link)))
          :category-links '(letter-category-bond-facet-link)	      
	  :instance-links 
	      (loop for l in *slipnet-letters*
		    collect 
		    (append-symbols 'letter-category- 
			            (send l :symbol-name) '-link))
          :lateral-slip-links '(letter-category-length-link)))


  (setq plato-string-position-category
        (make-instance 'slipnode 
	  :conceptual-depth 70
	  :pname "string-position-category"
          :short-name (case %slipnet-display-level%
			    (low '("string" "position"))
			    (medium '("string pos"))
			    (high '("string" "positn")))
	  :incoming-links '(leftmost-string-position-category-link 
			    rightmost-string-position-category-link
			    middle-string-position-category-link
                            single-string-position-category-link
			    whole-string-position-category-link)
	  :instance-links '(string-position-category-leftmost-link 
				string-position-category-rightmost-link
				string-position-category-middle-link
                                string-position-category-single-link
				string-position-category-whole-link)))


  (setq plato-alphabetic-position-category
        (make-instance 'slipnode 
	  :conceptual-depth 80
	  :pname "alphabetic-position-category"
          :short-name (case %slipnet-display-level%
			    (low '("alpha" "position"))
			    (medium '("alpha pos"))
			    (high '("alpha" "positn")))
	  :incoming-links '(first-alphabetic-position-category-link 
			    last-alphabetic-position-category-link)
	  :instance-links '(alphabetic-position-category-first-link 
				alphabetic-position-category-last-link)))
	

  (setq plato-direction-category
        (make-instance 'slipnode 
	  :conceptual-depth 70
	  :pname "direction-category"
          :short-name (case %slipnet-display-level%
			    (low '("direction"))
			    (medium '("direction"))
			    (high '("directn")))
	  :incoming-links '(left-direction-category-link 
			    right-direction-category-link)
	  :instance-links '(direction-category-left-link 
				direction-category-right-link)))

  (setq plato-bond-category
	(make-instance 'slipnode 
	  :conceptual-depth 80
    	  :pname "bond-category"
          :short-name (case %slipnet-display-level%
			    (low '("bond" "category"))
			    (medium '("bond cat"))
			    (high '("bond" "cat")))
	  :incoming-links
	  '(successor-bond-category-link 
	    predecessor-bond-category-link 
	    sameness-bond-category-link)
	  :instance-links 
	  '(bond-category-successor-link 
            bond-category-predecessor-link
            bond-category-sameness-link)))


  (setq plato-group-category 
        (make-instance 'slipnode 
	  :conceptual-depth 80
	  :pname "group-category"
          :short-name (case %slipnet-display-level%
			    (low '("group" "category"))
			    (medium '("group cat"))
			    (high '("group" "cat")))
	  :incoming-links '(predgrp-group-category-link
			    succgrp-group-category-link
			    samegrp-group-category-link)
	  :instance-links '(group-category-succgrp-link
				group-category-predgrp-link
				group-category-samegrp-link)))
  
  (setq plato-length
        (make-instance 'slipnode 
	  :conceptual-depth 60
	  :pname "length"
          :short-name (case %slipnet-display-level%
			;    (low '("length"))
			    (low '("length"))
			    (medium '("length"))
			    (high '("length")))
	  :cm-name "length"
	  :incoming-links
              (append (list 'letter-category-length-link 
			    'bond-facet-length-link
			    'predgrp-length-link
			    'succgrp-length-link
			    'samegrp-length-link)
 	            (loop for n in *slipnet-numbers*
		          collect (append-symbols (send n :pname) 
				                  '-length-link)))
          :category-links '(length-bond-facet-link)	      
	  :instance-links 
	      (loop for n in *slipnet-numbers*
		    collect (append-symbols 'length- 
				            (send n :pname) '-link))
          :lateral-slip-links '(length-letter-category-link)))


  (setq plato-object-category   
        (make-instance 'slipnode 
	  :conceptual-depth 90
	  :pname "object-category"
          :short-name (case %slipnet-display-level%
			    (low '("object" "category"))
			    (medium '("object cat"))
			    (high '("object" "cat")))
	  :incoming-links '(letter-object-category-link 
			    group-object-category-link)
	  :instance-links 
	  '(object-category-letter-link object-category-group-link)))
	

  (setq plato-bond-facet
  ; This node is the category for the different kinds of description-types
  ; that can be related by a bond.  Right now there are only two:  
  ; letter-category (for letters and groups) and length (for groups). 
        (make-instance 'slipnode 
	  :conceptual-depth 90
	  :pname "bond-facet"
          :short-name (case %slipnet-display-level%
			    (low '("bond facet"))
			    (medium '("bond facet"))
			    (high '("bond" "facet")))
          :incoming-links '(letter-category-bond-facet-link 
			    length-bond-facet-link)
	  :instance-links '(bond-facet-letter-category-link 
			       	bond-facet-length-link)))


  (setq *slipnet* 
        (list plato-a plato-b  plato-c plato-d
              plato-e plato-f plato-g plato-h
              plato-i plato-j plato-k plato-l 
              plato-m plato-n plato-o plato-p
              plato-q plato-r plato-s plato-t
              plato-u plato-v plato-w plato-x 
	      plato-y plato-z
	      plato-one plato-two plato-three plato-four plato-five
	      plato-leftmost plato-rightmost plato-middle 
	      plato-single plato-whole
	      plato-left plato-right  
	      plato-first plato-last
	      plato-predecessor plato-successor plato-sameness
	      plato-predgrp plato-succgrp plato-samegrp 
	      plato-letter plato-group 
              plato-identity plato-opposite 
	      plato-object-category 
              plato-letter-category 
              plato-length
              plato-alphabetic-position-category
              plato-string-position-category
              plato-direction-category
 	      plato-bond-category  
	      plato-group-category 
	      plato-bond-facet))

  ; Initialize all the links in the slipnet.
  (init-slipnet-links)
            
  ; Now set up some other instance variables for each node.
  (loop for slipnode in *slipnet* do
        (send slipnode :set-activation 0)
        (send slipnode :set-activation-buffer 0)

        ; Set up shrunk link-lengths.
        (if* (send slipnode :intrinsic-link-length) 
         then (send slipnode :set-shrunk-link-length 
		    (round (*  (send slipnode :intrinsic-link-length) .4))))

        ; Now evaluate all the links in the various links variables
        (send slipnode :set-incoming-links 
	               (mapcar 'eval (send slipnode :incoming-links)))
        (send slipnode :set-category-links 
	               (mapcar 'eval (send slipnode :category-links)))
        (send slipnode :set-instance-links 
	               (mapcar 'eval (send slipnode :instance-links)))
        (send slipnode :set-has-property-links 
	               (mapcar 'eval (send slipnode :has-property-links)))
        (send slipnode :set-lateral-slip-links 
	               (mapcar 'eval (send slipnode :lateral-slip-links)))
        (send slipnode :set-lateral-nonslip-links 
		               (mapcar 'eval (send slipnode 
						   :lateral-nonslip-links))))

  ; Now set up the codelets attached to nodes in the slipnet.

  (send plato-left :set-codelets
	(list (make-codelet 'top-down-bond-scout--direction 
		  (list plato-left) nil 'bond)
	      (make-codelet 'top-down-group-scout--direction 
		  (list plato-left) nil 'group)))

  (send plato-right :set-codelets
	(list (make-codelet 'top-down-bond-scout--direction 
		  (list plato-right) nil 'bond)
	      (make-codelet 'top-down-group-scout--direction 
		  (list plato-right) nil 'group)))

  (send plato-predecessor :set-codelets 
	(list (make-codelet 'top-down-bond-scout--category 
		  (list plato-predecessor) nil 'bond)))

  (send plato-successor :set-codelets 
	(list (make-codelet 'top-down-bond-scout--category 
		  (list plato-successor) nil 'bond)))

  (send plato-sameness :set-codelets 
	(list (make-codelet 'top-down-bond-scout--category 
		  (list plato-sameness) nil 'bond)))

  (send plato-predgrp :set-codelets 
	(list (make-codelet 'top-down-group-scout--category 
		  (list plato-predgrp) nil 'group)))

  (send plato-succgrp :set-codelets 
	(list (make-codelet 'top-down-group-scout--category 
		  (list plato-succgrp) nil 'group)))

  (send plato-samegrp :set-codelets 
	(list (make-codelet 'top-down-group-scout--category 
		  (list plato-samegrp) nil 'group)))
   
  (send plato-string-position-category :set-codelets 
	(list (make-codelet 'top-down-description-scout 
		  (list plato-string-position-category) nil 'description)))
				
  (send plato-alphabetic-position-category :set-codelets 
	(list (make-codelet 'top-down-description-scout 
		  (list plato-alphabetic-position-category) nil 'description)))
				
  (send plato-length :set-codelets 
	(list (make-codelet 'top-down-description-scout 
		  (list plato-length) nil 'description)))

  (setq *initially-clamped-slipnodes* 
	(list plato-letter-category plato-string-position-category))
                                
  (setq *slipnet-initialized* t)

)
					  
;---------------------------------------------

(defun get-label-node (from-node to-node)
; Returns the node representing the label of the link from FROM-NODE to 
; TO-NODE.  Returns nil if the link has no label. For now, I am assuming 
; that there is only one link from the FROM-NODE to the TO-NODE.
  (if* (eq from-node to-node)
   then plato-identity
   else (loop for link in (send from-node :outgoing-links)
              when (eq (send link :to-node) to-node)
              return (send link :label))))

;---------------------------------------------

(defmethod (slipnode :get-related-node) (relation)
; Returns the node related to the given node by the given relation
; (e.g., if given "left" and "opposite", returns "right").
  (if* (eq relation plato-identity)
   then self
   else (loop for link in (send self :outgoing-links)
	      when (eq (send link :label) relation)
              return (send link :to-node))))

;---------------------------------------------

(defmethod (slipnode :apply-slippages) (slippage-list)
; Returns the node that is the translation of the given node
; according to the given slippage list.
  (loop for s in slippage-list 
	when (eq (send s :descriptor1) self)
	return (send s :descriptor2)
	finally (return self)))

;---------------------------------------------

(defmethod (slipnode :get-possible-descriptors) (object &aux instance)
; Returns a list of the instances of the given node that could be used
; as descriptors for the given object.
  (loop for link in instance-links do    
	(setq instance (send link :to-node))
	when (funcall (send instance :description-tester) object)
	collect instance))

;---------------------------------------------

(defun clear-slipnet ()
; Sets activation to 0 for each node in the slipnet.
  (loop for node in *slipnet* do
	(send node :set-activation-buffer 0)
	(send node :set-activation 0)))

;---------------------------------------------

