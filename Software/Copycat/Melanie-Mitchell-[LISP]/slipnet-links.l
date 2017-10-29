;---------------------------------------------
; This file contains definitions for the Slipnet links.
;---------------------------------------------

(in-package 'user)

(defun init-slipnet-links (&aux dummy-list letter1 letter2 number1 number2)

; SUCCESSOR AND PREDECESSOR LINKS

  (setq dummy-list *slipnet-letters*)
  (loop until (= (length dummy-list) 1) do
	(setq letter1 (car dummy-list) 
              letter2 (cadr dummy-list))      
	(set (append-symbols (send letter1 :symbol-name) 
		             '- (send letter2 :symbol-name) '-link)
   	     (make-instance 'slipnet-link 
		 :from-node letter1 :to-node letter2 
		 :label plato-successor))
	(set (append-symbols (send letter2 :symbol-name) 
		             '- (send letter1 :symbol-name) '-link)
   	     (make-instance 'slipnet-link 
		 :from-node letter2 :to-node letter1 
		 :label plato-predecessor))
	(setq dummy-list (cdr dummy-list)))

  (setq dummy-list *slipnet-numbers*)
  (loop until (= (length dummy-list) 1) do
	(setq number1 (car dummy-list) 
              number2 (cadr dummy-list))      
	(set (append-symbols (send number1 :pname) 
		             '- (send number2 :pname) '-link)
   	     (make-instance 'slipnet-link 
		 :from-node number1 :to-node number2 
		 :label plato-successor))
	(set (append-symbols (send number2 :pname) 
		             '- (send number1 :pname) '-link)
   	     (make-instance 'slipnet-link 
		 :from-node number2 :to-node number1 
		 :label plato-predecessor))
	(setq dummy-list (cdr dummy-list)))

; LETTER-CATEGORY links

  (loop for letter in *slipnet-letters* do
        (set (append-symbols (send letter :symbol-name) 
		             '-letter-category-link)
	      (make-instance 'slipnet-link 
		  :from-node letter :to-node plato-letter-category 
		  :fixed-length 
		  (- (send plato-letter-category :conceptual-depth) 
		     (send letter :conceptual-depth))))

         (set (append-symbols 'letter-category- 
		              (send letter :symbol-name) '-link)
	      (make-instance 'slipnet-link 
		  :from-node plato-letter-category :to-node letter 
		  :fixed-length 97))) ; Length is 97 since 1/26 is about 3%
    
  (setq samegrp-letter-category-link 
	(make-instance 'slipnet-link
	    :from-node plato-samegrp :to-node plato-letter-category
	    :fixed-length 50))

; LENGTH links

  (loop for number in *slipnet-numbers* do
        (set (append-symbols (send number :pname) '-length-link)
	      (make-instance 'slipnet-link 
		  :from-node number :to-node plato-length 
		  :fixed-length 
		  (- (send plato-length :conceptual-depth) 
		     (send number :conceptual-depth))))

        (set (append-symbols 'length- (send number :pname) '-link)
	     (make-instance 'slipnet-link 
		           :from-node plato-length :to-node number
			   :fixed-length 100)))

  (setq predgrp-length-link 
	(make-instance 'slipnet-link
	    :from-node plato-predgrp :to-node plato-length
	    :fixed-length 95))

  (setq succgrp-length-link 
	(make-instance 'slipnet-link
	    :from-node plato-succgrp :to-node plato-length
	    :fixed-length 95))

  (setq samegrp-length-link 
	(make-instance 'slipnet-link
	    :from-node plato-samegrp :to-node plato-length
	    :fixed-length 95))

; OPPOSITE links

  (setq first-last-link 
	(make-instance 'slipnet-link 
	    :from-node plato-first :to-node plato-last 
	    :label plato-opposite))

  (setq last-first-link 
	(make-instance 'slipnet-link 
	    :from-node plato-last :to-node plato-first 
	    :label plato-opposite))

  (setq leftmost-rightmost-link 
	(make-instance 'slipnet-link 
	    :from-node plato-leftmost :to-node plato-rightmost 
	    :label plato-opposite))

  (setq rightmost-leftmost-link 
	(make-instance 'slipnet-link 
	    :from-node plato-rightmost :to-node plato-leftmost 
	    :label plato-opposite))

  (setq left-right-link 
	(make-instance 'slipnet-link 
	    :from-node plato-left :to-node plato-right 
	    :label plato-opposite))

  (setq right-left-link 
	(make-instance 'slipnet-link 
	    :from-node plato-right :to-node plato-left 
	    :label plato-opposite))

  (setq successor-predecessor-link 
	(make-instance 'slipnet-link 
	    :from-node plato-successor :to-node plato-predecessor 
	    :label plato-opposite))

  (setq predecessor-successor-link 
	(make-instance 'slipnet-link 
	    :from-node plato-predecessor :to-node plato-successor 
	    :label plato-opposite))

  (setq predgrp-succgrp-link 
	(make-instance 'slipnet-link 
	    :from-node plato-predgrp :to-node plato-succgrp 
	    :label plato-opposite))

  (setq succgrp-predgrp-link 
	(make-instance 'slipnet-link 
	    :from-node plato-succgrp :to-node plato-predgrp 
	    :label plato-opposite))


; HAS-PROPERTY links

  (setq a-first-link 
	(make-instance 'slipnet-link 
	    :from-node plato-a :to-node plato-first 
	    :fixed-length 75))

  (setq z-last-link 
	(make-instance 'slipnet-link 
	    :from-node plato-z :to-node plato-last 
	    :fixed-length 75))

; OBJECT-CATEGORY links

  (setq letter-object-category-link 
	(make-instance 'slipnet-link 
	    :from-node plato-letter :to-node plato-object-category  
	    :fixed-length (- (send plato-object-category :conceptual-depth) 
			     (send plato-letter :conceptual-depth))))

  (setq object-category-letter-link 
	(make-instance 'slipnet-link 
	    :from-node plato-object-category :to-node plato-letter 
	    :fixed-length 100))

  (setq group-object-category-link 
	(make-instance 'slipnet-link 
	    :from-node plato-group :to-node plato-object-category 
	    :fixed-length (- (send plato-object-category :conceptual-depth) 
			     (send plato-group :conceptual-depth))))

  (setq object-category-group-link 
	(make-instance 'slipnet-link 
	    :from-node plato-object-category :to-node plato-group 
	    :fixed-length 100))


; STRING-POSITION-CATEGORY links

  (setq leftmost-string-position-category-link 
	(make-instance 'slipnet-link 
	    :from-node plato-leftmost :to-node plato-string-position-category 
	    :fixed-length 
	    (- (send plato-string-position-category :conceptual-depth)
	       (send plato-leftmost :conceptual-depth))))

  (setq string-position-category-leftmost-link 
	(make-instance 'slipnet-link 
	    :from-node plato-string-position-category :to-node plato-leftmost 
	    :fixed-length 100))

  (setq rightmost-string-position-category-link 
	(make-instance 'slipnet-link 
	    :from-node plato-rightmost 
	    :to-node plato-string-position-category 
	    :fixed-length 
	    (- (send plato-string-position-category :conceptual-depth)
	       (send plato-rightmost :conceptual-depth))))

  (setq string-position-category-rightmost-link 
	(make-instance 'slipnet-link 
	    :from-node plato-string-position-category 
	    :to-node plato-rightmost 
	    :fixed-length 100))

  (setq middle-string-position-category-link 
	(make-instance 'slipnet-link 
	    :from-node plato-middle :to-node plato-string-position-category
	    :fixed-length 
	    (- (send plato-string-position-category :conceptual-depth)
	       (send plato-middle :conceptual-depth))))

  (setq string-position-category-middle-link 
	(make-instance 'slipnet-link 
	    :from-node plato-string-position-category :to-node plato-middle 
	    :fixed-length 100))

  (setq single-string-position-category-link 
	(make-instance 'slipnet-link 
	    :from-node plato-single :to-node plato-string-position-category
            :fixed-length 
	    (- (send plato-string-position-category :conceptual-depth)
	       (send plato-single :conceptual-depth))))

  (setq string-position-category-single-link 
	(make-instance 'slipnet-link 
	    :from-node plato-string-position-category :to-node plato-single 
	    :fixed-length 100))

  (setq whole-string-position-category-link 
	(make-instance 'slipnet-link 
	    :from-node plato-whole :to-node plato-string-position-category
            :fixed-length 
	    (- (send plato-string-position-category :conceptual-depth)
	       (send plato-whole :conceptual-depth))))

  (setq string-position-category-whole-link 
	(make-instance 'slipnet-link 
	    :from-node plato-string-position-category :to-node plato-whole 
	    :fixed-length 100))

; ALPHABETIC-POSITION-CATEGORY links

  (setq first-alphabetic-position-category-link 
	(make-instance 'slipnet-link 
	    :from-node plato-first 
	    :to-node plato-alphabetic-position-category 
	    :fixed-length (- (send plato-alphabetic-position-category 
				   :conceptual-depth)
			     (send plato-first :conceptual-depth))))

  (setq alphabetic-position-category-first-link 
	(make-instance 'slipnet-link 
	    :from-node plato-alphabetic-position-category 
	    :to-node plato-first :fixed-length 100))

  (setq last-alphabetic-position-category-link 
	(make-instance 'slipnet-link 
	    :from-node plato-last :to-node plato-alphabetic-position-category 
	    :fixed-length (- (send plato-alphabetic-position-category 
				   :conceptual-depth)
			     (send plato-last :conceptual-depth))))

  (setq alphabetic-position-category-last-link 
	(make-instance 'slipnet-link 
	    :from-node plato-alphabetic-position-category 
	    :to-node plato-last :fixed-length 100))


; DIRECTION-CATEGORY links

  (setq left-direction-category-link 
	(make-instance 'slipnet-link 
	    :from-node plato-left :to-node plato-direction-category 
	    :fixed-length (- (send plato-direction-category :conceptual-depth)
			     (send plato-left :conceptual-depth))))

  (setq direction-category-left-link 
	(make-instance 'slipnet-link 
	    :from-node plato-direction-category :to-node plato-left 
	    :fixed-length 100))

  (setq right-direction-category-link 
	(make-instance 'slipnet-link 
	    :from-node plato-right :to-node plato-direction-category 
	    :fixed-length (- (send plato-direction-category :conceptual-depth) 
			     (send plato-right :conceptual-depth))))

  (setq direction-category-right-link 
	(make-instance 'slipnet-link 
	    :from-node plato-direction-category :to-node plato-right 
	    :fixed-length 100))

; BOND-CATEGORY links

  (setq predecessor-bond-category-link 
	(make-instance 'slipnet-link 
	    :from-node plato-predecessor :to-node plato-bond-category 
	    :fixed-length (- (send plato-bond-category :conceptual-depth)
			     (send plato-predecessor :conceptual-depth))))

  (setq bond-category-predecessor-link 
	(make-instance 'slipnet-link 
	    :from-node plato-bond-category :to-node plato-predecessor 
	    :fixed-length 100))

  (setq successor-bond-category-link 
	(make-instance 'slipnet-link 
	    :from-node plato-successor :to-node plato-bond-category 
	    :fixed-length (- (send plato-bond-category :conceptual-depth)
			     (send plato-successor :conceptual-depth))))

  (setq bond-category-successor-link 
	(make-instance 'slipnet-link 
	    :from-node plato-bond-category :to-node plato-successor 
	    :fixed-length 100))

  (setq sameness-bond-category-link 
	(make-instance 'slipnet-link 
	    :from-node plato-sameness :to-node plato-bond-category 
	    :fixed-length (- (send plato-bond-category :conceptual-depth)
			     (send plato-sameness :conceptual-depth))))

  (setq bond-category-sameness-link 
	(make-instance 'slipnet-link 
	    :from-node plato-bond-category :to-node plato-sameness 
	    :fixed-length 100))

; GROUP-CATEGORY links

  (setq predgrp-group-category-link 
	(make-instance 'slipnet-link 
	    :from-node plato-predgrp :to-node plato-group-category 
	    :fixed-length (- (send plato-group-category :conceptual-depth) 
			     (send plato-predgrp :conceptual-depth))))

  (setq group-category-predgrp-link 
	(make-instance 'slipnet-link 
	    :from-node plato-group-category :to-node plato-predgrp 
	    :fixed-length 100))

  (setq succgrp-group-category-link 
	(make-instance 'slipnet-link 
	    :from-node plato-succgrp :to-node plato-group-category 
	    :fixed-length (- (send plato-group-category :conceptual-depth) 
			     (send plato-succgrp :conceptual-depth))))

  (setq group-category-succgrp-link 
	(make-instance 'slipnet-link 
	    :from-node plato-group-category :to-node plato-succgrp 
	    :fixed-length 100))

  (setq samegrp-group-category-link 
	(make-instance 'slipnet-link 
	    :from-node plato-samegrp :to-node plato-group-category 
	    :fixed-length (- (send plato-group-category :conceptual-depth) 
			     (send plato-samegrp :conceptual-depth))))

  (setq group-category-samegrp-link 
	(make-instance 'slipnet-link 
	    :from-node plato-group-category :to-node plato-samegrp 
	    :fixed-length 100))


; ASSOCIATED GROUP links

  (setq sameness-samegrp-link 
	(make-instance 'slipnet-link
	    :from-node plato-sameness :to-node plato-samegrp 
	    :fixed-length 30 
	    :label plato-group-category))

  (setq successor-succgrp-link 
	(make-instance 'slipnet-link
	    :from-node plato-successor :to-node plato-succgrp 
	    :fixed-length 60
	    :label plato-group-category))

  (setq predecessor-predgrp-link 
	(make-instance 'slipnet-link
	    :from-node plato-predecessor :to-node plato-predgrp 
	    :fixed-length 60
	    :label plato-group-category))

; ASSOCIATED BOND-CATEGORY links

  (setq samegrp-sameness-link 
	(make-instance 'slipnet-link
	    :from-node plato-samegrp :to-node plato-sameness 
	    :fixed-length 90
	    :label plato-bond-category))

  (setq succgrp-successor-link 
	(make-instance 'slipnet-link
	    :from-node plato-succgrp :to-node plato-successor 
	    :fixed-length 90
	    :label plato-bond-category))

  (setq predgrp-predecessor-link 
	(make-instance 'slipnet-link
	    :from-node plato-predgrp :to-node plato-predecessor 
	    :fixed-length 90
	    :label plato-bond-category))

; BOND-FACET links

  (setq letter-category-bond-facet-link
	(make-instance 'slipnet-link
	    :from-node plato-letter-category 
	    :to-node plato-bond-facet 
	    :fixed-length 
	    (- (send plato-bond-facet :conceptual-depth) 
	       (send plato-letter-category :conceptual-depth))))

  (setq bond-facet-letter-category-link
	(make-instance 'slipnet-link
	    :from-node plato-bond-facet 
	    :to-node plato-letter-category 
	    :fixed-length 100))

  (setq length-bond-facet-link
	(make-instance 'slipnet-link
	    :from-node plato-length 
	    :to-node plato-bond-facet 
	    :fixed-length 
	    (- (send plato-bond-facet :conceptual-depth) 
	       (send plato-length :conceptual-depth))))

  (setq bond-facet-length-link
	(make-instance 'slipnet-link
	    :from-node plato-bond-facet 
	    :to-node plato-length 
	    :fixed-length 100))

; LETTER-CATEGORY-LENGTH links

  (setq letter-category-length-link
	(make-instance 'slipnet-link
	    :from-node plato-letter-category :to-node plato-length 
	    :fixed-length 95))

  (setq length-letter-category-link
	(make-instance 'slipnet-link
	    :from-node plato-length :to-node plato-letter-category 
            :fixed-length 95))

; LETTER-GROUP links

  (setq letter-group-link 
	(make-instance 'slipnet-link 
	    :from-node plato-letter :to-node plato-group :fixed-length 90))

  (setq group-letter-link 
	(make-instance 'slipnet-link 
	    :from-node plato-group :to-node plato-letter :fixed-length 90))

; DIRECTION-POSITION, DIRECTION-NEIGHBOR, and POSITION-NEIGHBOR links

  (setq left-leftmost-link
	(make-instance 'slipnet-link 
	    :from-node plato-left :to-node plato-leftmost :fixed-length 90))

  (setq leftmost-left-link
	(make-instance 'slipnet-link 
	    :from-node plato-leftmost :to-node plato-left :fixed-length 90))

  (setq right-leftmost-link
	(make-instance 'slipnet-link 
	    :from-node plato-right :to-node plato-leftmost 
	    :fixed-length 100))

  (setq leftmost-right-link
	(make-instance 'slipnet-link 
	    :from-node plato-leftmost :to-node plato-right 
	    :fixed-length 100))

  (setq right-rightmost-link
	(make-instance 'slipnet-link 
	    :from-node plato-right :to-node plato-rightmost 
	    :fixed-length 90))

  (setq rightmost-right-link
	(make-instance 'slipnet-link 
	    :from-node plato-rightmost :to-node plato-right 
	    :fixed-length 90))

  (setq left-rightmost-link
	(make-instance 'slipnet-link 
	    :from-node plato-left :to-node plato-rightmost 
	    :fixed-length 100))

  (setq rightmost-left-link
	(make-instance 'slipnet-link 
	    :from-node plato-rightmost :to-node plato-left 
	    :fixed-length 100))

  (setq leftmost-first-link
	(make-instance 'slipnet-link 
	    :from-node plato-leftmost :to-node plato-first 
	    :fixed-length 100))

  (setq first-leftmost-link
	(make-instance 'slipnet-link 
	    :from-node plato-first :to-node plato-leftmost 
	    :fixed-length 100))

  (setq rightmost-first-link
	(make-instance 'slipnet-link 
	    :from-node plato-rightmost :to-node plato-first 
	    :fixed-length 100))

  (setq first-rightmost-link
	(make-instance 'slipnet-link 
	    :from-node plato-first :to-node plato-rightmost 
	    :fixed-length 100))

  (setq leftmost-last-link
	(make-instance 'slipnet-link 
	    :from-node plato-leftmost :to-node plato-last 
	    :fixed-length 100))

  (setq last-leftmost-link
	(make-instance 'slipnet-link 
	    :from-node plato-last :to-node plato-leftmost 
	    :fixed-length 100))

  (setq rightmost-last-link
	(make-instance 'slipnet-link 
	    :from-node plato-rightmost :to-node plato-last 
	    :fixed-length 100))

  (setq last-rightmost-link
	(make-instance 'slipnet-link 
	    :from-node plato-last :to-node plato-rightmost 
	    :fixed-length 100))

; OTHER LINKS

  (setq single-whole-link
	(make-instance 'slipnet-link 
	    :from-node plato-single :to-node plato-whole :fixed-length 90))

  (setq whole-single-link
	(make-instance 'slipnet-link 
	    :from-node plato-whole :to-node plato-single :fixed-length 90))

)			   			   			
 			   
 