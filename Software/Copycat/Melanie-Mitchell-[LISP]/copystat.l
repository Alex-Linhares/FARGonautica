;
;  COPYSTAT:  This program collects and stores statistical information 
;             from runs of the Copycat program.
;---------------------------------------------

(in-package 'user)

(proclaim '(special *run-number* *data-file* *verbose-data-file* 
	            *random-state-file*))
;---------------------------------------------

(defun copystat (initial-string modified-string target-string n 
		 &optional (directory "~/")
		 &aux universal-time hour minute day month year file-string
		      random-state-list old-num-of-runs)
		    
  (format t "~%In Copystat.~&")
  (setq file-string 
	(string-append initial-string "-" modified-string "-" target-string))
  (setq *data-file* 
	(string-append directory file-string ".data"))
  (setq *verbose-data-file* 
	(string-append directory file-string ".verbose-data"))
  (setq *random-state-file* 
	(string-append directory file-string ".random-state"))

  (if* (null (probe-file *verbose-data-file*))
   then (with-open-file (ostream1 *data-file* :direction :output 
	 	                  :if-does-not-exist :create) 
	    (with-open-file (ostream2 *verbose-data-file* :direction :output 
		   	     :if-exists :append  :if-does-not-exist :create)
		(format ostream2 "PROBLEM: If ~a => ~a then ~a => ?~%~%" 
			initial-string modified-string target-string)
  	        (format ostream1 "(") ; Start list.
  	        (format ostream2 "------------------------------------~&"))))

  ; Figure out how many runs are stored in the files.
  
  (if* (probe-file *random-state-file*)
   then (with-open-file (istream *random-state-file* :direction :input)
	    (setq random-state-list (read istream))))
  (setq old-num-of-runs (length random-state-list))


  (loop for i from 1 to n do
        (format t "Starting run ~a.~&" i)
        (with-open-file (ostream1 *data-file* :direction :output 
	  	         :if-exists :append :if-does-not-exist :create)
	    (with-open-file (ostream2 *verbose-data-file* :direction :output 
		  	    :if-exists :append  :if-does-not-exist :create)
                (setq universal-time 
		      (multiple-value-list 
			  (decode-universal-time  (get-universal-time))))
                (setq hour (fix-to-string (nth 2 universal-time)))
                (setq minute (fix-to-string (nth 1 universal-time)))
                (if* (= (length minute) 1) 
                 then (setq minute (string-append "0" minute)))
                (setq day (fix-to-string (nth 3 universal-time)))
                (setq month (fix-to-string (nth 4 universal-time)))
                (setq year (subseq (fix-to-string (nth 5 universal-time)) 2))
                (format ostream2 "Date: ~a/~a/~a ~%Time: ~a:~a~&" 
	                          month day year hour minute)

		(setq *run-number* (1- (+ i old-num-of-runs)))
                (init-ccat initial-string modified-string target-string 
		    :no-graphics t)
                (format ostream1 "(~a ~a ~a ~a ~a ~a ~a ~a)~%" 
			(send *answer-string* :pstring) *temperature* 
			*codelet-count* *snag-count* 
			*single-letter-group-count* 
			*single-letter-group-at-end-count*
			*length-description-count*
			*length-relevant-at-end*)
                (format ostream2 "~a. Answer: ~a~&" *run-number* 
			(send *answer-string* :pstring))
                (format ostream2 "   Temperature: ~a~&" *temperature*)
	        (format ostream2 "   Codelets run: ~a~&" *codelet-count*) 
	        (format ostream2 "   Snags hit: ~a~&" *snag-count*) 
	        (format ostream2 "   Single-letter-groups: ~a~&" 
			*single-letter-group-count*)
	        (format ostream2 "   Single-letter-groups at end: ~a~&" 
			*single-letter-group-at-end-count*)
		(format ostream2 "   Length descriptions made: ~a~&" 
			*length-description-count*)
		(format ostream2 "   Length relevant at end?: ~a~&"
			*length-relevant-at-end*)
                (format ostream2 "------------------------------------~&")))

        ; Write out the random-state.
        (if* (probe-file *random-state-file*)
         then (with-open-file (istream *random-state-file* :direction :input)
              (setq random-state-list 
		    (endcons *random-state-this-run* (read istream))))
         else (setq random-state-list (list *random-state-this-run*)))

        (with-open-file (ostream *random-state-file* :direction :output 
                                 :if-does-not-exist :create)
		  (print random-state-list ostream))))

;---------------------------------------------

(defflavor answer-summary
    (answer-string (frequency 0) (temperature-sum 0) (temperature-sqrs-sum 0)
    (codelet-sum 0) (codelet-sqrs-sum 0)
;     (snag-sum 0) (no-snag-runs 0)
;     (single-letter-group-runs 0) (single-letter-groups-at-end-runs 0)
;     (length-description-runs 0) (length-relevant-at-end-runs 0)
)
    ()
    :gettable-instance-variables
    :settable-instance-variables
    :initable-instance-variables
)    

;---------------------------------------------

(defun copystat-summary
    (input-file &optional (directory "~/")
	        &aux data-file temp-data-file all-answers 
		     current-answer-summary
		     answer-summary-list
		     answer-list hyphen-pos sorted-answer-summary-list
		     initial-string modified-string target-string new-string 
		     summary-file-list summary-file verbose-summary-file
		     (overall-temperature-sum 0)
		     (overall-temperature-sqrs-sum 0)
		     (overall-codelet-sum 0) (overall-codelet-sqrs-sum 0))

(block nil
  (setq data-file (string-append directory input-file ".data"))
  (setq temp-data-file (string-append data-file ".tmp"))

  (if* (null (probe-file data-file))
   then (format t "Error:  ~a:  No such file.~&" data-file)
        (return))

  (run-program "cp" :arguments (list data-file temp-data-file))

  ; End list in temporary data-file.
  (with-open-file 
      (ostream temp-data-file :direction :output :if-exists :append)
      (format ostream ")"))

  (with-open-file (istream temp-data-file :direction :input)
      (setq answer-list (read istream)))
  (delete-file temp-data-file)

  (loop for answer in answer-list do
	(if* (not (member (car answer) all-answers))
         then (setq all-answers (endcons (car answer) all-answers))
	      (setq current-answer-summary 
		    (make-instance 'answer-summary 
			:answer-string (car answer)))
	      (push current-answer-summary answer-summary-list)
	 else (setq current-answer-summary
		    (loop for answer-summary in answer-summary-list
			  when (eq (car answer) 
				   (send answer-summary :answer-string))
			  return answer-summary)))
  	
        (send current-answer-summary :set-frequency
	      (1+ (send current-answer-summary :frequency)))
	(send current-answer-summary :set-temperature-sum
	      (+ (send current-answer-summary :temperature-sum) 
		 (nth 1 answer)))
        (send current-answer-summary :set-temperature-sqrs-sum
              (+ (send current-answer-summary :temperature-sqrs-sum)
                 (sqr (nth 1 answer))))

	(incf overall-temperature-sum (nth 1 answer))
        (incf overall-temperature-sqrs-sum (sqr (nth 1 answer)))

	(send current-answer-summary :set-codelet-sum
	      (+ (send current-answer-summary :codelet-sum) (nth 2 answer)))

        (send current-answer-summary :set-codelet-sqrs-sum
 	      (+ (send current-answer-summary :codelet-sqrs-sum)
		 (sqr (nth 2 answer))))

	(incf overall-codelet-sum (nth 2 answer))
        (incf overall-codelet-sqrs-sum (sqr (nth 2 answer))))
    
  ; Now sort the answer-summary-list by frequency.
  (setq sorted-answer-summary-list 
	(sort answer-summary-list #'> :key '(lambda (x) (send x :frequency))))

  ; Now get initial-string, modified-string, and target-string.
  (setq hyphen-pos (position '#\- input-file))
  (setq initial-string (subseq input-file 0 hyphen-pos))
  (setq new-string (subseq input-file (1+ hyphen-pos)))
  (setq hyphen-pos (position '#\- new-string))
  (setq modified-string (subseq new-string  0 hyphen-pos))
  (setq target-string (subseq new-string (1+ hyphen-pos)))

  (setq summary-file (string-append directory input-file ".summary"))

  (setq verbose-summary-file
    (string-append directory input-file ".verbose-summary"))

  (if* (probe-file summary-file) then (delete-file summary-file))
  (if* (probe-file verbose-summary-file) 
   then (delete-file verbose-summary-file))

  ; Make the summary-file list.
  (setq summary-file-list
        (list (format nil "Problem: ~a --> ~a, ~a --> ?~&" 
		      (string-downcase initial-string)
		      (string-downcase modified-string)
	              (string-downcase target-string))
              (list-sum (send-method-to-list answer-summary-list :frequency))

	      ; overall temp mean
	      (round (/ overall-temperature-sum (length answer-list))) 

              (if* (= (length answer-list) 1) ; overall temp std err
              then 0.0
              else (reduce-decimal
		    (/ (sqrt (/ (abs (- overall-temperature-sqrs-sum
					(/ (sqr overall-temperature-sum)
					   (length answer-list))))
				(1- (length answer-list))))
		       (sqrt (length answer-list))) 1))

              ; overall codelet mean
	      (round (/ overall-codelet-sum (length answer-list))) 

              (if* (= (length answer-list) 1) ; overall codelet std-err
              then 0.0
              else (reduce-decimal
		    (/ (sqrt (/ (abs (- overall-codelet-sqrs-sum
					(/ (sqr overall-codelet-sum)
					   (length answer-list))))
				(1- (length answer-list))))
		       (sqrt (length answer-list))) 1))

              ; Stats for individual answers
              (loop for a in sorted-answer-summary-list 
	            collect
		    (list (string-downcase (send a :answer-string))
		          (send a :frequency)

			  ; Temperature mean 
                          (round (/ (send a :temperature-sum)
				    (send a :frequency)))

			  ; Temperature standard error
                          (if* (= (send a :frequency) 1)
                           then 0.0
                           else (reduce-decimal
			 	 (/ (sqrt (/ (abs (- (send a :temperature-sqrs-sum)
						     (/ (sqr (send a :temperature-sum))
						        (send a :frequency))))
					     (1- (send a :frequency))))
				    (sqrt (send a :frequency))) 1))

			  ; Codelet mean 
 	                  (round (/ (send a :codelet-sum)
				    (send a :frequency)))

			  ; Codelet standard error
                          (if* (= (send a :frequency) 1)
                           then 0.0
			   else (reduce-decimal
				 (/ (sqrt (/ (abs (- (send a :codelet-sqrs-sum)
						     (/ (sqr (send a :codelet-sum))
							(send a :frequency))))
					     (1- (send a :frequency))))
				    (sqrt (send a :frequency))) 1))))))
	                       

  ; Make the summary file
  (with-open-file (ostream summary-file :direction :output
		   :if-exists :append :if-does-not-exist :create)
      (print summary-file-list ostream))

  ; Make the verbose summary file.
  (with-open-file (verbose-ostream verbose-summary-file :direction :output
		   :if-exists :append :if-does-not-exist :create)
      (format verbose-ostream "Problem: ~a --> ~a, ~a --> ?~&" 
	      (string-downcase initial-string)
	      (string-downcase modified-string)
	      (string-downcase target-string))
      (format verbose-ostream "Number of runs: ~a~&" 
	      (list-sum (send-method-to-list answer-summary-list :frequency)))

      (format verbose-ostream "Overall temperature average: ~5,2f~&"
	      (round (/ overall-temperature-sum (length answer-list))))

      (format verbose-ostream "Overall temperature average standard error: ~5,2f~&"
              (if* (= (length answer-list) 1)
              then 0.0
              else (reduce-decimal
		    (/ (sqrt (/ (abs (- overall-temperature-sqrs-sum
					(/ (sqr overall-temperature-sum)
					   (length answer-list))))
				(1- (length answer-list))))
		       (sqrt (length answer-list))) 1)))

      (format verbose-ostream "Overall codelet average: ~5,2f~&"
	      (round (/ overall-codelet-sum (length answer-list))))

      (format verbose-ostream "Overall codelet average standard error: ~5,2f~&"
              (if* (= (length answer-list) 1)
              then 0.0
              else (reduce-decimal
		    (/ (sqrt (/ (abs (- overall-codelet-sqrs-sum
					(/ (sqr overall-codelet-sum)
					   (length answer-list))))
				(1- (length answer-list))))
		       (sqrt (length answer-list))) 1)))


      (format verbose-ostream "------------------------------------~&~%")
      (loop for a in sorted-answer-summary-list do

	   (format verbose-ostream "Answer: ~a~&" 
		   (string-downcase (send a :answer-string)))

           (format verbose-ostream "Frequency: ~a~&"
		   (send a :frequency))

	   (format verbose-ostream "Average temperature: ~a~&~%"	   
	           (round (/ (send a :temperature-sum)
 		             (send a :frequency))))
	   (format verbose-ostream "Average temperature standard error: ~a~&~%"
                   (if* (= (send a :frequency) 1)
                    then 0.0
                    else (reduce-decimal
			  (/ (sqrt (/ (abs (- (send a :temperature-sqrs-sum)
					      (/ (sqr (send a :temperature-sum))
						 (send a :frequency))))
				      (1- (send a :frequency))))
			     (sqrt (send a :frequency))) 1)))

	   (format verbose-ostream "Average number of codelets: ~a~&"
	           (round (/ (send a :codelet-sum) (send a :frequency))))

	   (format verbose-ostream "Average number of codelets standard error: ~a~&"
                   (if* (= (send a :frequency) 1)
                    then 0.0
                    else (reduce-decimal
			  (/ (sqrt (/ (abs (- (send a :codelet-sqrs-sum)
					      (/ (sqr (send a :codelet-sum))
						 (send a :frequency))))
				      (1- (send a :frequency))))
			     (sqrt (send a :frequency))) 1)))

	   (format verbose-ostream "------------------------------------~&~%")))))
		   
;---------------------------------------------







