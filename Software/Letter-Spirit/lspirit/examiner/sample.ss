
(set! *workspace*
      '((((22 25) (4 5) (5 22) (8 9) (9 25))
	 (Quanta (4 5 22 25 9 8)))
	(((20 23) (17 20) (14 17))
	 (Quanta (23 20 17 14)))))

(set! *quanta-list* '(14 17 20 23 4 5 22 25 9 8))

(define label-two
  (lambda ()
    (begin
      (label-type (car *workspace*) 'height)
      (label-type (car *workspace*) 'width)
      (label-type (car *workspace*) 'weight)
      (label-type (car *workspace*) 'vertical)
      (label-type (car *workspace*) 'horizontal)
      (label-type (car *workspace*) 'curve)
      (label-type (car *workspace*) 'contact)
      (label-type (car *workspace*) 'neighbors)
      (label-type (car *workspace*) 'ends)
      (label-type (car *workspace*) 'tips)
      (label-type (car *workspace*) 'closure)
      
      (label-type (cadr *workspace*) 'height)
      (label-type (car *workspace*) 'width)
      (label-type (car *workspace*) 'weight)
      (label-type (car *workspace*) 'vertical)
      (label-type (car *workspace*) 'horizontal)
      (label-type (car *workspace*) 'curve)
      (label-type (car *workspace*) 'contact)
      (label-type (car *workspace*) 'neighbors)
      (label-type (car *workspace*) 'ends)
      (label-type (car *workspace*) 'closure)
      (label-type (car *workspace*) 'tips))))

(define label-three
  (lambda ()
    (begin
      (label-type (car *workspace*) 'height)
      (label-type (car *workspace*) 'width)
      (label-type (car *workspace*) 'weight)
      (label-type (car *workspace*) 'vertical)
      (label-type (car *workspace*) 'horizontal)
      (label-type (car *workspace*) 'curve)
      (label-type (car *workspace*) 'contact)
      (label-type (car *workspace*) 'neighbors)
      (label-type (car *workspace*) 'tips)
      (label-type (car *workspace*) 'ends)
      (label-type (car *workspace*) 'closure)
      
      (label-type (cadr *workspace*) 'height)
      (label-type (car *workspace*) 'width)
      (label-type (car *workspace*) 'weight)
      (label-type (car *workspace*) 'vertical)
      (label-type (car *workspace*) 'horizontal)
      (label-type (car *workspace*) 'curve)
      (label-type (car *workspace*) 'contact)
      (label-type (car *workspace*) 'neighbors)
      (label-type (car *workspace*) 'closure)
      (label-type (car *workspace*) 'ends)
      (label-type (car *workspace*) 'tips)

      (label-type (caddr *workspace*) 'height)
      (label-type (car *workspace*) 'width)
      (label-type (car *workspace*) 'weight)
      (label-type (car *workspace*) 'vertical)
      (label-type (car *workspace*) 'horizontal)
      (label-type (car *workspace*) 'curve)
      (label-type (car *workspace*) 'contact)
      (label-type (car *workspace*) 'neighbors)
      (label-type (car *workspace*) 'closure)
      (label-type (car *workspace*) 'ends)
      (label-type (car *workspace*) 'tips))))

; stubby 'b'

(set! *workspace*
      '((((22 25) (4 5) (5 22) (8 9) (9 25))
	 (Quanta (4 5 22 25 9 8)))
	(((20 23) (17 20))
	 (Quanta (23 20 17)))))

(set! *quanta-list* '(17 20 23 4 5 22 25 9 8))

; 'm'

(set! *workspace*
      '((((20 23))
	 (Quanta (20 23)))
	(((36 21) (21 24))
	 (Quanta (36 21 24)))
	(((37 22) (22 25))
	 (Quanta (37 22 25)))))

(set! *quanta-list* '(20 23 36 21 24 37 22 25))

; swastika 'x'

(set! *workspace*
      '((((21 24) (5 21) (8 24))
	 (Quanta (8 24 21 5)))
	(((7 25) (6 7) (6 20))
	 (Quanta (20 6 7 25)))))

(set! *quanta-list* '(8 24 21 5 20 6 7 25))

; double backslash 'a'

(set! *workspace*
      '((((48 51) (51 25))
	 (Quanta (48 51 25)))
	(((6 24) (24 50) (50 6))
	 (Quanta (6 24 50)))))

(set! *quanta-list* '(48 51 25 6 24 50))

; double backslash 'y'

(set! *workspace*
      '((((48 51) (48 20))
	 (Quanta (20 48 51)))
	(((49 25) (25 28) (28 53))
	 (Quanta (49 25 28 53)))))

(set! *quanta-list* '(20 48 51 49 25 28 53))

(set! part1 (car *workspace*))
(set! part2 (cadr *workspace*))
(set! part3 (caddr *workspace*))

(define spark-all
  (lambda (part)
    (let
	((spark-it
	  (lambda (role)
	    (spark-check part role))))
      (mapcar spark-it *roles*))))

(define over-threshold
  (lambda (thresh ls)
    (cond
     ((null? ls) '())
     ((< (cadar ls) thresh) (over-threshold thresh (cdr ls)))
     (else (cons (car ls) (over-threshold thresh (cdr ls)))))))
