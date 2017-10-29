; anticipate
; these functions compute norms for a set of quanta AND the tip-2 they
; expect to end at

; BOTHER TO DO WEIGHT? YOU DO HAVE TO GET TO THAT TIP2
; MAYBE HEIGHT+WEIGHT (or somesuch)?

; pick an extra horizontal quantum to possibly change height
(define height-with-tips
  (lambda (qls tip1 tip2)
    (let*
	([qls-pts (cons tip1
			(cons tip2
			      (apply append (map quantum-get-points qls))))]
	 [pt-heights (map cadr (map point-coords qls-pts))])
      (- (apply max pt-heights)
	 (apply min pt-heights)))))

(define width-with-tips
  (lambda (qls tip1 tip2)
    (let*
	([qls-pts (cons tip1
			(cons tip2
			      (apply append (map quantum-get-points qls))))]
	 [pt-widths (map car (map point-coords qls-pts))])
      (- (apply max pt-widths)
	 (apply min pt-widths)))))

(define roof-with-tips
  (lambda (qls tip1 tip2)
    (let*
	([qls-pts (cons tip1
			(cons tip2
			      (apply append (map quantum-get-points qls))))]
	 [pt-heights (map cadr (map point-coords qls-pts))])
      (apply max pt-heights))))

(define floor-with-tips
  (lambda (qls tip1 tip2)
    (let*
	([qls-pts (cons tip1
			(cons tip2
			      (apply append (map quantum-get-points qls))))]
	 [pt-heights (map cadr (map point-coords qls-pts))])
      (apply min pt-heights))))

(define horiz-touches-with-tips
  (lambda (qls tip1 tip2)
    (list
     (or (overlap? qls *left-quanta*) (< tip1 8) (< tip2 8))
     (or (overlap? qls *middle-quanta*)
	 (between? tip1 8 14) (between? tip2 8 14))
     (or (overlap? qls *right-quanta*) (> tip1 14) (> tip2 14)))))
