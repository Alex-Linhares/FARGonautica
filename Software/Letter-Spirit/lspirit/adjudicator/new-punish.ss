(set! *feature-lists*
      '(*heights-list* *widths-list* *weights-list* *left-edge-list*
		       *right-edge-list* *roof-list* *floor-list*
		       *curves-list*))

(define feature-dimension-look
  (lambda (feature lists)
    (cond
     [(null? lists) 'error]
     [(member? feature (eval (car lists))) (car lists)]
     [else (feature-dimension-look feature (cdr lists))])))

(define feature-dimension
  (lambda (feature)
    (feature-dimension-look feature *feature-lists*)))

(define dimension-punish
  (lambda (feature norms)
    (let*
	([the-dimension (eval (feature-dimension feature))]
	 [acceptable (intersect the-dimension norms)]
	 [dimension-order
	  (lambda (item)
	    (order item the-dimension))]
	 [acceptable-orders (map dimension-order acceptable)]
	 [max-order (apply max acceptable-orders)]
	 [min-order (apply min acceptable-orders)]
	 [feature-order (order feature the-dimension)])
      (min
       (abs (- feature-order max-order))
       (abs (- feature-order min-order))))))
