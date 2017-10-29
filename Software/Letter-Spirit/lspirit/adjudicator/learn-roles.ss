; NEED TO ADD
; tip orientations
; neighbor info

(define find-roof
  (lambda (q-ls)
    (apply max (quanta-verts q-ls))))

(define find-floor
  (lambda (q-ls)
    (apply min (quanta-verts q-ls))))

(define touches-left
  (lambda (q-ls)
    (overlap*? q-ls *left-quanta*)))

(define touches-middle
  (lambda (q-ls)
    (overlap*? q-ls *middle-quanta*)))

(define touches-right
  (lambda (q-ls)
    (overlap*? q-ls *right-quanta*)))

; helper to histograph
(define count-remove
  (lambda (it n seen unseen)
    (cond
     ((null? unseen) (list (list it n) seen))
     ((equal? it (car unseen)) (count-remove it (+ n 1) seen (cdr unseen)))
     (else (count-remove it n (cons (car unseen) seen) (cdr unseen))))))

; counts occurrences of items in the list
(define histograph
  (lambda (ls)
    (cond
     ((null? ls) '())
     (else
      (let
	  ((breakdown (count-remove (car ls) 1 '() (cdr ls))))
      (cons (car breakdown)
	    (histograph (cadr breakdown))))))))

(define scale-ten-histograph
  (lambda (ls)
    (let*
	((hist (histograph ls))
	 (most (apply max (map cadr hist)))
	 (factor (/ 10.0 most)))
      (rescale hist factor))))

(define scale-40-histograph
  (lambda (ls)
    (let*
	((hist (histograph ls))
	 (most (apply max (map cadr hist)))
	 (factor (/ 40.0 most)))
      (rescale hist factor))))


(define these-tips
  (lambda (q-ls)
    (map point-coords (quanta-get-tips q-ls))))

(define tip-q-1
  (lambda (q-ls)
    (tail (reverse q-ls))))

(define tip-q-2
    (lambda (q-ls)
          (tail q-ls)))

(define rescale
  (lambda (ls factor)
    (cond
     ((null? ls) '())
     (else (cons
	    (list (caar ls) (round (* factor (cadar ls))))
	    (rescale (cdr ls) factor))))))

(define locate-tips
  (lambda (q-ls)
    (map point-coords (quanta-get-tips q-ls))))

; orientation at (heading outward) the initial endpoint
(define compass-1
  (lambda (q-ls)
    (tail (quanta-to-compass (reverse q-ls)))))

; orientation at (heading outward) the final endpoint
(define compass-2
  (lambda (q-ls)
    (tail (quanta-to-compass q-ls))))

(define truth-length
  (lambda (ls)
    (cond
     ((null? ls) 0)
     ((car ls) (+ 1 (truth-length (cdr ls))))
     (else (truth-length (cdr ls))))))

(define truth-ratio
  (lambda (ls)
    (round-3 (* 1.0 (/ (truth-length ls) (length ls))))))

(define property-info
  (lambda (name info)
    (list
     name
     (list
      (apply min info)
      (round-3 (* 1.0 (average info)))
      (apply max info)))))

(define if-tips
  (lambda (tips x)
    (if (eq? (length tips) 2) x '())))

(define info
  (lambda (ls)
    (role-info (cons (car ls) ls))))

(define role-info
  (lambda (ls)
    (if
	(member? 0 (map length (map quanta-get-tips ls)))
	(closed-role-info ls)
    (let*
	([ends (map quanta-get-tips ls)]
	 [end-1s (map car ends)]
	 [end-2s (map cadr ends)]
	 [compass-1s (map compass-1 ls)]
	 [compass-2s (map compass-2 ls)]
	 [crv-list (map curve-label (map part-curve ls))]
	 [crv1-list
	  (map curve-label (map part-curve
				      (map first-half ls)))]
	 [crv2-list
	  (map curve-label (map part-curve
				      (map second-half ls)))]
	 [ht-list (map height-label (map height ls))]
	 [wd-list (map width-label (map width ls))]
	 [wt-list (map weight-label (map length ls))]
	 [rf-list (map roof-label (map find-roof ls))]
	 [fl-list (map floor-label (map find-floor ls))]
	 [lf-list (map touches-left ls)]
	 [md-list (map touches-middle ls)]
	 [rt-list (map touches-right ls)])
      (list
       (list
	'tips
	(list
	 (cons
	  'location (scale-ten-histograph end-1s))
	 (cons
	  'orientation (scale-ten-histograph compass-1s)))
	(list
	 (cons
	  'location (scale-ten-histograph end-2s))
	 (cons
	  'orientation (scale-ten-histograph compass-2s))))
       (list
	'curve (scale-ten-histograph crv-list))
       (list
	'curve1 (scale-ten-histograph crv1-list))
       (list
	'curve2 (scale-ten-histograph crv2-list))
       (append
	'(norms)
	(scale-ten-histograph ht-list)
	(scale-ten-histograph wd-list)
	(scale-ten-histograph wt-list)
	(scale-ten-histograph rf-list)
	(scale-ten-histograph fl-list)
	(list (list
	 'left (round (+ 0.49 (* 10 (truth-ratio lf-list))))))
	(list (list
	 'middle (round (+ 0.49 (* 10 (truth-ratio md-list))))))
	(list (list
	'right (round (+ 0.49 (* 10 (truth-ratio rt-list))))))))))))

(define closed-role-info
  (lambda (ls)
    (let*
	([ht-list (map height-label (map height ls))]
	 [wd-list (map width-label (map width ls))]
	 [wt-list (map weight-label (map length ls))]
	 [rf-list (map roof-label (map find-roof ls))]
	 [fl-list (map floor-label (map find-floor ls))]
	 [lf-list (map touches-left ls)]
	 [md-list (map touches-middle ls)]
	 [rt-list (map touches-right ls)])
      (append
	(scale-ten-histograph ht-list)
	(scale-ten-histograph wd-list)
	(scale-ten-histograph wt-list)
	(scale-ten-histograph rf-list)
	(scale-ten-histograph fl-list)
	(list (list
	 'left (round (+ 0.49 (* 10 (truth-ratio lf-list))))))
	(list (list
	 'middle (round (+ 0.49 (* 10 (truth-ratio md-list))))))
	(list (list
	'right (round (+ 0.49 (* 10 (truth-ratio rt-list))))))))))

; abandoning left-middle-right for left-edge and right-edge

(define three-touches
  (lambda (q-ls)
    (list
     (touches-left q-ls)
     (touches-middle q-ls)
     (touches-right q-ls))))

(define three-touch-label-left
  (lambda (ls)
    (left-edge-label (car ls) (cadr ls) (caddr ls))))

(define three-touch-label-right
  (lambda (ls)
    (right-edge-label (car ls) (cadr ls) (caddr ls))))

(define new-horiz-info
    (lambda (ls)
      (let*
	  ([touch-list (map three-touches ls)]
	   [rt-list (map three-touch-label-right touch-list)]
	   [lf-list (map three-touch-label-left touch-list)])
	(append
	 (scale-ten-histograph lf-list)
	 (scale-ten-histograph rt-list)))))

(define role-info
  (lambda (name)
    (let
	((ls (eval name)))
      (if
	  (member? 0 (map length (map quanta-get-tips ls)))
	  name
	  (let*
	      ([tip-1s (map tip-q-1 ls)]
	       [tip-2s (map tip-q-2 ls)]
	       [compass-1s (map compass-1 ls)]
	       [compass-2s (map compass-2 ls)]
	       [bundle-1s (map list tip-1s compass-1s)]
	       [bundle-2s (map list tip-2s compass-2s)]
	       [crv-list (map curve-label (map part-curve ls))]
	       [crv1-list
		(map curve-label (map part-curve
					    (map first-half ls)))]
	       [crv2-list
		(map curve-label (map part-curve
					    (map second-half ls)))])
	    (cons
	     name
	     (list
	      'curve (scale-ten-histograph crv-list))
	     (list
	      'curve1 (scale-ten-histograph crv1-list))
	     (list
	      'curve2 (scale-ten-histograph crv2-list))))))))


(set! all-that
      (list
(role-info 'a-archs)
(role-info 'backslashs)
(role-info 'basebars)
(role-info 'caps)
(role-info 'center-posts)
(role-info 'circles)
(role-info 'crossbars)
(role-info 'dots)
(role-info 'down-arms)
(role-info 'down-circles)
(role-info 'e-bowls)
(role-info 'e-crossbars)
(role-info 'e-tails)
(role-info 'f-posts)
(role-info 'foreslashs)
(role-info 'halfposts)
(role-info 'left-bowls)
(role-info 'left-downbowls)
(role-info 'left-halfarcs)
(role-info 'left-halfarchs)
(role-info 'left-halfposts)
(role-info 'left-posts)
(role-info 'left-tails)
(role-info 'left-uparcs)
(role-info 'left-upbowls)
(role-info 'left-wings)
(role-info 'right-bowls)
(role-info 'right-buttresss)
(role-info 'right-curls)
(role-info 'right-downbowls)
(role-info 'right-halfarcs)
(role-info 'right-halfarchs)
(role-info 'right-halfposts)
(role-info 'right-hooks)
(role-info 'right-posts)
(role-info 'right-tails)
(role-info 'right-wings)
(role-info 's-bases)
(role-info 's-crossbars)
(role-info 't-posts)
(role-info 'up-arms)
(role-info 'up-circles)
(role-info 'z-caps)))
