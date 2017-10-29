(define mut-int
  (lambda (ls-ls)
    (cond
     [(null? ls-ls) '()]
     [(eq? 1 (length ls-ls)) (car ls-ls)]
     [(eq? 2 (length ls-ls)) (intersect (car ls-ls) (cadr ls-ls))]
     [else (intersect (car ls-ls)
		      (mut-in (cdr ls-ls)))])))

; gridletters per font that the Examiner performs very well on
; used to find intersections of high-performance for
; Adjudicator test sets
(set! bl '(a b c d e f g h i j k l n o p q r s t u v w x y z))
(set! br '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
(set! bt '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
(set! bw '(a f h i j k l t u v w x y))
(set! ch '(c d e f h i l m n o q r s t u v x y z))
(set! cl '(a b c d e f g h i j k l m n o p q r t u v x y z))
(set! dbs '(b c d e f g h i j k l n o p q s t v x y))
(set! flr '(b c d e f g h i j k l m p q r s t v y))
(set! fnt '(b d f h m n q r t v w y))
(set! hi4 '(b c d e f g i j k l n p q r s t u v w x y z))
(set! ho '(a b c d e f g h i j k l m n o p q r s t u x y z))
(set! hu4 '(a b c d e f g h i j k l m n p q r s t u v w x y z))
(set! int '(b c d f g i j l m o q r t x y z))
(set! sab '(a b c d e f g h i j k l n o p q r s t v y))
(set! sh '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
(set! slt '(a c d e f g h i j l m n o p q r s t u w x y z))
(set! sls '(a b c d e f g h i j k l m o p q r t v x y z))
(set! sn '(a b c d e f g h i j k l m n o p q r s t u w x y z))
(set! sqc '(b c d f h i j k l m n o p r s t u w x y z))
(set! stsq '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
(set! war '(a b c d e f g h i j l n o p q r s u v w y z))
