;;; graphics-battery.scm -- differential checks of the engine's part of the
;;; graphics files (item 13): general-graphics.ss's pexp builders and text
;;; helpers, group-graphics.ss, bridge-graphics.ss and rule-graphics.ss.
;;;
;;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Read form by form, after tests/diff/helpers.scm, by two runners:
;;;   chez_scheme/oracle/diff-eval.ss (the original loaded through prelude.ss)
;;;   racket/tests/graphics-diff-test.rkt (racket/engine.rkt)
;;; These procedures only build SGL expressions and message *workspace-window*,
;;; so each test checks the pexps (every coordinate, to the last bit of a
;;; flonum) and the window messages, against a fake window that answers the
;;; geometry queries with fixed values.  Fonts and colours are symbols here.

(define b:pt (lambda (x y) (make-rectangular x y)))

;; a fake Workspace window: logs each message and its arguments
(define b:window
  (lambda msg
    (let ((m (cadr msg)) (args (cddr msg)))
      (log! (cons m args))
      (case m
        ((get-string-width) (* 1/100 (string-length (car args))))
        ((get-character-width) (* 1/80 (string-length (car args))))
        ((get-character-height) 1/30)
        ((get-width-per-pixel) 1/800)
        ((get-rule-coord) (list 3/4 (- 1/3 (* 1/2 (cadr args)))))
        ((get-spanning-vertical-bridge-right-x) 3/10)
        (else 'done)))))

(b:set-global! '*workspace-window* b:window)
(b:set-global! '%bridge-label-font% 'bridge-label-font)
(b:set-global! '%rule-font% 'rule-font)
(b:set-global! '=white= 'white)

;; a fake object answering messages from an alist (symbol . value-or-procedure):
;; a procedure is called with the message's arguments, so objects (slipnodes,
;; fakes) are given as (b:const object)
(define b:const (lambda (v) (lambda args v)))
(define b:fake
  (lambda (type props)
    (lambda msg
      (let ((m (cadr msg)))
        (cond
          ((eq? m 'object-type) type)
          ((assq m props)
           => (lambda (p)
                (if (procedure? (cdr p)) (apply (cdr p) (cddr msg)) (cdr p))))
          (else (log! (list 'unexpected type m)) #f))))))

;;;---------------------------------------------------------------------------
;;; general-graphics.ss: constants and simple shapes

(test pi-constants (list pi pi/180 180/pi %dot-interval% %dash-length% %dash-density%))
(test platform (list *tcl/tk-version-8_3?* (memq *platform* '(linux windows macintosh)) %nice-graphics%))
(test circles (list (circle '(1 2) 3) (disk '(1/2 1/3) 0.25) (pie-slice '(0 0) 2 30 120)))
(test boxes (list (outline-box 0 0 1 2) (solid-box 1/3 1/4 0.5 0.75)))
(test ovaloids
  (list (centered-ovaloid 1/2 1/3 1/5 1/7 1/4)
        (filled-centered-ovaloid 0.5 0.25 0.1 0.3 0.5)))
(test rounded-boxes
  (list (centered-rounded-box 1/2 1/3 1/5 1/7 1/100)
        (filled-centered-rounded-box 1/2 1/3 1/5 1/7 1/100 1/800)
        (centered-rounded-box 0.5 0.4 0.2 0.1 0.01)))
(test octagons
  (list (centered-octagon 1/2 1/3 1/10) (filled-centered-octagon 0.25 0.75 0.3)))

;;;---------------------------------------------------------------------------
;;; Dotted, dashed, zigzag and jagged lines

(test dotted-line-horizontal (dotted-line 1/10 1/3 1/2 1/3 %dot-interval%))
(test dotted-line-vertical (dotted-line 1/10 1/10 1/10 1/2 1/125))
(test dotted-line-diagonal (dotted-line 0 0 1/2 1/3 %dot-interval%))
(test dotted-line-short (dotted-line 0.1 0.2 0.1001 0.2 %dot-interval%))
(test dotted-box (dotted-box 0.2 0.3 0.35 0.4 %dot-interval%))
(test dashed-line-horizontal (dashed-line 1/10 1/3 1/2 1/3 35/100 %dash-length%))
(test dashed-line-diagonal (dashed-line 0.6 0.2 0.3 0.45 %dash-density% %dash-length%))
(test dashed-box (dashed-box 0.2 0.3 0.35 0.4 49/100 1/200))
(test dashed-line-short
  (list (dashed-line 0.1 0.2 0.11 0.2 1/2 1/200) (dashed-line 1/10 1/5 1/10 9/40 35/100 1/200)))
(test dashed-box-dense (dashed-box 1/5 3/10 7/20 2/5 1/4 1/200))
(test zigzag-plus (zigzag-line (b:pt 0 0) (b:pt 1/2 1/3) 1/100 +))
(test zigzag-minus (zigzag-line (b:pt 0.2 0.5) (b:pt 0.25 0.1) 1/100 -))
(test zigzag-points-flonum (zigzag-line-points (b:pt 0.31 0.27) (b:pt 0.31 0.52) 0.01 +))
(test centered-zigzag-plus (centered-zigzag-line (b:pt 0.2 0.5) (b:pt 0.25 0.1) 1/100 +))
(test centered-zigzag-minus (centered-zigzag-line (b:pt 1/4 1/2) (b:pt 1/3 1/5) 1/100 -))
(test jagged (jagged-line 0 0 1/2 1/3 1/50))
(test nice-graphics-off
  (begin
    (b:set-global! '%nice-graphics% #f)
    (let ((r (list (dotted-line 0 0 1/2 1/3 %dot-interval%)
                   (dotted-box 0.2 0.3 0.35 0.4 %dot-interval%)
                   (dashed-line 0.6 0.2 0.3 0.45 %dash-density% %dash-length%)
                   (dashed-box 0.2 0.3 0.35 0.4 49/100 1/200)
                   (dotted-circular-arc 0 0 1/2 0 1/10 %dot-interval%)
                   (dotted-elliptical-arc 0.1 0.3 0.5 0.35 0.06 %dot-interval%))))
      (b:set-global! '%nice-graphics% #t)
      r)))

;;;---------------------------------------------------------------------------
;;; Arcs and arrows

(test circular-arcs
  (list (circular-arc 0 0 1/2 0 1/10) (circular-arc 0.1 0.2 0.4 0.3 0.05)
        (circular-arc 0 0 1 1 0)))
(test dotted-circular-arc (dotted-circular-arc 0.1 0.2 0.4 0.3 0.05 %dot-interval%))
(test dashed-circular-arc
  (list (dashed-circular-arc 0.1 0.2 0.4 0.3 0.05) (dashed-circular-arc 0 0 1/2 0 0)))
(test circular-arc-points (circular-arc-points 0.1 0.2 0.4 0.3 0.05 1/100))
(test elliptical-arcs
  (list (elliptical-arc 0.1 0.3 0.5 0.3 0.06)
        (elliptical-arc 0.1 0.3 0.5 0.35 0.06)
        (elliptical-arc 0.1 0.35 0.5 0.3 0.02)
        (elliptical-arc 1/10 3/10 1/2 3/10 0)
        (elliptical-arc 1/10 3/10 1/2 3/10 3/50)))
(test dotted-elliptical-arcs
  (list (dotted-elliptical-arc 0.1 0.3 0.5 0.3 0.06 %dot-interval%)
        (dotted-elliptical-arc 0.1 0.3 0.5 0.35 0.06 %dot-interval%)
        (dotted-elliptical-arc 0.1 0.3 0.5 0.3 0 %dot-interval%)))
(test dashed-elliptical-arcs
  (list (dashed-elliptical-arc 0.1 0.3 0.5 0.3 0.06)
        (dashed-elliptical-arc 0.1 0.35 0.5 0.3 0.06)
        (dashed-elliptical-arc 0.1 0.3 0.5 0.3 0)))
(test elliptical-arc-points
  (list (elliptical-arc-points 0.1 0.3 0.5 0.3 0.06 1/125)
        (elliptical-arc-points 0.1 0.35 0.5 0.3 0.02 1/125)))
(test arrowheads
  (list (arrowhead 1/2 1/3 0 1/100 60) (arrowhead 0.4 0.6 180 3/500 60)
        (arrowhead 0.25 0.5 45 0.01 30)))
(test double-arrows
  (list (centered-double-arrow 1/2 0.45 0 7/200 1/200 3/200 60)
        (centered-double-headed-double-arrow 1/2 1/3 90 0.05 0.01 0.02 60)))

;;;---------------------------------------------------------------------------
;;; Text helpers

(test text-helpers
  (list (remove-leading-blanks "   ab c") (remove-leading-blanks "")
        (remove-leading-blanks "    ")
        (separate-into-words "the quick  brown fox")
        (separate-into-words "word")
        (find-next-space-position "ab cd" 0) (find-next-space-position "abcd" 1)))
(test break-into-lines
  (with-log
    (lambda ()
      (list (break-into-lines b:window 'font 0.2 "the quick brown fox jumps over the lazy dog")
            (break-into-lines b:window 'font 0.05 "a verylongwordindeed b")
            (break-into-lines b:window 'font 1 "")))))

;;;---------------------------------------------------------------------------
;;; group-graphics.ss

(define b:group
  (lambda (span direction level . letcat)
    (b:fake 'group
      (list (cons 'get-graphics-x1 1/10) (cons 'get-graphics-y1 3/10)
            (cons 'get-graphics-x2 (+ 1/10 (* span 1/20)))
            (cons 'get-graphics-y2 0.4)
            (cons 'get-letter-span span) (cons 'get-direction (b:const direction))
            (cons 'get-proposal-level level)
            (cons 'get-letcat-graphics-pexp (if (null? letcat) #f (car letcat)))))))

(test group-dashed-line-density (map group-dashed-line-density '(1 2 3 4 5 6 8 10 12)))
(test group-pexps
  (map (lambda (args)
         (make-group-pexp (apply b:group args) (caddr args)))
    (list (list 1 plato-right %proposed%) (list 2 plato-left %evaluated%)
          (list 3 plato-right %built%) (list 1 plato-left %built%)
          (list 4 #f %built%) (list 5 #f %evaluated%)
          (list 2 #f %built% '(let-sgl ((font f)) (text (1/5 2/5) "A")))
          (list 2 #f %proposed% '(let-sgl ((font f)) (text (1/5 2/5) "A"))))))
(test group-grope
  (with-log (lambda () (draw-group-grope (b:group 3 plato-right %proposed%)))))

(define b:graphics-group
  (lambda (level drawn? drawn-coincident overlapping highest)
    (b:fake 'group
      (list (cons 'get-graphics-x1 0.1) (cons 'get-graphics-y1 0.3)
            (cons 'get-graphics-x2 0.25) (cons 'get-graphics-y2 0.4)
            (cons 'get-letter-span 3) (cons 'get-direction (b:const plato-right))
            (cons 'get-proposal-level level) (cons 'get-letcat-graphics-pexp #f)
            (cons 'drawn? drawn?) (cons 'get-graphics-pexp '(old-pexp))
            (cons 'get-drawn-coincident-group (b:const drawn-coincident))
            (cons 'get-drawn-overlapping-groups (b:const overlapping))
            (cons 'get-highest-level-coincident-group (b:const highest))
            (cons 'set-graphics-pexp (lambda (p) (log! (list 'set-graphics-pexp p)) 'done))))))

(test group-graphics-ops
  (let ((other (b:graphics-group %evaluated% #t #f '() #f)))
    (map (lambda (op group) (with-log (lambda () (group-graphics op group))))
      '(flash flash flash set-pexp-and-draw set-pexp-and-draw erase erase
        update-level update-level update-level update-level)
      (list (b:graphics-group %proposed% #t #f '() #f)
            (b:graphics-group %evaluated% #f other '() #f)
            (b:graphics-group %proposed% #f other '() #f)
            (b:graphics-group %proposed% #f #f '() #f)
            (b:graphics-group %proposed% #f other '() #f)
            (b:graphics-group %built% #t #f '() #f)
            (b:graphics-group %proposed% #f #f (list other other) other)
            (b:graphics-group %evaluated% #t #f '() #f)
            (b:graphics-group %evaluated% #f #f '() #f)
            (b:graphics-group %built% #f other '() #f)
            (b:graphics-group %proposed% #f other '() #f)))))

;;;---------------------------------------------------------------------------
;;; bridge-graphics.ss

(define b:letter (b:fake 'letter (list (cons 'get-graphics-pexp '(letter-pexp))
                                       (cons 'string-spanning-group? #f)
                                       (cons 'get-bridge-graphics-coord
                                             (lambda (o) (if (eq? o 'horizontal)
                                                           (b:pt 1/10 2/5)
                                                           (b:pt 1/10 0.38)))))))
(define b:letter2 (b:fake 'letter (list (cons 'get-graphics-pexp '(letter2-pexp))
                                        (cons 'string-spanning-group? #f)
                                        (cons 'get-bridge-graphics-coord
                                              (lambda (o) (if (eq? o 'horizontal)
                                                            (b:pt 0.3 0.41)
                                                            (b:pt 0.12 0.1)))))))
(define b:spanning
  (lambda (x y)
    (b:fake 'group (list (cons 'string-spanning-group? #t)
                         (cons 'get-group-spanning-bridge-graphics-coord
                               (lambda (o) (b:pt x y)))))))

(define b:bridge
  (lambda (orientation from to spanning? obj1 obj2 . more)
    (b:fake 'bridge
      (append more
        (list (cons 'get-orientation orientation)
              (cons 'get-from-graphics-coord from) (cons 'get-to-graphics-coord to)
              (cons 'group-spanning-bridge? spanning?)
              (cons 'get-object1 (b:const obj1)) (cons 'get-object2 (b:const obj2))
              (cons 'get-bridge-label-number 2))))))

(test horizontal-bridge-pexps
  (let ((g (b:fake 'group '())))
    (map (lambda (level)
           (list (make-bridge-pexp (b:bridge 'horizontal (b:pt 1/10 2/5) (b:pt 3/10 0.41) #f
                                     b:letter b:letter2) level)
                 (make-bridge-pexp (b:bridge 'horizontal (b:pt 0.1 0.4) (b:pt 0.3 0.4) #f
                                     g b:letter2) level)
                 (make-bridge-pexp (b:bridge 'horizontal (b:pt 0.05 0.45) (b:pt 0.6 0.47) #t
                                     g g) level)
                 (make-bridge-pexp (b:bridge 'horizontal (b:pt 0.05 0.47) (b:pt 0.6 0.45) #t
                                     g g) level)))
      (list %proposed% %evaluated% %built%))))
(test vertical-bridge-pexps
  (map (lambda (level)
         (with-log
           (lambda ()
             (list (make-bridge-pexp (b:bridge 'vertical (b:pt 1/10 0.38) (b:pt 0.12 0.1) #f
                                       b:letter b:letter2) level)
                   (make-bridge-pexp (b:bridge 'vertical (b:pt 0.2 0.38) (b:pt 0.15 0.1) #f
                                       b:letter b:letter2) level)
                   (make-bridge-pexp (b:bridge 'vertical (b:pt 0.25 0.4) (b:pt 0.26 0.12) #t
                                       b:letter b:letter2) level)))))
    (list %proposed% %evaluated% %built%)))
(test bridge-gropes
  (with-log
    (lambda ()
      (list (draw-bridge-grope 'horizontal b:letter b:letter2)
            (draw-bridge-grope 'vertical b:letter b:letter2)
            (draw-bridge-grope 'horizontal (b:spanning 0.05 0.45) (b:spanning 0.6 0.47))
            (draw-bridge-grope 'vertical (b:spanning 0.25 0.4) (b:spanning 0.26 0.12))))))

(define b:graphics-bridge
  (lambda (level drawn? drawn-coincident flipped? highest obj2)
    (b:bridge 'horizontal (b:pt 0.1 0.4) (b:pt 0.3 0.41) #f b:letter obj2
      (cons 'get-proposal-level level) (cons 'drawn? drawn?)
      (cons 'get-graphics-pexp '(old-bridge-pexp))
      (cons 'get-drawn-coincident-bridge (b:const drawn-coincident))
      (cons 'flipped-group1? flipped?) (cons 'flipped-group2? flipped?)
      (cons 'get-original-group1 'original-group1-not-drawable)
      (cons 'get-original-group2 (b:const (b:fake 'group (list (cons 'get-graphics-pexp '(og2))))))
      (cons 'get-highest-level-coincident-bridge (b:const highest))
      (cons 'set-graphics-pexp (lambda (p) (log! (list 'set-graphics-pexp p)) 'done)))))

(test bridge-graphics-ops
  (let ((other (b:graphics-bridge %evaluated% #t #f #f #f b:letter2))
        (g (b:fake 'group (list (cons 'get-graphics-pexp '(g-pexp))))))
    (map (lambda (op bridge) (with-log (lambda () (bridge-graphics op bridge))))
      '(flash flash flash set-pexp-and-draw set-pexp-and-draw erase erase erase
        update-level update-level update-level update-level)
      (list (b:graphics-bridge %proposed% #t #f #f #f b:letter2)
            (b:graphics-bridge %evaluated% #f other #f #f b:letter2)
            (b:graphics-bridge %proposed% #f other #f #f b:letter2)
            (b:graphics-bridge %proposed% #f #f #f #f b:letter2)
            (b:graphics-bridge %proposed% #f other #f #f b:letter2)
            (b:graphics-bridge %built% #t #f #f #f b:letter2)
            (b:graphics-bridge %built% #f #f #f other g)
            (b:graphics-bridge %built% #f #f #f #f b:letter2)
            (b:graphics-bridge %evaluated% #t #f #f #f b:letter2)
            (b:graphics-bridge %evaluated% #f #f #f #f b:letter2)
            (b:graphics-bridge %built% #f other #f #f b:letter2)
            (b:graphics-bridge %proposed% #f other #f #f b:letter2)))))

;;;---------------------------------------------------------------------------
;;; rule-graphics.ss

(define b:rule
  (lambda (type clauses)
    (b:fake 'rule
      (list (cons 'get-english-transcription clauses) (cons 'get-rule-type type)
            (cons 'set-graphics-pexp (lambda (p) (log! (list 'set-graphics-pexp p)) 'done))
            (cons 'set-auxiliary-rule-graphics-info
                  (lambda args (log! (cons 'aux args)) 'done))))))

(test rule-graphics
  (map (lambda (rule) (with-log (lambda () (initialize-rule-graphics rule))))
    (list (b:rule 'top '("Replace letter-category of rightmost letter by successor"))
          (b:rule 'bottom '("Replace letter-category of rightmost group" "by successor"
                            "and swap length"))
          (b:rule 'top '("Don't replace anything")))))
(test new-rule-pexps
  (with-log
    (lambda ()
      (list (make-new-rule-pexp 'top '("Replace C by D"))
            (make-new-rule-pexp 'bottom '("one" "two three"))))))
;; Racket's update-rule-pexps! returns the updated pexp; the original's
;; updates it in place (set-car!) and returns something else
(test update-rule-pexps
  (let* ((p (list 'let-sgl '()
              (list 'rule 'top (list "Replace C by D") '(old))
              (list 'erase 'white (list 'let-sgl '((font f))
                                    (list 'rule 'bottom (list "a" "b") '(old2))))
              (list 'text '(0 0) "unchanged")))
         (r (update-rule-pexps! p)))
    (if (and (pair? r) (eq? (car r) 'let-sgl)) r p)))

(define b:real-workspace *workspace*)
(test bridge-label-numbers
  (let ((numbered (lambda (n) (b:fake 'bridge (list (cons 'get-bridge-label-number n)
                                                    (cons 'get-bridge-type 'vertical))))))
    (let* ((b1 (numbered 1)) (b3 (numbered 3)) (b2 (numbered 2)) (new (numbered #f))
           (results
             (map (lambda (bridges)
                    (b:set-global! '*workspace*
                      (lambda msg (if (eq? (cadr msg) 'get-bridges) bridges 'done)))
                    (new-bridge-label-number new))
               (list '() (list new) (list b1 new) (list b3 b1 new) (list b2 b1 b3 new)
                     (list b3)))))
      (b:set-global! '*workspace* b:real-workspace)
      results)))
