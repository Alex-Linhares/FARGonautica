#lang racket/base
;; Item 12: the SGL interpreter's viewport, painting and fonts
;; (racket/gui/sgl.rkt, racket/gui/fonts.rkt, racket/gui/colors.rkt).
;; The interpreter's messages to the viewport are checked against the
;; original by sgl-diff-test.rkt; this file checks what the viewport does
;; with them: the Tk items it records, the tag operations, Tk's dash
;; patterns, text placement, the pixels it paints, and a pixel-for-pixel
;; snapshot of racket/tests/sgl-fixture.rkt's fixture of every SGL form
;; (racket/tests/snapshots/sgl-fixture.png).  To accept a deliberate change of
;; the rendering, run with METACAT_UPDATE_SNAPSHOTS=1, inspect the new PNG and
;; say why in PROGRESS.md.
;;
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
(require rackunit
         racket/class
         racket/draw
         racket/runtime-path
         "../gui/sgl.rkt"
         "../gui/fonts.rkt"
         "../gui/colors.rkt"
         (only-in "../utilities.rkt" tell)
         "sgl-fixture.rkt")

(define-runtime-path snapshot "snapshots/sgl-fixture.png")
(define-runtime-path sgl-path "../gui/sgl.rkt")

;;---------------------------------------------------------------------------
;; no racket/gui: the interpreter renders offscreen without a display

(parameterize ([current-namespace (make-base-empty-namespace)])
  (dynamic-require sgl-path #f)
  (check-false (module-declared? 'racket/gui/base #f))
  (check-true (module-declared? 'racket/draw #f)))

;;---------------------------------------------------------------------------
;; colours

(check-equal? (swl-color-rgb (swl-color "navy blue")) '(0 0 128))
(check-equal? (swl-color-rgb =black=) '(0 0 0))
(check-equal? (swl-color-rgb =orange=) '(255 165 0))
(check-equal? (length *color-names*) 752)

;;---------------------------------------------------------------------------
;; Tk dash patterns

(check-equal? (tk-dash-pattern "" 1) '())
(check-equal? (tk-dash-pattern "- " 1) '(6 6))
(check-equal? (tk-dash-pattern ". " 1) '(2 6))
(check-equal? (tk-dash-pattern "- " 2) '(12 11))
(check-equal? (tk-dash-pattern "-." 1) '(6 4 2 4))
(check-equal? (tk-dash-pattern "_" 3) '(24 12))
(check-equal? (graphics-dash-pattern) "- ")

;;---------------------------------------------------------------------------
;; fonts

(check-equal? (select-face '(nosuchface helvetica) '(helvetica arial) 'sans-serif) 'helvetica)
(check-equal? (select-face '(a b) '() 'serif) 'times)
(check-equal? (select-face '(a b) '() 'sans-serif) 'helvetica)
(check-equal? (select-face '(a b) '() 'fancy) 'times)
(check-true (and (memq serif '(|times new roman| times)) #t))
(check-true (and (memq sans-serif '(helvetica arial)) #t))
(check-true (andmap symbol? (swl:font-families)))

(let ([f (swl-font 'times 12 'bold 'italic)])
  (check-equal? (send f get-style) '(bold italic))
  (check-equal? (send f get-size) 12)
  (check-equal? (send (send f get-font) get-size) 16.0)   ; 12 pt at 96 dpi
  (check-equal? (send (send f get-font) get-weight) 'bold)
  (check-equal? (send (send f get-font) get-style) 'italic))
(check-equal? (send (swl-font 'helvetica 10 '(normal)) get-style) '(normal))
(check-equal? (get-actual-font-values (swl-font 'times -20 '(bold))) '(times 15 bold))
(check-equal? (get-actual-font-size (swl-font 'times 9)) 9)

(let* ([f (make-fixed-font serif -20 '(bold italic))]
       [m (tell f 'get-pixel-size "M")])
  (check-equal? (tell f 'object-type) 'fixed-font)
  (check-true (andmap exact-integer? m))
  (check-true (< 10 (cadr m) 40))
  (check-equal? (caddr m) (round (* 1/5 (cadr m))))
  (check-equal? (tell f 'get-pixel-height) (cadr m))
  (check-equal? (tell f 'get-baseline-offset) (caddr m))
  (check-true (> (tell f 'get-pixel-width "MMMM") (* 3 (car m))))
  (check-equal? (tell f 'get-pixel-width "") 0)
  (check-true (is-a? (tell f 'get-swl-font) swl-font%)))

(let* ([f (make-mfont sans-serif -10 '())]
       [h1 (tell f 'get-pixel-height)])
  (check-equal? (tell f 'object-type) 'mfont)
  (check-equal? (tell f 'resize 30) 'done)
  (check-true (> (tell f 'get-pixel-height) (* 2 h1)))
  (check-equal? (send (tell f 'get-swl-font) get-size) -30))

;; print and show-char-info write the original's text
(let ([out (open-output-string)])
  (parameterize ([current-output-port out])
    (tell (make-fixed-font 'times 12 '(normal)) 'print))
  (check-regexp-match #rx"^Font requested: \\(times 12 \\(normal\\)\\)\nFont assigned:  \\(times 12 normal\\)\nM pixel matrix: [0-9]+ x [0-9]+, offset [0-9]+\n$"
                      (get-output-string out)))

;;---------------------------------------------------------------------------
;; the viewport's items

(define (items vp)
  (for/list ([it (send vp get-items)])
    (list (item-kind it) (item-coords it) (item-tags it))))

;; a 200 x 100 canvas showing x in [0 2], y in [0 1], as make-graphics-window
;; makes them: x->pixel and y->pixel floor, y grows upwards
(define vp (make-test-viewport 200 100 0 0 2 1))
(void (draw! vp '(rectangle (1/2 1/2) (3/2 1))))
(void (draw! vp '(let-sgl ((origin (1/4 0))) (line (0 0) (1 1/2) (1 0) (2 1))) 'lines))
(void (draw! vp '(polyline (0 0) (1/100 1/100) (2/100 0)) 'poly))
(check-equal? (items vp)
              '((rectangle (50 50 150 0) (all))
                (line (25 100 125 50) (lines))
                (line (125 100 225 0) (lines))
                (line (0 100 1 99 2 100) (poly))))
(send vp move 1/2 -1/10 'lines)
(check-equal? (map cadr (items vp)) '((50 50 150 0) (75 110 175 60) (175 110 275 10) (0 100 1 99 2 100)))
(send vp move-pixels -75 -10 'lines)
(check-equal? (cadr (cadr (items vp))) '(0 100 100 50))
(send vp raise 'all)
(check-equal? (map car (items vp)) '(rectangle line line line))
(send vp raise 'lines)
(check-equal? (map caddr (items vp)) '((all) (poly) (lines) (lines)))
(send vp retag 'poly '(a b))
(check-equal? (caddr (cadr (items vp))) '(a b))
(send vp rescale 'b 2 3)
(check-equal? (cadr (cadr (items vp))) '(0 300 2 297 4 300))
(send vp delete 'a)
(check-equal? (length (items vp)) 3)
(send vp delete 'lines)
(check-equal? (items vp) '((rectangle (50 50 150 0) (all))))
;; clear deletes everything and changes the background
(void (draw! vp '(clear "red")))
(check-equal? (items vp) '())
(check-equal? (swl-color-rgb (send vp get-background-color)) '(255 0 0))
;; degenerate shapes make no items, as in the original
(void (draw! vp '(let-sgl () (rectangle (0 0) (0 1)) (filled-rectangle (0 0) (1 0))
                       (arc (0 0) (0 1) 0 90) (filled-arc (0 0) (1 0) 0 360))))
(check-equal? (items vp) '())

;; text: centre and baseline from the font's pixel size, as draw-text says
(let* ([f (make-fixed-font sans-serif -20 '())]
       [vp (make-test-viewport 300 100)]
       [size (tell f 'get-pixel-size "abc")]
       [w (car size)] [h (cadr size)] [b (caddr size)]
       [m (car (tell f 'get-pixel-size "M"))]
       [text-items (lambda (pexp)
                     (send vp delete 'all)
                     (draw! vp `(let-sgl ((font ,f)) ,pexp))
                     (items vp))])
  (check-equal? (text-items '(text (100 50) "abc"))
                `((text (,(+ 100 (/ w 2)) ,(+ 50 b)) (all))))
  (check-equal? (text-items '(let-sgl ((text-justification center)) (text (100 50) "abc")))
                `((text (100 ,(+ 50 b)) (all))))
  (check-equal? (text-items '(let-sgl ((text-justification right)) (text (100 50) "abc")))
                `((text (,(- 100 (/ w 2)) ,(+ 50 b)) (all))))
  (check-equal? (text-items '(let-sgl ((origin (100 50))) (text (text-relative (2 1)) "abc")))
                `((text (,(+ 100 (* 2 m) (/ w 2)) ,(+ 50 b (- (- h b)))) (all))))
  (check-equal? (text-items '(let-sgl ((text-mode image)) (text (100 50) "abc")))
                `((rectangle (101 ,(+ 50 b (- h) 1) ,(+ 100 w -1) ,(+ 50 b -1)) (all))
                  (text (,(+ 100 (/ w 2)) ,(+ 50 b)) (all)))))

;; the default font of init-env is a bare SWL font, which draw-text cannot
;; `tell': unbound text fails, as it would in the original (anomalies)
(check-exn exn:fail? (lambda () (draw! (make-test-viewport 10 10) '(text "x"))))

;; mouse presses: window pixels plus scroll offset, through pixel->x/y
(let ([vp (make-test-viewport 200 100 0 0 2 1)] [got '()])
  (send vp set-mouse-handlers!
        (lambda (w x y) (set! got (cons (list 'left x y) got)))
        (lambda (w x y) (set! got (cons (list 'right x y) got))))
  (send vp mouse-press 50 25 'left)
  (send vp set-scroll-position! 100 0)
  (send vp mouse-press 50 25 'shift-left)
  (send vp mouse-press 0 100 'right)
  (send vp mouse-press 0 100 'middle)
  (check-equal? (reverse got) '((left 1/2 3/4) (right 3/2 3/4) (right 1 0))))

;;---------------------------------------------------------------------------
;; painting

(define (pixel bm x y)
  (define b (make-bytes 4))
  (send bm get-argb-pixels x y 1 1 b)
  (list (bytes-ref b 1) (bytes-ref b 2) (bytes-ref b 3)))

(let ([vp (make-test-viewport 100 100 #:bg (swl-color "ivory"))])
  (draw! vp '(let-sgl ((foreground-color "red")) (filled-rectangle (10 10) (30 30))))
  (draw! vp '(let-sgl ((foreground-color "blue")) (rectangle (50 50) (80 80))))
  (draw! vp '(let-sgl ((line-style dashed)) (line (0 5) (99 5))))
  (send vp draw-hidden-filled-rectangle "green" 60 10 90 40 'h)
  (define bm (render-viewport vp))
  (check-equal? (pixel bm 20 80) '(255 0 0))          ; inside the red square
  (check-equal? (pixel bm 5 5) '(255 255 240))        ; ivory background
  (check-equal? (pixel bm 65 30) '(255 255 240))      ; inside the outline
  (check-equal? (pixel bm 50 35) '(0 0 255))          ; on the outline
  (check-equal? (pixel bm 75 75) '(255 255 240))      ; hidden: not painted
  ;; dashes along y = 5 (pixel row 95): 6 on, 6 off
  (check-equal? (for/list ([x (in-range 0 14)]) (if (equal? (pixel bm x 95) '(0 0 0)) 1 0))
                '(1 1 1 1 1 1 0 0 0 0 0 0 1 1))
  (send vp unhide 'h)
  (check-equal? (pixel (render-viewport vp) 75 75) '(0 255 0)))

;; the viewport tells its observer about every change
(let ([vp (make-test-viewport 10 10)] [n 0])
  (send vp set-changed-callback! (lambda () (set! n (add1 n))))
  (draw! vp '(let-sgl () (line (0 0) (1 1) (2 2) (3 3)) (rectangle (0 0) (5 5))))
  (send vp move-pixels 1 1 'all)
  (send vp delete 'all)
  (check-equal? n 5))

;;---------------------------------------------------------------------------
;; the fixture, pixel for pixel

(define fixture-vp (make-test-viewport fixture-width fixture-height))
(draw-fixture fixture-vp)
(define fixture (render-viewport fixture-vp))

(define (argb bm)
  (define w (send bm get-width))
  (define h (send bm get-height))
  (define b (make-bytes (* 4 w h)))
  (send bm get-argb-pixels 0 0 w h b)
  b)

(when (getenv "METACAT_UPDATE_SNAPSHOTS")
  (send fixture save-file snapshot 'png)
  (printf "wrote ~a\n" snapshot))

(let* ([expected (read-bitmap snapshot)]
       [a (argb fixture)]
       [e (argb expected)])
  (check-equal? (list (send expected get-width) (send expected get-height))
                (list fixture-width fixture-height))
  (define differing
    (for/sum ([i (in-range 0 (bytes-length a) 4)])
      (if (equal? (subbytes a i (+ i 4)) (subbytes e i (+ i 4))) 0 1)))
  (unless (zero? differing)
    (send fixture save-file "/tmp/sgl-fixture-actual.png" 'png))
  (check-equal? differing 0
                "pixels differing from snapshots/sgl-fixture.png (actual: /tmp/sgl-fixture-actual.png)"))
