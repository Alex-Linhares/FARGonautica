(define show-letter
  (lambda (qls)
    (set! *mystery-letter* #f)
    (set! *show* (create-graphics-window 150 200 15 20))
    (mys-draw-grid-back *show*)
    (mys-draw-grid *show*)
    (mys-draw-quanta *show* qls)))
