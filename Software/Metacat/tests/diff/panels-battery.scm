;;; panels-battery.scm -- differential checks of the panel code of item 14
;;; that works without a window: slipnet-, coderack-, temperature-, theme-,
;;; trace-, memory-, commentary- and eeg-graphics.ss.
;;;
;;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Read form by form, after tests/diff/helpers.scm, by two runners:
;;;   chez_scheme/oracle/diff-eval.ss (the original loaded through prelude.ss)
;;;   racket/tests/panels-diff-test.rkt (racket/engine.rkt and
;;;   racket/gui/views.rkt)
;;; The windows themselves need Tk (Chez) or racket/draw (Racket), so they
;;; are checked by racket/tests/views-test.rkt instead.  Here: the pexp
;;; builders (thermometer, mercury, Trace event icons, Memory icons), the
;;; Themespace panel layout procedures and panel objects, the mouse
;;; handlers of the Trace and Memory windows, the layout tables and names,
;;; and the EEG object, against fake windows and model objects that log
;;; every message.  Colours and fonts are SWL stubs under Chez and racket/draw
;;; objects in Racket, so b:clean turns every non-datum into 'obj.

(define b:clean
  (lambda (x)
    (cond
      ((or (number? x) (string? x) (symbol? x) (boolean? x) (null? x)) x)
      ((pair? x) (cons (b:clean (car x)) (b:clean (cdr x))))
      (else 'obj))))

;; with-log (helpers.scm), its result and log cleaned
(define b:with-log (lambda (thunk) (b:clean (with-log thunk))))

;; a fake object answering messages from an alist (symbol . value-or-procedure);
;; a procedure is called with the message's arguments, so objects (slipnodes,
;; fakes) are given as (b:const object); other messages are logged
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
          (else (log! (b:clean (cons type (cdr msg)))) 'done))))))

;; a fake graphics window: logs each message with its arguments, answers the
;; geometry queries with fixed values
(define b:g
  (lambda msg
    (let ((m (cadr msg)) (args (cddr msg)))
      (log! (b:clean (cons m args)))
      (case m
        ((get-string-width) (* 1/8 (string-length (car args))))
        ((get-string-height) 1/2)
        ((get-character-height) 3/50)
        ((get-text-offset) 1/80)
        ((get-width-per-pixel) 1/600)
        ((get-height-per-pixel) 1/500)
        (else 'done)))))

(define b:name (lambda (node) (if node (tell node 'get-short-name) #f)))

;;;---------------------------------------------------------------------------
;;; slipnet-graphics.ss

(test slipnet-layout-table
  (map (lambda (row) (map b:name (vector->list row)))
       (vector->list *13x5-layout-table*)))
(test slipnet-layout-dimensions
  (list (row-dimension *13x5-layout-table*) (column-dimension *13x5-layout-table*)))

;;;---------------------------------------------------------------------------
;;; temperature-graphics.ss

(test mercury-pexp
  (list (b:clean (mercury-pexp 1/2 1/2 1/2 3/4 'red))
        (b:clean (mercury-pexp 9/20 1/2 11/20 0.875 'red))
        (b:clean (mercury-pexp 0.45 0.5 0.55 1.95 =white=))))
(test draw-thermometer
  (b:with-log (lambda () (draw-thermometer b:g '(1/2 3/10) 3/10))))
(test draw-thermometer-off-centre
  (b:with-log (lambda () (draw-thermometer b:g '(0.6 0.25) 0.2))))

;;;---------------------------------------------------------------------------
;;; theme-graphics.ss: names, orders, layout

(test themespace-window-layout *themespace-window-layout*)
(test panel-order (map b:name *panel-order*))
(test panel-theme-order (map b:name *panel-theme-order*))
(test relation-names
  (map relation-name
       (list #f plato-identity plato-opposite plato-successor plato-predecessor plato-a)))
(test dimension-names
  (map (lambda (d) (list (dimension-name d) (abbreviated-dimension-name d)))
       (append *panel-order* (list plato-length plato-letter))))
(test sort-wrt-panel-order
  (map b:name
       (sort-wrt-order
         (remq plato-bond-category (tell *themespace* 'get-dimensions))
         *panel-order*)))
(test themespace-relations
  (map (lambda (type)
         (map (lambda (dim)
                (map b:name (sort-wrt-order (tell *themespace* 'get-relations type dim)
                                            *panel-theme-order*)))
              (remq plato-bond-category (tell *themespace* 'get-dimensions))))
       '(top-bridge bottom-bridge vertical-bridge)))

(define b:relation-sets
  (list (list plato-identity)
        (list plato-identity plato-successor)
        (list plato-identity plato-successor plato-predecessor #f)
        (list plato-identity plato-opposite #f)))

(test horizontal-panel-info
  (map (lambda (relations)
         (map (lambda (entry) (cons (b:name (car entry)) (cdr entry)))
              ((compute-horizontal-panel-info 1/4 7/30 9/100 1/80 1/60 3/50 1/25 1/100)
               relations '(1/2 7/30))))
       b:relation-sets))
(test horizontal-panel-info-flonum
  (map (lambda (relations)
         (map (lambda (entry) (cons (b:name (car entry)) (cdr entry)))
              ((compute-horizontal-panel-info 0.25 0.2333 0.09 0.0125 0.0166 0.06 0.04 0.01)
               relations '(0.0 0.0))))
       b:relation-sets))
(test vertical-panel-info
  (map (lambda (relations)
         (map (lambda (entry) (cons (b:name (car entry)) (cdr entry)))
              ((compute-vertical-panel-info 1/2 59/32 1/5 0 1/60 3/50 1/25 1/100
                 (lambda (r) (* 1/90 (string-length (relation-name r)))))
               relations '(1/2 59/16))))
       b:relation-sets))

;;;---------------------------------------------------------------------------
;;; theme-graphics.ss: a panel on a fake window, with a fake Themespace

(define b:dominant #f)
(define b:cluster-themes '())
(define b:cluster
  (b:fake 'theme-cluster
    (list (cons 'get-dominant-theme (lambda () b:dominant))
          (cons 'get-themes (lambda () b:cluster-themes)))))
(define b:pressure? #f)
(b:set-global! '*themespace*
  (b:fake 'themespace
    (list (cons 'get-cluster (lambda (type dim) b:cluster))
          (cons 'thematic-pressure? (lambda (type) b:pressure?)))))

(define b:make-theme
  (lambda (relation activation)
    (b:fake 'theme
      (list (cons 'get-relation (b:const relation))
            (cons 'get-activation (b:const activation))
            (cons 'get-normal-pexp (b:const (list 'normal (relation-name relation))))
            (cons 'get-highlight-pexp (b:const (list 'highlight (relation-name relation))))))))
(define b:iden (b:make-theme plato-identity 80))
(define b:succ (b:make-theme plato-successor -40))
(define b:diff (b:make-theme #f 100))

(define b:panel-info
  ((compute-horizontal-panel-info 1/4 7/30 9/100 1/80 1/60 3/50 1/25 1/100)
   (list plato-identity plato-successor #f) '(1/4 0)))

(define b:panel
  (make-panel b:g plato-letter-category 'top-bridge b:panel-info "Letter Category"
    '(3/8 1/40) '(1/4 0) '(1/2 7/30) '(1/2 0) '(1/4 7/30) 9/100 1/10
    'dim-normal 'dim-highlight 'rel-normal 'rel-highlight))

(test panel-initialize
  (begin
    (set! b:cluster-themes (list b:iden b:succ))
    (b:with-log (lambda () (tell b:panel 'initialize)))))
(test panel-pexps
  (b:clean (list (tell b:panel 'get-normal-panel-pexp) (tell b:panel 'get-highlighted-panel-pexp)
        (map b:name (tell b:panel 'get-relations)))))
(test panel-relations
  (b:with-log
    (lambda ()
      (tell b:panel 'add-relation #f)
      (tell b:panel 'remove-relation plato-successor)
      (list (map b:name (tell b:panel 'get-relations))
            (tell b:panel 'get-normal-panel-pexp)))))
(test panel-no-relations
  (b:with-log
    (lambda ()
      (tell b:panel 'delete-all-relations)
      (list (tell b:panel 'get-normal-panel-pexp)
            (tell b:panel 'get-highlighted-panel-pexp)))))
(test panel-set-theme-graphics-parameters
  (b:with-log
    (lambda ()
      (tell b:panel 'set-theme-graphics-parameters b:iden)
      (tell b:panel 'set-theme-graphics-parameters b:diff))))
(test panel-draw-dominant
  (begin
    (set! b:cluster-themes (list b:iden b:succ b:diff))
    (tell b:panel 'add-relation plato-identity)
    (tell b:panel 'add-relation plato-successor)
    (set! b:dominant b:iden)
    (b:with-log (lambda () (tell b:panel 'draw-panel) (tell b:panel 'get-highlighted-theme)))))
(test panel-update-same-dominant
  (b:with-log (lambda () (tell b:panel 'update-graphics))))
(test panel-update-new-dominant
  (begin
    (set! b:dominant b:succ)
    (b:with-log (lambda () (tell b:panel 'update-graphics)))))
(test panel-update-no-dominant
  (begin
    (set! b:dominant #f)
    (b:with-log (lambda () (tell b:panel 'update-graphics)))))
(test panel-activations
  (b:with-log
    (lambda ()
      (tell b:panel 'set-highlight-color 'yellow)
      (tell b:panel 'draw-absolute-activation '(1/3 1/10) -60)
      (tell b:panel 'draw-absolute-activation '(1/3 1/10) 25)
      (set! b:pressure? #t)
      (tell b:panel 'decrease-absolute-activation '(1/3 1/10) -30)
      (set! b:pressure? #f)
      (tell b:panel 'decrease-absolute-activation '(1/3 1/10) 30)
      (tell b:panel 'erase-activation '(1/3 1/10))
      (set! b:dominant b:iden)
      (tell b:panel 'draw-panel)
      (tell b:panel 'decrease-absolute-activation '(1/3 1/10) 70)
      (tell b:panel 'erase-activation '(1/3 1/10))
      (list (eq? *fg-color* %default-fg-color%) (tell b:panel 'dimension? plato-letter-category)
            (tell b:panel 'dimension? plato-length) (tell b:panel 'get-activation-diameter)))))

;;;---------------------------------------------------------------------------
;;; trace-graphics.ss: event icons on a fake window

(define b:letter
  (lambda (descriptor)
    (b:fake 'letter (list (cons 'get-descriptor-for (lambda (facet) descriptor))))))
(define b:group-of
  (lambda (direction facet objects)
    (b:fake 'group
      (list (cons 'get-direction (b:const direction))
            (cons 'get-bond-facet (b:const facet))
            (cons 'get-constituent-objects (b:const objects))
            (cons 'get-descriptor-for (lambda (f) plato-c))))))
(define b:groups
  (list (b:group-of plato-right plato-letter-category
                    (list (b:letter plato-a) (b:letter plato-b) (b:letter plato-c)))
        (b:group-of plato-left plato-letter-category
                    (list (b:letter plato-x) (b:letter plato-y)))
        (b:group-of #f plato-length
                    (list (b:letter plato-one) (b:letter plato-two)
                          (b:group-of #f plato-letter-category '())))))

(define b:events
  (append
    (list (b:fake 'answer-event
            (list (cons 'get-type (b:const 'answer))
                  (cons 'get-answer-string
                        (b:const (b:fake 'workspace-string (list (cons 'print-name "xyd"))))))))
    (list (b:fake 'clamp-event (list (cons 'get-type (b:const 'clamp)))))
    (map (lambda (node)
           (b:fake 'concept-activation-event
             (list (cons 'get-type (b:const 'concept-activation))
                   (cons 'get-slipnode (b:const node)))))
         (list plato-opposite plato-a plato-alphabetic-position-category))
    (map (lambda (group)
           (b:fake 'group-event
             (list (cons 'get-type (b:const 'group)) (cons 'get-group (b:const group)))))
         b:groups)
    (map (lambda (type)
           (b:fake 'rule-event
             (list (cons 'get-type (b:const 'rule)) (cons 'get-rule-type (b:const type)))))
         '(top bottom))
    (list (b:fake 'concept-mapping-event
            (list (cons 'get-type (b:const 'concept-mapping))
                  (cons 'get-concept-mapping
                        (b:const (b:fake 'concept-mapping
                                   (list (cons 'print-name "rmost=>lmost"))))))))
    (list (b:fake 'snag-event (list (cons 'get-type (b:const 'snag)))))))

(define b:event-pexp-info
  (lambda (event)
    (case (tell event 'get-type)
      ((answer) answer-event-pexp-info)
      ((clamp) clamp-event-pexp-info)
      ((concept-activation) concept-activation-event-pexp-info)
      ((group) group-event-pexp-info)
      ((rule) rule-event-pexp-info)
      ((concept-mapping) concept-mapping-event-pexp-info)
      ((snag) snag-event-pexp-info))))

(test trace-constants
  (list %group-event-icon-arrowhead-length% %minimum-event-width% %minimum-event-height%))
(test event-pexp-info
  (map (lambda (event)
         (b:with-log (lambda () ((b:event-pexp-info event) b:g event 3/5 1/2))))
       b:events))
(test event-pexp-info-flonum
  (map (lambda (event)
         (b:clean ((b:event-pexp-info event) b:g event 2.2 0.5)))
       b:events))
(test group-event-text-strings (map group-event-pexp-text-string b:groups))
(test group-event-arrowheads
  (list (group-event-pexp-arrowhead plato-right 1/2 3/4)
        (group-event-pexp-arrowhead plato-left 0.5 0.75)
        (group-event-pexp-arrowhead #f 1/2 3/4)))

;;;---------------------------------------------------------------------------
;;; memory-graphics.ss: icons on a fake window

(test memory-icon-pexp-info
  (map (lambda (name)
         (let* ((answer (b:fake 'answer-description (list (cons 'print-name name))))
                (info #f)
                (log (b:with-log
                       (lambda ()
                         (set! info (get-memory-icon-pexp-info b:g answer 1/2 19/20))
                         'done))))
           (list log (b:clean (cdr info))
                 (map (lambda (a) (b:clean ((car info) a))) '(0 37 100)))))
       '("abc -> abd, xyz -> wyz" "abc -> abd, xyz -> SNAG" "")))

;;;---------------------------------------------------------------------------
;;; The Trace and Memory windows' mouse handlers, with fake windows and model

(define b:window
  (lambda (name)
    (lambda msg (log! (b:clean (cons name (cdr msg)))) 'done)))
(define b:highlighted? #f)
(define b:event
  (b:fake 'answer-event
    (list (cons 'toggle-highlight (lambda () (set! b:highlighted? (not b:highlighted?))))
          (cons 'highlighted? (lambda () b:highlighted?))
          (cons 'display (lambda () (log! 'event-display))))))
(define b:previous #f)
(define b:answer
  (b:fake 'answer-description
    (list (cons 'toggle-highlight (lambda () (set! b:highlighted? (not b:highlighted?))))
          (cons 'highlighted? (lambda () b:highlighted?))
          (cons 'display (lambda () (log! 'answer-display))))))
(define b:snag
  (b:fake 'snag-description
    (list (cons 'toggle-highlight (lambda () (set! b:highlighted? (not b:highlighted?))))
          (cons 'highlighted? (lambda () b:highlighted?))
          (cons 'display (lambda () (log! 'snag-display))))))
(define b:selected #f)

(define b:install-handler-fakes
  (lambda ()
    (b:set-global! '*running?* #f)
    (b:set-global! '*trace*
      (b:fake 'temporal-trace
        (list (cons 'get-mouse-selected-event
                    (lambda (x y) (log! (list 'select-event x y)) b:selected)))))
    (b:set-global! '*memory*
      (b:fake 'memory
        (list (cons 'get-mouse-selected-answer
                    (lambda (x y) (log! (list 'select-answer x y)) b:selected))
              (cons 'get-other-highlighted-answer (lambda (a) b:previous)))))
    (b:set-global! '*themespace* (b:fake 'themespace '()))
    (b:set-global! '*workspace-window* (b:window 'workspace-window))
    (b:set-global! '*slipnet-window* (b:window 'slipnet-window))
    (b:set-global! '*coderack-window* (b:window 'coderack-window))
    (b:set-global! '*temperature-window* (b:window 'temperature-window))
    (b:set-global! '*comment-window* (b:window 'comment-window))
    (b:set-global! '*temperature* 37)))

(test trace-press-handler
  (begin
    (b:install-handler-fakes)
    (list
      (b:with-log (lambda () (set! b:selected #f) (trace-window-press-handler 'win 1/2 1/3)))
      (b:with-log (lambda () (set! b:selected b:event) (trace-window-press-handler 'win 3 1/2)))
      (b:with-log (lambda () (trace-window-press-handler 'win 3 1/2)))
      (b:with-log (lambda ()
                  (b:set-global! '*running?* #t)
                  (trace-window-press-handler 'win 3 1/2)
                  (b:set-global! '*running?* #f))))))
(test memory-press-handler
  (begin
    (b:install-handler-fakes)
    (set! b:highlighted? #f)
    (list
      (b:with-log (lambda () (set! b:selected #f) (memory-window-press-handler 'win 1/2 1/3)))
      (b:with-log (lambda () (set! b:selected b:answer) (memory-window-press-handler 'win 1/2 2)))
      (b:with-log (lambda () (memory-window-press-handler 'win 1/2 2)))
      (b:with-log (lambda ()
                  (set! b:selected b:snag) (set! b:previous b:answer)
                  (memory-window-press-handler 'win 1/2 2))))))

;;;---------------------------------------------------------------------------
;;; eeg-graphics.ss: the EEG object over a sequence of Workspace activities

(test EEG-table
  (map (lambda (entry) (list (1st entry) (2nd entry) (3rd entry) (4th entry) (5th entry)))
       %EEG-table%))
(test EEG-constants (list %EEG-buffer-size% %max-EEG-window-cycles%))

(define b:activity 100)
(test EEG-recording
  (begin
    (b:set-global! '*workspace*
      (b:fake 'workspace (list (cons 'get-activity (lambda () b:activity)))))
    (tell *EEG* 'initialize)
    (let loop ((i 0) (acc '()))
      (if (= i 47)
          (list (reverse acc)
                (tell *EEG* 'get-previous-values 0)
                (tell *EEG* 'get-previous-values 2 5)
                (tell *EEG* 'get-average-value 0)
                (tell *EEG* 'get-max-variation 0)
                (tell *EEG* 'get-max-variation 2 8))
          (begin
            (set! b:activity (- 100 (* 3 i) (if (odd? i) 1/3 0)))
            (b:set-global! '*temperature* (if (< i 20) (* 4 i) (- 100 i)))
            (tell *EEG* 'record-current-values)
            (loop (+ i 1)
                  (cons (list (tell *EEG* 'get-current-values)
                              (tell *EEG* 'get-current-value 1)
                              (tell *EEG* 'get-average-value 2 3))
                        acc)))))))
