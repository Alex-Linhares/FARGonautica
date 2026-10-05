;;; slipnet-battery.scm -- differential checks of the engine's slipnet.ss and
;;; images.ss (item 05) against the original under Chez Scheme 10.
;;;
;;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Read form by form, after tests/diff/helpers.scm, by two runners:
;;;   chez_scheme/oracle/diff-eval.ss      (Chez, with the whole original loaded)
;;;   racket/tests/slipnet-diff-test.rkt   (Racket: compat + utilities + engine)
;;; The rules of utilities-battery.scm apply: no two side-effecting
;;; subexpressions in one call or one `let'; lists for `map' from b:iota or
;;; b:copy.  Globals of the original are set with b:set-global!.
;;;
;;; Parts not ported yet are replaced, in both runners, through
;;; b:set-global!: monitor-slipnode-activation-change (trace.ss) and
;;; temp-adjusted-probability (formulas.ss) by logging fakes, *themespace*,
;;; *workspace* by fakes.

;;;---------------------------------------------------------------------------
;;; Helpers

(define b:name (lambda (node) (if node (tell node 'get-name-symbol) #f)))
(define b:names (lambda (nodes) (map b:name nodes)))

;; any model object (or nested list of them) as data
(define b:deep
  (lambda (x)
    (cond
      ((pair? x) (let* ((a (b:deep (car x))) (d (b:deep (cdr x)))) (cons a d)))
      ((procedure? x)
       (let ((type (tell x 'object-type)))
         (case type
           ((slipnode) (tell x 'get-name-symbol))
           ((slipnet-link) (list 'link (tell x 'print-name)))
           ((image) (list 'image (b:deep (tell x 'generate))))
           ((string-image) (list 'string-image (b:deep (tell x 'generate))))
           (else (list 'object type)))))
      (else x))))

(define b:link
  (lambda (link)
    (list (tell link 'print-name)
          (tell link 'get-link-type)
          (b:name (tell link 'get-from-node))
          (b:name (tell link 'get-to-node))
          (b:name (tell link 'get-label-node))
          (tell link 'get-link-length)
          (tell link 'get-intrinsic-degree-of-assoc)
          (tell link 'get-degree-of-assoc))))

(define b:links (lambda (links) (map b:link links)))

(define b:node
  (lambda (node)
    (list (tell node 'get-name-symbol)
          (tell node 'get-short-name)
          (tell node 'get-lowercase-name)
          (tell node 'get-uppercase-name)
          (tell node 'get-CM-short-name)
          (tell node 'print-name)
          (tell node 'get-conceptual-depth)
          (tell node 'get-activation)
          (tell node 'frozen?)
          (tell node 'get-intrinsic-link-length)
          (tell node 'get-shrunk-link-length)
          (tell node 'get-degree-of-assoc)
          (tell node 'category?)
          (tell node 'instance?)
          (b:name (tell node 'get-category))
          (b:names (tell node 'get-instance-nodes))
          (tell node 'get-graphics-coord)
          (tell node 'get-graphics-label-coord))))

(define b:node-links
  (lambda (node)
    (list (tell node 'get-name-symbol)
          (list 'incoming (b:links (tell node 'get-incoming-links)))
          (list 'category (b:links (tell node 'get-category-links)))
          (list 'property (b:links (tell node 'get-property-links)))
          (list 'lateral (b:links (tell node 'get-lateral-links)))
          (list 'sliplinks (b:links (tell node 'get-lateral-sliplinks)))
          (list 'labeled (b:links (tell node 'get-links-labeled-by-node)))
          (list 'outgoing (b:links (tell node 'get-outgoing-links))))))

(define b:activations
  (lambda ()
    (map (lambda (node) (tell node 'get-activation)) *slipnet-nodes*)))

(define b:frozen
  (lambda ()
    (map (lambda (node) (if (tell node 'frozen?) 1 0)) *slipnet-nodes*)))

(define b:index-of
  (lambda (x l)
    (let loop ((l l) (i 0))
      (cond
        ((null? l) #f)
        ((eq? x (car l)) i)
        (else (loop (cdr l) (+ i 1)))))))

;; a 0/1 string for a binary relation over every pair of slipnodes
(define b:relation-table
  (lambda (rel)
    (map (lambda (n1)
           (list->string
             (map (lambda (n2) (if (rel n1 n2) #\1 #\0)) *slipnet-nodes*)))
         *slipnet-nodes*)))

;;;---------------------------------------------------------------------------
;;; The initial slipnet, as loaded (before any reset)

(test slipnet-constants
  (list %max-activation% %workspace-activation% %full-activation-threshold%
        (length *slipnet-nodes*)))

(test slipnet-node-lists
  (list (b:names *slipnet-nodes*) (b:names *slipnet-letters*)
        (b:names *slipnet-numbers*) (b:names *top-down-slipnodes*)
        (b:names *initially-clamped-slipnodes*)))

(test slipnet-nodes-initial (map b:node *slipnet-nodes*))

(test slipnet-links-initial (map b:node-links *slipnet-nodes*))

(test slipnet-link-count
  (apply + (map (lambda (n) (length (tell n 'get-outgoing-links))) *slipnet-nodes*)))

(test slipnet-node-top-level-values
  (map (lambda (n) (eq? n (top-level-value (tell n 'get-name-symbol)))) *slipnet-nodes*))

;; every link is the top-level value named after its nodes
(test slipnet-link-top-level-values
  (map (lambda (n)
         (map (lambda (link)
                (let* ((from (tell (tell link 'get-from-node) 'get-name-symbol))
                       (to (tell (tell link 'get-to-node) 'get-name-symbol))
                       (sfrom (symbol->string from))
                       (sto (symbol->string to))
                       (name (string->symbol
                               (string-append
                                 (substring sfrom 6 (string-length sfrom)) "-"
                                 (substring sto 6 (string-length sto)) "-link"))))
                  (eq? link (top-level-value name))))
              (tell n 'get-outgoing-links)))
       *slipnet-nodes*))

(test slipnet-print
  (list (b:capture (lambda () (tell plato-a 'print)))
        (b:capture (lambda () (tell plato-letter-category 'print)))
        (b:capture (lambda () (tell (car (tell plato-a 'get-lateral-links)) 'print)))))

;;;---------------------------------------------------------------------------
;;; Relations between nodes

(test get-label-table
  (map (lambda (n1)
         (map (lambda (n2) (b:name (get-label n1 n2))) *slipnet-nodes*))
       *slipnet-nodes*))

(test linked-table (b:relation-table linked?))
(test related-table (b:relation-table related?))
(test slip-linked-table (b:relation-table slip-linked?))

(test relationship-between
  (map (lambda (l) (b:name (relationship-between l)))
       (list (list plato-a plato-b plato-c)
             (list plato-c plato-b plato-a)
             (list plato-a plato-a plato-a)
             (list plato-a plato-c)
             (list plato-a plato-b plato-d)
             (list plato-one plato-two plato-three)
             (list plato-three plato-two)
             (list plato-a #f plato-b)
             (list plato-leftmost plato-rightmost)
             (list plato-left plato-right plato-left))))

;; one node, or none: adjacency-map gives no relations (all-same? of '())
(test relationship-between-one (b:name (relationship-between (list plato-a))))
(test relationship-between-none (b:name (relationship-between (list))))

(define b:relation-nodes
  (list plato-identity plato-opposite plato-successor plato-predecessor
        plato-group-category plato-bond-category plato-letter-category
        plato-length))

(test get-related-node-table
  (map (lambda (node)
         (map (lambda (rel) (b:name (tell node 'get-related-node rel)))
              b:relation-nodes))
       *slipnet-nodes*))

(test inverse-table
  (map (lambda (node) (b:name (inverse node))) (cons #f *slipnet-nodes*)))

(test platonic-predicates
  (map (lambda (node)
         (list (platonic-letter? node) (platonic-number? node)
               (platonic-relation? node) (platonic-literal? node)))
       *slipnet-nodes*))

(test platonic-numbers
  (list (map (lambda (n) (b:name (number->platonic-number n))) (b:iota 7))
        (map platonic-number->number *slipnet-numbers*)))

(test coattail-slippage-probability
  (map (lambda (n)
         (map (lambda (link) (coattail-slippage-probability #f #f n link))
              (tell n 'get-lateral-sliplinks)))
       *slipnet-nodes*))

;;;---------------------------------------------------------------------------
;;; Descriptor predicates, with fake workspace objects

(define b:make-fake-object
  (lambda (type len spanning leftmost rightmost middle whole letter-cat)
    (lambda msg
      (record-case (cdr msg)
        (object-type () type)
        (get-group-length () len)
        (string-spanning-group? () spanning)
        (leftmost-in-string? () leftmost)
        (rightmost-in-string? () rightmost)
        (middle-in-string? () middle)
        (spans-whole-string? () whole)
        (get-descriptor-for (cat) (if (eq? cat plato-letter-category) letter-cat #f))
        (else 'invalid-message-indicator)))))

(define b:fake-objects
  (list (b:make-fake-object 'letter 1 #f #t #f #f #f plato-a)
        (b:make-fake-object 'letter 1 #f #f #t #f #f plato-z)
        (b:make-fake-object 'letter 1 #f #f #f #t #f plato-m)
        (b:make-fake-object 'letter 1 #f #t #t #f #t plato-a)
        (b:make-fake-object 'group 1 #f #t #f #f #f plato-b)
        (b:make-fake-object 'group 2 #f #f #t #f #f #f)
        (b:make-fake-object 'group 3 #t #t #t #f #t #f)
        (b:make-fake-object 'group 4 #f #f #f #t #f plato-z)
        (b:make-fake-object 'group 5 #f #t #f #f #f plato-a)
        (b:make-fake-object 'group 6 #t #t #t #f #t #f)))

(test possible-descriptor-table
  (map (lambda (node)
         (map (lambda (obj) (if (tell node 'possible-descriptor? obj) 1 0))
              b:fake-objects))
       *slipnet-nodes*))

(test possible-descriptors
  (map (lambda (node)
         (list (b:name node)
               (map (lambda (obj)
                      (list (tell node 'description-possible? obj)
                            (b:names (tell node 'get-possible-descriptors obj))))
                    b:fake-objects)))
       (filter (lambda (n) (tell n 'category?)) *slipnet-nodes*)))

;;;---------------------------------------------------------------------------
;;; Reset, clamping and the activation messages

(define b:monitor-log '())
(define b:monitor
  (lambda (node old new)
    (set! b:monitor-log (cons (list (b:name node) old new) b:monitor-log))))
(b:set-global! 'monitor-slipnode-activation-change b:monitor)

(define b:take-monitor-log
  (lambda ()
    (let ((l (reverse b:monitor-log)))
      (set! b:monitor-log '())
      l)))

(define b:reset-slipnet
  (lambda ()
    (for-each (lambda (node) (tell node 'reset)) *slipnet-nodes*)
    (set! b:monitor-log '())))

(test reset-values
  (begin
    (b:reset-slipnet)
    (list (b:activations) (b:frozen))))

(test node-messages
  (let* ((n plato-successor)
         (r1 (tell n 'set-activation 30))
         (a1 (tell n 'get-activation))
         (r2 (tell n 'increment-activation-buffer 25))
         (r3 (tell n 'flush-activation-buffer))
         (a2 (tell n 'get-activation))
         (r4 (tell n 'activate-from-workspace))
         (r5 (tell n 'flush-activation-buffer))
         (a3 (tell n 'get-activation))
         (r6 (tell n 'decrement-activation-buffer 7))
         (r7 (tell n 'flush-activation-buffer))
         (a4 (tell n 'get-activation))
         (r8 (tell n 'freeze))
         (r9 (tell n 'set-activation 3))
         (r10 (tell n 'update-activation 4))
         (r11 (tell n 'increment-activation-buffer 9))
         (r12 (tell n 'flush-activation-buffer))
         (a5 (tell n 'get-activation))
         (r13 (tell n 'unfreeze))
         (r14 (tell n 'update-activation 64))
         (a6 (tell n 'get-activation))
         (r15 (tell n 'clamp 100))
         (a7 (list (tell n 'get-activation) (tell n 'frozen?)))
         (deg (list (tell n 'get-degree-of-assoc)
                    (b:links (tell n 'get-links-labeled-by-node))))
         (r16 (tell n 'reset)))
    (list (list r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16)
          (list a1 a2 a3 a4 a5 a6 a7) deg
          (b:take-monitor-log) (tell n 'get-activation))))

(test decay-and-spread-single
  (map (lambda (node)
         (let* ((r0 (b:reset-slipnet))
                (r1 (tell node 'set-activation 100))
                (r2 (tell node 'decay-activation))
                (r3 (tell node 'spread-activation))
                (r4 (for-each (lambda (n) (tell n 'flush-activation-buffer)) *slipnet-nodes*)))
           (list (b:name node) (b:activations))))
       *slipnet-nodes*))

;;;---------------------------------------------------------------------------
;;; update-slipnet-activations: 20 updates from fixed states

(define b:theme-log '())
(define b:make-fake-theme
  (lambda (k node amount)
    (lambda msg
      (record-case (cdr msg)
        (object-type () 'theme)
        (spread-activation-to-slipnet ()
          (set! b:theme-log (cons k b:theme-log))
          (tell node 'increment-activation-buffer amount)
          'done)
        (else 'invalid-message-indicator)))))

(define b:active-themes '())
(define b:fake-themespace
  (lambda msg
    (record-case (cdr msg)
      (object-type () 'themespace)
      (get-all-active-themes () b:active-themes)
      (else 'invalid-message-indicator))))
(b:set-global! '*themespace* b:fake-themespace)

;; a fixed starting state: activation of the i-th node from a formula
(define b:set-state
  (lambda (k)
    (b:reset-slipnet)
    (let loop ((nodes *slipnet-nodes*) (i 0))
      (if (pair? nodes)
          (begin
            (tell (car nodes) 'set-activation (modulo (+ (* i 37) (* k 11)) 101))
            (loop (cdr nodes) (+ i 1)))))
    (if (odd? k)
        (for-each (lambda (n) (tell n 'clamp 100)) *initially-clamped-slipnodes*))
    (set! b:monitor-log '())
    (set! b:theme-log '())))

(define b:twenty-updates
  (lambda (k seed themes)
    (b:set-state k)
    (set! b:active-themes themes)
    (random-seed seed)
    (let loop ((i 0) (acc '()))
      (if (= i 20)
          (let* ((mlog (b:take-monitor-log)) (tlog (reverse b:theme-log)))
            (set! b:active-themes '())
            (list (reverse acc) (length mlog) mlog tlog))
          (begin
            (if (= i 10)
                (for-each (lambda (n) (tell n 'unfreeze)) *initially-clamped-slipnodes*))
            (update-slipnet-activations)
            (let* ((acts (b:activations))
                   (fr (b:frozen))
                   (s (random-seed)))
              (loop (+ i 1) (cons (list i acts fr s) acc))))))))

(test twenty-updates-k0 (b:twenty-updates 0 1 '()))
(test twenty-updates-k1 (b:twenty-updates 1 42 '()))
(test twenty-updates-k2
  (b:twenty-updates 2 123456789
    (list (b:make-fake-theme 1 plato-successor 30)
          (b:make-fake-theme 2 plato-rightmost 55))))
(test twenty-updates-k3
  (b:twenty-updates 3 2147483648 (list (b:make-fake-theme 3 plato-opposite 80))))
(test twenty-updates-k4 (b:twenty-updates 4 3141592653 '()))
(test twenty-updates-k5 (b:twenty-updates 5 99 '()))
(test twenty-updates-k6 (b:twenty-updates 6 65536 '()))
(test twenty-updates-k7 (b:twenty-updates 7 7 (list (b:make-fake-theme 4 plato-a 100))))

;; every seed of helpers.scm, activations and generator state only
(test twenty-updates-seeds
  (map (lambda (seed)
         (let ((r (b:twenty-updates (modulo seed 9) seed '())))
           (map (lambda (step) (list (cadr step) (cadddr step))) (car r))))
       b:seeds))

;; from the state a run starts in: everything 0, two clamped nodes
(test twenty-updates-from-reset
  (begin
    (b:reset-slipnet)
    (for-each (lambda (n) (tell n 'clamp 100)) *initially-clamped-slipnodes*)
    (random-seed 5)
    (b:repeat 20 (lambda ()
                   (update-slipnet-activations)
                   (list (b:activations) (random-seed))))))

;;;---------------------------------------------------------------------------
;;; Property links, slippages, top-down codelets

(define b:tap-log '())
(b:set-global! 'temp-adjusted-probability
  (lambda (p) (set! b:tap-log (cons p b:tap-log)) p))

(test similar-property-links
  (map (lambda (seed)
         (b:seeded seed
           (lambda ()
             (set! b:tap-log '())
             (let* ((a (b:links (tell plato-a 'get-similar-property-links)))
                    (z (b:links (tell plato-z 'get-similar-property-links)))
                    (b (b:links (tell plato-b 'get-similar-property-links))))
               (list a z b (reverse b:tap-log))))))
       b:seeds))

(define b:slip-log '())
(define b:make-fake-slippage
  (lambda (d1 d2 label cm-type)
    (lambda msg
      (record-case (cdr msg)
        (object-type () 'concept-mapping)
        (get-descriptor1 () d1)
        (get-descriptor2 () d2)
        (get-label () label)
        (get-CM-type () cm-type)
        (else 'invalid-message-indicator)))))
(define b:fake-sliplog
  (lambda msg
    (record-case (cdr msg)
      (object-type () 'sliplog)
      (applied (s)
        (set! b:slip-log (cons (list 'applied (b:name (tell s 'get-descriptor1))) b:slip-log))
        'done)
      (coattail (node label node2 s)
        (set! b:slip-log
          (cons (list 'coattail (b:name node) (b:name label) (b:name node2)
                      (b:name (tell s 'get-descriptor1)))
                b:slip-log))
        'done)
      (else 'invalid-message-indicator))))

(define b:slippage-lists
  (list
    (list (b:make-fake-slippage plato-leftmost plato-rightmost plato-opposite
                                plato-string-position-category))
    (list (b:make-fake-slippage plato-successor plato-predecessor plato-opposite
                                plato-bond-category))
    (list (b:make-fake-slippage plato-left plato-right plato-opposite
                                plato-direction-category)
          (b:make-fake-slippage plato-letter plato-group #f plato-object-category))
    (list (b:make-fake-slippage plato-a plato-z plato-opposite plato-letter-category)
          (b:make-fake-slippage plato-one plato-two plato-successor plato-length))))

(define b:slippage-nodes
  (list plato-leftmost plato-rightmost plato-successor plato-predecessor plato-left
        plato-right plato-alphabetic-first plato-alphabetic-last plato-letter
        plato-a plato-one plato-predgrp plato-succgrp plato-single))

(test apply-slippages
  (map (lambda (seed)
         (b:seeded seed
           (lambda ()
             (map (lambda (slippages)
                    (map (lambda (node)
                           (set! b:slip-log '())
                           (let ((r (b:name (tell node 'apply-slippages slippages b:fake-sliplog))))
                             (list r (reverse b:slip-log))))
                         b:slippage-nodes))
                  b:slippage-lists))))
       (list 1 2 3 42 1000)))

(test apply-slippages-full-activation
  (begin
    (b:reset-slipnet)
    (tell plato-opposite 'clamp 100)
    (let ((r (b:seeded 7
               (lambda ()
                 (map (lambda (node)
                        (set! b:slip-log '())
                        (let ((r (b:name (tell node 'apply-slippages (car b:slippage-lists)
                                           b:fake-sliplog))))
                          (list r (reverse b:slip-log))))
                      b:slippage-nodes)))))
      (b:reset-slipnet)
      r)))

(define b:fake-workspace
  (lambda msg
    (record-case (cdr msg)
      (object-type () 'workspace)
      (get-average-intra-string-unhappiness () 60)
      (get-min-mapping-strength () 30)
      (get-average-unhappiness () 45)
      (get-rough-num-of-unrelated-objects () 'some)
      (get-bonds () (list 'b1))
      (get-rough-num-of-ungrouped-objects () 'many)
      (else 'invalid-message-indicator))))

(define b:codelets
  (lambda ()
    (map (lambda (c)
           (list (tell c 'get-codelet-type-name) (tell c 'get-relative-urgency)
                 (b:deep (tell c 'get-argument 0))
                 (b:deep (tell c 'get-argument 1))))
         (tell *coderack* 'get-all-codelets))))

(test top-down-codelets
  (begin
    (b:set-global! '%coderack-graphics% #f)
    (b:set-global! '%workspace-graphics% #f)
    (b:set-global! '%slipnet-graphics% #f)
    (b:set-global! '*workspace* b:fake-workspace)
    (b:set-global! '*codelet-count* 0)
    (b:set-global! '*temperature* 50)
    (map (lambda (seed)
           (b:seeded seed
             (lambda ()
               (b:reset-slipnet)
               (tell *coderack* 'initialize)
               (let loop ((nodes *slipnet-nodes*) (i 0))
                 (if (pair? nodes)
                     (begin
                       (tell (car nodes) 'set-activation (modulo (+ (* i 29) seed) 101))
                       (loop (cdr nodes) (+ i 1)))))
               (for-each (lambda (n) (tell n 'attempt-to-post-top-down-codelets))
                         *slipnet-nodes*)
               (tell *coderack* 'post-deferred-codelets)
               (b:codelets))))
         (list 1 2 3 42 1000 65535))))

;;;---------------------------------------------------------------------------
;;; Images

(b:reset-slipnet)

;; (b:try (lambda (fail) ...)): the value, or 'failed if fail was called
(define b:try
  (lambda (f)
    (call-with-current-continuation
      (lambda (k) (f (lambda () (k 'failed)))))))

(define b:letter-images
  (lambda (nodes) (map make-letter-image nodes)))

(define b:image-data
  (lambda (image)
    (list (b:deep (tell image 'generate))
          (b:name (tell image 'get-letter))
          (b:name (tell image 'get-bond-facet))
          (b:name (tell image 'get-letter-relation))
          (b:name (tell image 'get-length-relation))
          (b:name (tell image 'get-direction))
          (b:name (tell image 'get-length))
          (tell image 'letter-image?)
          (length (tell image 'get-sub-images))
          (b:deep (tell image 'get-swapped-image))
          (b:deep (tell image 'get-instantiated-object)))))

;; fresh images to apply operations to
(define b:image-makers
  (list
    ;; a
    (lambda () (make-letter-image plato-a))
    ;; abc: successor group of letters
    (lambda ()
      (make-image plato-a plato-letter-category plato-successor plato-identity
                  plato-right (b:letter-images (list plato-a plato-b plato-c))))
    ;; cba, as a leftward predecessor group
    (lambda ()
      (make-image plato-c plato-letter-category plato-predecessor plato-identity
                  plato-left (b:letter-images (list plato-c plato-b plato-a))))
    ;; xyz
    (lambda ()
      (make-image plato-x plato-letter-category plato-successor plato-identity
                  plato-right (b:letter-images (list plato-x plato-y plato-z))))
    ;; mmm: sameness group
    (lambda ()
      (make-image plato-m plato-letter-category plato-identity plato-identity
                  #f (b:letter-images (list plato-m plato-m plato-m))))
    ;; aabbcc: successor group of sameness groups, letter facet
    (lambda ()
      (make-image plato-a plato-letter-category plato-successor plato-identity
                  plato-right
                  (list (make-image plato-a plato-letter-category plato-identity
                                    plato-identity #f (b:letter-images (list plato-a plato-a)))
                        (make-image plato-b plato-letter-category plato-identity
                                    plato-identity #f (b:letter-images (list plato-b plato-b)))
                        (make-image plato-c plato-letter-category plato-identity
                                    plato-identity #f (b:letter-images (list plato-c plato-c))))))
    ;; abbccc: length facet, successor lengths
    (lambda ()
      (make-image plato-a plato-length plato-successor plato-successor plato-right
                  (list (make-image plato-a plato-letter-category plato-identity
                                    plato-identity #f (b:letter-images (list plato-a)))
                        (make-image plato-b plato-letter-category plato-identity
                                    plato-identity #f (b:letter-images (list plato-b plato-b)))
                        (make-image plato-c plato-letter-category plato-identity
                                    plato-identity #f
                                    (b:letter-images (list plato-c plato-c plato-c))))))
    ;; ab with no letter relation
    (lambda ()
      (make-image plato-a plato-letter-category #f plato-identity plato-right
                  (b:letter-images (list plato-a plato-q))))))

(define b:operations
  (list
    (list 'none (lambda (i fail) 'done))
    (list 'start-succ (lambda (i fail) (tell i 'new-start-letter plato-successor fail)))
    (list 'start-pred (lambda (i fail) (tell i 'new-start-letter plato-predecessor fail)))
    (list 'start-iden (lambda (i fail) (tell i 'new-start-letter plato-identity fail)))
    (list 'start-x (lambda (i fail) (tell i 'new-start-letter plato-x fail)))
    (list 'start-b (lambda (i fail) (tell i 'new-start-letter plato-b fail)))
    (list 'start-false (lambda (i fail) (tell i 'new-start-letter #f fail)))
    (list 'alpha-first (lambda (i fail) (tell i 'new-alpha-position-category plato-alphabetic-first fail)))
    (list 'alpha-last (lambda (i fail) (tell i 'new-alpha-position-category plato-alphabetic-last fail)))
    (list 'alpha-opp (lambda (i fail) (tell i 'new-alpha-position-category plato-opposite fail)))
    (list 'length-succ (lambda (i fail) (tell i 'new-length plato-successor fail)))
    (list 'length-pred (lambda (i fail) (tell i 'new-length plato-predecessor fail)))
    (list 'length-iden (lambda (i fail) (tell i 'new-length plato-identity fail)))
    (list 'length-one (lambda (i fail) (tell i 'new-length plato-one fail)))
    (list 'length-two (lambda (i fail) (tell i 'new-length plato-two fail)))
    (list 'length-five (lambda (i fail) (tell i 'new-length plato-five fail)))
    (list 'length-a (lambda (i fail) (tell i 'new-length plato-a fail)))
    (list 'reverse-dir (lambda (i fail) (tell i 'reverse-direction fail)))
    (list 'reverse-letters (lambda (i fail) (tell i 'reverse-medium plato-letter-category fail)))
    (list 'reverse-lengths (lambda (i fail) (tell i 'reverse-medium plato-length fail)))
    (list 'letter (lambda (i fail) (tell i 'letter fail)))
    (list 'group (lambda (i fail) (tell i 'group fail)))
    (list 'shorten (lambda (i fail) (tell i 'shorten fail)))
    (list 'extend-succ (lambda (i fail) (tell i 'extend plato-successor plato-identity fail)))
    (list 'extend-iden-succ (lambda (i fail) (tell i 'extend plato-identity plato-successor fail)))
    (list 'extend-a-three (lambda (i fail) (tell i 'extend plato-a plato-three fail)))
    (list 'singleton (lambda (i fail) (tell i 'letter->singleton-group fail)))
    (list 'succ-then-reverse
          (lambda (i fail)
            (tell i 'new-start-letter plato-successor fail)
            (tell i 'reverse-medium plato-letter-category fail)))
    (list 'length-succ-then-start-pred
          (lambda (i fail)
            (tell i 'new-length plato-successor fail)
            (tell i 'new-start-letter plato-predecessor fail)))))

;; apply each operation to a fresh image, then a copy, a reset and a walk
(test image-operations
  (map (lambda (make)
         (map (lambda (op)
                (let* ((image (make))
                       (before (b:image-data image))
                       (r (b:try (lambda (fail) ((cadr op) image fail))))
                       (after (b:image-data image))
                       (copy (b:image-data (tell image 'copy)))
                       (leaves (let ((l '()))
                                 (tell image 'leaf-walk
                                   (lambda (i) (set! l (cons (b:name (tell i 'get-letter)) l))))
                                 (reverse l)))
                       (interior (let ((l '()))
                                   (tell image 'postorder-interior-walk
                                     (lambda (i) (set! l (cons (b:deep (tell i 'generate)) l))))
                                   (reverse l)))
                       (state (b:deep (tell image 'get-state)))
                       (reset (tell image 'reset))
                       (after-reset (b:image-data image)))
                  (list (car op) before r after copy leaves interior state reset after-reset)))
              b:operations))
       b:image-makers))

(test image-swapped-and-state
  (let* ((i1 ((cadr b:image-makers)))
         (i2 ((car b:image-makers)))
         (r1 (tell i1 'update-swapped-image i2))
         (s (tell i1 'get-state))
         (r2 (b:try (lambda (fail) (tell i1 'reverse-medium plato-letter-category fail))))
         (mid (b:image-data i1))
         (r3 (tell i1 'new-state s))
         (back (b:image-data i1))
         (r4 (tell i1 'reset)))
    (list r1 r2 mid r3 back r4 (b:image-data i1))))

;; (not mmm: printing a group image without a direction is an error)
(test image-print
  (map (lambda (k)
         (let* ((image ((list-ref b:image-makers k))))
           (b:capture (lambda () (tell image 'print)))))
       (list 0 1 2 3 5 6 7)))

;; a fake workspace string whose constituent objects have the given images
(define b:make-fake-string
  (lambda (images)
    (lambda msg
      (record-case (cdr msg)
        (object-type () 'string)
        (get-constituent-objects ()
          (map (lambda (image)
                 (lambda msg
                   (record-case (cdr msg)
                     (object-type () 'letter)
                     (get-image () image)
                     (else 'invalid-message-indicator))))
               images))
        (else 'invalid-message-indicator)))))

(define b:string-image-data
  (lambda (si)
    (list (b:deep (tell si 'generate))
          (b:deep (tell si 'get-sub-images))
          (b:deep (tell si 'get-ordered-sub-images))
          (b:name (tell si 'get-length)))))

(define b:string-makers
  (list
    (lambda () (b:letter-images (list plato-a plato-b plato-c)))
    (lambda () (b:letter-images (list plato-x plato-y plato-z)))
    (lambda () (list ((cadr b:image-makers)) (make-letter-image plato-q)))
    (lambda () (list ((list-ref b:image-makers 5)) ((list-ref b:image-makers 6))))))

(define b:take
  (lambda (l n) (if (= n 0) '() (cons (car l) (b:take (cdr l) (- n 1))))))

(define b:string-operations
  (list
    (list 'none (lambda (si fail) 'done))
    (list 'start-succ (lambda (si fail) (tell si 'new-start-letter plato-successor fail)))
    (list 'start-a (lambda (si fail) (tell si 'new-start-letter plato-a fail)))
    (list 'alpha (lambda (si fail) (tell si 'new-alpha-position-category plato-predecessor fail)))
    (list 'length (lambda (si fail) (tell si 'new-length plato-two fail)))
    (list 'appearance (lambda (si fail) (tell si 'new-appearance (list plato-q plato-r))))
    (list 'reverse-dir (lambda (si fail) (tell si 'reverse-direction fail)))
    (list 'reverse-letters (lambda (si fail) (tell si 'reverse-medium plato-letter-category fail)))
    (list 'reverse-lengths (lambda (si fail) (tell si 'reverse-medium plato-length fail)))
    (list 'replace-all
          (lambda (si fail)
            (let ((n (length (tell si 'get-sub-images))))
              (tell si 'replace-all 'new-length
                (b:take (list plato-two plato-successor plato-three) n) fail))))
    ;; the middle image fails: what the others look like depends on the
    ;; order in which map applies the procedure
    (list 'replace-all-fail
          (lambda (si fail)
            (let ((n (length (tell si 'get-sub-images))))
              (tell si 'replace-all 'new-length
                (b:take (list plato-two plato-a plato-three) n) fail))))
    (list 'letter (lambda (si fail) (tell si 'letter fail)))
    (list 'group (lambda (si fail) (tell si 'group fail)))))

(test string-image-operations
  (map (lambda (make)
         (map (lambda (op)
                (let* ((images (make))
                       (si (make-string-image (b:make-fake-string images) plato-right))
                       (before (b:string-image-data si))
                       (r (b:try (lambda (fail) ((cadr op) si fail))))
                       (after (b:string-image-data si))
                       (walk (let ((l '()))
                               (tell si 'do-walk 'leaf-walk
                                 (lambda (i) (set! l (cons (b:name (tell i 'get-letter)) l))))
                               (reverse l)))
                       (reset (tell si 'reset))
                       (after-reset (b:string-image-data si)))
                  (list (car op) before r after walk reset after-reset
                        (tell si 'object-type))))
              b:string-operations))
       b:string-makers))

(test string-image-left
  (let* ((si (make-string-image
               (b:make-fake-string (b:letter-images (list plato-a plato-b plato-c)))
               plato-left)))
    (b:string-image-data si)))

(test change-length-first
  (map (lambda (arg)
         (map (lambda (cur) (change-length-first? arg cur))
              (list #f plato-one plato-two plato-three plato-five)))
       (list plato-predecessor plato-successor plato-identity
             plato-one plato-two plato-three plato-five plato-a)))

(test enumerate-letter
  (map (lambda (args)
         (b:try (lambda (fail)
                  (b:names (enumerate-letter (car args) (cadr args) (caddr args) fail)))))
       (list (list plato-a plato-successor 3)
             (list plato-x plato-successor 3)
             (list plato-x plato-successor 4)
             (list plato-c plato-predecessor 3)
             (list plato-c plato-predecessor 4)
             (list plato-m plato-identity 5)
             (list plato-m #f 1)
             (list plato-m #f 2)
             (list plato-one plato-successor 5)
             (list plato-one plato-successor 6))))

;; utilities.ss's reveal (jootsing.ss calls it in a vprintf, so only in
;; verbose mode) names slipnodes with rules.ss's format-slipnode (item 17)
(test reveal-slipnodes
  (list (reveal plato-length)
        (reveal (list plato-length (list plato-successor plato-a) 'x 3))
        (reveal (list (list plato-letter-category plato-identity)
                      (list plato-string-position-category plato-opposite)))))
