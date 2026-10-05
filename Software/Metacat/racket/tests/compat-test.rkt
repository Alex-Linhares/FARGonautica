#lang racket/base
;; Item 03: Racket-side checks of compat.rkt and utilities.rkt that the
;; differential battery (utilities-diff-test.rkt) cannot express: forms that
;; only exist at module level, hygiene, cross-module mutation, reset.
(require rackunit
         racket/port
         racket/runtime-path
         (only-in racket/base [map rkt-map])
         "../compat.rkt"
         "../utilities.rkt")

(define-runtime-path compat-path "../compat.rkt")

;; Known answers from Chez Scheme 10 (let* so the draws are sequenced)
(random-seed 1)
(let* ([a (random 100)] [b (random 1.0)] [c (random 4294967296)] [d (random-seed)])
  (check-equal? (list a b c d) '(78 0.15832092000368547 3685624746 732599397)))
(random-seed 4294967295)
(let* ([a (random 2)] [b (random 0.5)] [d (random-seed)])
  (check-equal? (list a b d) '(1 0.012584221365574688 3197957931)))

;; one-armed if
(check-equal? (if #f 'x) (void))
(check-equal? (if #t 'x) 'x)

;; Chez map order and for-each value
(let ([log '()])
  (map (lambda (x) (set! log (cons x log))) '(1 2 3 4 5 6 7))
  (check-equal? (reverse log) '(7 5 6 3 4 1 2)))
(check-equal? (for-each add1 '(1 2 3)) 4)
(check-equal? (sort < '(3 1 2)) '(1 2 3))
(check-equal? (remq 'a '(a b a c)) '(b c))

;; record-case binds its own temporary hygienically
(let ([r 'mine])
  (check-equal? (record-case '(msg 1) (msg (x) (list r x)) (else 'no)) '(mine 1)))

;; Chez's record-case binds formals with car/cdr, so extra arguments are
;; ignored (trace.ss sends draw-string-letters a string and a tag; the
;; Workspace window's method takes the string only), and too few raise
(check-equal? (record-case '(msg 1 2 3) (msg (x) x) (else 'no)) 1)
(check-equal? (record-case '(msg 1 2 3) (msg () 'none) (else 'no)) 'none)
(check-equal? (record-case '(msg 1 2 3) ((msg other) (x . more) (list x more)) (else 'no))
              '(1 (2 3)))
(check-equal? (record-case '(msg 1 2) (msg all all) (else 'no)) '(1 2))
(check-exn exn:fail? (lambda () (record-case '(msg 1) (msg (x y) y) (else 'no))))

;; extend-syntax keywords are matched by name, even when bound locally
(let ([from 'f] [to 't] [each 'e] [in 'i])
  (let ([acc '()])
    (for* k from 1 to 3 do (set! acc (cons k acc)))
    (for* each x in '(a b) do (set! acc (cons x acc)))
    (check-equal? acc '(b a 3 2 1))))

;; module-level slipnet and codelet definers
(define made '())
(define (make-slipnode name short depth)
  (set! made (cons name made))
  (list 'node name short depth))
(define-slipnet-node-list* *test-nodes*
  (plato-m "m" conceptual-depth: 10)
  (plato-n "n" conceptual-depth: 30))
(check-equal? plato-m '(node plato-m "m" 10))
(check-equal? *test-nodes* (list plato-m plato-n))
(check-eq? (top-level-value 'plato-n) plato-n)
(check-equal? (reverse made) '(plato-m plato-n))
(check-equal? (symbol->letter-categories 'mnm) (list plato-m plato-n plato-m))

(define (make-codelet-type name labels)
  (let ([proc #f])
    (lambda msg
      (record-case (cdr msg)
        (set-codelet-procedure (p) (set! proc p) 'done)
        (make-codelet (urgency . args) (list name urgency args))
        (run args (apply proc args))
        (else 'invalid-message-indicator)))))
(define-codelet-type-list* *test-types* (unit-scout "Unit" "scout") (unit-builder "Unit builder"))
(check-eq? (car *test-types*) unit-scout)
(check-eq? (top-level-value 'unit-builder) unit-builder)

;; define-codelet-procedure* and fizzle across modules; say and post-codelet*
;; find %verbose%, say-object, tell and *coderack* where they are used
(define %verbose% #f)
(define posted '())
(define *coderack* (lambda msg (record-case (cdr msg) (post (c) (set! posted (cons c posted)) 'ok))))
;; (its value, 'done from the codelet type, would be printed at module level)
(void
 (define-codelet-procedure* unit-scout
   (lambda (x)
     (if (eq? x 'give-up) (fizzle))
     (post-codelet* urgency: 50 unit-builder x)
     (list 'finished x))))
(check-equal? (tell unit-scout 'run 'go) '(finished go))
(check-equal? posted '((unit-builder 50 (go))))
(check-equal? (tell unit-scout 'run 'give-up) 'done)
(check-false fizzle)
(set! %verbose% #t)
(check-equal? (with-output-to-string (lambda () (tell unit-scout 'run 'go)))
              "----------------------------------------------\nIn unit-scout codelet...\n")
(set! %verbose% #f)

;; reset: the default handler raises a metacat-reset; tell halts with it
(check-exn metacat-reset? (lambda () (reset)))
(define out (open-output-string))
(check-exn metacat-reset?
           (lambda ()
             (parameterize ([current-output-port out])
               (tell (lambda msg (if (eq? (cadr msg) 'object-type) 'gadget 'invalid-message-indicator))
                     'frobnicate))))
(check-regexp-match #rx"^Ooops: bad message \"frobnicate\" sent to object of type gadget\n$" (get-output-string out))

;; printing
(check-equal? (with-output-to-string (lambda () (printf "~a ~s~%" 1e10 "q") (newline)))
              "1e10 \"q\"\n\n")
(check-equal? (with-output-to-string (lambda () (format #t "~a" 'x))) "x")
(check-equal? (format #f "~a" 0.0001) "1e-4")
(check-exn #rx"^who: bad 1$" (lambda () (error 'who "bad ~a" 1)))
(check-exn #rx"^plain$" (lambda () (error #f "plain")))

;; mcat checks its tokens at expansion time, like the original's fender
(define *control-panel* (lambda msg (cadr (cdr msg))))
(check-equal? (mcat abc abd xyz 7) '(abc abd xyz 7))
(check-exn exn:fail:syntax?
           (lambda ()
             (parameterize ([current-namespace (make-base-namespace)])
               (namespace-require compat-path)
               (eval '(mcat abc abd)))))
