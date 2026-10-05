#lang racket/base
;; Item 16: demos.ss (racket/engine/demos.rktl).
;;
;;  - every demo problem the engine defines is the original's, read from
;;    chez_scheme/original/demos.ss (the commented-out misc6-misc9 stay
;;    undefined, as in the original);
;;  - every demo problem, with its seed, is a golden run in tests/problems.txt,
;;    so golden-test.rkt checks each one event for event against the oracle;
;;  - the seed caveat (docs/demos.md): the port replays the runs whose
;;    outcome demos.ss documents and the oracle still reproduces, answer and
;;    codelet as written there.  Each run is a fresh process (racket/cli.rkt),
;;    so the Episodic Memory starts empty, as demos.ss says it should.
;;
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
(require rackunit
         racket/file
         racket/list
         racket/port
         racket/string
         racket/system
         racket/runtime-path
         (prefix-in e: "../engine.rkt"))

(define-runtime-path original-demos "../../chez_scheme/original/demos.ss")
(define-runtime-path problems-file "../../tests/problems.txt")
(define-runtime-path cli "../cli.rkt")

;; (name . problem) for every (define name '(...)) of the original demos.ss
(define original-demos-alist
  (call-with-input-file original-demos
    (lambda (in)
      (let loop ([acc '()])
        (let ([form (read in)])
          (cond
            [(eof-object? form) (reverse acc)]
            [(and (pair? form) (eq? (car form) 'define)
                  (pair? (caddr form)) (eq? (car (caddr form)) 'quote))
             (loop (cons (cons (cadr form) (cadr (caddr form))) acc))]
            [else (loop acc)]))))))

(define-runtime-path engine "../engine.rkt")
(define (engine-value name)
  (dynamic-require engine name (lambda () 'undefined)))

(test-case "the engine's demo problems are the original's"
  (check-equal? (length original-demos-alist) 35)
  (for ([d (in-list original-demos-alist)])
    (check-equal? (engine-value (car d)) (cdr d) (format "~a" (car d))))
  ;; misc6-misc9 are commented out in demos.ss
  (for ([name '(misc6 misc7 misc8 misc9)])
    (check-equal? (engine-value name) 'undefined (format "~a" name)))
  (check-true (procedure? e:demo)))

;; problems.txt: ("INITIAL MODIFIED TARGET [ANSWER]" . seeds)
(define golden-problems
  (for/list ([line (file->lines problems-file)]
             #:unless (regexp-match? #rx"^[ \t]*(#|$)" line))
    (define fields (map string-trim (string-split (car (string-split line "#")) "|")))
    (cons (string-join (string-split (first fields)) " ")
          (map string->number (string-split (second fields))))))

(test-case "every demo problem and seed is a golden run"
  (for ([d (in-list original-demos-alist)])
    (define problem (cdr d))
    (define strings (string-join (map symbol->string (drop-right problem 1)) " "))
    (define seed (last problem))
    (define entry (assoc strings golden-problems))
    (check-true (and entry (memv seed (cdr entry)) #t)
                (format "~a: ~a seed ~a in tests/problems.txt" (car d) strings seed))))

;; The documented runs that replay, as the oracle shows (docs/demos.md):
;; (name cli-arguments ((answer codelet) ...) codelets-at-the-end).
;; misc1-misc9: the outcomes written in demos.ss's comments (the oracle's
;; demo-replay-check.ss checks the same against the original).  The others:
;; the time steps Chapter 5 of the dissertation gives.  run4 finds no
;; answer; the dissertation's Run 4 gives up at 3228 too.
(define documented
  '((misc1 "abc cba mrrjjj mmmrrj --seed 3538780671 --max-codelets 9000"
           (("mmmrrj" 7794)) 7794)
    (misc2 "abc abd ijk abd --seed 3386544399 --max-codelets 3000"
           (("abd" 1126)) 1126)
    (misc4 "a b z --seed 3861033416 --max-codelets 1000 --keep-going"
           (("b" 453) ("y" 945)) 1000)
    (misc5 "abc abd glz --seed 1108779034 --max-codelets 1800 --keep-going"
           (("flz" 1695) ("dlz" 1710) ("hlz" 1721)) 1800)
    (misc9 "abc abd xyz --seed 692549763 --max-codelets 3000"
           (("dyz" 2257)) 2257)
    (run2 "xqc xqd mrrjjj mrrkkk --seed 1248075611 --max-codelets 10000"
          (("mrrkkk" 1747)) 1747)
    (run3 "rst rsu xyz uyz --seed 2330176791 --max-codelets 10000"
          (("uyz" 3163)) 3163)
    (run4 "abc abd xyz dyz --seed 2836825623 --max-codelets 10000"
          () 3228)
    (run5 "xqc xqd mrrjjj mrrjjjj --seed 3729474543 --max-codelets 10000"
          (("mrrjjjj" 4493)) 4493)
    (run7 "abc abd xyz --seed 3852097033 --max-codelets 10000"
          (("wyz" 2170)) 2170)
    (fig5.7 "aabc aabd ijkk ijll --seed 2351730219 --max-codelets 10000"
            (("ijll" 1172)) 1172)
    (fig5.8 "aabc aabd ijkk hjkk --seed 1810079903 --max-codelets 10000"
            (("hjkk" 733)) 733)))

(define (codelets-of output)
  (let ([m (regexp-match #px"(?m:^Codelets: ([0-9]+)$)" output)])
    (and m (string->number (cadr m)))))

;; the (answer codelet) of each "Answer: X  quality Q  codelet N ..." line
(define (answers-of output)
  (for/list ([m (regexp-match* #px"(?m:^Answer: ([a-z]+) +quality +[0-9]+ +codelet +([0-9]+))"
                               output #:match-select cdr)])
    (list (first m) (string->number (second m)))))

(define racket-exe (find-executable-path (find-system-path 'exec-file)))

(test-case "the documented demo runs replay in the port"
  (define results
    ;; one process per run, all at once
    (for/list ([d (in-list documented)])
      (define out (open-output-string))
      (define-values (sp stdout stdin stderr)
        (apply subprocess #f #f 'stdout racket-exe (path->string cli)
               (string-split (second d))))
      (close-output-port stdin)
      (list d sp (thread (lambda () (copy-port stdout out))) out)))
  (for ([r (in-list results)])
    (define-values (d sp th out) (apply values r))
    (subprocess-wait sp)
    (thread-wait th)
    (check-equal? (subprocess-status sp) 0 (format "~a exit status" (first d)))
    (check-equal? (answers-of (get-output-string out)) (third d)
                  (format "~a: ~a" (first d) (second d)))
    (check-equal? (codelets-of (get-output-string out)) (fourth d)
                  (format "~a: codelets at the end" (first d)))))
