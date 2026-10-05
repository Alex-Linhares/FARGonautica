;; Item 01: the demo seeds in chez_scheme/original/demos.ss still replay.
;; Chez 10's random/random-seed is the same generator Metacat was developed
;; under, so the runs documented in demos.ss comments come out as written
;; (answer and time step).  Each run is a fresh process, so the Episodic
;; Memory starts empty, as demos.ss says it should.
;; Not checked: misc3 (documented kji/kkkjjjiii/kkjjii at 1240/1470/1485;
;; the oracle finds kkjjii 1240, kkkjjjiii 1264, kkjjii 1280, kji 1317) and the
;; commented-out "not used" misc6-misc8, which do not replay.

(define demos
  ;; (name run.ss-arguments ((answer codelet) ...))
  '((misc1 "abc cba mrrjjj mmmrrj --seed 3538780671 --max-codelets 9000"
           (("mmmrrj" 7794)))
    (misc2 "abc abd ijk abd --seed 3386544399 --max-codelets 3000"
           (("abd" 1126)))
    (misc4 "a b z --seed 3861033416 --max-codelets 1000 --keep-going"
           (("b" 453) ("y" 945)))
    (misc5 "abc abd glz --seed 1108779034 --max-codelets 1800 --keep-going"
           (("flz" 1695) ("dlz" 1710) ("hlz" 1721)))
    (misc9 "abc abd xyz --seed 692549763 --max-codelets 3000"
           (("dyz" 2257)))))

(define scheme-program
  (if (= 0 (system "command -v scheme > /dev/null 2>&1")) "scheme" "chezscheme"))

(define tmp "/tmp/metacat-demo-replay-check.out")

;; The (answer codelet) pairs of the "Answer: X  quality Q  codelet N ..."
;; lines in the output of run.ss.
(define answers-of
  (lambda (path)
    (call-with-input-file path
      (lambda (in)
        (let loop ((acc '()))
          (let ((line (get-line in)))
            (if (eof-object? line)
                (reverse acc)
                (let ((p (open-input-string line)))
                  (if (eq? (read p) 'Answer:)
                      (let* ((answer (symbol->string (read p)))
                             (_q (read p)) (_qv (read p)) (_c (read p))
                             (codelet (read p)))
                        (loop (cons (list answer codelet) acc)))
                      (loop acc))))))))))

(define failures 0)
(for-each
  (lambda (demo)
    (let* ((name (car demo)) (args (cadr demo)) (expected (caddr demo))
           (status (system (format "~a --script chez_scheme/oracle/run.ss ~a > ~a 2>&1"
                                   scheme-program args tmp)))
           (actual (if (= status 0) (answers-of tmp) 'failed)))
      (if (equal? actual expected)
          (printf "  ok ~a: ~s~%" name actual)
          (begin
            (set! failures (+ failures 1))
            (printf "FAIL: ~a (~a): expected ~s, got ~s~%" name args expected actual)))))
  demos)
(when (file-exists? tmp) (delete-file tmp))

(if (> failures 0)
    (begin (printf "demo-replay-check: ~a failures~%" failures) (exit 1))
    (printf "demo-replay-check: ~a documented demo runs replay exactly~%" (length demos)))
