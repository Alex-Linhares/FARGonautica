;;; make-golden.ss -- write the golden traces tests/golden/*.jsonl.
;;;
;;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;;   scheme --script chez_scheme/oracle/make-golden.ss            # (re)write tests/golden/
;;;   scheme --script chez_scheme/oracle/make-golden.ss --check    # compare, write nothing
;;;
;;; Run from the repository root.  Reads tests/problems.txt; for every
;;; problem and seed there, runs the original headless (run.ss --trace, one
;;; fresh Chez process per run, several in parallel) and writes the trace
;;; to tests/golden/<strings joined by ->_<seed>.jsonl, e.g.
;;; tests/golden/abc-abd-xyz_3852097033.jsonl.  Writing removes any other
;;; .jsonl file in tests/golden/.  With --check, the traces are written to
;;; a temporary directory and compared byte for byte with tests/golden/;
;;; any difference, missing or extra file makes it exit 1.
;;;
;;; Golden files come only from here; they are never edited by hand, and
;;; never regenerated to make a failing port pass.
;;;
;;; tests/problems.txt: one problem per line, `#' starts a comment.
;;;   INITIAL MODIFIED TARGET [ANSWER] | SEED ... | CAP [| keep-going]
;;; CAP is run.ss's --max-codelets; keep-going continues past answers
;;; (run.ss --keep-going) until CAP.  The same strings may appear on several
;;; lines (with different seeds or flags); a strings+seed pair only once.

(define scheme-program
  (if (= 0 (system "command -v scheme > /dev/null 2>&1")) "scheme" "chezscheme"))

(define problems-file "tests/problems.txt")
(define golden-dir "tests/golden")

(define check? (member "--check" (command-line-arguments)))

(define fail
  (lambda (fmt . args)
    (apply fprintf (current-error-port) (string-append "make-golden: " fmt "~%") args)
    (exit 1)))

(unless (file-exists? problems-file)
  (fail "~a not found (run from the repository root)" problems-file))

;;; Parsing

(define read-lines
  (lambda (path)
    (call-with-input-file path
      (lambda (in)
        (let loop ((acc '()))
          (let ((line (get-line in)))
            (if (eof-object? line) (reverse acc) (loop (cons line acc)))))))))

(define split
  (lambda (s ch)
    (let loop ((i 0) (start 0) (acc '()))
      (cond
        ((= i (string-length s)) (reverse (cons (substring s start i) acc)))
        ((char=? (string-ref s i) ch) (loop (+ i 1) (+ i 1) (cons (substring s start i) acc)))
        (else (loop (+ i 1) start acc))))))

(define words
  (lambda (s)
    (filter (lambda (w) (not (string=? w ""))) (split (list->string (map (lambda (c) (if (char=? c #\tab) #\space c)) (string->list s))) #\space))))

(define strip-comment
  (lambda (line)
    (let loop ((i 0))
      (cond ((= i (string-length line)) line)
            ((char=? (string-ref line i) #\#) (substring line 0 i))
            (else (loop (+ i 1)))))))

(define join
  (lambda (strs sep)
    (if (null? strs) ""
        (fold-left (lambda (acc s) (string-append acc sep s)) (car strs) (cdr strs)))))

(define positive-integer
  (lambda (s line)
    (let ((n (string->number s)))
      (if (and n (exact? n) (integer? n) (> n 0)) n
          (fail "bad number ~s in line: ~a" s line)))))

;; Each run: (file-name strings seed cap keep-going?)
(define runs
  (let loop ((lines (read-lines problems-file)) (acc '()))
    (if (null? lines)
        (reverse acc)
        (let* ((line (car lines))
               (fields (map words (split (strip-comment line) #\|))))
          (cond
            ((andmap null? fields) (loop (cdr lines) acc))
            ((not (memv (length fields) '(3 4)))
             (fail "expected STRINGS | SEEDS | CAP [| keep-going]: ~a" line))
            (else
             (let ((strings (car fields))
                   (seeds (map (lambda (s) (positive-integer s line)) (cadr fields)))
                   (cap (if (= (length (caddr fields)) 1)
                            (positive-integer (car (caddr fields)) line)
                            (fail "expected one codelet cap: ~a" line)))
                   (keep? (and (= (length fields) 4)
                               (if (equal? (cadddr fields) '("keep-going")) #t
                                   (fail "unknown flag in line: ~a" line)))))
               (unless (memv (length strings) '(3 4))
                 (fail "expected 3 or 4 strings: ~a" line))
               (when (null? seeds) (fail "no seeds: ~a" line))
               (loop (cdr lines)
                     (append (reverse
                               (map (lambda (seed)
                                      (list (format "~a_~a.jsonl" (join strings "-") seed)
                                            strings seed cap keep?))
                                    seeds))
                             acc)))))))))

(let loop ((names (map car runs)))
  (unless (null? names)
    (when (member (car names) (cdr names))
      (fail "~a listed twice in ~a" (car names) problems-file))
    (loop (cdr names))))

;;; Running

(define out-dir
  (if check?
      (format "/tmp/metacat-golden-check-~a" (random 1000000000))
      golden-dir))

(define shell
  (lambda (fmt . args)
    (system (apply format fmt args))))

(shell "mkdir -p ~a" out-dir)
(unless check? (shell "rm -f ~a/*.jsonl" out-dir))

(define commands-file (format "~a/.commands" out-dir))
(call-with-output-file commands-file
  (lambda (p)
    (for-each
      (lambda (run)
        (let ((name (car run)) (strings (cadr run)) (seed (caddr run))
              (cap (cadddr run)) (keep? (car (cddddr run))))
          (fprintf p "~a --script chez_scheme/oracle/run.ss ~a --seed ~a --max-codelets ~a~a --trace ~a/~a > /dev/null || echo 'make-golden: FAILED: ~a'~%"
                   scheme-program (join strings " ") seed cap (if keep? " --keep-going" "")
                   out-dir name name)))
      runs))
  'replace)

(define jobs
  (let ((tmp (format "~a/.nproc" out-dir)))
    (shell "nproc > ~a 2>/dev/null || echo 4 > ~a" tmp tmp)
    (let ((n (call-with-input-file tmp read)))
      (delete-file tmp)
      (if (and (integer? n) (> n 0)) n 4))))

(define log-file (format "~a/.log" out-dir))
(define status
  (shell "xargs -d '\\n' -P ~a -n 1 sh -c < ~a > ~a 2>&1" jobs commands-file log-file))
(define log (call-with-input-file log-file (lambda (in) (get-string-all in))))
(delete-file commands-file)
(delete-file log-file)
(unless (and (= status 0) (or (eof-object? log) (string=? log "")))
  (display (if (eof-object? log) "" log) (current-error-port))
  (when check? (shell "rm -rf ~a" out-dir))
  (fail "some runs failed"))

;;; Checking

(define golden-files
  (let ((tmp (format "/tmp/metacat-golden-ls-~a" (random 1000000000))))
    (shell "ls ~a 2>/dev/null | grep '\\.jsonl$' > ~a" golden-dir tmp)
    (let ((names (read-lines tmp)))
      (delete-file tmp)
      names)))

(if check?
    (let ((problems 0))
      (for-each
        (lambda (run)
          (let ((name (car run)))
            (cond
              ((not (member name golden-files))
               (set! problems (+ problems 1))
               (printf "missing from ~a: ~a~%" golden-dir name))
              ((not (= 0 (shell "cmp ~a/~a ~a/~a" golden-dir name out-dir name)))
               (set! problems (+ problems 1))
               (printf "differs: ~a/~a~%" golden-dir name)))))
        runs)
      (for-each
        (lambda (name)
          (unless (assoc name runs)
            (set! problems (+ problems 1))
            (printf "not in ~a: ~a/~a~%" problems-file golden-dir name)))
        golden-files)
      (shell "rm -rf ~a" out-dir)
      (if (> problems 0)
          (fail "~a of ~a golden traces do not match" problems (length runs))
          (printf "make-golden: all ~a golden traces reproduced byte for byte~%" (length runs))))
    (printf "make-golden: wrote ~a traces to ~a/~%" (length runs) golden-dir))
(exit 0)
