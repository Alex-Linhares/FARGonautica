;; Item 00: trivial Chez check. Chez 10 is the expected version, and the
;; Chez reader accepts every source file in chez_scheme/original/ (the
;; files are only read, never loaded or modified).
(define version (scheme-version))
(unless (and (>= (string-length version) 22)
             (string=? (substring version 0 22) "Chez Scheme Version 10"))
  (printf "FAIL: expected Chez Scheme 10, got ~a~%" version)
  (exit 1))

(define original-dir "chez_scheme/original")

(define read-all-forms
  (lambda (path)
    (call-with-input-file path
      (lambda (in)
        (let loop ((n 0))
          (if (eof-object? (read in)) n (loop (+ n 1))))))))

(define files
  (filter (lambda (f) (let ((n (string-length f)))
                        (and (> n 3) (string=? (substring f (- n 3) n) ".ss"))))
          (directory-list original-dir)))

(unless (= (length files) 45)
  (printf "FAIL: expected 45 .ss files in ~a, found ~a~%" original-dir (length files))
  (exit 1))

(define total
  (apply + (map (lambda (f) (read-all-forms (string-append original-dir "/" f))) files)))

(printf "reader-check: ~a read ~a top-level forms from ~a files~%"
        version total (length files))
