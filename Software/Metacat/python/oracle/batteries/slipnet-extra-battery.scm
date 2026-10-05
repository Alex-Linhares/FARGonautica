;;; slipnet-extra-battery.scm -- images.ss and slipnet.ss cases that
;;; tests/diff/slipnet-battery.scm does not reach (loop0002 item 05).
;;;
;;; Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Captured like tests/diff/*-battery.scm by python/oracle/capture.py:
;;; chez_scheme/oracle/diff-eval.ss evaluates tests/diff/helpers.scm, then this
;;; file, with the original loaded.  The Python side is the end of
;;; python/tests/test_slipnet.py.  Same rules as the frozen batteries: no two
;;; side-effecting subexpressions in one call or one `let'.  Nothing here draws
;;; a random number or a window.

(define x:try
  (lambda (f)
    (call-with-current-continuation
      (lambda (k) (f (lambda () (k 'failed)))))))

;; nested lists of slipnodes as names
(define x:names
  (lambda (x)
    (cond
      ((pair? x) (let* ((a (x:names (car x))) (d (x:names (cdr x)))) (cons a d)))
      ((procedure? x) (tell x 'get-name-symbol))
      (else x))))

(define x:letter-images
  (lambda (nodes) (map make-letter-image nodes)))

(define x:abcde
  (lambda ()
    (make-image plato-a plato-letter-category plato-successor plato-identity plato-right
                (x:letter-images (list plato-a plato-b plato-c plato-d plato-e)))))

;; replace-all on a group image whose k-th new letter is #f: the images that
;; map reached before the failing one keep their new letters, so the result
;; shows map's order of application in images.ss's replace-all
(test image-replace-all-fail
  (map (lambda (k)
         (let* ((image (x:abcde))
                (args (map (lambda (i) (if (= i k) #f (list-ref *slipnet-letters* (+ i 20))))
                           (list 0 1 2 3 4)))
                (r (x:try (lambda (fail) (tell image 'replace-all 'new-start-letter args fail)))))
           (list k r (x:names (tell image 'generate)))))
       (list 0 1 2 3 4)))

;; extend with a length argument that is changed first (predecessor, or a
;; smaller number than the copied sub-image's length) or not: the order of
;; new-length and new-start-letter decides whether xyz can be extended
(define x:xyz-of-groups
  (lambda ()
    (make-image plato-x plato-length plato-identity plato-identity plato-right
                (list (make-image plato-x plato-letter-category plato-successor plato-identity
                                  plato-right
                                  (x:letter-images (list plato-x plato-y plato-z)))))))

(test extend-length-first
  (map (lambda (args)
         (let* ((image (x:xyz-of-groups))
                (r (x:try (lambda (fail) (tell image 'extend (car args) (cadr args) fail)))))
           (list (x:names args) r (x:names (tell image 'generate))
                 (x:names (tell image 'get-letter-relation))
                 (x:names (tell image 'get-length-relation)))))
       (list (list plato-successor plato-predecessor)
             (list plato-successor plato-one)
             (list plato-successor plato-two)
             (list plato-successor plato-three)
             (list plato-successor plato-identity)
             (list plato-predecessor plato-predecessor)
             (list plato-identity plato-one)
             (list plato-a plato-two))))

(test number-to-platonic-number-range
  (x:names (list (number->platonic-number 5) (number->platonic-number 6))))

;; list-ref of -1 is an error in Chez (Python's l[-1] is not)
(test number-to-platonic-number-zero (number->platonic-number 0))
