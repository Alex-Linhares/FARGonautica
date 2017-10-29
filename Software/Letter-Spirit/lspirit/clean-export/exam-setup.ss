(load "loader.ss")
(load "tools.ss")
(load "role-defs.ss")
(load "whole-defs.ss")
(load "recog.ss")
(load "know.ss")
(load "r-roles.ss")
(load "gridmath.ss")
; (load "fillers.ss")
(load "norm-viols.ss")
(load "labels.ss")
(load "labeler.ss")
; (load "learn-roles.ss")
(load "examiner.ss")
(load "memory.ss")
(load "quickact.ss")
(load "topdown.ss")
(load "blurred-proto.ss")
(load "coderack.ss")
(load "get-mystery.ss")
(load "smartparse.ss")
(load "stats.ss")
(define do-graf
  (lambda ()
    (load "graphics.ss")))
; (do-graf)
(define graf-off
  (lambda ()
    (set! *graphics* #f)))
(define graf-on
  (lambda ()
    (set! *graphics* #t)))
(graf-off)
(define graf-rec
  (lambda ()
    (set! *graphics* #t)
    (run-examiner)))

(define typ
  (lambda ()
    (case (length *workspace*)
      [1 (set! part1 (car *workspace*))]
      [2 (begin
	   (set! part1 (car *workspace*)) (set! part2 (cadr *workspace*)))]
      [3 (begin
	   (set! part1 (car *workspace*))
	   (set! part2 (cadr *workspace*))
	   (set! part3 (caddr *workspace*)))])))

