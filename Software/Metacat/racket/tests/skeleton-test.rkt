#lang racket/base
;; Item 00: the skeleton exists and the entry point loads without a display.
(require rackunit
         "../main.rkt")

(check-equal? metacat-version "1.2")
(check-pred procedure? main)
