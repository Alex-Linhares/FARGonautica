;;=============================================================================
;; Copyright (c) 1999, 2003 by James B. Marshall
;;
;; This file is part of Metacat.
;;
;; Metacat is based on Copycat, which was originally written in Common
;; Lisp by Melanie Mitchell.
;;
;; Metacat is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.
;;
;; Metacat is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;=============================================================================

;; Demo runs from Chapter 5 of Jim Marshall's PhD thesis,
;; "Metacat: A Self-Watching Cognitive Architecture for
;; Analogy-Making and High-Level Perception", Department
;; of Computer Science, Indiana University, 1999.

(define demo
  (lambda (problem)
    (tell *control-panel* 'run-new-problem problem)))

;; Type (demo run1) to run the first demo, etc.

;;-------------------------------------------------------
;; Sample runs (section 5.2)

;; Note: To reproduce Runs 1-8 exactly as described in
;; Chapter 5, the Episodic Memory should be cleared at
;; the beginning of each run.

(define run1 '(abc abd mrrjjj mrrjjjj 1092119323))
(define run2 '(xqc xqd mrrjjj mrrkkk 1248075611))
(define run3 '(rst rsu xyz uyz 2330176791))
(define run4 '(abc abd xyz dyz 2836825623))
(define run5 '(xqc xqd mrrjjj mrrjjjj 3729474543))
(define run6 '(eqe qeq abbbc aaabccc 789090523))
(define run7 '(abc abd xyz 3852097033))
(define run8 '(eqe qeq abbbc 3557912874))

;;-------------------------------------------------------
;; Answer comparison and reminding (section 5.2.3)

;; xyz family
(define abc-xyd '(abc abd xyz xyd 1760747975))
(define abc-wyz '(abc abd xyz wyz 3100511611))
(define abc-dyz '(abc abd xyz dyz 2107869027))
(define rst-xyu '(rst rsu xyz xyu 939480183))
(define rst-wyz '(rst rsu xyz wyz 720286361))
(define rst-uyz '(rst rsu xyz uyz 2330176791))

;; mrrjjj family
(define abc-mrrkkk '(abc abd mrrjjj mrrkkk 4211806334))
(define abc-mrrjjjj '(abc abd mrrjjj mrrjjjj 1092119323))
(define xqc-mrrkkk '(xqc xqd mrrjjj mrrkkk 1248075611))
(define xqc-mrrjjjj '(xqc xqd mrrjjj mrrjjjj 3729474543))

;; eqe family
(define eqe-baaab '(eqe qeq abbba baaab 3635369418))
(define eqe-aaabaaa '(eqe qeq abbba aaabaaa 4209674874))
(define eqe-qeeeq '(eqe qeq abbbc 2302461154))
(define eqe-aaabccc '(eqe qeq abbbc aaabccc 789090523))

;;-------------------------------------------------------
;; Implausible rules (section 5.3.1)

(define fig5.4-top '(eeqee qeeq xxixx 698282038))
(define fig5.4-bottom '(eeqee qeeq xxixx 175910650))
(define fig5.5-top '(eeqee qeeq xxixx 698282038))
(define fig5.5-bottom '(eeqee qeeq xxixx 4109591222))

;;-------------------------------------------------------
;; Poor thematic characterizations (section 5.3.2)

(define fig5.7 '(aabc aabd ijkk ijll 2351730219))
(define fig5.8 '(aabc aabd ijkk hjkk 1810079903))
(define fig5.10 '(abc abd xyz 3009318743))
(define fig5.11 '(abc abd xyz 2006188493))

;;-------------------------------------------------------
;; Other sample runs, not discussed in the dissertation.

;; Justifies mmmrrj (7794 time steps).
(define misc1 '(abc cba mrrjjj mmmrrj 3538780671))

;; Describes a and b as "changing" in abc->abd (1126 time steps).
(define misc2 '(abc abd ijk abd 3386544399))

;; Answers kji at time 1240 (applying succgrp=>predgrp slippage),
;; kkkjjjiii at time 1470 (not applying the slippage), kkjjii at
;; 1485 (using a literal rule).
(define misc3 '(abc aabbcc kkjjii 912835776))

;; First answers b, then y on account of a coattail slippage
;; induced by a first=>last slippage (945 time steps).
(define misc4 '(a b z 3861033416))

;; Answers flz due to a coattail slippage, then dlz, then hlz
;; at time step 1721.
(define misc5 '(abc abd glz 1108779034))

;;-------------------------------------------------------
;; not used

;;;; answers qbbbq due to lack of a middle description,
;;;; then qeeeq, then qcccb due to a stupid rule (time 2782)
;;(define misc6 '(eqe qeq abbbc 4089168737))

;;;; justifies xbbbx (1945 time steps)
;;(define misc7 '(eqe qeq bxxxb xbbbx 3227071918))

;;;; clamps several times, settles for unjustified bbbxbbb (time 5888)
;;(define misc8 '(eqe qeq bxxxb bbbxbbb 4244517374))

;;;; snags, maps abc-xyz symmetrically, answers dyz (time 2257)
;;(define misc9 '(abc abd xyz 692549763))
