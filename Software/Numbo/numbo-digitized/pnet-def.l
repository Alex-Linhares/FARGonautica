;; May  5 11:13 1987  pnet-def.l Page 1

; PNODE FLAVOR
(defflavor pnode
           (activation
            spreadable-activation
            temp-activation-holder
            neighbors
            instances
            name
            type
            value
            codelets
            short-name ; for graphics
            x-region
            y-region
            x-box
            y-box
            x-act
            y-act
            x-name
            y-name
            old-boxsize)

            ()
; Pnet node      - activation is the level of activation
;                - spreadable-activation is the level of activation times a
;                                              decay factor
;                - temp-activation-holder is a temporary storage of activation
;                                              received
;                - neigbors are the neighbors and the correspond type of link
;                - instances are the instances in the cyto
;                - name is the full name of the node
;                - type is the type of the node (num,op,other)
;                - value is the possible value associated with the node
;                - codelets are the codelets attached to the node
            :settable-instance-variables
            :inittable-instance-variables
            :gettable-instance-variables)

;INIT PNET
(defun init-pnet ()
; initializes the pnet.
  (setq node-1 (make-instance 'pnode
                 :value 1
                 :neighbors
                    '((plus1-1 result+)
                      (plus1-2 result+)
                      (plus1-3 result+)
                      (plus1-4 result+)
                      (plus1-5 result+)
                      (plus1-6 result+)
                      (plus1-7 result+)
                      (plus1-8 result+)
                      (plus1-9 result+))
                 :name 'one
                 :short-name "1"
;;
;; May  5 11:13 1987  pnet-def.l Page 2
;;
                 :type 'num))
  (setq node-2 (make-instance 'pnode
                 :value 2
                 :neighbors
                    '((plus1-2 result+)
                      (plus2-2 result+)
                      (plus2-3 result+)
                      (plus2-4 result+)
                      (p1us2-5 result+)
                      (plus2-6 result+)
                      (plus2-7 result+)
                      (plus2-8 result+)
                      (times2-2 operand)
                      (times2-3 operand)
                      (times2-4 operand)
                      (times2-5 operand)
                      (times2-7 operand)
                      (times2-10 operand)
                      (times2-20 operand)
                      (times2-12 operand)
                      (plus1-1 result+))
                 :name 'two
                 :short-name "2"
                 :type 'num))
  (setq node-3 (make-instance 'pnode
                 :value 3
                 :neighbors
                    '((plus1-3 result+)
                      (plus2-3 result+)
                      (plus3-3 result+)
                      (plus3-4 result+)
                      (plus3-5 result+)
                      (plus3-6 result+)
                      (plus3-7 operand)
                      (times2-3 operand)
                      (times3-3 operand)
                      (times3-10 operand)
                      (times3-20 operand)
                      (plus1-2 result+)
                      (node-4 similar))
                 :name 'three
                 :short-name "3"
                 :type 'num))
  (setq node-4 (make-instance 'pnode
                 :value 4
                 :neighbors
                    '((plus1-4 result+)
                      (plus2-4 result+)
                      (plus3-4 result+)
                      (plus4-4 result+)
                      (plus4-5 result+)
                      (plus4-6 result+)
                      (times2-4 operand)
                      (times4-4 operand)
                      (times4-10 operand)
                      (times4-20 operand)
;;
;; May  5 11:13 1987  pnet-def.l Page 3
;;
                      (plus1-3 result+)
                      (plus2-2 result+)
                      (times2-2 resultx)
                      (node-3 similar)
                      (node-5 similar))
                 :name 'four
                 :short-name "4"
                 :type 'num))

  (setq node-5 (make-instance 'pnode
                 :value 5
                 :neighbors
                    '((plus1-5 result+)
                      (plus2-5 result+)
                      (plus3-5 result+)
                      (plus4-5 result+)
                      (plus5-5 result+)
                      (plus5-10 result+)
                      (times2-5 operand)
                      (times5-5 operand)
                      (times5-6 operand)
                      (times5-10 operand)
                      (times5-20 operand)
                      (plus1-4 result+)
                      (plus2-3 result+)
                      (node-4 similar)
                      (node-6 similar))
                 :name 'five
                 :short-name "5"
                 :type 'num))

  (setq node-6 (make-instance 'pnode
                 :value 6
                 :neighbors
                   '((plus1-6 result+)
                     (plus2-6 result+)
                     (plus3-6 result+)
                     (plus4-6 result+)
                     (plus1-5 result+)
                     (plus2-4 result+)
                     (plus3-3 result+)
                     (times2-3 resultx)
                     (times5-6 operand)
                     (times6-10 operand)
                     (node-5 similar)
                     (node-7 similar))
                 :name 'six
                 :short-name "6"
                 :type 'num))

  (setq node-7 (make-instance 'pnode
                 :value 7
                 :neighbors
                    '((plus1-7 result+)
                      (plus2-7 result+)
                      (plus3-7 result+)
;;
;; May  5 11:13 1987  pnet-def.l Page 4
;;
                      (plus7-8 result+)
                      (times2-7 operand)
                      (times7-7 operand)
                      (times7-10 operand)
                      (plus1-6 result+)
                      (plus2-5 result+)
                      (plus3-4 result+)
                      (node-6 similar)
                      (node-8 similar))
                :name 'seven
                :short-name "7"
                :type 'num))

  (setq node-8 (make-instance 'pnode
                :value 8
                :neighbors
                   '((plus1-8 result+)
                     (plus2-8 result+)
                     (plus1-7 result+)
                     (plus2-6 result+)
                     (plus3-5 result+)
                     (plus4-4 result+)
                     (plus7-8 result+)
                     (times2-4 result+)
                     (times8-10 operand)
                     (node-7 similar)
                     (node-9 similar))
                :name 'eight
                :short-name "8"
                :type 'num))

 (setq node-9 (make-instance 'pnode
               :value 9
               :neighbors
                  '((plus1-9 result+)
                    (times9-9 operand)
                    (times9-10 operand)
                    (plus1-8 result+)
                    (plus2-7 result+)
                    (plus3-6 result+)
                    (plus4-5 result+)
                    (times3-3 resultx)
                    (node-8 similar)
                    (node-10 similar))
               :name 'nine
               :short-name "9"
               :type 'num))

 (setq node-10 (make-instance 'pnode
               :value 10
               :neighbors
                  '((times2-10 operand)
                    (times3-10 operand)
                    (times4-10 operand)
                    (times5-10 operand)
                    (times6-10 operand)
;;
;; May  5 11:13 1987  pnet-def.l Page 5
;;
                    (times7-10 operand)
                    (times8-10 operand)
                    (times9-10 operand)
                    (times10-10 operand)
                    (times10-15 operand)
                    (plus1-9 result+)
                    (plus2-8 result+)
                    (plus3-7 result+)
                    (plus4-6 result+)
                    (plus5-5 result+)
                    (plus5-10 result+)
                    (times2-5 resultx)
                    (node-9 similar))
               :name 'ten
               :short-name "10"
               :type 'num))

 (setq node-12 (make-instance 'pnode
               :value 12
               :neighbors
                  '((times2-12 operand)
                    (node-15 similar))
               :name 'twelfe
               :short-name "12"
               :type 'num))

 (setq node-15 (make-instance 'pnode
               :value 15
               :neighbors
                  '((plus7-8 result+)
                    (plus5-10 result+)
                    (times2-7 similar)
                    (times10-15 operand)
                    (node-12 similar)
                    (node-16 similar)
                    (node-20 similar))
               :name 'fifteen
               :short-name "15"
               :type 'num))

 (setq node-16 (make-instance 'pnode
               :value 16
               :neighbors
                  '((times4-4 resultx))
               :name 'sixteen
               :short-name "16"
               :type 'num))

 (setq node-20 (make-instance 'pnode
               :value 20
               :neighbors
                  '((times2-20 operand)
                    (times3-20 operand)
                    (times4-20 operand)
                    (times5-20 operand)
                    (times2-10 operand)
;;
;; May  5 11:13 1987  pnet-def.l Page 6
;;
                    (node-15 similar)
                    (node-25 similar))
               :name 'twenty
               :short-name "20"
               :type 'num))

 (setq node-25 (make-instance 'pnode
               :value 25
               :neighbors
                  '((times5-5 resultx)
                    (times2-12 similar)
                    (node-20 similar)
                    (node-30 similar))
               :name 'twenty-five
               :short-name "25"
               :type 'num))

 (setq node-30 (make-instance 'pnode
               :value 30
               :neighbors
                  '((times3-10 resultx)
                    (times5-6 resultx)
                    (node-25 similar)
                    (node-40 similar))
               :name 'thirty
               :short-name "30"
               :type 'num))

 (setq node-40 (make-instance 'pnode
               :value 40
               :neighbors
                  '((times4-10 resultx)
                    (times2-20 resultx)
                    (node-30 similar)
                    (node-50 similar))
               :name 'fourty
               :short-name "40"
               :type 'num))

 (setq node-50 (make-instance 'pnode
               :value 50
               :neighbors
                  '((times5-10 resultx)
                    (times7-7 similar)
                    (node-40 similar)
                    (node-60 similar))
               :name 'fifty
               :short-name "50"
               :type 'num))

 (setq node-60 (make-instance 'pnode
               :value 60
               :neighbors
                  '((times6-10 resultx)
                    (times3-20 resultx)
                    (node-50 similar)
;;
;; May  5 11:13 1987  pnet-def.l Page 7
;;
                    (node-70 similar))
               :name 'sixty
               :short-name "60"
               :type 'num))

 (setq node-70 (make-instance 'pnode
               :value 70
               :neighbors
                  '((times7-10 resultx)
                    (node-60 similar)
                    (node-80 similar))
               :name 'seventy
               :short-name "70"
               :type 'num))

 (setq node-80 (make-instance 'pnode
               :value 80
               :neighbors
                  '((times8-10 resultx)
                    (times4-20 resultx)
                    (node-70 similar)
                    (node-81 similar)
                    (node-90 similar))
               :name 'eighty
               :short-name "80"
               :type 'num))

 (setq node-81 (make-instance 'pnode
               :value 81
               :neighbors
                  '((times9-9 resultx)
                    (node-80 similar))
               :name 'eighty-one
               :short-name "81"
               :type 'num))

 (setq node-90 (make-instance 'pnode
               :value 90
               :neighbors
                  '((times9-10 resultx)
                    (node-80 similar)
                    (node-100 similar))
               :name 'ninety
               :short-name "90"
               :type 'num))

 (setq node-100 (make-instance 'pnode
               :value 100
               :neighbors
                  '((times10-10 resultx)
                    (times5-20 resultx)
                    (node-90 similar)
                    (node-150 similar))
               :name 'one-hundred
               :short-name "100"
               :type 'num))

;;
;; May  5 11:13 1987  pnet-def.l Page 8
;;

 (setq node-150 (make-instance 'pnode
               :value 150
               :neighbors
                  '((times10-15 resultx)
                    (node-100 similar))
               :name 'one-hundred-fifty
               :short-name "150"
               :type 'num))

 (setq node-mu10 (make-instance 'pnode
                :neighbors
                   '((node-10 instance))
                :name 'multiple-of-ten
                :short-name "mu10"))

 (setq node-multiply (make-instance 'pnode
                :neighbors
                   '((node-add operation)
                     (node-subtract operation))
                :short-name "mult"
                :name 'multiply))

 (setq node-add (make-instance 'pnode
                :neighbors
                   '((node-multiply operation)
                     (node-subtract operation))
                :short-name "add"
                :name 'add))

 (setq node-subtract (make-instance 'pnode
                :neighbors
                   '((node-multiply operation)
                     (node-add operation))
                :short-name "sub"
                :name 'subtraction
                :codelets
                   '((look-for-diff %first-threshold%
                         %first-urgency% (0)))))

 (setq plus1-1 (make-instance 'pnode
                :neighbors
                   '((node-1 result+)
                     (node-2 result+))
                :name 'plus1-1
                :short-name "1+1"
                :codelets
                   '((look-for-bl+ %first-threshold%
                          %second-urgency% (2 1 1)))))

 (setq plus1-2 (make-instance 'pnode
                :neighbors
                   '((node-1 result+)
                     (node-2 result+)
                     (node-3 result+))
;;
;; May  5 11:13 1987  pnet-def.l Page 9
;;
                :name 'plus1-2
                :short-name "1+2"
                :codelets
                   '((look-for-bl+ %first-threshold%
                          %second-urgency% (3 1 2)))))

 (setq plus1-3 (make-instance 'pnode
                :neighbors
                   '((node-1 result+)
                     (node-3 result+)
                     (node-4 result+))
                :name 'plus1-3
                :short-name "1+3"
                :codelets
                   '((look-for-bl+ %first-threshold%
                          %second-urgency% (4 1 3)))))

 (setq plus1-4 (make-instance 'pnode
                :neighbors
                   '((node-1 result+)
                     (node-4 result+)
                     (node-5 result+))
                :name 'plus1-4
                :short-name "1+4"
                :codelets
                   '((look-for-bl+ %first-threshold%
                          %second-urgency% (5 1 4)))))

 (setq plus1-5 (make-instance 'pnode
               :neighbors
                  '((node-1 result+)
                    (node-5 result+)
                    (node-6 result+))
               :name 'plus1-5
               :short-name "1+5"
               :codelets
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (6 1 5)))))

 (setq plus1-6 (make-instance 'pnode
               :neighbors
                  '((node-1 result+)
                    (node-6 result+)
                    (node-7 result+))
               :name 'plus1-6
               :short-name "1+6"
               :codelets
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (7 1 6)))))

 (setq plus1-7 (make-instance 'pnode
               :neighbors
                  '((node-1 result+)
                    (node-7 result+)
                    (node-8 result+))
               :name 'plus1-7
;;
;; May  5 11:13 1987  pnet-def.l Page 10
;;
               :short-name "1+7"
               :codelets
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (8 1 7)))))

 (setq plus1-8 (make-instance 'pnode
               :neighbors
                  '((node-1 result+)
                    (node-8 result+)
                    (node-9 result+))
               :name 'plus1-8
               :short-name "1+8"
               :codelets
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (9 1 8)))))

 (setq plus1-9 (make-instance 'pnode
               :neighbors
                  '((node-1 result+)
                    (node-9 result+)
                    (node-10 result+))
               :name 'plus1-9
               :short-name "1+9"
               :codelets
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (10 1 9)))))

 (setq plus2-2 (make-instance 'pnode
               :neighbors
                  '((node-2 result+)
                    (node-4 result+))
               :name 'plus2-2
               :short-name "2+2"
               :codelets
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (4 2 2)))))

 (setq plus2-3 (make-instance 'pnode
               :neighbors
                  '((node-2 result+)
                    (node-3 result+)
                    (node-5 result+))
               :name 'plus2-3
               :short-name "2+3"
               :codelets
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (5 2 3)))))

 (setq plus2-4 (make-instance 'pnode
               :neighbors
                  '((node-2 result+)
                    (node-4 result+)
                    (node-6 result+))
               :name 'plus2-4
;;
;; May  5 11:13 1987  pnet-def.l Page 11
;;
               :short-name "2+4"
               :codelets
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (6 2 4)))))

 (setq plus2-5 (make-instance 'pnode
               :neighbors
                  '((node-2 result+)
                    (node-5 result+)
                    (node-7 result+))
               :name 'plus2-5
               :short-name "2+5"
               :codelets
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (7 2 5)))))

 (setq plus2-6 (make-instance 'pnode
               :neighbors
                  '((node-2 result+)
                    (node-6 result+)
                    (node-8 result+))
               :name 'plus2-6
               :short-name "2+6"
               :codelets
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (8 2 6)))))

 (setq plus2-7 (make-instance 'pnode
               :neighbors
                  '((node-2 result+)
                    (node-7 result+)
                    (node-9 result+))
               :name 'plus2-7
               :short-name "2+7"
               :codelets
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (9 2 7)))))

 (setq plus2-8 (make-instance 'pnode
               :neighbors
                  '((node-2 result+)
                    (node-8 result+)
                    (node-10 result+))
               :name 'plus2-8
               :short-name "2+8"
               :codelets
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (10 2 8)))))

 (setq plus3-3 (make-instance 'pnode
               :neighbors
                  '((node-3 result+)
                    (node-6 result+))
               :name 'plus3-3
               :short-name "3+3"
               :codelets
;;
;; May  5 11:13 1987  pnet-def.l Page 12
;;
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (6 3 3)))))

 (setq plus3-4 (make-instance 'pnode
               :neighbors
                  '((node-3 result+)
                    (node-4 result+)
                    (node-7 result+))
               :name 'plus3-4
               :short-name "3+4"
               :codelets
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (7 3 4)))))

 (setq plus3-5 (make-instance 'pnode
               :neighbors
                  '((node-3 result+)
                    (node-5 result+)
                    (node-8 result+))
               :name 'plus3-5
               :short-name "3+5"
               :codelets
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (8 3 5)))))

 (setq plus3-6 (make-instance 'pnode
               :neighbors
                  '((node-3 result+)
                    (node-6 result+)
                    (node-9 result+))
               :name 'plus3-6
               :short-name "3+6"
               :codelets
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (9 3 6)))))

 (setq plus3-7 (make-instance 'pnode
               :neighbors
                  '((node-3 result+)
                    (node-7 result+)
                    (node-10 result+))
               :name 'plus3-7
               :short-name "3+7"
               :codelets
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (10 3 7)))))

 (setq plus4-4 (make-instance 'pnode
               :neighbors
                  '((node-4 result+)
                    (node-8 result+))
               :name 'plus4-4
               :short-name "4+4"
               :codelets
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (8 4 4)))))

;;
;; May  5 11:13 1987  pnet-def.l Page 13
;;

 (setq plus4-5 (make-instance 'pnode
               :neighbors
                  '((node-4 result+)
                    (node-5 result+)
                    (node-9 result+))
               :name 'plus4-5
               :short-name "4+5"
               :codelets
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (9 4 5)))))

 (setq plus4-6 (make-instance 'pnode
               :neighbors
                  '((node-4 result+)
                    (node-6 result+)
                    (node-10 result+))
               :name 'plus4-6
               :short-name "4+6"
               :codelets
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (10 4 6)))))

 (setq plus5-5 (make-instance 'pnode
               :neighbors
                  '((node-5 result+)
                    (node-10 result+))
               :name 'plus5-5
               :short-name "5+5"
               :codelets
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (10 5 5)))))

 (setq plus7-8 (make-instance 'pnode
               :neighbors
                  '((node-7 result+)
                    (node-8 result+)
                    (node-15 result+))
               :name 'plus7-8
               :short-name "7+8"
               :codelets
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (15 7 8)))))

 (setq plus5-10 (make-instance 'pnode
               :neighbors
                  '((node-5 result+)
                    (node-10 result+)
                    (node-15 result+))
               :name 'plus5-10
               :short-name "5+10"
               :codelets
                  '((look-for-bl+ %first-threshold%
                         %second-urgency% (15 5 10)))))

;;
;; May  5 11:13 1987  pnet-def.l Page 14
;;

 (setq times2-2 (make-instance 'pnode
               :neighbors
                  '((node-2 operand)
            (node-4 resultx))
               :name 'times2-2
               :short-name "2x2"
               :codelets
                  '((look-for-blx %first-threshold%
                         %second-urgency% (4 2 2)))))

 (setq times2-3 (make-instance 'pnode
               :neighbors
                  '((node-2 operand)
                    (node-3 operand)
                    (node-6 resultx))
               :name 'times2-3
               :short-name "2x3"
               :codelets
                  '((look-for-blx %first-threshold%
                         %second-urgency% (6 2 3)))))

 (setq times2-4 (make-instance 'pnode
               :neighbors
                  '((node-2 operand)
                    (node-4 operand)
                    (node-8 resultx))
               :name 'times2-4
               :short-name "2x4"
               :codelets
                  '((look-for-blx %first-threshold%
                         %second-urgency% (8 2 4)))))

 (setq times2-5 (make-instance 'pnode
               :neighbors
                  '((node-2 operand)
                    (node-5 operand)
                    (node-10 resultx))
               :name 'times2-5
               :short-name "2x5"
               :codelets
                  '((look-for-blx %first-threshold%
                         %second-urgency% (10 2 5)))))

 (setq times2-7 (make-instance 'pnode
               :neighbors
                  '((node-2 operand)
                    (node-7 operand)
                    (node-15 similar))
               :name 'times2-7
               :short-name "2x7"
               :codelets
                  '((look-for-blx %first-threshold%
                         %second-urgency% (15 2 7)))))

 (setq times2-10 (make-instance 'pnode
               :neighbors
;;
;; May  15 11:13 1987  pnet-def.l Page 15
;;
                  '((node-2 operand)
                    (node-10 operand)
                    (node-20 resultx))
               :name 'times2-10
               :short-name "2x10"
               :codelets
                  '((look-for-blx %first-threshold%
                         %second-urgency% (20 2 10)))))

 (setq times2-12 (make-instance 'pnode
               :neighbors
                  '((node-2 operand)
                    (node-12 operand)
                    (node-25 similar))
               :name 'times2-12
               :short-name "2x12"
               :codelets
                  '((look-for-blx %first-threshold%
                          %second-urgency% (24 2 12)))))

 (setq times2-20 (make-instance 'pnode
               :neighbors
                  '((node-2 operand)
                    (node-20 operand)
                    (node-40 resultx))
               :name 'times2-20
               :short-name "2x20"
               :code1ets
                  '((look-for-blx %first-threshold%
                         %second-urgency% (40 2 20)))))

 (setq times3-3 (make-instance 'pnode
               :neighbors
                  '((node-3 operand)
                    (node-9 resultx))
               :name 'times3-3
               :short-name "3x3"
               :codelets
                  '((look-for-blx %first-threshold%
                         %second-urgency% (9 3 3)))))

 (setq times3-10 (make-instance 'pnode
               :neighbors
                  '((node-3 operand)
                    (node-10 operand)
                    (node-30 resultx))
               :name 'times3-10
               :short-name "3x10"
               :codelets
                  '((look-for-blx %first-threshold%
                          %second-urgency% (30 3 10)))))

 (setq times3-20 (make-instance 'pnode
               :neighbors
                  '((node-3 operand)
                    (node-20 operand)
;;
;; May  5 11:13 1987  pnet-def.l Page 16
;;
                    (node-60 resultx))
               :name 'times3-20
               :short-name "3x20"
               :codelets
                  '((look-for-blx %first-threshold%
                          %second-urgency% (60 3 20)))))

 (setq times4-4 (make-instance 'pnode
               :neighbors
                  '((node-4 operand)
                    (node-16 resultx))
               :name 'times4-4
               :short-name "4x4"
               :codelets
                  '((look-for-blx %first-threshold%
                         %second-urgency% (16 4 4)))))

 (setq times4-10 (make-instance 'pnode
               :neighbors
                  '((node-4 operand)
                    (node-10 operand)
                    (node-40 resultx))
               :name 'times4-10
               :short-name "4x10"
               :codelets
                  '((look-for-blx %first-threshold%
                         %second-urgency% (40 4 10)))))

 (setq times4-20 (make-instance 'pnode
               :neighbors
                  '((node-4 operand)
                    (node-20 operand)
                    (node-80 resultx))
               :name 'times4-20
               :short-name "4x20"
               :codelets
                  '((look-for-blx %first-threshold%
                         %second-urgency% (80 4 20)))))

 (setq times5-5 (make-instance 'pnode
               :neighbors
                  '((node-5 operand)
                    (node-25 resultx))
               :name 'times5-5
               :short-name "5x5"
               :codelets
                  '((look-for-blx %first-threshold%
                         %second-urgency% (25 5 5)))))

 (setq times5-6 (make-instance 'pnode
               :neighbors
                  '((node-5 operand)
                    (node-6 operand)
                    (node-30 resultx))
;;
;; May  5 11:13 1987  pnet-def.l Page 17
;;
               :name 'times5-6
               :short-name "5x6"
               :codelets
                  '((look-for-blx %first-threshold%
                         %second-urgency% (30 5 6)))))

 (setq times5-10 (make-instance 'pnode
               :neighbors
                  '((node-5 operand)
                    (node-10 operand)
                    (node-50 resultx))
               :name 'times5-10
               :short-name "5x10"
               :codelets
                  '((look-for-blx %first-threshold%
                         %second-urgency% (50 5 10)))))

 (setq times5-20 (make-instance 'pnode
               :neighbors
                  '((node-5 operand)
                    (node-20 operand)
                    (node-100 resultx))
               :name 'times5-20
               :short-name "5x20"
               :codelets
                  '((look-for-blx %first-threshold%
                         %second-urgency% (100 5 20)))))

 (setq times6-10 (make-instance 'pnode
               :neighbors
                  '((node-6 operand)
                    (node-10 operand)
                    (node-60 resultx))
               :name 'times6-10
               :short-name "6x10"
               :codelets
                  '((look-for-blx %first-threshold%
                         %second-urgency% (60 6 10)))))

 (setq times7-7 (make-instance 'pnode
               :neighbors
                  '((node-7 operand)
                    (node-50 similar))
               :name 'times7-7
               :short-name "7x7"
               :codelets
                  '((look-for-blx %first-threshold%
                         %second-urgency% (49 7 7)))))

 (setq times7-10 (make-instance 'pnode
               :neighbors
                  '((node-7 operand)
                    (node-10 operand)
                    (node-70 resultx))
               :name 'times7-10
;;
;; May  5 11:13 1987  pnet-def.l Page 18
;;
               :short-name "7x10"
               :codelets
                  '((look-for-blx %first-threshold%
                         %second-urgency% (70 7 10)))))

 (setq times8-10 (make-instance 'pnode
               :neighbors
                  '((node-8 operand)
                    (node-10 operand)
                    (node-80 resultx))
               :name 'times8-10
               :short-name "8x10"
               :codelets
                  '((look-for-blx %first-threshold%
                         %second-urgency% (80 8 10)))))

 (setq times9-9 (make-instance 'pnode
               :neighbors
                  '((node-9 operand)
                    (node-81 resultx))
               :name 'times9-9
               :short-name "9x9"
               :codelets
                  '((look-for-blx %first-threshold%
                          %second-urgency% (81 9 9)))))

 (setq times9-10 (make-instance 'pnode
               :neighbors
                  '((node-9 operand)
                    (node-10 operand)
                    (node-90 resultx))
               :name 'times9-10
               :short-name "9x10"
               :codelets
                  '((look-for-blx %first-threshold%
                         %second-urgency% (90 9 10)))))

 (setq times10-10 (make-instance 'pnode
               :neighbors
                  '((node-10 operand)
                    (node-100 resultx))
               :name 'times10-10
               :short-name "10x10"
               :codelets
                  '((look-for-blx %first-threshold%
                          %second-urgency% (100 10 10)))))

 (setq times10-15 (make-instance 'pnode
               :neighbors
                  '((node-10 operand)
                    (node-15 operand)
                    (node-150 resultx))
               :name 'times10-15
;;
;; May  5 11:13 1987  pnet-def.l Page 19
;;
               :short-name "10x15"
               :codelets
                  '((look-for-blx %first-threshold%
                         %second-urgency% (150 10 15)))))

 (setq operand (make-instance 'pnode
               :name 'operand
               :short-name "operd"))

 (setq result+ (make-instance 'pnode
               :name 'result+
               :short-name "resl+"))

 (setq resultx (make-instance 'pnode
               :name 'resultx
               :short-name "reslx"))

 (setq similar (make-instance 'pnode
               :name 'similar
               :short-name "simil"))

 (setq operation (make-instance 'pnode
               :name 'operation
               :short-name "opion"))

 (setq instance (make-instance 'pnode
               :name 'instance
               :short-name "inst"))

 (setq plus (make-instance 'pnode
                :name 'plus
                :short-name "plus"))

 (setq minus (make-instance 'pnode
                 :name 'minus
                 :short-name "diff"))

 (setq times (make-instance 'pnode
                 :name 'times
                 :short-name "prod"))
 )

 (init-pnet)

(setq *pnet* (list node-1 node-2 node-3 node-4 node-5 node-6 node-7 node-8
                   node-9 node-10 node-12 node-15 node-16 node-20 node-25
                   node-30 node-40 node-50 node-60 node-70 node-80
                   node-81 node-90 node-100 node-150
                   plus1-1 plus1-2 plus1-3 plus1-4 plus1-5 plus1-6 plus1-7
                   plus1-8 plus1-9 plus2-2 plus2-3 plus2-4 plus2-5 plus2-6
                   plus2-7 plus2-8 plus3-3 plus3-4 plus3-5 plus3-6 plus3-7
                   plus4-4 plus4-5 plus4-6 plus5-5 plus5-10 plus7-8
                   times2-2 times2-3 times2-4 times2-5 times2-7
                   times2-10 times2-12 times2-20
                   times3-3 times3-10 times3-20 times4-4 times4-10
;;
;; May  5 11:13 1987  pnet-def.l Page 20
;;
                   times4-20 times5-5 times5-6 times5-10 times5-20
                   times6-10  times7-7 times7-10 times8-10 times9-9
                   times9-10 times10-10 times10-15
                   node-mu10 node-multiply node-add node-subtract
                   operand result+ resultx similar operation instance))

(defun print-pnet-all ()
; Prints all the nodes and activations in the pnet.
   (loop for pnode in *pnet* do
       (format t "(~a ~a)"
               (send pnode :name)
               (send pnode :activation))))

(defun print-pnet (threshold)
; Prints the nodes and activations that are > threshold.
   (loop for pnode in *pnet* do
     (let (act)
       (if (> (setq act (send pnode :activation)) threshold)
        then (format t "(~a ~a)"
             (send pnode :name)
             (send pnode :activation))))))

(compile-flavor-methods pnode)
