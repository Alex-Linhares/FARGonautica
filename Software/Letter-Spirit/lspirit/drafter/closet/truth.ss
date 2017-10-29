(define truth-string
  (lambda (ls)
    (cond
     [(null? ls) '()]
     [(cadar ls) (cons (caar ls) (truth-string (cdr ls)))]
     [else (truth-string (cdr ls))])))

(define ar-norm
  (lambda (hex)
    (let* ([code (string->list hex)]
           [bls (hex->bin code)])
      (set! *quanta* bls)
      (set! *quanta-list* (q-list *quanta* 0))
      (truth-string (map list *rule-types* (map rule-type *rule-types*))))))

(define plato-norm
  (lambda (cat hex1 hex2)
    (list cat (subtract *rule-types*
			(intersect (ar-norm hex1) (ar-norm hex2))))))

(set! rule-norms (list
(plato-norm 'a "0fc00340000000" "0B000040010060")
(plato-norm 'b "0cc24b40000000" "00024900090060")
(plato-norm 'c "0cc00900000000" "00400000080060")
(plato-norm 'd "0cc09b40000000" "00009240090060")
(plato-norm 'e "0fc00b00000000" "03400000080060")
(plato-norm 'f "4c012480000000" "0C002480400000")
(plato-norm 'g "0ccc0b49000000" "00080248091060")
(plato-norm 'h "0c024b40000000" "00024940080040")
(plato-norm 'i "14000240000000" "08000900800000")
(plato-norm 'j "14040249000000" "04000490402000")
(plato-norm 'k "00024900060010" "02024900040010")
(plato-norm 'l "00012480000000" "00024800000020")
(plato-norm 'm "0c000fc0000000" "00000DC0080040")
(plato-norm 'n "0c000b40000000" "00000940080040")
(plato-norm 'o "0cc00b40000000" "00000000090060")
(plato-norm 'p "0cc00b64000000" "00000924090060")
(plato-norm 'q "0cc00b49000000" "00000249090060")
(plato-norm 'r "0c000900000000" "04000900080000")
(plato-norm 's "0fc00840000000" "07800000090000")
(plato-norm 't "0c402480000000" "0C002480010000")
(plato-norm 'u "00c00b40000000" "00800B40010000")
(plato-norm 'v "00000a00010020" "00000A00010020")
(plato-norm 'w "00c00fc0000000" "00000E80010020")
(plato-norm 'x "00000000060090" "00000000060090")
(plato-norm 'y "00cc0b49000000" "00080A48011020")
(plato-norm 'z "0cc00000060000" "0CC00000060000")))
