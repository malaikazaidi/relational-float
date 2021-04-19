#lang racket
(require rackunit rackunit/text-ui "test.rkt" "mk-float.rkt" "test-numbers.rkt")

(define equal-signs-test
  (test-suite "Tests for fp numbers that have equal signs"

        (test/fp-relation-r "1 + 1 = ?" two : (fp-pluso one one x) (x))
        (test/fp-relation-r "2 + 1 = ?" three : (fp-pluso one two x) (x))
        (test/fp-relation-r "42 + 4 = ?" p46 : (fp-pluso fortytwo four x) (x))
        (test/fp-relation-r "72 + 60 = ?" p132 : (fp-pluso seventytwo sixty x) (x))
        (test/fp-relation-r "-4 + -1 = ?" n5 : (fp-pluso negfour negone x) (x))
        (test/fp-relation-r "-4 + -70 = ?" n74 : (fp-pluso negfour negseventy x) (x))
        (test/fp-relation-r "-421 + -1 = ?" n422 : (fp-pluso neg421 negone x) (x))
        ;(test/fp-relation-r "2.5 + 3 = ?" p5.5 : (fp-pluso p2.5 three x) (x))
       ; (test/fp-relation-r "2.5 + 3.25 = ?" p5.75 : (fp-pluso p2.5 p3.25 x) (x))
        ;(test/fp-relation-r "1.05 + 3.2325 = ?" p4.2825 : (fp-pluso p1.05 p3.2325 x) (x))
        ;(test/fp-relation-r "-4 + -9 = ?" n13 :(fp-pluso negfour n9 x) (x))
       ; (test/fp-relation-r "-10.12 + ? = -33.365" n23.245 : (fp-pluso n10.12 x n33.365) (x))
        ;(test/fp-relation-r "? + -10.12 = -33.365" n23.245 : (fp-pluso x n10.12 n33.365) (x))

        (test/fp-relation-r "42 + ? = 46" four : (fp-pluso fortytwo x p46) (x))
        (test/fp-relation-r "? + 42 = 46" four : (fp-pluso x fortytwo p46) (x))
        (test/fp-relation-r "72 + ? = 132" sixty : (fp-pluso seventytwo x p132) (x))
        (test/fp-relation-r "? + 72 = 132" sixty : (fp-pluso x seventytwo p132) (x))
        (test/fp-relation-r "-4 + ? = -5" negone : (fp-pluso negfour x n5) (x))
        (test/fp-relation-r "? + -4 = -5" negone : (fp-pluso x negfour n5) (x))

        (test/fp-relation-r "2 + ? = 3" one : (fp-pluso two x three) (x))
        (test/fp-relation-r "? + 2 = 3" one : (fp-pluso x two three) (x))

        (test/fp-relation-r "-421 + ? = -422" negone : (fp-pluso neg421 x n422) (x)) ; these take ridculously long +15 minutes, answer not aquired
        (test/fp-relation-r "? + -421 = -422" negone : (fp-pluso x neg421 n422) (x)) ;DOESN'T TAKE LONG NOW :)
              ))

(define nonequal-signs-test
    (test-suite "Tests for fp numbers that have different signs"
        (test/fp-relation-r "1 + (-4) = ?" nthree : (fp-pluso one negfour x) (x))
        (test/fp-relation-r "(-4) + 1 = ?" nthree : (fp-pluso negfour one x) (x))
        (test/fp-relation-r "4 + (-1) = ?" three : (fp-pluso four negone x) (x))
        (test/fp-relation-r "2 + (-1) = ?" one : (fp-pluso two negone x) (x))
        (test/fp-relation-r "(-42) + 4 = ?" n38 : (fp-pluso nfortytwo four x) (x))
        (test/fp-relation-r "(-72) + 60 = ?" n12 : (fp-pluso nseventytwo sixty x) (x)) ;takes really long, doesn't give a response
        (test/fp-relation-r "(-421) + 1 = ?" n420 : (fp-pluso neg421 one x) (x)) ;takes really long, doesn't give a response
        (test/fp-relation-r "2 + ? = 1" negone : (fp-pluso two x one) (x))
        (test/fp-relation-r "? + 2 = -2" negfour : (fp-pluso x two ntwo) (x))
        (test/fp-relation-r "42 + ? = 38" negfour : (fp-pluso fortytwo x p38) (x)) ;Takes really long, doesn't give a response
        (test/fp-relation-r "? + 42 = 38" negfour : (fp-pluso x fortytwo p38) (x)) ;Takes really long, doesn't give a response
        (test/fp-relation-r "72 + ? = 70" ntwo : (fp-pluso seventytwo x seventy) (x)) ;Takes really long, doesn't give a response
        (test/fp-relation-r "(-4) + x = -3" one : (fp-pluso negfour x nthree) (x))
        (test/fp-relation-r "x + (-4) = -3" one : (fp-pluso x negfour nthree) (x))))
       ; (test/fp-relation-r "4.5 + -7 = ?" n2.5 : (fp-pluso p4.5 n7 x) (x))
       ; (test/fp-relation-r "(-5.5) + 3.25 = ?" n2.25 : (fp-pluso n5.5 p3.25 x) (x))
       ; (test/fp-relation-r "5.5 + (-3.25) = ?" p2.25 : (fp-pluso p5.5 n3.25 x) (x))))

(displayln "Tests for same sign fp numbers")
(run-tests equal-signs-test)
(displayln "Tests for different sign fp numbers")
(run-tests nonequal-signs-test)