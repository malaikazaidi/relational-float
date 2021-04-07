#lang racket
(require rackunit rackunit/text-ui "mk-float.rkt" "test-numbers.rkt" "test.rkt")

(define equal-signs-test
  (test-suite "Tests for fp numbers that have equal signs" ;(test/fp-relation-r <test-name> <expected> : <relation-expr> (<lvar> <lvars> ...))
    (test/fp-relation-r "1 * 1 = ?" one : (fp-multo one one x) (x))
    (test/fp-relation-r "2 * 1 = ?" two : (fp-multo two one x) (x))
    (test/fp-relation-r "3 * 3 = ?" p9 : (fp-multo three three x) (x))
    (test/fp-relation-r "42 * 4 = ?" p168 : (fp-multo fortytwo four x) (x))
    (test/fp-relation-r "72 * 60 = ?" p4320 : (fp-multo seventytwo sixty x) (x))
    (test/fp-relation-r "(-4) * (-1) = ?" four : (fp-multo negfour negone x) (x))
    (test/fp-relation-r "(-4) * (-70) = ?" p280 : (fp-multo negfour negseventy x) (x))
    (test/fp-relation-r "(-421) * (-1) = ?" p421 : (fp-multo neg421 negone x) (x))

    (test/fp-relation-r "2 * ? = 6" three : (fp-multo two x p6) (x))
    (test/fp-relation-r "? * 2 = 6" three : (fp-multo x two p6) (x))
    (test/fp-relation-r "2.5 * 3 = ?" p7.5 : (fp-multo p2.5 three x) (x))
    (test/fp-relation-r "2.5 * 3.25 = ?" p8.125 : (fp-multo p2.5 p3.25 x) (x))
    (test/fp-relation-r "(-4) * (-9) = ?" p36 : (fp-multo negfour n9 x) (x))))

(define nonequal-signs-test
  (test-suite "Tests for fp numbers that have different signs"
    (test/fp-relation-r "1 * (-4) = ?" negfour : (fp-multo one negfour x) (x))
    (test/fp-relation-r "4 * (-1) = ?" negfour : (fp-multo four negone x) (x))
    (test/fp-relation-r "2 * (-1) = ?" ntwo : (fp-multo two negone x) (x))
    (test/fp-relation-r "(-42) * 4 = ?" n168 : (fp-multo nfortytwo four x) (x))
    (test/fp-relation-r "(-72) * 60 = ?" n4320 : (fp-multo nseventytwo sixty x) (x))
    (test/fp-relation-r "(-421) * 1 = ?" neg421 : (fp-multo neg421 one x) (x))
    (test/fp-relation-r "2 * ? = (-2)" negone : (fp-multo two x ntwo) (x))
    (test/fp-relation-r "? * 2 = (-6)" nthree : (fp-multo x two n6) (x))
    (test/fp-relation-r "4.5 * (-7) = ?" n31.5 : (fp-multo p4.5 n7 x) (x))
    #;(test/fp-relation-r "(-10.12) * 4.6 = ?" n46.552 : (fp-multo n10.12 p4.6 x) (x)) ;unrepresentable
    #;(test/fp-relation-r "4.5 * (-6.3) = ?" n28.35 : (fp-multo p4.5 n6.3 x) (x)) ;unrepresentable
              ))

(displayln "Tests for same sign fp numbers")
(run-tests equal-signs-test)
(displayln "Tests for different sign fp numbers")
(run-tests nonequal-signs-test)