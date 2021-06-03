#lang racket

(require rackunit rackunit/text-ui "mk-float.rkt" "test-numbers.rkt" "test.rkt" "mk.rkt")

(define table1-test-suite
    (test-suite "Tests for Table 1"
        (test/fp-relation-r "1 + pi = ?" ((fp-pluso one pi x) (x))
            (check-results-fp-equal? 'results p4.14 ))
    
        (test/fp-relation-r "1 + pi = ?" ((fp-pluso one pi x) (x))
            (check-results-fp-equal? 'results p4.14 ))

        (test/fp-relation-r "pi + 1 = ?" ((fp-pluso pi one x) (x))
            (check-results-fp-equal? 'results p4.14 ))

        (test/fp-relation-r "100 + 100pi = ?" ((fp-pluso p100 100pi x) (x))
            (check-results-fp-equal? 'results p414.15 ))

        (test/fp-relation-r "100pi + 100= ?" ((fp-pluso 100pi p100 x) (x))
            (check-results-fp-equal? 'results p414.15 ))
        
        (test/fp-relation-r "1 + x = pi" ((fp-pluso one x pi) (x))
            (check-results-fp-equal? 'results p2.14 ))

        (test/fp-relation-r "x + 1 = pi" ((fp-pluso x one pi) (x))
            (check-results-fp-equal? 'results p2.14 ))

        (test/fp-relation-r "x + pi = 1" ((fp-pluso x pi one) (x))
            (check-results-fp-equal? 'results n2.14 ))
        (test/fp-relation-r "pi + x = 1" ((fp-pluso pi x one) (x))
            (check-results-fp-equal? 'results n2.14 ))

        ))



(run-tests table1-test-suite)
(displayln "x + y = pi")
(time(run 1 (x y) (fp-pluso x y pi)))
(time(run 10 (x y) (fp-pluso x y pi)))
;(time(run 3 (x y) (fp-pluso x y pi)))
;(time(run 4 (x y) (fp-pluso x y pi)))
;(time(run 5 (x y) (fp-pluso x y pi)))
(displayln "x + pi = y")
(time(run 1 (x y) (fp-pluso x pi y)))
(time(run 10 (x y) (fp-pluso x pi y)))
;(time(run 3 (x y) (fp-pluso x pi y)))
;(time(run 4 (x y) (fp-pluso x pi y)))
;(time(run 5 (x y) (fp-pluso x pi y)))
(displayln "pi + x = y")
(time(run 1 (x y) (fp-pluso pi x y)))
(time(run 10 (x y) (fp-pluso pi x y)))
;(time(run 3 (x y) (fp-pluso pi x y)))
;(time(run 4 (x y) (fp-pluso pi x y)))
;(time(run 5 (x y) (fp-pluso pi x y)))

(displayln "x + y = z")
(time(run 1 (x y z) (fp-pluso x y z)))
