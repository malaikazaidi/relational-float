#lang racket

(require rackunit rackunit/text-ui "mk-float.rkt" "test-numbers.rkt" "test.rkt" "mk.rkt")

(define table1-test-suite
    (test-suite "Tests for Table 1"
    
        (test/fp-relation-r "1 + pi = ?" ((fp-pluso one pi x) (x))
            (check-results-fp-equal? 'results p4.14 ))

        (test/fp-relation-r "pi + 1 = ?" ((fp-pluso pi one x) (x))
            (check-results-fp-equal? 'results p4.14 ))

        (test/fp-relation-r "100 + 100pi = ?" ((fp-pluso p100 100pi x) (x))
            (check-results-fp-equal? 'results p414.15 ))

        (test/fp-relation-r "100pi + 100= ?" ((fp-pluso 100pi p100 x) (x))
            (check-results-fp-equal? 'results p414.15 ))
        
        (test/fp-relation-r "1+ x = pi" ((fp-pluso one x pi) (x))
            (check-results-fp-equal? 'results p2.14 ))

        (test/fp-relation-r "x + 1 = pi" ((fp-pluso x one pi) (x))
            (check-results-fp-equal? 'results p2.14 ))))


(run-tests table1-test-suite)
(time(run 1 (x y) (fp-pluso x y pi)))
(time(run 1 (x y z) (fp-pluso x y z)))