#lang racket

(require rackunit rackunit/text-ui "mk-float.rkt" "test-numbers.rkt" "test.rkt" "mk.rkt")

(define table2-test-suite
    (test-suite "Tests for Table 2"
        (test/fp-relation-r "1 * pi = ?" ((fp-multo one pi x) (x))
            (check-results-fp-equal? 'results pi ))
        (test/fp-relation-r "pi * 1 = ?" ((fp-multo pi one x) (x))
            (check-results-fp-equal? 'results pi)) 
        (test/fp-relation-r "125 * 20pi = ?" ((fp-multo p125 20pi x) (x))
            (check-results-fp-equal? 'results p7850 ))
        (test/fp-relation-r "20pi * 125 = ?" ((fp-multo 20pi p125 x) (x))
            (check-results-fp-equal? 'results p7850))
        (test/fp-relation-r "1* x = pi" ((fp-multo one x pi) (x))
            (check-results-fp-equal? 'results pi ))
        (test/fp-relation-r "x * 1 = pi" ((fp-multo x one pi) (x))
            (check-results-fp-equal? 'results pi ))))
            
(run-tests table2-test-suite)
(time(run 1 (x y) (fp-multo x y pi))) 
(time(run 1 (x y z) (fp-multo x y z)))
