#lang racket

(require rackunit rackunit/text-ui "mk-float.rkt" "test-numbers.rkt" "test.rkt" "mk.rkt")

(define table2-test-suite
    (test-suite "Tests for Table 2"
        (test/fp-relation-r "1 * 0 = 0" ((fp-multo one zero x) (x))
            (check-results-fp-equal? 'results zero ))
        ;twice to ensure the timing is accurate
        (test/fp-relation-r "1 * 0 = 0" ((fp-multo one zero x) (x))
            (check-results-fp-equal? 'results zero ))
        (test/fp-relation-r "1 * pi = ?" ((fp-multo one pi x) (x))
            (check-results-fp-equal? 'results pi ))
        (test/fp-relation-r "pi * 1 = ?" ((fp-multo pi one x) (x))
            (check-results-fp-equal? 'results pi)) 
        (test/fp-relation-r "125 * 20pi = ?" ((fp-multo p125 20pi x) (x))
            (check-results-fp-equal? 'results p7850 ))
        (test/fp-relation-r "20pi * 125 = ?" ((fp-multo 20pi p125 x) (x))
            (check-results-fp-equal? 'results p7850))
        (test/fp-relation-r "-2.5 * 3.25 = -8.125" ((fp-multo n2.5 p3.25 x) (x))
            (check-results-fp-equal? 'results n8.125))
        (test/fp-relation-r "-2.5 * -3.25 = 8.125" ((fp-multo n2.5 n3.25 x) (x))
            (check-results-fp-equal? 'results p8.125))
        (test/fp-relation-r "x * y = pi" ((fp-multo x y pi) (x y))
            (check-results-fp-equal? 'results pi))
       
))


            
(run-tests table2-test-suite)
(time (run 3 (x) (fp-multo x two p40)))
(time (run 10 (x) (fp-multo x zero zero)))
(time (run 3 (x y) (fp-multo p20 x p40)))
(time(run 1 (x y) (fp-multo x y pi)))
(time(run 3 (x y) (fp-multo x y n9)))
(time(run 5 (x y) (fp-multo negfour x y))) 
(time(run 1 (x y z) (fp-multo x y z)))

