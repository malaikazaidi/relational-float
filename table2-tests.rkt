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

        (run/time "20 * ? = 40" : (fp-pluso p20 x p40) 3 (x))
        (run/time "? * 2 = 40" : (fp-pluso x two p40) 3 (x))
        
        (test/fp-relation-r "-2.5 * 3.25 = -8.125" ((fp-multo n2.5 p3.25 x) (x))
            (check-results-fp-equal? 'results n8.125))
        (test/fp-relation-r "-2.5 * -3.25 = 8.125" ((fp-multo n2.5 n3.25 x) (x))
            (check-results-fp-equal? 'results p8.125))

        (run/time "? * 0 = 0" : (fp-pluso x zero zero) 10 (x))
        (run/time "? * ? = pi" : (fp-pluso x y pi) 1 (x y))
        (run/time "? * ? = -9" : (fp-pluso x y n9) 3 (x y))
        (run/time "-4 * ? = ?" : (fp-pluso negfour x y) 5 (x y))
        (run/time "? * ? = ?" : (fp-pluso x y z) 3 (x y z))

       
))


            
(run-tests table2-test-suite)


