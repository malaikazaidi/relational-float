#lang racket

(require rackunit rackunit/text-ui "mk-float.rkt" "test-numbers.rkt" "test.rkt" "mk.rkt")

(define table2-test-suite
    (test-suite "Tests for Table 2"

        (test/fp-relation-r "THROW AWAY" ((fp-multo one zero x) (x))
            (check-results-fp-equal? 'results zero ))

        (test-suite "Single Variable Generic"
        
            (test/fp-relation-r "1 * 0 = ?" ((fp-multo one zero x) (x))
                (check-results-fp-equal? 'results zero ))
            (test/fp-relation-r "1 * pi = ?" ((fp-multo one pi x) (x))
                (check-results-fp-equal? 'results pi ))
            (test/fp-relation-r "pi * 1 = ?" ((fp-multo pi one x) (x))
                (check-results-fp-equal? 'results pi)) 
            (test/fp-relation-r "125 * 20pi = ?" ((fp-multo p125 20pi x) (x))
                (check-results-fp-equal? 'results p7850 ))
            (test/fp-relation-r "20pi * 125 = ?" ((fp-multo 20pi p125 x) (x))
                (check-results-fp-equal? 'results p7850))
            #;(test/fp-relation-r "0.01 * 0.01 = ?" ((fp-multo p0.01 p0.01 x) (x)))
            (test/fp-relation-r "0.05 * 0.05 = ?" ((fp-multo p0.05 p0.05 x) (x)))
            (test/fp-relation-r "0.001 * 0.001 = ?" ((fp-multo p0.001 p0.001 x) (x)))

            (test/fp-relation-r "20 * ? = 40" ((fp-multo p20 x p40) (x))
                (check-results-fp-equal? 'results two))
            (test/fp-relation-r "? * 20 = 40" ((fp-multo x p20 p40) (x))
                (check-results-fp-equal? 'results two))
            (test/fp-relation-r "2 * ? = 40" ((fp-multo two x p40) (x))
                (check-results-fp-equal? 'results p20))
            (test/fp-relation-r "? * 2 = 40" ((fp-multo x two p40) (x))
                (check-results-fp-equal? 'results p20)))

        (test-suite "Single Variable INC. Magnitude Diff Tests"

            (test/fp-relation-r "100 * 0.01 = ?" ((fp-multo p100 p0.01 x) (x))
                (check-results-fp-equal? 'results one ))
            (test/fp-relation-r "0.01 * 100 = ?" ((fp-multo p0.01 p100 x) (x))
                (check-results-fp-equal? 'results one ))
            (test/fp-relation-r "100 * ? = 0.01" ((fp-multo p100 x p0.01) (x))
                (check-results-fp-equal? 'results p0.0001 ))
            (test/fp-relation-r "? * 100 = 0.01" ((fp-multo x p100 p0.01) (x))
                (check-results-fp-equal? 'results p0.0001 ))
            (test/fp-relation-r "0.01 * ? = 100" ((fp-multo p0.01 x p100) (x))
                (check-results-fp-equal? 'results p10000 ))
            (test/fp-relation-r "? * 0.01 = 100" ((fp-multo x p0.01 p100) (x))
                (check-results-fp-equal? 'results p10000 ))

            (test/fp-relation-r "1000 * 0.001 = ?" ((fp-multo p1000 p0.001 x) (x))
                (check-results-fp-equal? 'results one ))
            (test/fp-relation-r "0.001 * 1000 = ?" ((fp-multo p0.001 p1000 x) (x))
                (check-results-fp-equal? 'results one ))
            (test/fp-relation-r "1000 * ? = 0.001" ((fp-multo p1000 x p0.001) (x))
                (check-results-fp-equal? 'results p0.000001 ))
            (test/fp-relation-r "? * 1000 = 0.001" ((fp-multo x p1000 p0.001) (x))
                (check-results-fp-equal? 'results p0.000001 )) 
            (test/fp-relation-r "0.001 * ? = 1000" ((fp-multo p0.001 x p1000) (x))
                (check-results-fp-equal? 'results p100000 ))
            (test/fp-relation-r "? * 0.001 = 1000" ((fp-multo x p0.001 p1000) (x))
                (check-results-fp-equal? 'results p100000 ))

            (test/fp-relation-r "10000 * 0.0001 = ?" ((fp-multo p10000 p0.0001 x) (x))
                (check-results-fp-equal? 'results one ))
            (test/fp-relation-r "0.0001 * 10000 = ?" ((fp-multo p0.0001 p10000 x) (x))
                (check-results-fp-equal? 'results one ))
            (test/fp-relation-r "10000 * ? = 0.0001" ((fp-multo p10000 x p0.0001) (x))
                (check-results-fp-equal? 'results p0.0000001 )) 
            (test/fp-relation-r "? * 10000 = 0.0001" ((fp-multo x p10000 p0.0001) (x))
                (check-results-fp-equal? 'results p0.0000001 ))
            (test/fp-relation-r "0.0001 * ? = 10000" ((fp-multo p0.0001 x p10000) (x))
                (check-results-fp-equal? 'results p1000000 ))
            (test/fp-relation-r "? * 0.0001 = 10000" ((fp-multo x p0.0001 p10000) (x))
                (check-results-fp-equal? 'results p1000000 )))

    
        (test-case "Two Variable"
            (test/fp-relation-r "? * ? = pi" ((fp-multo x y pi) (x y)))
            (test/fp-relation-r "? * ? = pi" 15 15 ((fp-multo x y pi) (x y)))
            (test/fp-relation-r "pi * ? = ?" ((fp-multo pi x y) (x y)))
            (test/fp-relation-r "pi * ? = ?" 15 15 ((fp-multo pi x y) (x y)))
            (test/fp-relation-r "? * pi = ?" ((fp-multo x pi y) (x y)))
            (test/fp-relation-r "? * pi = ?" 15 15 ((fp-multo x pi y) (x y))))))


            
(run-tests table2-test-suite)