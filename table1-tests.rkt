#lang racket

(require rackunit rackunit/text-ui "mk-float.rkt" "test-numbers.rkt" "test.rkt" "mk.rkt")



(define table1-test-suite
    (test-suite "Table 1 tests"

        #;(test-suite "One variable"


            (test/fp-relation-r "3 + 0 = ?" ((fp-pluso three zero x) (x))
                (check-results-fp-equal? 'results three ))

            (test/fp-relation-r "1 + pi = ?" ((fp-pluso one pi x) (x))
                (check-results-fp-equal? 'results p4.14 ))
            (test/fp-relation-r "pi + 1 = ?" ((fp-pluso pi one x) (x))
                (check-results-fp-equal? 'results p4.14 ))

            (test/fp-relation-r "100 + 100pi = ?" ((fp-pluso p100 100pi x) (x))
                (check-results-fp-equal? 'results p414.15 ))
            (test/fp-relation-r "100pi + 100 = ?" ((fp-pluso 100pi p100 x) (x))
                (check-results-fp-equal? 'results p414.15 ))

            (test/fp-relation-r "(-100) + 100pi = ?" ((fp-pluso n100 100pi x) (x))
                (check-results-fp-equal? 'results p214 ))
            (test/fp-relation-r "100pi + (-100) = ?" ((fp-pluso 100pi n100 x) (x))
                (check-results-fp-equal? 'results p214 ))

            (test/fp-relation-r "100 + (-100pi) = ?" ((fp-pluso p100 n100pi x) (x))
                (check-results-fp-equal? 'results n214 ))
            (test/fp-relation-r "(-100pi) + 100 = ?" ((fp-pluso n100pi p100 x) (x))
                (check-results-fp-equal? 'results n214 ))         

            (test/fp-relation-r "1 + ? = pi" ((fp-pluso one x pi) (x))
                (check-results-fp-equal? 'results p2.14 ))
            (test/fp-relation-r "? + 1 = pi" ((fp-pluso x one pi) (x))
                (check-results-fp-equal? 'results p2.14 ))

            (test/fp-relation-r "-1 + 1.05 = ?" ((fp-pluso negone p1.05 x) (x))
                (check-results-fp-equal? 'results p0.05 ))

            (test/fp-relation-r "1 + (-1.05) = ?" ((fp-pluso one n1.05 x) (x))
                (check-results-fp-equal? 'results n0.05 ))

            (test/fp-relation-r "-1 + (-0.05) = ?" ((fp-pluso negone n0.05 x) (x))
                (check-results-fp-equal? 'results n1.05 ))

            ;put it twice to get the real time of the test 
            (test/fp-relation-r "3 + 0 = ?" ((fp-pluso three zero x) (x))
                (check-results-fp-equal? 'results three ))
            (test/fp-relation-r "0 + 3 = ?" ((fp-pluso zero three x) (x))
                (check-results-fp-equal? 'results three ))
    
            (test/fp-relation-r "100 + 0.01 = ?" ((fp-pluso p100 p0.01 x) (x))
                (check-results-fp-equal? 'results p100.01 ))
            (test/fp-relation-r "0.01 + 100 = ?" ((fp-pluso p0.01 p100 x) (x))
                (check-results-fp-equal? 'results p100.01 ))

            (test/fp-relation-r "100 + (-0.01) = ?" ((fp-pluso p100 n0.01 x) (x))
                (check-results-fp-equal? 'results p99.99 ))
            (test/fp-relation-r "(-0.01) + 100 = ?" ((fp-pluso n0.01 p100 x) (x))
                (check-results-fp-equal? 'results p99.99 ))

            (test/fp-relation-r "100 + ? = 0.01" ((fp-pluso p100 x p0.01) (x))
                (check-results-fp-equal? 'results n99.99 ))
            (test/fp-relation-r "? + 100 = 0.01" ((fp-pluso x p100 p0.01) (x))
                (check-results-fp-equal? 'results n99.99 ))

            (test/fp-relation-r "100 + ? = (-0.01)" ((fp-pluso p100 x n0.01) (x))
                (check-results-fp-equal? 'results n100.01 ))
            (test/fp-relation-r "? + 100 = (-0.01)" ((fp-pluso x p100 n0.01) (x))
                (check-results-fp-equal? 'results n100.01 ))
        
        )
    
    (test-suite "Two variables" 

        (test/fp-relation-r "? + pi = ?" ((fp-pluso x pi y) (x y)))
        (test/fp-relation-r "? + pi = ?" 15 15 ((fp-pluso x pi y) (x y)))
        
        (test/fp-relation-r "pi + ? = ?" ((fp-pluso pi x y) (x y)))
        (test/fp-relation-r "pi + ? = ?" 15 15 ((fp-pluso pi x y) (x y)))

        (test/fp-relation-r "? + ? = pi" ((fp-pluso x y pi) (x y)))
        (test/fp-relation-r "? + ? = pi" 15 15 ((fp-pluso x y pi) (x y))))
        
    #;(test-suite "Three variables" 

        (test/fp-relation-r "? + ? = ?" ((fp-pluso x y z) (x y z)))
        (test/fp-relation-r "? + ? = ?" 10 10 ((fp-pluso x y z) (x y z))))))


;(run/time "? + ? = ?" : (fp-pluso x y z) 1 (x y z))


(run-tests table1-test-suite)

