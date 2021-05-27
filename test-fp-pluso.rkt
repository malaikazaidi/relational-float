#lang racket
(require rackunit rackunit/text-ui "test.rkt" "mk-float.rkt" "test-numbers.rkt")

(define forward-equal-signs-1unknown-test
    (test-suite "Test Suite: forward addition one unknown with equal signs"

        (test/fp-relation-r "1 + 1 = ?" ((fp-pluso one one x) (x))
            (check-results-fp-equal? 'results two))

        (test/fp-relation-r "2 + 1 = ?" ((fp-pluso one two x) (x))
            (check-results-fp-equal? 'results three))

        (test/fp-relation-r "42 + 4 = ?" ((fp-pluso fortytwo four x) (x))
            (check-results-fp-equal? 'results p46))

        (test/fp-relation-r "72 + 60 = ?" ((fp-pluso seventytwo sixty x) (x))
            (check-results-fp-equal? 'results p132))

        (test/fp-relation-r "-4 + -1 = ?" ((fp-pluso negfour negone x) (x))
            (check-results-fp-equal? 'results n5))

        (test/fp-relation-r "-4 + -70 = ?" ((fp-pluso negfour negseventy x) (x))
            (check-results-fp-equal? 'results n74))

        (test/fp-relation-r "-421 + -1 = ?" ((fp-pluso neg421 negone x) (x))
            (check-results-fp-equal? 'results n422))

        (test/fp-relation-r "2.5 + 3 = ?" ((fp-pluso p2.5 three x) (x))
            (check-results-fp-equal? 'results p5.5))

        (test/fp-relation-r "2.5 + 3.25 = ?" ((fp-pluso p2.5 p3.25 x) (x))
            (check-results-fp-equal? 'results p5.75))

        (test/fp-relation-r "1.05 + 3.2325 = ?" ((fp-pluso p1.05 p3.2325 x) (x))
            (check-results-fp-equal? 'results p4.2825))

        (test/fp-relation-r "-4 + -9 = ?" ((fp-pluso negfour n9 x) (x))
            (check-results-fp-equal? 'results n13))))

(define reverse-equal-signs-1unknown-test
    (test-suite "Test Suite: reverse addition with single unknown and equal signs."

        (test/fp-relation-r "42 + ? = 46" ((fp-pluso fortytwo x p46) (x))
            (check-results-fp-equal? 'results four))

        (test/fp-relation-r "? + 42 = 46" ((fp-pluso x fortytwo p46) (x))
            (check-results-fp-equal? 'results four))

        (test/fp-relation-r "72 + ? = 132" ((fp-pluso seventytwo x p132) (x))
            (check-results-fp-equal? 'results sixty))

        (test/fp-relation-r "? + 72 = 132" ((fp-pluso x seventytwo p132) (x))
            (check-results-fp-equal? 'results sixty))

        (test/fp-relation-r "-4 + ? = -5" ((fp-pluso negfour x n5) (x))
            (check-results-fp-equal? 'results negone))

        (test/fp-relation-r "? + -4 = -5" ((fp-pluso x negfour n5) (x))
            (check-results-fp-equal? 'results negone))

        (test/fp-relation-r "2 + ? = 3" ((fp-pluso two x three) (x))
            (check-results-fp-equal? 'results one))

        (test/fp-relation-r "? + 2 = 3" ((fp-pluso x two three) (x))
            (check-results-fp-equal? 'results one))

        (test/fp-relation-r "-421 + ? = -422" ((fp-pluso neg421 x n422) (x))
            (check-results-fp-equal? 'results negone))
        
        (test/fp-relation-r "? + -421 = -422" ((fp-pluso x neg421 n422) (x))
            (check-results-fp-equal? 'results negone))))

(define forward-nonequal-signs-1unknown-test
    (test-suite "Test Suite: forward addition with single unknown and different signs"
    
        (test/fp-relation-r "(-4) + 1 = ?" ((fp-pluso negfour one x) (x))
            (check-results-fp-equal? 'results nthree))
        
        (test/fp-relation-r "1 + (-4) = ?" ((fp-pluso one negfour x) (x))
            (check-results-fp-equal? 'results nthree))

        (test/fp-relation-r "4 + (-1) = ?" ((fp-pluso four negone x) (x))
            (check-results-fp-equal? 'results three))

        (test/fp-relation-r "2 + (-1) = ?" ((fp-pluso two negone x) (x))
            (check-results-fp-equal? 'results one))

        (test/fp-relation-r "(-42) + 4 = ?" ((fp-pluso nfortytwo four x) (x))
            (check-results-fp-equal? 'results n38))

        (test/fp-relation-r "(-72) + 60 = ?" ((fp-pluso nseventytwo sixty x) (x))
            (check-results-fp-equal? 'results n12))

        (test/fp-relation-r "(-421) + 1 = ?" ((fp-pluso neg421 one x) (x))
            (check-results-fp-equal? 'results n420))

        (test/fp-relation-r "4.5 + -7 = ?" ((fp-pluso p4.5 n7 x) (x))
            (check-results-fp-equal? 'results n2.5))

        (test/fp-relation-r "(-5.5) + 3.25 = ?" ((fp-pluso n5.5 p3.25 x) (x))
            (check-results-fp-equal? 'results n2.25))

        (test/fp-relation-r "5.5 + (-3.25) = ?" ((fp-pluso p5.5 n3.25 x) (x))
            (check-results-fp-equal? 'results p2.25))))

(define reverse-nonequal-signs-1unknown-test
    (test-suite "Test Suite: reverse addition with single unknown and different signs"

        (test/fp-relation-r "2 + ? = 1" ((fp-pluso two x one) (x))
            (check-results-fp-equal? 'results negone))

        (test/fp-relation-r "? + 2 = -2" ((fp-pluso x two ntwo) (x))
            (check-results-fp-equal? 'results negfour))

        (test/fp-relation-r "42 + ? = 38" ((fp-pluso fortytwo x p38) (x))
            (check-results-fp-equal? 'results negfour))

        (test/fp-relation-r "? + 42 = 38" ((fp-pluso x fortytwo p38) (x))
            (check-results-fp-equal? 'results negfour))

        (test/fp-relation-r "72 + ? = 70" ((fp-pluso seventytwo x seventy) (x))
            (check-results-fp-equal? 'results ntwo))

        (test/fp-relation-r "(-4) + ? = -3" ((fp-pluso negfour x nthree) (x))
            (check-results-fp-equal? 'results one))

        (test/fp-relation-r "? + (-4) = -3" ((fp-pluso x negfour nthree) (x))
            (check-results-fp-equal? 'results one)) ))

(define denormal-forward-addition-1unknown-test
    (test-suite "Test Suite: forward addition on denormal numbers with one unknown"

        

    )
)



(displayln "Test Suite: forward addition one unknown with equal signs")
(run-tests forward-equal-signs-1unknown-test)

(displayln "Test Suite: reverse addition with single unknown and equal signs.")
(run-tests reverse-equal-signs-1unknown-test)

(displayln "Test Suite: forward addition with single unknown and different signs")
(run-tests forward-nonequal-signs-1unknown-test)

(displayln "Test Suite: reverse addition with single unknown and different signs")
(run-tests reverse-nonequal-signs-1unknown-test)
