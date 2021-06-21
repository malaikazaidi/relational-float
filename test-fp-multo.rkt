#lang racket
(require rackunit rackunit/text-ui "mk-float.rkt" "test-numbers.rkt" "test.rkt")
(require "mk.rkt")
(require "mk-float.rkt")

(define equal-signs-test
    (test-suite "Tests for fp numbers that have equal signs" ;(test/fp-relation-r <test-name> <expected> : <relation-expr> (<lvar> <lvars> ...))


        (test/fp-relation-r "1 * 1 = ?" ((fp-multo one one x) (x))
            (check-results-fp-equal? 'results one))

        (test/fp-relation-r "2 * 1 = ?" ((fp-multo two one x) (x))
            (check-results-fp-equal? 'results two))
        
        (test/fp-relation-r "3 * 3 = ?" ((fp-multo three three x) (x))
            (check-results-fp-equal? 'results p9))
        
        (test/fp-relation-r "42 * 4 = ?" ((fp-multo fortytwo four x) (x))
            (check-results-fp-equal? 'results p168))
        
        (test/fp-relation-r "72 * 60 = ?" ((fp-multo seventytwo sixty x) (x))
            (check-results-fp-equal? 'results p4320))
        
        (test/fp-relation-r "(-4) * (-1) = ?" ((fp-multo negfour negone x) (x))
            (check-results-fp-equal? 'results four))
        
        (test/fp-relation-r "(-4) * (-70) = ?" ((fp-multo negfour negseventy x) (x))
            (check-results-fp-equal? 'results p280))

        (test/fp-relation-r "(-421) * (-1) = ?" ((fp-multo neg421 negone x) (x))
            (check-results-fp-equal? 'results p421))


        (test/fp-relation-r "2 * ? = 6" ((fp-multo two x p6) (x))
            (check-results-fp-equal? 'results three))

        (test/fp-relation-r "? * 2 = 6" ((fp-multo x two p6) (x))
            (check-results-fp-equal? 'results three))

        (test/fp-relation-r "2.5 * 3 = ?" ((fp-multo p2.5 three x) (x))
            (check-results-fp-equal? 'results p7.5))
        
        (test/fp-relation-r "2.5 * 3.25 = ?" ((fp-multo p2.5 p3.25 x) (x))
            (check-results-fp-equal? 'results p8.125))

        (test/fp-relation-r "(-4) * (-9) = ?" ((fp-multo negfour n9 x) (x))
            (check-results-fp-equal? 'results p36))))

(define nonequal-signs-test
    (test-suite "Tests for fp numbers that have different signs"
  
        (test/fp-relation-r "1 * (-4) = ?" ((fp-multo one negfour x) (x))
            (check-results-fp-equal? 'results negfour))
            
        (test/fp-relation-r "4 * (-1) = ?" ((fp-multo four negone x) (x))
            (check-results-fp-equal? 'results negfour))
            
        (test/fp-relation-r "2 * (-1) = ?" ((fp-multo two negone x) (x))
            (check-results-fp-equal? 'results ntwo))
            
        (test/fp-relation-r "(-42) * 4 = ?" ((fp-multo nfortytwo four x) (x))
            (check-results-fp-equal? 'results n168))
            
        (test/fp-relation-r "(-72) * 60 = ?" ((fp-multo nseventytwo sixty x) (x))
            (check-results-fp-equal? 'results n4320))
            
        (test/fp-relation-r "(-421) * 1 = ?" ((fp-multo neg421 one x) (x))
            (check-results-fp-equal? 'results neg421))
            
        (test/fp-relation-r "2 * ? = (-2)" ((fp-multo two x ntwo) (x))
            (check-results-fp-equal? 'results negone))
        
        (test/fp-relation-r "? * 2 = (-6)" ((fp-multo x two n6) (x))
            (check-results-fp-equal? 'results nthree))

        (test/fp-relation-r "4.5 * (-7) = ?" ((fp-multo p4.5 n7 x) (x))
            (check-results-fp-equal? 'results n31.5))

        #;(test/fp-relation-r "(-10.12) * 4.6 = ?" ((fp-multo n10.12 p4.6 x) (x)) ;unrepresentable
            (check-results-fp-equal? 'results n46.552))
        #;(test/fp-relation-r "4.5 * (-6.3) = ?" ((fp-multo p4.5 n6.3 x) (x)) ;unrepresentable
            (check-results-fp-equal? 'results n28.35))))

(define unknown-signs
    (test-suite "Tests for fp numbers that have different signs"
        (test/fp-relation-r "1 * 0 = ?" ((fp-multo one zero x) (x))
            (check-results-fp-equal? 'results zero))
        (test/fp-relation-r "0 * 1 = ?" ((fp-multo zero one x) (x))
            (check-results-fp-equal? 'results zero))
        (test/fp-relation-r "x * 1 = 0" ((fp-multo x one zero) (x))
            (check-results-fp-equal? 'results zero))


        (test/fp-relation-r "x * 2 = y" ((fp-multo x two y) (x y))
            (displayln 'results))

        (test/fp-relation-r "x * y = 1" ((fp-multo x y one) (x y))
            (displayln 'results))

        (test/fp-relation-r "x * y = 2" ((fp-multo x y two) (x y))
            (displayln 'results))

        (test/fp-relation-r "x * y = (-1)" ((fp-multo x y negone) (x y))
            (displayln 'results))

        (test/fp-relation-r "x * y = (-6)" ((fp-multo x y n6) (x y))
            (displayln 'results))))

(displayln "Tests for same sign fp numbers")
;(run-tests equal-signs-test)
(displayln "Tests for different sign fp numbers")
;(run-tests nonequal-signs-test)

(run-tests unknown-signs)
;(displayln epsilon)
;(displayln (run 1 (x) (fp-multo epsilon epsilon x)))
