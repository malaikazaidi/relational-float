#lang racket
(require rackunit rackunit/text-ui "mk-float.rkt" "test-numbers.rkt" "test.rkt")
(require "mk.rkt")
(require "mk-float.rkt")

(define (base-float mant) `(,(first four) ,(second four) ,mant))
(define msb-1one (base-float '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)))
(define msb-2one (base-float '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1)))
(define msb-3one (base-float '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)))
(define msb-4one (base-float '(0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1)))
(define msb-5one (base-float '(0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1)))
(define msb-6one (base-float '(0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1)))
(define msb-7one (base-float '(0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1)))
(define msb-8one (base-float '(0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1)))
(define msb-9one (base-float '(0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1)))
(define msb-10one (base-float '(0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1)))
(define lsb-1one (base-float '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))
(define lsb-2one (base-float '(1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))
(define lsb-3one (base-float '(1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 1)))
(define lsb-4one (base-float '(1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 1)))
(define lsb-5one (base-float '(1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 1)))
(define lsb-6one (base-float '(1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 1)))
(define lsb-7one (base-float '(1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 1)))
(define lsb-8one (base-float '(1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 1)))
(define lsb-9one (base-float '(1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 1)))

(define mix1 (base-float '(0 0 1 0 0 1 1 0 1 1 0 0 0 1 0 1)))
(define mix2 (base-float '(0 0 0 0 1 1 0 0 1 0 0 1 1 0 1 1)))
(define mix3 (base-float '(0 0 1 0 1 0 1 0 0 0 1 0 1 1 0 1)))
(define mix4 (base-float '(1 1 1 0 0 1 0 1 1 0 0 0 0 0 0 1)))
(define mix5 (base-float '(0 1 1 0 0 0 0 1 0 0 0 1 1 0 1 1)))
(define mix6 (base-float '(1 0 0 0 1 1 0 0 0 1 0 0 1 1 0 1)))
(define mix7 (base-float '(1 0 0 0 0 1 1 0 0 1 1 0 0 1 0 1)))
(define mix8 (base-float '(1 0 0 1 0 0 0 0 0 1 1 1 0 0 1 1)))
(define mix9 (base-float '(0 0 0 1 0 0 0 0 1 0 1 1 1 1 0 1)))

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

(define ones-test 
    (test-suite "Tests with number and positioning of ones"

        #;(test-suite "MSB bit side"
            (test/fp-relation-r "Just Leading Bit" ((fp-multo four four x) (x)))
            (test/fp-relation-r "Leading Bit + 1"  ((fp-multo msb-1one msb-1one x) (x)))
            (test/fp-relation-r "Leading Bit + 2"  ((fp-multo msb-2one msb-2one x) (x)))
            (test/fp-relation-r "Leading Bit + 3"  ((fp-multo msb-3one msb-3one x) (x)))
            (test/fp-relation-r "Leading Bit + 4"  ((fp-multo msb-4one msb-4one x) (x)))
            (test/fp-relation-r "Leading Bit + 5"  ((fp-multo msb-5one msb-5one x) (x)))
            (test/fp-relation-r "Leading Bit + 6"  ((fp-multo msb-6one msb-6one x) (x)))
            (test/fp-relation-r "Leading Bit + 7"  ((fp-multo msb-7one msb-7one x) (x)))
            (test/fp-relation-r "Leading Bit + 8"  ((fp-multo msb-8one msb-8one x) (x)))
            (test/fp-relation-r "Leading Bit + 9"  ((fp-multo msb-9one msb-9one x) (x))))

        #;(test-suite "LSB bit side"
            (test/fp-relation-r "Just Leading Bit" ((fp-multo four four x) (x)))
            (test/fp-relation-r "Leading Bit + 1"  ((fp-multo lsb-1one lsb-1one x) (x)))
            (test/fp-relation-r "Leading Bit + 2"  ((fp-multo lsb-2one lsb-2one x) (x)))
            (test/fp-relation-r "Leading Bit + 3"  ((fp-multo lsb-3one lsb-3one x) (x)))
            (test/fp-relation-r "Leading Bit + 4"  ((fp-multo lsb-4one lsb-4one x) (x)))
            (test/fp-relation-r "Leading Bit + 5"  ((fp-multo lsb-5one lsb-5one x) (x)))
            (test/fp-relation-r "Leading Bit + 6"  ((fp-multo lsb-6one lsb-6one x) (x)))
            (test/fp-relation-r "Leading Bit + 7"  ((fp-multo lsb-7one lsb-7one x) (x)))
            (test/fp-relation-r "Leading Bit + 8"  ((fp-multo lsb-8one lsb-8one x) (x)))
            (test/fp-relation-r "Leading Bit + 9"  ((fp-multo lsb-9one lsb-9one x) (x))))

        (test-suite "6-bits randomly placed"
            (test/fp-relation-r "1"  ((fp-multo mix1 mix1 x) (x)))
            (test/fp-relation-r "2"  ((fp-multo mix2 mix2 x) (x)))
            (test/fp-relation-r "3"  ((fp-multo mix3 mix3 x) (x)))
            (test/fp-relation-r "4"  ((fp-multo mix4 mix4 x) (x)))
            (test/fp-relation-r "5"  ((fp-multo mix5 mix5 x) (x)))
            (test/fp-relation-r "6"  ((fp-multo mix6 mix6 x) (x)))
            (test/fp-relation-r "7"  ((fp-multo mix7 mix7 x) (x)))
            (test/fp-relation-r "8"  ((fp-multo mix8 mix8 x) (x)))
            (test/fp-relation-r "9"  ((fp-multo mix9 mix9 x) (x))))))

(displayln "Tests for same sign fp numbers")
(run-tests equal-signs-test)
(displayln "Tests for different sign fp numbers")
(run-tests nonequal-signs-test)
(displayln "Unknown signs")
(run-tests unknown-signs)
(displayln "Ones test")
;(run-tests ones-test)
;(displayln epsilon)
;(displayln (run 1 (x) (fp-multo epsilon epsilon x)))
;(displayln "running 10 msb test")
;(time (run 1 (x) (fp-multo msb-10one msb-10one x)))

