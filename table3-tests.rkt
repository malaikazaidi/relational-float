#lang racket

(require rackunit rackunit/text-ui "mk-float.rkt" "test-numbers.rkt" "test.rkt" "mk.rkt")
(provide precision)

(define precision 16)
(define table3-test-suite
    (test-suite "Tests for Table 3"
        (test/fp-relation-r "x + x = 10" ((fp-pluso x x p10) (x))
            (displayln 'results))
        (test/fp-relation-r "x + (x*x) = 1" (((fp-pluso x y one) (fp-multo x x y)) (x y))
            (displayln 'results))
        (test/fp-relation-r "x * (x + 1) = 1" (((fp-multo x y one) (fp-pluso x one y)) (x y))
            (displayln 'results))))


(set! precision 4)
(run-tests table3-test-suite)

(set! precision 5)
(run-tests table3-test-suite)

(set! precision 6)
(run-tests table3-test-suite)

(set! precision 7)
(run-tests table3-test-suite)