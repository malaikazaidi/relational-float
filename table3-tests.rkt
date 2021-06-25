#lang racket

(require rackunit rackunit/text-ui "mk-float.rkt" "test-numbers.rkt" "test.rkt" "mk.rkt")
(provide precision)

(define ten (list (first p10) (second p10) (take-right (third p10) precision)))
(define p1 (list (first one) (second one) (take-right (third one) precision)))


(define (quadratic-form1 x) 
    (fresh (y)
        (fp-pluso x y p1)(fp-multo x x y)
        ))

(define (quadratic-form2 x)
    (fresh (y)
        (fp-pluso p1 x y) (fp-multo x y p1)))

(displayln (string-append "Precision test p =" (number->string precision)))

(define table3-test-suite
    (test-suite "Tests for Table 3"
        (test/fp-relation-r "x + x = 10" ((fp-pluso x x ten) (x))
            (displayln 'results))
        (test/fp-relation-r "x + (x*x) = 1" ((quadratic-form1 x) (x))
            (displayln 'results))
        (test/fp-relation-r "x * (x + 1) = 1" ((quadratic-form2 x) (x))
            (displayln 'results))))


(run-tests table3-test-suite)
