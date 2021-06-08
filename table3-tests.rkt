#lang racket

(require rackunit rackunit/text-ui "mk-float.rkt" "test-numbers.rkt" "test.rkt" "mk.rkt")

(define (to-precision fl p)
  (list (first fl)
        (second fl)
        (reverse (take (reverse (third fl)) p))))

(define pone (to-precision one precision))
(define ppi (to-precision pi precision))
(define p20pi (to-precision 20pi precision))
(define pp125 (to-precision p125 precision))
(define pp7850 (to-precision p7850 precision))

(define table3-test-suite
    (test-suite "Tests for Table 3"
        (test/fp-relation-r "1 * 1 = x" ((fp-multo pone pone x) (x))
            (displayln 'results))
        (test/fp-relation-r "x + x = pi" ((fp-pluso x x ppi) (x))
            (displayln 'results))
        (test/fp-relation-r "ppi * x = 1" ((fp-multo ppi x pone) (x))
            (displayln 'results))
        (test/fp-relation-r "pi * pi = x" ((fp-multo ppi ppi x) (x))
            (displayln 'results))
        (test/fp-relation-r "20pi * x = pi" ((fp-multo p20pi x ppi) (x))
            (displayln 'results))
        (test/fp-relation-r "x * 20pi = pi" ((fp-multo x p20pi ppi) (x))
            (displayln 'results))
        ;(test/fp-relation-r "20pi * x = pi" ((fp-multo p20pi x ppi) (x))
        ;    (displayln 'results))
        ;(test/fp-relation-r "x * 20pi = pi" ((fp-multo x p20pi ppi) (x))
        ;    (displayln 'results))
        
        ;(test/fp-relation-r "x * x = pi" ((fp-multo x x ppi) (x))
        ;    (displayln 'results)); (p=6, ~24501ms)

))

(displayln ppi)
(displayln pone)

(run-tests table3-test-suite)
;(time(run 1 (x y) (fp-multo x y pi))) 
;(time(run 1 (x y z) (fp-multo x y z)))
;(time(run 10 (x y z) (fp-multo x y z)))
;(time(run 20 (x y z) (fp-multo x y z)))

(time (run 1 (x) (fresh (y)
   (fp-pluso x pone y)
   (fp-multo x x y))))

;(fp-negateo x y)
;x^2 + x = 1

; x^2 = x + 1


