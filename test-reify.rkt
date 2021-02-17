#lang racket
(require rackunit rackunit/text-ui "float.rkt" "test-numbers.rkt" "numbers.rkt")

(define reify-exp-tests
  (test-suite "Tests for reify-exp"
              (test-case "Given 127 -> 0"
                         (check = (reify-exp (build-num 127)) 0))
              (test-case "Given 0 -> -127"
                         (check = (reify-exp (build-num 0)) -127))
              (test-case "Given 1 -> -126"
                         (check = (reify-exp (build-num 127)) 0))
              (test-case "Given 254 -> 127"
                         (check = (reify-exp (build-num 127)) 0))))

(define reify-sign-tests
  (test-suite "Tests for reify-sign"
              (test-case "Given 0 -> 1"
                         (check = (reify-sign 0) 1))
              (test-case "Given 1 -> -1"
                         (check = (reify-sign 1) -1))))

(define reify-frac-tests
  (test-suite "Tests for reify-frac"
              (test-case "Given oleg 0 -> 0"
                         (check = (reify-frac '()) 0))
              (test-case "Given non-oleg 0 -> 0"
                         (check = (reify-frac '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 0))
              (test-case "Given '(1)->(expt 2 -23)"
                         (check = (reify-frac '(1)) (expt 2 -23)))
              (test-case "Given '(1 1)-> (+ (expt 2 -22) (expt 2 -23))"
                         (check = (reify-frac '(1 1)) (+ (expt 2 -22) (expt 2 -23))))))

(define reify-tests
  (test-suite "Tests for reify"
              (test-case "Given mkfp(1) -> 1"
                         (check = (reify one) 1))
              (test-case "Given mkfp(0) -> 10"
                         (check = (reify zero) 0))
              (test-case "Given mkfp(-421) -> -421"
                         (check = (reify neg421) -421))
              (test-case "Given mkfp(42) -> 42"
                         (check = (reify fortytwo) 42))
              (test-case "Given mkfp(3) -> 3"
                         (check = (reify three) 3))
              (test-case "Given mkfp(12.5) -> 12.5"
                         (check = (reify p12.5) 12.5))
              (test-case "Given mkfp(-45.125) -> -45.125"
                         (check = (reify n45.125) -45.125))
              (test-case "Given mkfp(2^23) -> 2^23"
                         (check = (reify p2^23) (expt 2 -23)))
              (test-case "Given mkfp(2^-149) -> 2^-149"
                         (check = (reify psmallestd) (expt 2 -149)))
              (test-case "Given mkfp((1-2^(-23))2^-126) -> (1-2^(-23))2^-126"
                         (check = (reify plargestd) (* (expt 2 -126) (- 1 (expt 2 -23)))))
              (test-case "Given mkfp(qNaN) -> 'qNaN"
                         (check equal? (reify qnan) qNaN))
              (test-case "Given mkfp(sNaN) -> 'sNaN"
                         (check equal? (reify snan) sNaN))
              (test-case "Given mkfp(pinf) -> 'positive-infinity"
                         (check equal? (reify pinf) POS-INFINITY))
              (test-case "Given mkfp(ninf) -> 'negative-infinity"
                         (check equal? (reify ninf) NEG-INFINITY))))

(run-tests reify-exp-tests)
(run-tests reify-sign-tests)
(run-tests reify-frac-tests)
(run-tests reify-tests)
