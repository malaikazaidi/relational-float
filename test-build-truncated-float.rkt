#lang racket

(require rackunit rackunit/text-ui "build-float.rkt" "test-numbers.rkt")

(define int-truncation-test
  (test-suite "Tests for integers on build-truncated-float.rkt"
              (test-case "Zero"
                         (check-equal? (build-truncated-float 0) zero))
              (test-case "One"
                         (check-equal? (build-truncated-float 1) one))
              (test-case "- One"
                         (check-equal? (build-truncated-float -1) negone))
              (test-case "Four"
                         (check-equal? (build-truncated-float 4) four))
              (test-case "- Four"
                         (check-equal? (build-truncated-float -4) negfour))
              (test-case "Seventy Two"
                         (check-equal? (build-truncated-float 72) seventytwo))
              (test-case "- Seventy"
                         (check-equal? (build-truncated-float -70) negseventy))
              (test-case "Forty Two"
                         (check-equal? (build-truncated-float 42) fortytwo))
              (test-case "- Four Hundred Twenty One"
                         (check-equal? (build-truncated-float -421) neg421))
              (test-case "Sixty"
                         (check-equal? (build-truncated-float 60) sixty))
              (test-case "Two Thousand Forty Nine"
                         (check-equal? (build-truncated-float 2049) p2049))

              (test-case "Largest Normal Number"
                         (let* ([testnum (* (expt 2 127) (- 2 (expt 2 -23)))])
                          (check-equal? (build-truncated-float testnum) largenorm)))
              (test-case "Three"
                         (check-equal? (build-truncated-float 3) three))))

(define normaldecimal-finite-truncation-test
  (test-suite "Tests for normal decimals with finite representations on build-truncated-float.rkt"
              (test-case "Smallest Normal Number"
                         (check-equal? (build-truncated-float (expt 2 -126)) smallnorm))
              (test-case "12.5"
                         (check-equal? (build-truncated-float 12.5) p12.5))
              (test-case "- 45.125"
                         (check-equal? (build-truncated-float -45.125) n45.125))
              (test-case "- 1020.625"
                         (check-equal? (build-truncated-float -1020.625) n1020.625))))

(define normaldecimal-infinite-truncation-test
  (test-suite "Tests for normal decimals with infinite representations on build-truncated-float.rkt"
              (test-case "12.8"
                         (check-equal? (build-truncated-float 12.8) p12.8))
              (test-case "- 45.2"
                         (check-equal? (build-truncated-float -45.2) n45.2))
              (test-case "0.636018991"
                         (check-equal? (build-truncated-float 0.636018991) p0.636018991))
  )
)

(define denormaldecimal-truncation-test
  (test-suite "Tests for denormal decimals on build-truncated-float.rkt"
              (test-case "2^-149"
                         (check-equal? (build-truncated-float (expt 2 -149))) psmallestd)
              (test-case "Largest Denormal Number"
                         (let* ([testnum (* (expt 2 -126) (- 1 (expt 2 -23)))])
                          (check-equal? (build-truncated-float testnum) largenorm)))
  )
)

;test execution below.
(displayln "Tests for Integers")
(run-tests int-truncation-test)

(displayln "Tests for normal decimals with finite representation.")
(run-tests normaldecimal-finite-truncation-test)

(displayln "Test for normal decimals with non-finite representation")
(run-tests normaldecimal-infinite-truncation-test)

(displayln "Tests for denormal decimals on build-truncated-float.rkt")
(run-tests denormaldecimal-truncation-test)