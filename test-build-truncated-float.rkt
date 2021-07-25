#lang racket

(require rackunit rackunit/text-ui "float.rkt")

(define zero  `(0 ,(list) (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)))
(define one   '(0 (1 1 1 1 1 1 1)   (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 1)))
(define none  '(1 (1 1 1 1 1 1 1)   (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 1)))
(define four  '(0 (1 0 0 0 0 0 0 1) (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 1)))
(define nfour '(1 (1 0 0 0 0 0 0 1) (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 1)))
(define p72   '(0 (1 0 1 0 0 0 0 1) (0 0 0 0  0 0 0 0  0 0 0 0  1 0 0 1)))
(define n70   '(1 (1 0 1 0 0 0 0 1) (0 0 0 0  0 0 0 0  0 0 1 1  0 0 0 1)))
(define p42   '(0 (0 0 1 0 0 0 0 1) (0 0 0 0  0 0 0 0  0 0 0 1  0 1 0 1)))
(define p45   '(0 (0 0 1 0 0 0 0 1) (0 0 0 0  0 0 0 0  0 0 1 0  1 1 0 1)))
(define n45.2 '(1 (0 0 1 0 0 0 0 1) (0 0 1 1  0 0 1 1  0 0 1 0  1 1 0 1))) ; IEEE-754 when using truncated rounding.
(define n421  '(1 (1 1 1 0 0 0 0 1) (0 0 0 0  0 0 0 1  0 1 0 0  1 0 1 1)))
(define p60   '(0 (0 0 1 0 0 0 0 1) (0 0 0 0  0 0 0 0  0 0 0 0  1 1 1 1)))
(define p2049 '(0 (0 1 0 1 0 0 0 1) (0 0 0 0  1 0 0 0  0 0 0 0  0 0 0 1)))
(define three '(0 (0 0 0 0 0 0 0 1) (0 0 0 0  0 0 0 0  0 0 0 0  0 0 1 1)))

(define largenorm `(0 (0 1 1 1 1 1 1 1) ,(make-list 16 1)))
(define smallnorm `(0 (1) (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 1)))
(define p12.5     '(0 (0 1 0 0 0 0 0 1) (0 0 0 0  0 0 0 0  0 0 0 1  0 0 1 1)))
(define p12.8     '(0 (0 1 0 0 0 0 0 1) (0 0 1 1  0 0 1 1  0 0 1 1  0 0 1 1))); IEEE-754 when using truncated rounding.
(define n45.125   '(1 (0 0 1 0 0 0 0 1) (0 0 0 0  0 0 0 1  0 0 1 0  1 1 0 1)))
(define n1020.625 '(1 (0 0 0 1 0 0 0 1) (0 0 0 1  0 1 0 0  1 1 1 1  1 1 1 1)))

(define p0.9921875 '(0 (0 1 1 1 1 1 1) (0 0 0 0  0 0 0 0  0 1 1 1  1 1 1 1)))

(define p0.555555522442 '(0 (0 1 1 1 1 1 1) (0 0 0 1  1 1 0 0  0 1 1 1  0 0 0 1)))
(define p0.00000190734  '(0 (0 0 1 1 0 1 1) (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 1)))
(define p0.636018991    '(0 (0 1 1 1 1 1 1) (0 1 0 0  1 0 1 1  0 1 0 0  0 1 0 1))) ; closest IEEE-754 number to 0.636018991

(define psmallestd `(0 ,(list) (1)))
(define plargestd `(0 ,(list) ,(make-list 16 1))); 2^-126 * (1-2^(-23))

(define int-truncation-test
  (test-suite "Tests for integers on build-truncated-float.rkt"
              (test-case "Zero"
                         (check-equal? (build-truncated-float 0) zero))
              (test-case "One"
                         (check-equal? (build-truncated-float 1) one))
              (test-case "- One"
                         (check-equal? (build-truncated-float -1) none))
              (test-case "Four"
                         (check-equal? (build-truncated-float 4) four))
              (test-case "- Four"
                         (check-equal? (build-truncated-float -4) nfour))
              (test-case "Seventy Two"
                         (check-equal? (build-truncated-float 72) p72))
              (test-case "- Seventy"
                         (check-equal? (build-truncated-float -70) n70))
              (test-case "Forty Two"
                         (check-equal? (build-truncated-float 42) p42))
              (test-case "- Four Hundred Twenty One"
                         (check-equal? (build-truncated-float -421) n421))
              (test-case "Sixty"
                         (check-equal? (build-truncated-float 60) p60))
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
                         (check-equal? (build-truncated-float -1020.625) n1020.625))
              (test-case "0.9921875"
                         (check-equal? (build-truncated-float 0.9921875) p0.9921875))
              (test-case "0.555555522442"
                         (check-equal? (build-truncated-float 0.555555522442) p0.555555522442))
              (test-case "45.0000019073"
                         (check-equal? (build-truncated-float 45.0000019073) p45)); This value is not representable by single. It should be rounded down to 45.
              (test-case "2^-19"
                         (check-equal? (build-truncated-float (expt 2 -19)) p0.00000190734))
  ))
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
  (test-suite "Tests for denormal decimals on build-truncated-float.rkt, should all be zero"
              (test-case "2^-149"
                         (check-equal? (build-truncated-float (expt 2 -149)) zero))
              (test-case "Largest Denormal Number"
                         (let* ([testnum (* (expt 2 -126) (- 1 (expt 2 -23)))])
                          (check-equal? (build-truncated-float testnum) plargestd)))
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
