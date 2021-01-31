#lang racket

(require rackunit rackunit/text-ui "build-float.rkt" "special-values.rkt")

(define expected-length 32)
(define (mkfp-equal? mkfp) (curry equal? mkfp))
(define/match (negate-mkfp mkfp)
  [((cons 0 rest)) (cons 1 rest)]
  [((cons 1 rest)) (cons 0 rest)])

(define truncation-test
  (test-suite "Tests for build-truncated-float.rkt"
              
              (test-case "Check zero with expected 32-bit length"
                (let* ([mkfp (build-truncated-float 0)])
                 (check = (length mkfp) expected-length)
                 (check-pred (mkfp-equal? ZERO) mkfp)))
              
              (test-case "Check one with expected 32-bit length"
                (let* ([mkfp (build-truncated-float 1)])
                 (check = (length mkfp) expected-length)
                 (check-pred (mkfp-equal? ONE) mkfp)))
              
              (test-case "Check the smallest denormal number with expected 32-bit length"
                (let* ([smallest-denorm (expt 2 -149)]
                       [mkfp (build-truncated-float smallest-denorm)])
                 (check = (length mkfp) expected-length)
                 (check-pred (mkfp-equal? SMALLEST-DENORM) mkfp)))

              (test-case "Check the largest denormal number with expected 32-bit length"
                (let* ([largest-denorm (* (expt 2 -126) (- 1 (expt 2 -23)))]
                       [mkfp (build-truncated-float largest-denorm)])
                 (check = (length mkfp) expected-length)
                 (check-pred (mkfp-equal? LARGEST-DENORM) mkfp)))

              (test-case "Check the smallest normal number with expected 32-bit length"
                (let* ([smallest-norm (expt 2 -126)]
                       [mkfp (build-truncated-float smallest-norm)])
                 (check = (length mkfp) expected-length)
                 (check-pred (mkfp-equal? SMALLEST-NORM) mkfp)))

              (test-case "Check the largest normal number with expected 32-bit length"
                (let* ([largest-norm (* (expt 2 127) (- 2 (expt 2 -23)))]
                       [mkfp (build-truncated-float largest-norm)])
                 (check = (length mkfp) expected-length)
                 (check-pred (mkfp-equal? LARGEST-NORM) mkfp)))

              (test-case "Check the largest normal number less than 1 with expected 32-bit length"
                (let* ([largest-norm (- 1 (expt 2 -24))]
                       [mkfp (build-truncated-float largest-norm)])
                 (check = (length mkfp) expected-length)
                 (check-pred (mkfp-equal? LARGEST-LESSTHAN-ONE) mkfp)))

              (test-case "Check the smallest normal number larger than 1 with expected 32-bit length"
                (let* ([smallest-norm (+ 1 (expt 2 -23))]
                       [mkfp (build-truncated-float smallest-norm)])
                 (check = (length mkfp) expected-length)
                 (check-pred (mkfp-equal? SMALLEST-LARGERTHAN-ONE) mkfp)))

              (test-case "Check the 72 with expected 32-bit length"
                (let* ([actual-mkfp   (build-truncated-float 72)]
                       [sign          '(0)]
                       [exponent      '(1 0 1 0  0 0 0 1)]
                       [mantissa      '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0  1 0 0)]
                       [expected-mkfp (append sign exponent mantissa)])
                 (check = (length actual-mkfp) expected-length)
                 (check-pred (mkfp-equal? expected-mkfp) actual-mkfp)))

              (test-case "Check the -421 with expected 32-bit length"
                (let* ([actual-mkfp   (build-truncated-float -421)]
                       [sign          '(1)]
                       [exponent      '(1 1 1 0  0 0 0 1)]
                       [mantissa      '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 1  0 1 0 0  1 0 1)]
                       [expected-mkfp (append sign exponent mantissa)])
                 (check = (length actual-mkfp) expected-length)
                 (check-pred (mkfp-equal? expected-mkfp) actual-mkfp)))

              (test-case "Check the -70 with expected 32-bit length"
                (let* ([actual-mkfp   (build-truncated-float -70)]
                       [sign          '(1)]
                       [exponent      '(1 0 1 0  0 0 0 1)]
                       [mantissa      '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0  0 0 1 1  0 0 0)]
                       [expected-mkfp (append sign exponent mantissa)])
                 (check = (length actual-mkfp) expected-length)
                 (check-pred (mkfp-equal? expected-mkfp) actual-mkfp)))

              (test-case "Check the 42 with expected 32-bit length"
                (let* ([actual-mkfp   (build-truncated-float 42)]
                       [sign          '(0)]
                       [exponent      '(0 0 1 0  0 0 0 1)]
                       [mantissa      '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 1  0 1 0)]
                       [expected-mkfp (append sign exponent mantissa)])
                 (check = (length actual-mkfp) expected-length)
                 (check-pred (mkfp-equal? expected-mkfp) actual-mkfp)))

              (test-case "Check the 60 with expected 32-bit length"
                (let* ([actual-mkfp   (build-truncated-float 60)]
                       [sign          '(0)]
                       [exponent      '(0 0 1 0  0 0 0 1)]
                       [mantissa      '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0  1 1 1)]
                       [expected-mkfp (append sign exponent mantissa)])
                 (check = (length actual-mkfp) expected-length)
                 (check-pred (mkfp-equal? expected-mkfp) actual-mkfp)))

              (test-case "Check the 2049 with expected 32-bit length"
                (let* ([actual-mkfp   (build-truncated-float 2049)]
                       [sign          '(0)]
                       [exponent      '(0 1 0 1  0 0 0 1)]
                       [mantissa      '(0 0 0 0  0 0 0 0  0 0 0 0  1 0 0 0  0 0 0 0  0 0 0)]
                       [expected-mkfp (append sign exponent mantissa)])
                 (check = (length actual-mkfp) expected-length)
                 (check-pred (mkfp-equal? expected-mkfp) actual-mkfp)))

              (test-case "Check the 3 with expected 32-bit length"
                (let* ([actual-mkfp   (build-truncated-float 3)]
                       [sign          '(0)]
                       [exponent      '(0 0 0 0  0 0 0 1)]
                       [mantissa      '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0  0 0 1)]
                       [expected-mkfp (append sign exponent mantissa)])
                 (check = (length actual-mkfp) expected-length)
                 (check-pred (mkfp-equal? expected-mkfp) actual-mkfp)))))

;test execution below.

(run-tests truncation-test)


