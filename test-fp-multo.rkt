#lang racket
(require rackunit rackunit/text-ui "mk-float.rkt" "test-numbers.rkt" "mk.rkt" "build-float.rkt")

(define equal-signs-test
  (test-suite "Tests for fp numbers that have equal signs"
              (test-case "1*1"
                         (check-equal? (first (run 1 (x) (fp-multo one one x))) one))
              (test-case "2*1"
                         (check-equal? (first (run 1 (x) (fp-multo two one x))) two))
              (test-case "3*3"
                         (check-equal? (first (run 1 (x) (fp-multo three three x))) (build-truncated-float 9)))
              (test-case "42*4"
                         (check-equal? (first(run 1 (x) (fp-multo fortytwo four x))) (build-truncated-float 168)))
              (test-case "72*60"
                         (check-equal? (first(run 1 (x) (fp-multo seventytwo sixty x))) (build-truncated-float 4320)))
              (test-case "-4 * -1"
                         (check-equal? (first(run 1 (x) (fp-multo negfour negone x))) (build-truncated-float 4)))
              (test-case "-4 * -70"
                         (check-equal? (first(run 1 (x) (fp-multo negfour negseventy x))) (build-truncated-float 280)))
              (test-case "-421 * -1"
                         (check-equal? (first(run 1 (x) (fp-multo neg421 negone x))) (build-truncated-float 421)))
              ;SLOW
              #;(test-case "2*x = 6"
                         (check-equal? (first (run 1 (x) (fp-multo two x (build-truncated-float 6)))) three))
              ;SLOW
              #;(test-case "x*2 = 6"
                         (check-equal? (first (run 1 (x) (fp-multo x two (build-truncated-float 6)))) three))
              
              (test-case "2.5*3"
                         (check-equal? (first (run 1 (x) (fp-multo (build-truncated-float 2.5) three x))) (build-truncated-float 7.5)))
              (test-case "2.5*3.25"
                         (check-equal? (first (run 1 (x) (fp-multo (build-truncated-float 2.5) (build-truncated-float 3.25) x))) (build-truncated-float 8.125)))
           
              (test-case "-4 * -9"
                         (check-equal? (first (run 1 (x) (fp-multo (build-truncated-float -4) (build-truncated-float -9) x))) (build-truncated-float 36)))
            
              ))

(define nonequal-signs-test
  (test-suite "Tests for fp numbers that have different signs"
              (test-case "1*(-4)"
                         (check-equal? (first (run 1 (x) (fp-multo one negfour x))) (build-truncated-float -4)))
              (test-case "2*(-1)"
                         (check-equal? (first (run 1 (x) (fp-multo two negone x))) (build-truncated-float -2)))
              (test-case "(-42)*4"
                         (check-equal? (first(run 1 (x) (fp-multo nfortytwo four x))) (build-truncated-float -168)))
              (test-case "(-72)*60"
                         (check-equal? (first(run 1 (x) (fp-multo nseventytwo sixty x))) (build-truncated-float -4320)))
              (test-case "4 * -1"
                         (check-equal? (first(run 1 (x) (fp-multo four negone x))) (build-truncated-float -4)))
              (test-case "-421 * 1"
                         (check-equal? (first(run 1 (x) (fp-multo neg421 one x))) (build-truncated-float -421)))
              ;SLOW
              #;(test-case "2*x = -2"
                         (check-equal? (first (run 1 (x) (fp-multo two x (build-truncated-float -2)))) negone))
              ;SLOW
              #;(test-case "x*2 = -6"
                         (check-equal? (first (run 1 (x) (fp-multo x two (build-truncated-float -6)))) (build-truncated-float -3)))
              
              (test-case "4.5 * -7 = -31.5"
                         (check-equal? (first(run 1 (x) (fp-multo (build-truncated-float 4.5) (build-truncated-float -7) x))) (build-truncated-float -31.5)))
              
              (test-case "-10.12 * 4.6 = -46.552"
                         (check-equal? (first(run 1 (x) (fp-multo (build-truncated-float -10.12) (build-truncated-float 4.6) x))) (build-truncated-float -46.552)))
              (test-case "4.5 * -6.3 = -28.35"
                         (check-equal? (first(run 1 (x) (fp-multo (build-truncated-float 4.5) (build-truncated-float -6.3) x))) (build-truncated-float -28.35)))

              ))

(displayln "Tests for same sign fp numbers")
(run-tests equal-signs-test)
(displayln "Tests for different sign fp numbers")
(run-tests nonequal-signs-test)