#lang racket
(require rackunit rackunit/text-ui "mk-float.rkt" "test-numbers.rkt" "test.rkt" "mk.rkt")

(define less-than-sign-test
  (test-suite "Tests for fp numbers that -<"
    (displayln (string-append "\n" "Running: " "1 < 1 = ?"))
    (time (test-equal? "1 < 1 = ?" (run 1 (x) (fp-< one one)) '()))
    (displayln (string-append "\n" "Running: " "2 < 1 = ?"))
    (time (test-equal? "2 < 1 = ?" (run 1 (x) (fp-< two one)) '()))
    (displayln (string-append "\n" "Running: " "42 < 4 = ?"))
    (time (test-equal? "42 < 4 = ?" (run 1 (x) (fp-< fortytwo four)) '()))
    (displayln (string-append "\n" "Running: " "4 < 42 = ?"))
    (time (test-equal? "4 < 42 = ?" (run 1 (x) (fp-< four fortytwo)) '(_.0)))
    (displayln (string-append "\n" "Running: " "1 < 2 = ?"))
    (time (test-equal? "1 < 2 = ?" (run 1 (x) (fp-< one two)) '(_.0)))
    (displayln (string-append "\n" "Running: " "4.5 < 4.6 = ?"))
    (time (test-equal? "4.5 < 4.6 = ?" (run 1 (x) (fp-< p4.5 p4.6)) '(_.0)))
    (displayln (string-append "\n" "Running: " "-3.25 < 4.6 = ?"))
    (time (test-equal? "-3.25 < 4.6 = ?" (run 1 (x) (fp-< n3.25 p4.6)) '(_.0)))
    (displayln (string-append "\n" "Running: " "-3.25 < -3.25 = ?"))
    (time (test-equal? "4.6 < -3.25 = ?" (run 1 (x) (fp-< p4.6 n3.25)) '()))
    ))
  

(define less-than-and-equal-to
  (test-suite "Tests for fp numbers that -<= "
    (displayln (string-append "\n" "Running: " "1 <= 1 = ?"))
    (time (test-equal? "1 <= 1 = ?" (run 1 (x) (fp-<= one one)) '(_.0)))
    (displayln (string-append "\n" "Running: " "2 <= 1 = ?"))
    (time (test-equal? "2 <= 1 = ?" (run 1 (x) (fp-<= two one)) '()))
    (displayln (string-append "\n" "Running: " "42 <= 4 = ?"))
    (time (test-equal? "42 <= 4 = ?" (run 1 (x) (fp-<= fortytwo four)) '()))
    (displayln (string-append "\n" "Running: " "4 <= 42 = ?"))
    (time (test-equal? "4 <= 42 = ?" (run 1 (x) (fp-<= four fortytwo)) '(_.0)))
    (displayln (string-append "\n" "Running: " "1 <= 2 = ?"))
    (time (test-equal? "1 <= 2 = ?" (run 1 (x) (fp-<= one two)) '(_.0)))
    (displayln (string-append "\n" "Running: " "4.5 <= 4.6 = ?"))
    (time (test-equal? "4.5 <= 4.6 = ?" (run 1 (x) (fp-<= p4.5 p4.6)) '(_.0)))
    (displayln (string-append "\n" "Running: " "4.6 <= 4.6 = ?"))
    (time (test-equal? "4.6 <= 4.6 = ?" (run 1 (x) (fp-<= p4.6 p4.6)) '(_.0)))
    (displayln (string-append "\n" "Running: " "-3.25 <= 4.6 = ?"))
    (time (test-equal? "-3.25 <= 4.6 = ?" (run 1 (x) (fp-<= n3.25 p4.6)) '(_.0)))
    (displayln (string-append "\n" "Running: " "-3.25 <= -3.25 = ?"))
    (time (test-equal? "4.6 <= -3.25 = ?" (run 1 (x) (fp-<= p4.6 n3.25)) '()))
    ))

(define equal-to
  (test-suite "Tests for fp numbers that -= "
    (displayln (string-append "\n" "Running: " "1 = 1 = ?"))
    (time (test-equal? "1 = 1 = ?" (run 1 (x) (fp-= one one)) '(_.0)))
    (displayln (string-append "\n" "Running: " "2 = 1 = ?"))
    (time (test-equal? "2 = 1 = ?" (run 1 (x) (fp-= two one)) '()))
    (displayln (string-append "\n" "Running: " "42 = 4 = ?"))
    (time (test-equal? "42 = 4 = ?" (run 1 (x) (fp-= fortytwo four)) '()))
    (displayln (string-append "\n" "Running: " "4 = 42 = ?"))
    (time (test-equal? "4 = 42 = ?" (run 1 (x) (fp-= four fortytwo)) '()))
    (displayln (string-append "\n" "Running: " "1 = 2 = ?"))
    (time (test-equal? "1 = 2 = ?" (run 1 (x) (fp-= one two)) '()))
    (displayln (string-append "\n" "Running: " "4.5 = 4.6 = ?"))
    (time (test-equal? "4.5 = 4.6 = ?" (run 1 (x) (fp-= p4.5 p4.6)) '()))
    (displayln (string-append "\n" "Running: " "4.6 = 4.6 = ?"))
    (time (test-equal? "4.6 = 4.6 = ?" (run 1 (x) (fp-= p4.6 p4.6)) '(_.0)))
    (displayln (string-append "\n" "Running: " "-3.25 = -3.25 = ?"))
    (time (test-equal? "-3.25 = -3.25 = ?" (run 1 (x) (fp-= n3.25 n3.25)) '(_.0)))
    ))

(displayln "Tests for fp numbers that -<")
(run-tests less-than-sign-test)
(displayln"Tests for fp numbers that -<= ")
(run-tests less-than-and-equal-to)
(displayln"Tests for fp numbers that -= ")
(run-tests equal-to)