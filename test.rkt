#lang racket

(require rackunit rackunit/text-ui)

(define-syntax check-equal?/time
     (syntax-rules ()
     [(check-equal?/time test-expr pre-check-proc expected-expr)
          (let* ([evald (time test-expr)]
                 [processed-expr (pre-check-proc evald)])
                (check-equal? processed-expr expected-expr))]
     [(check-equal?/time test-expr expected-expr)
          (check-equal? (time test-expr) expected-expr)]))


(define test 
     (test-suite "macro check"
          (test-case "test macro no pre-check-proc: test should fail."
               (check-equal?/time 1 2))
          (test-case "test macro no pre-check-proc: test should pass."
               (check-equal?/time '(1 2 3 4) '(1 2 3 4)))
          (test-case "test macro with pre-check-proc: test should fail."
               (check-equal?/time '(4 3 2 2) last 1))
          (test-case "test macro with pre-check-proc: test should pass"
               (check-equal?/time '(4 3 2 2) last 2))

     ))

(run-tests test 'verbose)