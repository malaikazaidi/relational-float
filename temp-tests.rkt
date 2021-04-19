#lang racket

(require "mk.rkt")
(require "numbers.rkt")
(require "mk-float.rkt")
(define one
  '(0 (1 1 1 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))
(define two
  '(0 (0 0 0 0 0 0 0 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))
(define four
  '(0 (1 0 0 0 0 0 0 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))

(displayln "x+y=1")
(time (run 1 (m1 m2) (fp-pluso
                  m1
                  m2
                  one)))
(displayln "x+1=y")
(time (run 1 (m1 m2) (fp-pluso
                  m1
                  one
                  m2)))
(displayln "1+x=y")
(time (run 1 (m1 m2) (fp-pluso
                  one
                  m1
                  m2)))