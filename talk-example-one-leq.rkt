#lang racket

#|
Example using the comparison relation 'fp-<='.

Here we will find five numbers greater than or equal to one.
|#

(require "mk-float.rkt")
(require "mk.rkt")

; PRECISION is 8

(define one (build-truncated-float 1)); (0 (1 1 1 1  1 1 1) (0 0 0 0  0 0 0 1))

(time (run 5 (x) (fp-<= one x))) ; Find 5 numbers satisfying,  one <= x.
