#lang racket

(require "mk-float.rkt")
(require "mk.rkt")

#|
Example using the comparison relation 'fp-<='.

Here we will find five numbers greater than or equal to one.
|#


; PRECISION is 8

(define one '(0 (1 1 1 1  1 1 1) (0 0 0 0  0 0 0 1))) 

(time (run 5 (x) (fp-<= one x))) ; Find 5 numbers satisfying,  one <= x.
