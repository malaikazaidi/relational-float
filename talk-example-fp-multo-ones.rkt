#lang racket

#|
Example using the 'fp-multo' relation.

Here we show that the position of ones set in the mantissa
greatly affect the runtime for multiplication.
|#

(require "mk-float.rkt")
(require "mk.rkt")

; PRECISION 13

(define fp0.05 (build-truncated-float 0.05))
(define fp0.001 (build-truncated-float 0.001))

(displayln "Representation of 0.05")
fp0.05

(displayln "")

(displayln "Representation of 0.001")
fp0.001

(displayln "")

(time (run 1 (x) (fp-multo fp0.05 fp0.05 x)))
(displayln "")
(time (run 1 (x) (fp-multo fp0.001 fp0.001 x)))
