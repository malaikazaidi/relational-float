#lang racket

#|
Example using the 'fp-multo' relation.

Here we compute the product of twenty and two and
compute the quotient between twenty and two 
|#

(require "mk-float.rkt")
(require "mk.rkt")

; PRECISION is 8

(define two (build-truncated-float 2))     ; (0 (0 0 0 0  0 0 0 1) (0 0 0 0  0 0 0 1))
(define twenty (build-truncated-float 20)) ; (0 (1 1 0 0  0 0 0 1) (0 1 0 0  0 0 0 1))

(define product (first (time (run 1 (x) (fp-multo two twenty x)))))

(reify product); 40

(define quotient (first (time (run 1 (x) (fp-multo two x twenty)))))
(reify quotient) ; 10

