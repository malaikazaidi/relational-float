#lang racket

#|
Examples using the addition relation 'fp-pluso'.

Here we find the sum of pi and 4 as well as the difference between 4 and pi.
|#

(require "mk-float.rkt")
(require "mk.rkt")

; PRECISION is 8

(define four (build-truncated-float 4))  ; (0 (1 0 0 0  0 0 0 1) (0 0 0 0  0 0 0 1))
(define pi (build-truncated-float 3.14)) ; (0 (0 0 0 0  0 0 0 1) (0 0 0 1  0 0 1 1))

; Forward Query [Addition]
(define sum (first (time (run 1 (x) (fp-pluso pi four x)))))

(reify sum) ; 7.125 ~= 7.141592654

; Backward Query [Substraction]
(define diff (first (time (run 1 (x) (fp-pluso pi x four)))))

diff; 0.875 ~= 0.858407346 
