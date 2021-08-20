#lang racket

#|
Example using a quadratic equation to show the effect precision has on the runtime.

Here we use the quadratic equation for the golden ratio in two forms.

Form 1: x + 1 = x^2

Form 2: x(x - 1) = 1

Note: We must manually exclude infinity from form 1 since we are looking for finite answers only
|#

(require "mk-float.rkt")
(require "mk-float-base.rkt")
(require "mk.rkt")

; PRECISION is 4-7
(define precision-msg (string-append "PRECISION is " (number->string PRECISION)))


(displayln precision-msg)

(define one (build-truncated-float 1))

(define (quadratic-form1 x)
  (fresh (y)
         (fp-finite? x)
         (fp-pluso x one y)
         (fp-multo x x y)))

(define (quadratic-form2 x)
  (fresh (y)
         (fp-pluso y one x) ; (y = x - 1 <=> y + 1 = x)
         (fp-multo x y one)))

(time (run 1 (x) (quadratic-form1 x)))
(time (run 1 (x) (quadratic-form2 x)))
(displayln "")
