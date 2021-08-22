#lang racket

(require "mk-float.rkt")
(require "mk-float-base.rkt")
(require "mk.rkt")

(define (printer fp)
  (begin
    (display fp)
    (display "\t")
    (let*
        ([real-value (reify fp)])
      (cond
        [(symbol? real-value) (displayln real-value)]
        [else (displayln (number->string (reify fp)))]))))

#|
Example using a quadratic equation to show the effect precision has on the runtime.

Here we use the quadratic equation for the golden ratio in two forms.

Form 1: x + 1 = x^2

Form 2: x(x - 1) = 1

Note: We must manually exclude infinity from form 1 since we are looking for finite answers only
|#

; PRECISION is 4-7
(define precision-msg (string-append "PRECISION is " (number->string PRECISION)))
(displayln precision-msg)

(define one (build-truncated-float 1))

(define (quadratic-form1 x)
  (fresh (y) 
         (fp-finite? x)
         (fp-pluso x one y)
         (fp-multo x x y))) ; y = x^2

(define (quadratic-form2 x)
  (fresh (y)
         (fp-pluso y one x) ; (y = x - 1 <=> y + 1 = x)
         (fp-multo x y one)))


(displayln "Running x^2 = x + 1 query")
(define q1-result (first (time (run 1 (x) (quadratic-form1 x)))))

(printer q1-result)

(displayln "Running x(x-1) = 1 query")
(define q2-result (first (time (run 1 (x) (quadratic-form2 x)))))

(printer q2-result)

(displayln "")
