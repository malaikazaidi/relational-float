#lang racket

(require "mk-float.rkt")
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
Examples using the addition relation 'fp-pluso'.

Here we find the sum of pi and 4 as well as the difference between 4 and pi.
|#

; PRECISION is 8

(define four '(0 (1 0 0 0  0 0 0 1) (0 0 0 0  0 0 0 1))) 
(define pi   '(0 (0 0 0 0  0 0 0 1) (0 0 0 1  0 0 1 1))) ; rounded down to 3.125 

; Forward Query [Addition]
;(define sum (first (time (run 1 (x) (fp-pluso pi four x)))))

;(printer sum) ; 7.125 ~= 7.141592654

; Backward Query [Substraction]
(define diff (first (time (run 1 (x) (fp-pluso pi x four)))))

diff; 0.875 ~= 0.858407346 
