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
Example using the 'fp-multo' relation which times how long fp-multo 
takes to compute a square of a number.
|#

; PRECISION 13

(define fp0.001 '(0 (1 0 1 0  1 1 1)   (0 1 0 0  0 1 1 0  0 0 0 0  1)))
(define fp0.05  '(0 (0 1 0 1  1 1 1)   (1 0 0 1  1 0 0 1  1 0 0 1  1)))
(define fp63    '(0 (0 0 1 0  0 0 0 1) (0 0 0 0  0 0 0 1  1 1 1 1  1)))
(define fp32888 '(0 (0 1 1 1  0 0 0 1) (1 1 1 1  0 0 0 0  0 0 0 0  1)))

#|
(squareo x y)
    x: A MKFP number
    y: A MKFP number

This relation succeeds when y = x^2.
|#
(define (squareo x y)
  (fp-multo x x y))

(displayln "Running squareo...")
(printer (first (time (run 1 (x) (squareo fp32888 x)))))
