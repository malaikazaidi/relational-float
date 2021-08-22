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
Example using the 'fp-multo' relation.

Here we compute the product of twenty and two and
compute the quotient between twenty and two 
|#

; PRECISION is 8

(define two    '(0 (0 0 0 0  0 0 0 1) (0 0 0 0  0 0 0 1))) 
(define twenty '(0 (1 1 0 0  0 0 0 1) (0 0 0 0  0 1 0 1))) 

; Forward Query [Multiplication]
;(define product (first (time (run 1 (x) (fp-multo two twenty x)))))

;(printer product); 40

; Backward Query [Division]
(define quotient (first (time (run 1 (x) (fp-multo two x twenty)))))
(printer quotient) ; 10

