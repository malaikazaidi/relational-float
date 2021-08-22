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
Example using fp-multo to generate overflow.
|#


; PRECISION is 8
(define 2^127 '(0 (0 1 1 1  1 1 1 1) (0 0 0 0  0 0 0 1)))
(define five  '(0 (1 0 0 0  0 0 0 1) (0 0 0 0  0 1 0 1)))
(define pinf  '(0 (1 1 1 1  1 1 1 1) (0 0 0 0  0 0 0 1))) ; positive infinity

(define pluso-results (first (time (run 1 (x) (fp-pluso 2^127 2^127 x)))))
(printer pluso-results)


;(define multo-results (time (run 8 (x) (fp-multo five x pinf))))
;(for-each printer multo-results)
