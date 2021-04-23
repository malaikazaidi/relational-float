#lang racket

(require "mk.rkt")
(require "numbers.rkt")
(require "mk-float.rkt")
(define one
  '(0 (1 1 1 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))
(define two
  '(0 (0 0 0 0 0 0 0 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))
(define ntwo
  '(1 (0 0 0 0 0 0 0 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))
(define three `(0 ,(build-num 128) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)))
(define four
  '(0 (1 0 0 0 0 0 0 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))
(define nfour
  '(1 (1 0 0 0 0 0 0 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))
(define nsix `(1 ,(build-num 129) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)))
(define/match (run-printer res acc)
    [((list) _) (void)]
    [((cons rf rr) acc) (begin
        (displayln (string-append "#" (number->string acc)))
        (displayln rf)
        (run-printer rr (+ acc 1))
    )]
)


#;(displayln "x+y=4")
#;(time (run 1 (x y) (fp-pluso x y four)))

; (displayln "2*x=1")
; (time (run 1 (x) (fp-multo two x one)))

; (displayln "x*1=y")
; (define t1-res (time (run 5 (x y) (fp-multo x one y))))
; (run-printer t1-res 1)

(displayln "x*y=-6")
(run-printer (time (run 1 (x y) (fp-multo x y nsix))) 1)

(displayln "exponento tests")
(run-printer (time (run 1 (x) (exponento '(0 0 0 0 )))) 1)

;((1 (1 1 1 1 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)) (1 () (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)))\
; (define a '(1 (1 1 1 1 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))
; (define b '(1 () (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)))
; (displayln "cross-check")
; (run-printer (time (run 1 (x) (fp-multo a b x))) 1)

; (displayln "3*2 = x")
; (run-printer (time (run 1 (x) (fp-multo two three x))) 1)

#|
(displayln "x*1=y")
(time (run 1 (m1 m2) (fp-multo
                  m1
                  one
                  m2)))
(displayln "1*x=y")
(time (run 1 (m1 m2) (fp-multo
                  one
                  m1
                  m2)))
|#
