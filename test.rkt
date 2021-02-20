#lang racket

(require "mk.rkt")
(require "numbers.rkt")
(require "mk-float.rkt")

(define one `(0 ,(build-num 127) ,(make-list 23 0)))

(define one-plus-one 
  '(0 (0 0 0 0 0 0 0 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(displayln "1+1=?")
(run 1 (x) (fp-pluso one one x))

(displayln "?+1=2 (this was slow)")
(run 1 (x) (fp-pluso x one one-plus-one)) ;slow

(displayln "1+?=2 (this used to be fast--but now slow when we fixed #digits)")
(run 1 (x) (fp-pluso one x one-plus-one)) ;fast
; result: (0 (1 1 1 1 1 1 1) ())
; alternative that we could have gotten:
;   (0 (1 1 1 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(run 1 (x) (fp-pluso `(0 (1 1 1 1 1 1 1) ,x)
                     one
                     one-plus-one)) ;also slow


(displayln "pluso only")
(run 1 (d01)
     (pluso '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1)
            '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
            d01)) ;also fast

(run 1 (d07 d08 d09 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23)
     (pluso (list 0 0 0 0 0 0 d07 d08 d09 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23 1)
            '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
            '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1))) ;also fast




(displayln "?+1=2")
(run 1 (x) (fp-pluso `(0 ,x (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
                     one
                     one-plus-one)) ;fast

(run 1 (d01 d02 d03 d04 d05 d06 d07 d08 d09 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23)
     (fp-pluso (list 0 '(1 1 1 1 1 1 1) (list 0 0 0 0 0 0 0 0 0 0 0 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23))
               one
               one-plus-one)) ;also fast

(run 1 (d01 d02 d03 d04 d05 d06 d07 d08 d09 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23)
     (fp-pluso (list 0 '(1 1 1 1 1 1 1) (list 0 0 0 0 0 0 d07 d08 d09 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23))
               one
               one-plus-one)) ;also fast


(run 1 (d01 d02 d03 d04 d05 d06 d07 d08 d09 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23)
     (fp-pluso (list 0 '(1 1 1 1 1 1 1) (list d01 d02 d03 d04 d05 d06 d07 d08 d09 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23))
               one
               one-plus-one)) ;also fast


