#lang racket

(require "mk.rkt")
(require "numbers.rkt")

(define float-format '(sign
                       expo ; Oleg number
                       frac ; kind of Oleg number?
                      ))

(define (get-signo f sign)
  (fresh (expo frac)
    (== f (list sign expo frac))))

(define (get-expo f expo)
  (fresh (sign frac)
    (== f (list sign expo frac))))

(define (get-fraco f frac)
  (fresh (sign expo)
    (== f (list sign expo frac))))

; delete?
(define (maxo n1 n2 m)
  (conde ((<=o n1 n2)
          (== m n2))
         ((<o n2 n1)
          (== m n1))))


(define (shifto frac n result)
  (conde ((zeroo n)
          (== frac result))
         ((poso n)
          (fresh (shifted-frac n-minus-1)
            (== shifted-frac (cons 0 frac))
            (pluso n-minus-1 '(1) n)
            (shifto shifted-frac n-minus-1 result)))))

(define (shift-expo man2 manr exp2 expr)
  (conde ((== man2 '())
          (== manr '())
          (== exp2 expr))
         ((== man2 '())
          (=/= manr '())
          (pluso exp2 '(1) expr))
         ((fresh (man2f man2r manrf manrr)
            (== man2 (cons man2f man2r))
            (== manr (cons manrf manrr))
            (shift-expo man2r manrr exp2 expr)))))
;(conde ((=lengtho man-sum man2)
;        (== exp2 exp-result))
;       ((>lengtho man-sum man2)
;        (== exp2 exp-result)))
; rounding?


; (0 1 0 1 0 1 0 1 0 1 0 1 1)   - man2
; (1 0 1 0 1 0 1 0 1 0 1 1)     - man1
; (1 1 1 1 1 1 1 1 1 1 1 0 0 1) - man-sum





(define (fp-pluso f1 f2 r)
  (fresh (sign1 expo1 frac1 sign2 expo2 frac2)
    (== f1 (list sign1 expo1 frac1))
    (== f2 (list sign2 expo2 frac2))
    (conde
      ((== sign1 sign2)
       (<o expo2 expo1)
       (fp-pluso f2 f1 r)) ; just swap the args
      ((== sign1 sign2)
       (<=lo expo1 expo2)
       (fresh (expo-diff shifted-frac2 man1 man2 man-sum frac-result exp-result)
         (pluso expo1 expo-diff expo2)
         ;shift the frac of the SMALLER exponent
         (shifto frac2 expo-diff shifted-frac2)
         ; get mantissas
         ;this doesn't work for denormalized num-s
         (appendo frac1 '(1) man1)
         (appendo shifted-frac2 '(1) man2)
         ; oleg number addition
         (pluso man1 man2 man-sum)
         ; exponent shift
         (shift-expo man2 man-sum expo2 exp-result)
         ; return result
         ;(appendo frac-result '(1) man-sum)
         (== r (list sign1 expo2 man1))


         )
       )
      ((=/= sign1 sign2))

   )))



(displayln
  (run 1 (x) (get-signo '(1 (0 0 0 0 0) (1 1 1)) x)))
(displayln
  (run 1 (x) (get-expo '(1 (0 0 0 0 0) (1 1 1)) x)))
(displayln
  (run 1 (x) (get-fraco '(1 (0 0 0 0 0) (1 1 1)) x)))

; maxo
(displayln
  (run 2 (x) (maxo (build-num 1) (build-num 2) x)))
(displayln
  (run 2 (x) (maxo (build-num 2) (build-num 2) x))) ; 1 elemet
(displayln
  (run 2 (x) (maxo (build-num 3) (build-num 2) x)))

;shifto
(displayln
  (run 2 (x) (shifto '(1 0 1 0 1 0) (build-num 3) x)))
(displayln
  (run 2 (x) (shifto '(1 0 1 0 1 0) (build-num 0) x)))


(displayln
  (run* (r) (<=lo (build-num 8) (build-num 9))))

; main test

(define f1 (list 0 (build-num 8) '(1 0 1 0 1 0 1 0 1 0 1)))
(define f2 (list 0 (build-num 9) '(1 0 1 0 1 0 1 0 1 0 1)))

(displayln
  (run 2 (r) (fp-pluso f1 f2 r)))

;# length should be same as man2
;# except probably 1 bigger if carry
;# in which case we need to increase the
;# exponent
;(1 1 1 1 1 1 1 1 1 1 1 1)

(displayln
  (run 2 (r) (shift-expo 
               '(1 0 1 0 1 0 1 0 1 0 1 1)
               '(1 1 1 1 1 1 1 1 1 1 1 0 0 1)
               (build-num 8)
               r)))


