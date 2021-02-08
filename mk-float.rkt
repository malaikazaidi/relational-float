#lang racket

(require "mk.rkt")
(require "numbers.rkt")
(require "test-numbers.rkt")

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

#|
Drops most significant bit in the mantissa.
|#
(define (drop-mostsig-bito frac fracr)
  (conde ((== frac '())
          (== fracr frac))
         ((fresh (frachead)
                 (conde ((appendo frachead '(0) frac))
                        ((appendo frachead '(1) frac)))
                 (== fracr frachead)))))

#|
Drops least significant bit in the mantissa, where cap is 23 bits.
|#
(define (drop-leastsig-bito frac fracr)
  (conde ((== frac '())
          (== fracr frac))
         ((fresh (fracfst fracrst)
                 (== frac (cons fracfst fracrst))
                 (conde ((<o (build-num 16777215) frac)
                         (drop-leastsig-bito fracrst fracr))
                        ((<=o frac (build-num 16777215))
                         (== fracr frac)))))))
                   
#|
Shifts the exponent. 
|# 
(define (shifto frac n result)
  (conde ((zeroo n)
          (== frac result))
         ((poso n)
          (fresh (shifted-frac n-minus-1)
            (== shifted-frac (cons 0 frac))
            (pluso n-minus-1 '(1) n)
            (shifto shifted-frac n-minus-1 result)
            ))))

(define (shift-expo man man-sum exp exp-sum)
  (conde ((== man '())
          (== man-sum '())
          (== exp exp-sum))
         ((== man '())
          (=/= man-sum '())
          (pluso exp '(1) exp-sum))
         ((fresh (manfst manrst man-sumfst man-sumrst)
            (== man (cons manfst manrst))
            (== man-sum (cons man-sumfst man-sumrst))
            (shift-expo manrst man-sumrst exp exp-sum)))))
#|
Shifts exponent for denormalized nums
|# 
(define (shift-expod man-sum exp exp-sum)
  (conde ((== man-sum '())
          (== exp exp-sum))
         ((<o (build-num 8388607) man-sum)
          (pluso exp '(1) exp-sum))
         ((=/= man-sum '())
          (<=o man-sum (build-num 8388607))
          (== exp exp-sum))))
        
  

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
       (<=o expo1 expo2)
       ;keep frac-result as final return value
       ;check if denormalized
       (conde ((fresh (man-sum shifted-exp man-result)
                     (== expo1 '())
                     (== expo2 '())
                     ; oleg number addition
                     (pluso frac1 frac2 man-sum)
                     ; exponent shift
                     (shift-expod man-sum expo2 shifted-exp)
                     (conde ((=/= shifted-exp '())
                             (drop-mostsig-bito man-sum man-result))
                            ((== shifted-exp '())
                             (== man-sum man-result)))
                
                     (== r (list sign1 shifted-exp man-result))
                     ))
              ((fresh (expo-diff shifted-frac2 man1 man2 man-sum frac-result exp-result man-result)
    
                      (=/= expo2 '())
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
                      ;drop least-sig bit
                      (drop-leastsig-bito man-sum man-result) 
                             
                      ;drop most-sig bit
                      (drop-mostsig-bito man-result frac-result)
    
                      ; return result
                      ;(appendo frac-result '(1) man-sum)
                      (== r (list sign1 exp-result frac-result))))

         )
       )
      ((=/= sign1 sign2))

   )))




