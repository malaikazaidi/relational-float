#lang racket

(require "mk.rkt")
(require "numbers.rkt")
(require "test-numbers.rkt")

(provide fp-pluso)

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


#|
Drops most significant bit in the mantissa.
|#
(define (drop-mostsig-bito frac fracr)
  (conde ((fresh (frachead)
                 (conde ((appendo frachead '(0) frac))
                        ((appendo frachead '(1) frac)))
                 (== fracr frachead)))))

#|
Drops least significant bit in the mantissa, where cap is 23 bits.
|#

(define (drop-leastsig-bito frac fracr)
  (fresh (foo)
    (frac-lengtho fracr)
    (appendo foo fracr frac)))

;  (conde ((== frac '())
;          (== fracr frac))
;         ((fresh (fracfst fracrst)
;                 (== frac (cons fracfst fracrst))
;                 (conde (
;                         (== fracr frac))
;                        ((drop-leastsig-bito fracrst fracr)))))))

#|
Determines if list is of same length
|# 
(define (same-lengtho lst1 lst2)
  (conde ((== lst1 '())
          (== lst2 '()))
         ((fresh (f1 r1 f2 r2)
                 (== lst1 (cons f1 r1))
                 (== lst2 (cons f2 r2))
                 (same-lengtho r1 r2)))))

#|
Shifts the exponent. 
|# 
(define (shifto-old frac n result)
  (conde ((zeroo n)
          (== frac result))
         ((poso n)
          (fresh (shifted-frac n-minus-1)
                 (== shifted-frac (cons 0 frac))
                 (pluso n-minus-1 '(1) n)
                 (shifto-old shifted-frac n-minus-1 result)
                 ))))

(define (shifto frac expo1 expo2 result)
  (conde ((== expo1 expo2)
          (== frac result))
         ((=/= expo1 expo2)
          (fresh (shifted-frac expo+1)
                 (== shifted-frac (cons 0 frac))
                 (pluso expo1 '(1) expo+1)
                 (shifto shifted-frac expo+1 expo2 result)
                 ))))

#|
Shifts exponent
|# 
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


(define (adjust-expo man-sum exp exp-sum)
  (conde ((<=o (build-num 33554431) man-sum)
          (pluso '(1) exp exp-sum))
         ((<o man-sum (build-num 33554431))
          (== exp-sum exp))))
          
  
  
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
        
#|
Negates sign
|#  
(define (negsigno sign nsign)
  (conde ((== sign 1)
          (== nsign 0))
         ((== sign 0)
          (== nsign 1))))
#|
Determines resultant sign of C in an operation A+(-B) = C
|#
(define (finalsigno sign1 sign2 expo1 expo2 rsign)
  (conde ((<o expo1 expo2)
          (== rsign sign2))
         ((<=o expo2 expo1)
          (== rsign sign1))))
#|
Adding leading bit to mantissa.
|#
(define (addleadbito exp man r)
  (conde ((zeroo exp)
          (== r man))
         ((poso exp)
          (appendo man '(1) r))))
         
;(conde ((=lengtho man-sum man2)
;        (== exp2 exp-result))
;       ((>lengtho man-sum man2)
;        (== exp2 exp-result)))
; rounding?


; (0 1 0 1 0 1 0 1 0 1 0 1 1)   - man2
; (1 0 1 0 1 0 1 0 1 0 1 1)     - man1
; (1 1 1 1 1 1 1 1 1 1 1 0 0 1) - man-sum


(define (frac-lengtho m)
  (fresh (d01 d02 d03 d04 d05 d06 d07 d08 d09 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23)
      (== m (list d01 d02 d03 d04 d05 d06 d07 d08 d09 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23))
   ))

(define (fp-pluso f1 f2 r)
  (fresh (sign1 expo1 frac1 sign2 expo2 frac2 rsign rexpo rfrac)
         (== f1 (list sign1 expo1 frac1))
         (frac-lengtho frac1)
         (== f2 (list sign2 expo2 frac2))
         (frac-lengtho frac2)
         (== r (list rsign rexpo rfrac))
         (frac-lengtho rfrac)
         (conde 
            ((== sign1 sign2)
              ;keep frac-result as final return value
             ;check if denormalized
             (fresh (expo-diff shifted-frac2 man1 man2 man-sum frac-result man-result)
                                  (== rsign sign1)
                                  (=/= expo2 '())
                                   ; get mantissas
                                  (appendo frac1 '(1) man1)
                                  (pluso expo1 expo-diff expo2)
                                  ;shift the frac of the SMALLER exponent
                                  (shifto-old frac2 expo-diff shifted-frac2)
                                  (appendo shifted-frac2 '(1) man2)
                                  ; exponent shift
                                  (shift-expo man2 man-sum expo2 rexpo)
                                  ;drop most-sig bit
                                  (appendo man-result '(1) man-sum); (drop-mostsig-bito man-sum man-result)
                                  ;drop least-sig bit
                                  (drop-leastsig-bito man-result rfrac) 
                                   ; oleg number addition
                                  (pluso man1 man2 man-sum)
                                  ; return result
                                  ;(appendo frac-result '(1) man-sum)
                                  ))
                  ;when signs are opposite
                  ;When C has the same sign as A (+)
                  ;A+(-B) = C -> A = C + B
                  ; fp-pluso (C, -B, A)
                  ;When C has the same sign as B (-)
                  ;A + (-B) = -C -> -B = -C + (-A)
                  ;fp-pluso (C, -A, B)
                  ((=/= sign1 sign2)
                   (fresh (rsign newsign newf)
                          (finalsigno sign1 sign2 expo1 expo2 rsign)
                          (conde ((== sign1 0)
                                  (== rsign 0)
                                  (== newsign 1)
                                  (== newf (list newsign expo2 frac2))
                                  (fp-pluso r newf f1))
                                 ((== sign2 1)
                                  (== rsign 1)
                                  (== newsign 1)
                                  (== newf (list newsign expo1 frac1))
                                  (fp-pluso r newf f2)))))

                  )))



