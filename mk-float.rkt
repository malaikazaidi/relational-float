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
                 (conde ((same-lengtho (make-list 23 0) frac)
                         (== fracr frac))
                        ((drop-leastsig-bito fracrst fracr)))))))

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
(define (shifto frac n result)
  (conde ((zeroo n)
          (== frac result))
         ((poso n)
          (fresh (shifted-frac n-minus-1)
                 (== shifted-frac (cons 0 frac))
                 (pluso n-minus-1 '(1) n)
                 (shifto shifted-frac n-minus-1 result)
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


(define (fp-pluso f1 f2 r)
  (fresh (sign1 expo1 frac1 sign2 expo2 frac2 rsign rexpo rfrac)
         (== f1 (list sign1 expo1 frac1))
         (== f2 (list sign2 expo2 frac2))
         (== r (list rsign rexpo rfrac))
         (conde 
                ((<=o expo1 expo2)
                 (conde
                  ((== sign1 sign2)
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
                                  (== r (list sign1 exp-result frac-result))))))
                   

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

                  ))
                ((<o expo2 expo1)
                 (fp-pluso f2 f1 r))
                   

                 )))



