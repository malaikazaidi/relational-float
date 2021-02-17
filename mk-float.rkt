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
        
#|
Negates sign
|#  
(define (negsigno sign nsign)
  (conde ((== sign 1)
          (== nsign 0))
         ((== sign 0)
          (== nsign 1))))

#|
Detects if there is at least one bit set
|#
(define (anybito bit-lst)
  (conde ((== bit-lst '(1)))
         ((=/= bit-lst '(1)) ;Where we have at least 2 elements
          (fresh (first-bit rest-lst)
                 (== bit-lst (cons first-bit rest-lst))
                 (conde ((== first-bit 0) ; need to recurse
                         (anybito rest-lst)) 
                        ((== first-bit 1))))))) ; we are done.


#|
Checks if the bit list has exactly 22 bits.
Can't think of a better way. ask lisa.
|#
(define (has22-bitso lst)
  (<=o lst '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
  (<o '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ) lst))

#|
Detects any inf
|#
(define (info mkfp)
  (conde ((posinfo mkfp))
         ((neginfo mkfp))))

#|
Detects pos inf
|#
(define (posinfo mkfp)
  (fresh (sign exp frac)
         (== mkfp (list sign exp frac))
         (== sign 0)
         (== frac '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
         (== exp '(1 1 1 1 1 1 1 1))))

#|
Detects neg inf
|#
(define (neginfo mkfp)
  (fresh (sign exp frac)
         (== mkfp (list sign exp frac))
         (== sign 1)
         (== frac '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
         (== exp '(1 1 1 1 1 1 1 1))))

#|
Detects qNaN
(technically works but its too slow; culprit hasn-bitso)
|#
(define (qNaNo mkfp)
  (fresh (sign exp frac partial-frac)
         (== mkfp (list sign exp frac))
         (conde ((== sign 1)) ((== sign 0))) ; just to ensure we have a correct sign.
         (== exp '(1 1 1 1 1 1 1 1))
         (has22-bitso partial-frac);'(0 1 1 0 1) = 22
         (appendo partial-frac '(1) frac))) 

#|
Detects sNaN
(technically works but its too slow; culprit hasn-bitso)
|#
(define (sNaNo mkfp)
  (fresh (sign exp frac partial-frac)
         (== mkfp (list sign exp frac))
         (conde ((== sign 1)) ((== sign 0))) ; just to ensure we have a correct sign.
         (== exp '(1 1 1 1 1 1 1 1))
         (appendo partial-frac '(0) frac)
         (has22-bitso partial-frac)
         (anybito partial-frac)))

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
         (conde ((<o expo2 expo1)
                 (fp-pluso f2 f1 r))
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
                  ((=/= sign1 sign2)
                   (conde ((== 0 sign2) ; (-a)+b
                           (fresh (nres exp man sign nsign)
                                  (fp-pluso f2 f1 nres) ; swap args
                                  (== nres (list sign exp man))
                                  (negsigno sign nsign)
                                  (== r (list nsign exp man))))
                          ((== 1 sign2) ; a+ (-b)
                           ;keep frac-result as final return value
                           ;check if denormalized
                           (conde ( (== expo1 '())
                                    (== expo2 '())
                                    ;to do
                                    )
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
                                          (== r (list sign1 exp-result frac-result)))))))))

                 ))))



