#lang racket

(require "mk.rkt")
(require "numbers.rkt")
(require "test-numbers.rkt")
(provide fp-pluso fp-multo)

(define BIAS (build-num 127))

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
  (fresh (bit)
         (frac-lengtho fracr)
         (appendo bit fracr frac)))

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
Adding leading bit to mantissa.
|#
(define (addleadbito exp man r)
  (conde ((zeroo exp)
          (== r man))
         ((poso exp)
          (appendo man '(1) r))))

(define (frac-lengtho m)
  (fresh (d01 d02 d03 d04 d05 d06 d07 d08 d09 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23)
         (== m (list d01 d02 d03 d04 d05 d06 d07 d08 d09 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23))
         ))

#|
Adds 2 exponents and subtracts bias (127)
|#
(define (shifted-pluso expo1 expo2 rexpo)
  (fresh (exposum) 
         (pluso expo1 expo2 exposum)
         (pluso rexpo BIAS exposum)
         )
  )

#|
Floating-Point Addition
|#
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
           (fresh (expo-diff shifted-frac2 man1 man2 man-sum frac-result man-result)
                  (== rsign sign1)
                  ; get mantissas
                  (appendo frac1 '(1) man1)
                  (pluso expo1 expo-diff expo2)
                  
                  ;shift the frac of the SMALLER exponent
                  (shifto frac2 expo-diff shifted-frac2)
                                  
                  ; exponent shift
                  (shift-expo man2 man-sum expo2 rexpo)
                  (appendo shifted-frac2 '(1) man2)
                                  
                  ;drop most-sig bit
                  (appendo man-result '(1) man-sum)
                                  
                  ;drop least-sig bit
                  (drop-leastsig-bito man-result rfrac)
                                  
                  ; oleg number addition
                  (pluso man1 man2 man-sum)
                  ))

          ((== sign1 sign2)
           (=/= expo1 expo2)
           (fp-pluso f2 f1 r))
        
          ;when signs are opposite
          ;When C has the same sign as A (+)
          ;A+(-B) = C -> A = C + B
          ; fp-pluso (-B, C, A)
          ;When C has the same sign as B (-)
          ;A + (-B) = -C -> -B = -C + (-A)
          ;fp-pluso (C, -A, B)
          ((=/= sign1 sign2)
           (fresh (resign newf newsign)
                  (conde ((== resign sign1)
                          (negsigno sign2 newsign)
                          (== newf (list newsign expo2 frac2))
                          (fp-pluso newf r f1))
                         ((== resign sign2)
                          (negsigno sign1 newsign)
                          (== newf (list newsign expo1 frac1))
                          (fp-pluso newf r f2)))))
          )))
#|
Performs long multiplication on the mantissa.
|#
(define (long-multo man1 man2 manr acc)
  (conde 
   ((== man2 '())
    (== acc manr))

   ((=/= man2 '())
    (fresh (bit man2-rest shifted-man acc^) 

           (== shifted-man (cons 0 man1))  

           (== man2 (cons bit man2-rest))
           (conde
            ((== bit 0)
             (== acc acc^))
            ((== bit 1)
             (pluso acc man1 acc^)))

           (long-multo shifted-man man2-rest manr acc^)))))
#|
Normalizes the exponent.
|#
(define (mult-expo-normalize pre-expo man1 man2 manr rexpo)
  (fresh (man1man2)
         (appendo man1 man2 man1man2)
         (mult-check-lengtho man1man2 manr pre-expo rexpo)
         )
  )
#|
Checks if exponent needs to be normalized or not according to the length.
|#
(define (mult-check-lengtho man1man2 manr pre-expo rexpo)
  (conde ((== manr '())
          (== man1man2 manr)
          (pluso '(1) pre-expo rexpo))
         ((== manr '())
          (=/= man1man2 manr)
          (== pre-expo rexpo))
         ((=/= manr '())
          (fresh (b1 b2 restmanr restman1man2)
                 (== man1man2 (cons b1 restman1man2))
                 (== manr (cons b2 restmanr))
                 (mult-check-lengtho restman1man2 restmanr pre-expo rexpo)
                 ))
         )
  )

#|
Floating-Point Multiplication
|#
(define (fp-multo f1 f2 r)
  (fresh (sign1 expo1 frac1 sign2 expo2 frac2 rsign rexpo rfrac pre-rexpo man1 man2 manr pre-fracr)
         (== f1 (list sign1 expo1 frac1))
         (frac-lengtho frac1)
         (== f2 (list sign2 expo2 frac2))
         (frac-lengtho frac2)
         (== r (list rsign rexpo rfrac))
         (frac-lengtho rfrac)
         
         (conde 
          ((== sign1 sign2)
           (== rsign 0))
          ((=/= sign1 sign2)
           (== rsign 1))
          )

         (shifted-pluso expo1 expo2 pre-rexpo) ;Still need to determine if 1 needs to be added

         ; add leading ones
         (appendo frac1 '(1) man1)
         (appendo frac2 '(1) man2)
         (appendo pre-fracr '(1) manr)
         (drop-leastsig-bito pre-fracr rfrac)

         (mult-expo-normalize pre-rexpo man1 man2 manr rexpo)
         (long-multo man1 man2 manr '())
         
         
         ))