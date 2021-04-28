#lang racket

(require "mk.rkt")
(require "numbers.rkt")
(require "test-numbers.rkt")
(require "build-float.rkt")
(provide fp-pluso fp-multo frac-lengtho exponento)

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
(not-specialvalo fp)
Checks if fp does not represent an infinity/NaN (i.e a special value).
|#
(define (not-specialvalo fp)
    (fresh (sign expo frac)
        (fp-decompo fp sign expo frac)
        (=/= expo '(1 1 1 1  1 1 1 1))
    ) 
) 

#|
Drops most significant bit in the mantissa.
|#
(define (drop-mostsig-bito frac fracr)
  (conde ((fresh (frachead)
                 (conde ((appendo frachead '(0) frac))
                        ((appendo frachead '(1) frac)))
                 (== fracr frachead)))))

#|
Drops least significant bit in the mantissa, where cap is 24 bits.
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
Shifts the exponent. Removes least sig bits
|# 

(define (correct-shifto frac n result curr-digit)
  (conde ((== n '())
          (== frac result))
         ((fresh (n-first n-rest tmp b1 b2 b3 b4 b5 b6 b7 b8 next-digit)
            (== n (cons n-first n-rest))
            (conde ((== n-first 0)
                    (== frac tmp))
                   ((== n-first 1)
                    (conde
                        ((== curr-digit 1) ; remove 1 digit
                         (== frac (cons b1 tmp)))
                        ((== curr-digit 2) ; remove 2 digit
                         (== frac `(,b1 ,b2 . ,tmp)))
                        ((== curr-digit 3) ; remove 4 digits
                         (== frac `(,b1 ,b2 ,b3 ,b4 . ,tmp)))
                        ((== curr-digit 4) ; remove 8 digits
                         (== frac `(,b1 ,b2 ,b3 ,b4 ,b5 ,b6 ,b7 ,b8 . ,tmp))))))
            (conde ((== curr-digit 1) (== next-digit 2))
                   ((== curr-digit 2) (== next-digit 3))
                   ((== curr-digit 3) (== next-digit 4))
                   ((== curr-digit 4) (== next-digit 5)))
            (correct-shifto tmp n-rest result next-digit)))))

#|
Shifts exponent
|# 
(define (shift-expo man man-sum exp exp-sum)
  (conde ((== man '())
          (== man-sum '())
          (== exp exp-sum))
         ((== man '())
          (=/= man-sum '())
          (pluso '(1) exp exp-sum))
         ((fresh (manfst manrst man-sumfst man-sumrst)
                 (== man (cons manfst manrst))
                 (== man-sum (cons man-sumfst man-sumrst))
                 (shift-expo manrst man-sumrst exp exp-sum)))))
  
  
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
  (fresh (d01 d02 d03 d04 d05 d06 d07 d08 d09 d10 d11 d12 d13 d14 d15 d16)
         (== m (list d01 d02 d03 d04 d05 d06 d07 d08 d09 d10 d11 d12 d13 d14 d15 d16))
         ))

#|
Adds 2 exponents and subtracts bias (127)
|#
(define (shifted-pluso expo1 expo2 rexpo)
  (fresh (exposum) 
         (pluso expo1 expo2 exposum)
         (pluso BIAS rexpo exposum)
         )
  )

#|
Ensures that the exponent length is no more than 8.
|#
(define (exponento expo) 
   (fresh (a b b0 b1 b2 b3 b4 b5 b6 b7)
        (== b (list b0 b1 b2 b3 b4 b5 b6 b7))
        (appendo expo a b)))

; add constraint for -zero and +zero to be equal to one another.
(define (fp-<= f1 f2)
  (fresh (sign1 expo1 frac1 sign2 expo2 frac2)
         (== f1 (list sign1 expo1 frac1))
         (== f2 (list sign2 expo2 frac2))
         (conde ((== sign1 sign2)
                 (== expo1 expo2)
                 (<=o frac1 frac2))
                ((== sign1 sign2)
                 (<=o expo1 expo2))
                ((=/= sign1 sign2)
                 (== sign1 1)
                 (== sign2 0)))
         ))


(define (fp-< f1 f2)
  (fresh (sign1 expo1 frac1 sign2 expo2 frac2)
         (== f1 (list sign1 expo1 frac1))
         (== f2 (list sign2 expo2 frac2))
         (conde ((== sign1 sign2)
                 (== expo1 expo2)
                 (<o frac1 frac2))
                ((== sign1 sign2)
                 (<o expo1 expo2))
                ((=/= sign1 sign2)
                 (== sign1 1)
                 (== sign2 0))
                )
         ))

(define (fp-= f1 f2)
  (fresh (sign1 expo1 frac1 sign2 expo2 frac2)
         (== f1 (list sign1 expo1 frac1))
         (== f2 (list sign2 expo2 frac2))
         (conde ((== sign1 sign2)
                 (== expo1 expo2)
                 (== frac1 frac2)))
         ))

#|
Decomposes fp number into sign, exponent, and mantissa
|#
(define (fp-decompo fp sign expo frac)
 (fresh ()
         (== fp (list sign expo frac))
          (frac-lengtho frac))
  )
#|
Floating-Point Addition for same signs
|#

(define (fp-samesignaddero sign1 expo1 frac1 sign2 expo2 frac2 expo-diff rsign rexpo rfrac)
  ;keep frac-result as final return value
  (fresh (shifted-frac1 frac-sum frac-result)
         (== sign1 sign2)
         (== rsign sign1)
         ;shift the frac of the SMALLER exponent
         (correct-shifto frac1 expo-diff shifted-frac1 1)
         ; exponent shift
         (shift-expo frac2 frac-sum expo2 rexpo)
                  
         ;drop least-sig bit
         (drop-leastsig-bito frac-sum rfrac)
                   
         ; oleg number addition
         (pluso shifted-frac1 frac2 frac-sum)
         )
  )

#|
swaps the order of parameters
|#
(define (fp-swapo sign1 expo1 frac1 sign2 expo2 frac2 rsign rexpo rfrac)
  (fresh (expo-diff)  
         (conde
          ((pluso expo-diff expo1 expo2)
           (fp-samesignaddero sign1 expo1 frac1 sign2 expo2 frac2 expo-diff rsign rexpo rfrac))
          ((=/= expo1 expo2)
           (pluso expo-diff expo2 expo1)
           (fp-samesignaddero sign2 expo2 frac2 sign1 expo1 frac1 expo-diff rsign rexpo rfrac))
          ))
  )

#|
Floating-Point Addition
|#
(define (fp-pluso f1 f2 r)
  (fresh (sign1 expo1 frac1 sign2 expo2 frac2 rsign rexpo rfrac)
         (fp-decompo f1 sign1 expo1 frac1)
         (fp-decompo f2 sign2 expo2 frac2)
         (fp-decompo r rsign rexpo rfrac)
         (conde 
          ((== sign1 sign2)
           (== sign2 rsign)
           (fp-swapo sign1 expo1 frac1 sign2 expo2 frac2 rsign rexpo rfrac))
          
          ;when signs are opposite
          ;When C has the same sign as A (+)
          ;A+(-B) = C -> A = C + B
          ; fp-pluso (-B, C, A)
          ;When C has the same sign as B (-)
          ;A + (-B) = -C -> -B = -C + (-A)
          ;fp-pluso (C, -A, B)
          ((=/= sign1 sign2)
           (fresh (newsign)
                  (conde ((== sign1 rsign)
                          (negsigno sign2 newsign)
                          (fp-swapo newsign expo2 frac2 rsign rexpo rfrac sign1 expo1 frac1))
                         ((== sign2 rsign)
                          (negsigno sign1 newsign)
                          (fp-swapo newsign expo1 frac1 rsign rexpo rfrac sign2 expo2 frac2)))))
          )))

#|
Performs and operation on bit1 and bit2. Outputs an Oleg number.
|#
(define (ando bit1 bit2 bitr)
  (conde ((== bit1 bit2)
          (== bit1 1)
          (== bitr '(1)))
         
         ((== bit1 bit2)
          (== bit1 0)
          (== bitr '()))
         
         ((=/= bit1 bit2)
          (== bitr '()))))


#|
Normalizes the exponent.
|#

(define (mult-expo-normalize pre-expo pre-fracr rexpo)
    (fresh (a b b-first b-rest rem temp)
        ; a and b have the same length as pre-fracr
        (frac-lengtho a)
        (frac-lengtho b)

        ; Decompose b into its first bit and remaining bits.
        (== b (cons b-first b-rest))

        ; Decompose pre-frac r into (a) ++ (b-rest) ++ (rem) 
        ; (a) ++ (b-rest) length = 16 + 15 = 31 bits
        (appendo a b-rest temp)
        (appendo temp rem pre-fracr)

        ; rem == '()  --> length pre-fracr = 31
        ; rem =/= '() --> length pre-fracr >= 32
        (conde 
            ((== rem '())
             (== pre-expo rexpo))
            ((=/= rem '())
             (pluso '(1) pre-expo rexpo))
        )
    )
)


#|
 XOR relation
|#
(define (xoro b1 b2 br)
  (conde 
   ((== b1 1) (== b2 1) (== br 0))
   ((== b1 0) (== b2 0) (== br 0))
   ((== b1 1) (== b2 0) (== br 1))
   ((== b1 0) (== b2 1) (== br 1))
   )
  )

#|
Floating-Point Multiplication
|#
(define (fp-multo f1 f2 r)
  (fresh (sign1 expo1 frac1 sign2 expo2 frac2 rsign rexpo rfrac pre-rexpo pre-fracr)
         (== f1 (list sign1 expo1 frac1))
         (frac-lengtho frac1)
         (== f2 (list sign2 expo2 frac2))
         (frac-lengtho frac2)
         (== r (list rsign rexpo rfrac))
         (frac-lengtho rfrac)

         (not-specialvalo f1)
         (not-specialvalo f2)
         
         (xoro sign1 sign2 rsign)
         (drop-leastsig-bito pre-fracr rfrac)
         
         (mult-expo-normalize pre-rexpo pre-fracr rexpo)
         
         (*o frac1 frac2 pre-fracr)
         (shifted-pluso expo1 expo2 pre-rexpo)

         (exponento expo1)
         (exponento expo2)
         (exponento rexpo)
         ))