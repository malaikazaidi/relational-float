#lang racket

(require "mk.rkt")
(require "numbers.rkt")
(require "test-numbers.rkt") ;Un-comment require to play with test-numbers in REPL.
(provide fp-pluso fp-multo frac-lengtho expo-lengtho fp-< fp-<= fp-=)

(define BIAS (build-num 127))

(define float-format '(sign
                       expo ; Oleg number
                       frac)) ; kind of Oleg number?))

#|
(noto a not-a)
    a: 0 or 1
    not-a: The result of ~a.

    Negates the bit a, i.e negation relation.
|#  
(define (noto a not-a)
    (conde ((== a 1)
            (== not-a 0))
           ((== a 0)
            (== not-a 1))))

#|
(xoro b1 b2 br)
    b1: 0 or 1
    b2: 0 or 1
    br: the result of (b1 xor b2)

    An exclusive-or relation.
|#
(define (xoro b1 b2 br)
    (conde 
        ((== b1 1) (== b2 1) (== br 0))
        ((== b1 0) (== b2 0) (== br 0))
        ((== b1 1) (== b2 0) (== br 1))
        ((== b1 0) (== b2 1) (== br 1))))

#|
(dropr-zeroo lst stripped-lst)
    lst: A binary list.
    stripped-lst: The resulting list by stripping the right most zeros from lst.

    Remove the zeros on the right side of lst.
|#
(define (dropr-zeroo lst stripped-lst)
    (conde ((== lst '()) (== stripped-lst '()))
        ((fresh (last-bit lst-head) 
         (appendo lst-head (list last-bit) lst)
         (conde ((== last-bit 1) (== stripped-lst lst))
                ((== last-bit 0) (dropr-zeroo lst-head stripped-lst)))))))


#|
Decomposes fp number into sign, exponent, and mantissa
|#
(define (fp-decompo fp sign expo frac)
    (fresh ()
        (== fp (list sign expo frac))
        (frac-lengtho frac)))

#|
(not-specialvalo fp)
    fp: A MKFP number.

Checks if fp does not represent an infinity/NaN (i.e a special value).
|#
(define (not-specialvalo fp)
    (fresh (sign expo frac)
        (fp-decompo fp sign expo frac)
        (=/= expo '(1 1 1 1  1 1 1 1)))) 



#|
(prepare-fraco expo frac prepared-frac)
    expo: The exponent of a MKFP number
    frac: The corresponding fraction of a MKFP number
    prepared-frac: A fraction that is prepared for pluso.

    Determines if the number with exponent (expo) and fraction (frac) is denormal and prepares the 
    fraction for addition.
|#
(define (prepare-fraco expo frac prepared-frac)
    (fresh (frac-head last-bit)
        (conde 
            ((poso expo) (== frac prepared-frac)) 
            ((zeroo expo)
             (appendo frac-head (list last-bit) frac)
             (== last-bit 0)
             (dropr-zeroo frac prepared-frac)))))

#|
(frac-shifto frac n result)
    frac: The fraction of a MKFP number. 
    n: the number of least-significant bits from frac to remove.
    result: The result of removing n of the least significant bits from frac.

    Removes n of the least significant bits from frac and equates that to result.
|#
(define (frac-shifto frac n result)
    (fresh () (shifto-helper frac n result 1)))

#|
(shifto-helper frac n result curr-bit)
    frac: The fraction of a MKFP number. 
    n: the number of least-significant bits from frac to remove.
    result: The result of removing n of the least significant bits from frac.
    curr-bit: The current bit of n that we are iterating on.

    Removes n of the least significant bits from frac and equates that to result.
|#
(define (shifto-helper frac n result curr-bit)
  (conde ((== n '())
          (== frac result))

         ((fresh (n-first n-rest tmp b1 b2 b3 b4 b5 b6 b7 b8 next-bit)
            (== n (cons n-first n-rest))

            (conde ((== n-first 0)
                    (== frac tmp))
                   ((== n-first 1)
                    (conde
                        ((== curr-bit 1) ; remove 1 digit
                         (== frac (cons b1 tmp)))

                        ((== curr-bit 2) ; remove 2 digit
                         (== frac `(,b1 ,b2 . ,tmp)))
                        
                        ((== curr-bit 3) ; remove 4 digits
                         (== frac `(,b1 ,b2 ,b3 ,b4 . ,tmp)))
                        
                        ((== curr-bit 4) ; remove 8 digits
                         (== frac `(,b1 ,b2 ,b3 ,b4 ,b5 ,b6 ,b7 ,b8 . ,tmp))))))

            (conde ((== curr-bit 1) (== next-bit 2))
                   ((== curr-bit 2) (== next-bit 3))
                   ((== curr-bit 3) (== next-bit 4))
                   ((== curr-bit 4) (== next-bit 5)))
            
            (shifto-helper tmp n-rest result next-bit)))))

#|
(expo-lengtho expo)
    expo: A Oleg number.

    Ensures that expo contains no more than 8 bits.
|#
(define (expo-lengtho expo) 
    (fresh (r b b0 b1 b2 b3 b4 b5 b6 b7)
        (== b (list b0 b1 b2 b3 b4 b5 b6 b7)); b is a list of 8 bits.
        (appendo expo r b))) ; ensure that expo (++) r = b

#|
(frac-lengtho frac)
    frac: A Oleg number.

    Ensures that fraction contains exactly 16 bits.
|#
(define (frac-lengtho frac)
    (fresh (b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16)
        (== frac (list b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16))))

#|
(drop-leastsig-bito frac fracr)
    frac: The fraction being created that may have more than 24 bits.
    fracr: The MKFP fraction that takes the 24 most significant bits from frac.

Drops least significant bit in the fraction, where cap is 24 bits.
|#
(define (drop-leastsig-bito frac fracr)
    (fresh (bit)
        (frac-lengtho fracr)
        (appendo bit fracr frac)))

#|
(bias-shifted-pluso expo1 expo2 rexpo)
    expo1: The exponent of a MKFP number being multiplied.
    expo2: The exponent of a MKFP number being multiplied.
    rexpo: The result of [(expo1 + expo2) - 127]
|#
(define (bias-shifted-pluso expo1 expo2 rexpo)
    (fresh (exposum) 
        (pluso expo1 expo2 exposum)
        (pluso BIAS rexpo exposum)))

#|
(fp-pluso-normalize-expo frac frac-sum expo norm-expo)
    frac: The fraction of the MKFP number with the larger exponent.
    expo: The corresponding exponent of the MKFP number containing frac. 
    frac-sum: The result of the Oleg addition of the two fractions being added.
    norm-expo: The result of normalizing the exponent.

    Normalize the exponent (expo) based on the values of frac and frac-sum. 
    (adds one to expo when the addition rolls to a new bit position.)
|# 
(define (fp-pluso-normalize-expo frac expo frac-sum norm-expo)
    (conde ((== frac '())
            (== frac-sum '())
            (== expo norm-expo))
           
           ((== frac '())
            (=/= frac-sum '())
            (pluso '(1) expo norm-expo))

           ((fresh (fracfst fracrst frac-sumfst frac-sumrst)
                (== frac (cons fracfst fracrst))
                (== frac-sum (cons frac-sumfst frac-sumrst))
                (fp-pluso-normalize-expo fracrst expo frac-sumrst norm-expo)))))

#|
(fp-multo-normalize-expo expo frac-prod norm-expo)
    expo: The exponent of the product before normalization
    frac-prod: The fractoin of the product before dropping the least significant bits.
    norm-expo: The normalized exponent based off of expo and frac-prod. 

    Normalizes the exponent in floating point multiplication.
|#
(define (fp-multo-normalize-expo expo frac-prod norm-expo)
    (fresh (a b b-first b-rest rem temp)
        ; a and b have the same length as pre-fracr
        (frac-lengtho a)
        (frac-lengtho b)

        ; Decompose b into its first bit and remaining bits.
        (== b (cons b-first b-rest))

        ; Decompose pre-frac r into (a) ++ (b-rest) ++ (rem) 
        ; (a) ++ (b-rest) length = 16 + 15 = 31 bits
        (appendo a b-rest temp)
        (appendo temp rem frac-prod)

        ; rem == '()  --> length pre-fracr = 31
        ; rem =/= '() --> length pre-fracr >= 32
        (conde 
            ((== rem '())
             (== expo norm-expo))
            ((=/= rem '())
             (pluso '(1) expo norm-expo)))))

#|
(fp-samesignaddero sign1 expo1 frac1 sign2 expo2 frac2 expo-diff rsign rexpo rfrac)
    sign1: The sign of the MKFP number with the smaller exponent
    expo1: The exponent of the MKFP number with the smaller exponent
    frac1: The fraction of the MKFP number with the smaller exponent
    sign2: The sign of the MKFP number with the larger exponent
    expo2: The exponent of the MKFP number with the larger exponent
    frac2: The fraction of the MKFP number with the larger exponent
    expo-diff: An Oleg number that equals (expo2 - expo1)
    rsign: The resulting sign of the addition f1 + f2
    rexpo: The resulting exponent of the addition f1 + f2
    rfrac: The resulting fraction of the addition f1 + f2

    Floating-Point Addition for same signs
|#
(define (fp-samesignaddero sign1 expo1 frac1 sign2 expo2 frac2 expo-diff rsign rexpo rfrac)
    (fresh (shifted-frac1 frac-sum)
        (== sign1 sign2)
        (== rsign sign1); Ensure the signs are all the same before continuing

        ;shift the frac of the SMALLER exponent
        (frac-shifto frac1 expo-diff shifted-frac1)
        ; exponent shift to normalize
        (fp-pluso-normalize-expo frac2 expo2 frac-sum rexpo)
                 
        ;drop least-sig bit
        (drop-leastsig-bito frac-sum rfrac)
                  
        ; oleg number addition
        (pluso shifted-frac1 frac2 frac-sum)))

#|
(fp-swapo sign1 expo1 frac1 sign2 expo2 frac2 rsign rexpo rfrac)
    sign1: The sign of the MKFP number with the smaller exponent
    expo1: The exponent of the MKFP number with the smaller exponent
    frac1: The fraction of the MKFP number with the smaller exponent
    sign2: The sign of the MKFP number with the larger exponent
    expo2: The exponent of the MKFP number with the larger exponent
    frac2: The fraction of the MKFP number with the larger exponent
    rsign: The resulting sign of the addition f1 + f2
    rexpo: The resulting exponent of the addition f1 + f2
    rfrac: The resulting fraction of the addition f1 + f2

    Calls fp-samesignaddero so that the number out of f1 and f2 with the
    smaller exponent is entered first.
|#
(define (fp-swapo sign1 expo1 frac1 sign2 expo2 frac2 rsign rexpo rfrac)
    (fresh (expo-diff)  
        (conde
            ((pluso expo-diff expo1 expo2)
             (fp-samesignaddero sign1 expo1 frac1 sign2 expo2 frac2 expo-diff rsign rexpo rfrac))
            ((=/= expo1 expo2)
             (pluso expo-diff expo2 expo1)
             (fp-samesignaddero sign2 expo2 frac2 sign1 expo1 frac1 expo-diff rsign rexpo rfrac)))))

#|
(fp-pluso f1 f2 r)
    f1: A MKFP number.
    f2: A MKFP number.
    r: A MKFP number that satisfies f1 + f2 = r (under the rules of floating point addition)

    General Floating-Point Addition
|#
(define (fp-pluso f1 f2 r)
    (fresh (sign1 expo1 frac1 prepared-frac1
            sign2 expo2 frac2 prepared-frac2
            rsign rexpo rfrac prepared-rfrac)

        (fp-decompo f1 sign1 expo1 frac1)
        (fp-decompo f2 sign2 expo2 frac2)
        (fp-decompo r rsign rexpo rfrac)

        (prepare-fraco expo1 frac1 prepared-frac1)
        (prepare-fraco expo2 frac2 prepared-frac2)
        (prepare-fraco rexpo rfrac prepared-rfrac)

        (conde 
            ((== sign1 sign2)
             (== sign2 rsign)
             (fp-swapo sign1 expo1 prepared-frac1 sign2 expo2 prepared-frac2 rsign rexpo prepared-rfrac))
          

            ;Approach when signs are opposite
            ;When r has the same sign as f1 (+)
            ;f1+(-f2) = r -> f1 = r + f2
            ;fp-pluso (-f2, r, f1)
            ;When r has the same sign as f2 (-)
            ;f1 + (-f2) = -r -> -f2 = -r + (-f1)
            ;fp-pluso (r, -f1, f2)
            ((=/= sign1 sign2)
             (fresh (newsign)
                (conde ((== sign1 rsign)
                        (noto sign2 newsign)
                        (fp-swapo newsign expo2 prepared-frac2 rsign rexpo prepared-rfrac sign1 expo1 prepared-frac1))

                       ((== sign2 rsign)
                        (noto sign1 newsign)
                        (fp-swapo newsign expo1 prepared-frac1 rsign rexpo prepared-rfrac sign2 expo2 prepared-frac2))))))))


#|
(fp-multo f1 f2 r)
    f1: A MKFP number.
    f2: A MKFP number.
    r: A MKFP number that satisfies f1 * f2 = r (under the rules of floating point multiplication)

    General Floating Point Addition
|#
(define (fp-multo f1 f2 r)
    (fresh (sign1 expo1 frac1 sign2 expo2 frac2 rsign rexpo rfrac pre-rexpo pre-fracr)
        (fp-decompo f1 sign1 expo1 frac1)
        (fp-decompo f2 sign2 expo2 frac2)
        (fp-decompo r rsign rexpo rfrac)

        (not-specialvalo f1)
        (not-specialvalo f2)
         
        (xoro sign1 sign2 rsign)
        (drop-leastsig-bito pre-fracr rfrac)
         
        (fp-multo-normalize-expo pre-rexpo pre-fracr rexpo)
         
        (*o frac1 frac2 pre-fracr)
        (bias-shifted-pluso expo1 expo2 pre-rexpo)

        (expo-lengtho expo1)
        (expo-lengtho expo2)
        (expo-lengtho rexpo)))

; TODO: add constraint for -zero and +zero to be equal to one another.
#|
(fp-<= f1 f2)
    f1: A MKFP number.
    f2: A MKFP number.

    Relation that ensures f1 <= f2.
|#
(define (fp-<= f1 f2)
    (fresh (sign1 expo1 frac1 sign2 expo2 frac2)
        (fp-decompo f1 sign1 expo1 frac1)
        (fp-decompo f2 sign2 expo2 frac2)
        (conde ((== sign1 sign2)
                (== expo1 expo2)
                (<=o frac1 frac2))
               ((== sign1 sign2)
                (<=o expo1 expo2))
               ((=/= sign1 sign2)
                (== sign1 1)
                (== sign2 0)))))


#|
(fp-< f1 f2)
    f1: A MKFP number.
    f2: A MKFP number.

    Relation that ensures f1 < f2.
|#
(define (fp-< f1 f2)
  (fresh (sign1 expo1 frac1 sign2 expo2 frac2)
        (fp-decompo f1 sign1 expo1 frac1)
        (fp-decompo f2 sign2 expo2 frac2)
        (conde ((== sign1 sign2)
                (== expo1 expo2)
                (<o frac1 frac2))
               ((== sign1 sign2)
                (<o expo1 expo2))
               ((=/= sign1 sign2)
                (== sign1 1)
                (== sign2 0)))))

#|
(fp-= f1 f2)
    f1: A MKFP number.
    f2: A MKFP number.

    Relation that ensures f1 == f2.
|#
(define (fp-= f1 f2)
    (fresh (sign1 expo1 frac1 sign2 expo2 frac2)
        (fp-decompo f1 sign1 expo1 frac1)
        (fp-decompo f2 sign2 expo2 frac2)
        (conde ((== sign1 sign2)
                (== expo1 expo2)
                (== frac1 frac2)))))