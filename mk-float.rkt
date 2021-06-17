#lang racket

(require "mk.rkt")
(require "numbers.rkt")
(require "test-numbers.rkt") ;Un-comment require to play with test-numbers in REPL.
(provide fp-pluso fp-multo mantissa-lengtho expo-lengtho fp-< fp-<= fp-= precision)

(define BIAS (build-num 127))
(define precision 16)

(define float-format '(sign
                       expo ; Oleg number
                       mantissa)) ; kind of Oleg number?))


#|
(xoro b0 b1 br)
    b0 : 0 or 1
    b1: 0 or 1
    br: the result of (b0 xor b1)

    An exclusive-or relation.
|#
(define (xoro b0 b1 br)
    (conde 
        ((== b0 1) (== b1 1) (== br 0))
        ((== b0 0) (== b1 0) (== br 0))
        ((== b0 1) (== b1 0) (== br 1))
        ((== b0 0) (== b1 1) (== br 1))))

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
Decomposes fp number into sign, exponent, and mantissa
|#
(define (fp-decompo fp sign expo mantissa)
    (fresh (mantissa-head)
        (== fp (list sign expo mantissa))
        (mantissa-lengtho mantissa)
        (conde 
            ((=/= expo '()) (appendo mantissa-head (list 1) mantissa))
            ((== expo '())  (== mantissa (make-list precision 0))))))

#|
(fp-finiteo sign expo mantissa)
    sign: The sign bit of a MKFP number.
    expo: The exponent of a MKFP number.
    mantissa: The mantissa of a MKFP number.

Checks if the floating point number given by the arguments -- sign, expo, and mantissa --
do not represent an infinity/NaN (i.e a special value).
|#
(define (fp-finiteo sign expo mantissa)
    (fresh ()
        (conde 
            ((== sign 0))
            ((== sign 1)))
        (=/= expo '(1 1 1 1  1 1 1 1))))

#|
(fp-infiniteo sign expo mantissa)
    sign: The sign bit of a MKFP number.
    expo: The exponent of a MKFP number.
    mantissa: The mantissa of a MKFP number.

Checks if the floating point number given by the arguments -- sign, expo, and mantissa --
represent an infinity/NaN (i.e a special value).
|#
(define (fp-infiniteo sign expo mantissa)
    (fresh ()
        (conde 
            ((== sign 0))
            ((== sign 1)))
        (== expo '(1 1 1 1  1 1 1 1))
        (== mantissa (append (make-list (- precision 1) 0) '(1)))))

#|
(fp-zeroo sign expo mantissa)
    sign: The sign bit of a MKFP number.
    expo: The exponent of a MKFP number.
    mantissa: The mantissa of a MKFP number.

Ensures that the floating point number given by the arguments -- sign, expo, and mantissa -- represent
zero.
|#
(define (fp-zeroo sign expo mantissa)
    (fresh ()
        (conde 
            ((== sign 0))
            ((== sign 1)))
        (== expo '())
        (== mantissa (make-list precision 0))))

#|
(fp-nonzeroo  sign expo mantissa)
    sign: The sign bit of a MKFP number.
    expo: The exponent of a MKFP number.
    mantissa: The mantissa of a MKFP number.

Ensures that the floating point number given by the arguments -- sign, expo, and mantissa -- represent
a positive or negative quantity.
|#
(define (fp-nonzeroo sign expo mantissa)
    (fresh ()
        (conde 
            ((== sign 0))
            ((== sign 1)))
        (=/= expo '())
        (=/= mantissa (make-list precision 0))))



#|
(mantissa-shifto mantissa n result)
    mantissa: The mantissa of a MKFP number. 
    n: the number of least-significant bits from mantissa to remove.
    result: The result of removing n of the least significant bits from mantissa

    Removes n of the least significant bits from mantissa and equates that to result.
|#
(define (mantissa-shifto mantissa n result)
    (fresh (template remain b0 b1 b2 b3)
        (== template (list b0 b1 b2 b3))

        (conde 
            ((appendo n remain template) ;captures when length n <= 4
             (shifto-helper mantissa n result 1))
            
            ((=/= remain '()); Remainder of list cannot be empty
             (appendo template remain n); Together with above captures length n > 4 -> n >= 16
             (== result '())))))

#|
(advance-bit#o bit next-bit)
    curr-bit: The current bit of an oleg number we are iterating over.
    next-bit: The next bit of an oleg number we are iterating over.
    (== nextbit (+ 1 bit))
|#
(define (advance-bit#o bit next-bit)
    (fresh () 
        (conde 
            ((== bit 1) (== next-bit 2))
            ((== bit 2) (== next-bit 3))
            ((== bit 3) (== next-bit 4))
            ((== bit 4) (== next-bit 5)))))

#|
(shifto-helper mantissa n result curr-bit)
    mantissa: The mantissa of a MKFP number. 
    n: the number of least-significant bits from mantissa to remove.
    result: The result of removing n of the least significant bits from mantissa
    curr-bit: The current bit of n that we are iterating on.

    Removes n of the least significant bits from mantissa and equates that to result.
|#
(define (shifto-helper mantissa n result curr-bit)
  (conde 
        ((== n '())
         (== mantissa result))

        ((fresh (n-first n-rest next-bit next-n next-mantissa)
         (== n (cons n-first n-rest))
         (conde 
            ((== n-first 0) (== next-mantissa mantissa) (== next-n n-rest))

            ((== n-first 1)
                (conde
                ((== curr-bit 1) ; removing 1 binary digit
                    (fresh (remain b0) 
                        (conde 
                            ((== mantissa (cons b0 remain)) ; Captures when (length mantissa) >= 1
                                (== next-mantissa remain)
                                (== next-n n-rest))
                                                            
                            ((== mantissa '()) 
                                (== next-mantissa '())
                                (== next-n '())))))
                        
                ((== curr-bit 2) ; remove 2 binary digits
                    (fresh (template remain b0 b1)
                        (== template (list b0 b1))
                        (conde 
                            ((appendo template remain mantissa) ; Captures when (length mantissa) >= 2
                                (== next-mantissa remain)
                                (== next-n n-rest))

                            ((=/= remain '())
                                (appendo mantissa remain template) ; Captures when (length mantissa) < 2 
                                (== next-mantissa '())
                                (== next-n '())))))
                
                ((== curr-bit 3) ; remove 4 binary digits
                    (fresh (template remain b0 b1 b2 b3) 
                        (== template (list b0 b1 b2 b3))
                        (conde
                            ((appendo template remain mantissa) ; Captures when (length mantissa) >= 4
                                (== next-mantissa remain)
                                (== next-n n-rest)) 
                                
                            ((=/= remain '())
                                (appendo mantissa remain template) ; Captures when (length mantissa) < 4
                                (== next-mantissa '())
                                (== next-n '())))))
                
                ((== curr-bit 4) ; remove 8 binary digits
                    (fresh (template remain b0 b1 b2 b3 b4 b5 b6 b7)
                        (== template (list b0 b1 b2 b3 b4 b5 b6 b7)) 
                        (conde 
                            ((appendo template remain mantissa) ; Captures when (length mantissa) >= 4
                                (== next-mantissa remain)
                                (== next-n n-rest))

                            ((=/= remain '())
                                (appendo mantissa remain template) ; Captures when (length mantissa) < 4
                                (== next-mantissa '())
                                (== next-n '()))))))))
        
            (advance-bit#o curr-bit next-bit)
            
            (shifto-helper next-mantissa next-n result next-bit) ))))

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
(mantissa-lengtho mantissa)
    mantissa: A Oleg number.

    Ensures that the mantissa contains exactly 16 bits.
|#
(define (mantissa-lengtho mantissa)
    (fresh (b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15)
        (== mantissa
            (take (list b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15) precision))))

#|
(drop-leastsig-bito mantissa rmantissa)
    mantissa: The mantissa being created that may have more than 24 bits.
    rmantissa: The MKFP mantissa that takes the 24 most significant bits from mantissa.

Drops least significant bit in the mantissa, where cap is 24 bits.
|#
(define (drop-leastsig-bito mantissa rmantissa bit)
    (fresh ()
        (mantissa-lengtho rmantissa)
        (appendo bit rmantissa mantissa)))


#|
(fp-overflowo expo mant rmant)
    expo: A MKFP oleg exponent
    mant: A MKFP mantissa
    rmant: A MKFP mantissa

    If expo is '(1 1 1 1  1 1 1 1), set the rmant to '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 1)
    otherwise mant == rmant
|#
(define (fp-overflowo expo mant rmant)
    (conde 
        ((== expo '(1 1 1 1  1 1 1 1))
         (== rmant (append (make-list (- precision 1) 0) '(1)))) 
        ((=/= expo '(1 1 1 1  1 1 1 1))
         (== mant rmant))))

#|
(fp-samesignaddero sign1 expo1 mant1 sign2 expo2 mant2 expo-diff rsign rexpo rmant)
    sign1: The sign of the MKFP number with the smaller exponent
    expo1: The exponent of the MKFP number with the smaller exponent
    mant1: The mantissa of the MKFP number with the smaller exponent
    sign2: The sign of the MKFP number with the larger exponent
    expo2: The exponent of the MKFP number with the larger exponent
    mant2: The mantissa of the MKFP number with the larger exponent
    expo-diff: An Oleg number that equals (expo2 - expo1)
    rsign: The resulting sign of the addition f1 + f2
    rexpo: The resulting exponent of the addition f1 + f2
    rmant: The resulting mantissa of the addition f1 + f2

    Floating-Point Addition for same signs
|#
(define (fp-samesignaddero sign1 expo1 mant1 sign2 expo2 mant2 expo-diff rsign rexpo rmant)
    (fresh (shifted-mant1 mant-sum pre-rmant bit)
        (== sign1 sign2)
        (== rsign sign1); Ensure the signs are all the same before continuing

        ;shift the mantissa of the SMALLER exponent
        (mantissa-shifto mant1 expo-diff shifted-mant1)
        
        (conde ((== bit '()) (== rexpo expo2))
               ((=/= bit '()) (pluso '(1) expo2 rexpo)))
        (drop-leastsig-bito mant-sum pre-rmant bit)
        (fp-overflowo rexpo pre-rmant rmant)

        ; oleg number addition
        (pluso shifted-mant1 mant2 mant-sum)))

#|
(fp-swapo sign1 expo1 mant1 sign2 expo2 mant2 rsign rexpo rmant)
    sign1: The sign of the MKFP number with the smaller exponent
    expo1: The exponent of the MKFP number with the smaller exponent
    mant1: The mantissa of the MKFP number with the smaller exponent
    sign2: The sign of the MKFP number with the larger exponent
    expo2: The exponent of the MKFP number with the larger exponent
    mant2: The mantissa of the MKFP number with the larger exponent
    rsign: The resulting sign of the addition f1 + f2
    rexpo: The resulting exponent of the addition f1 + f2
    rmant: The resulting mantissa of the addition f1 + f2

    Calls fp-samesignaddero so that the number out of f1 and f2 with the
    smaller exponent is entered first.
|#
(define (fp-swapo sign1 expo1 mant1 sign2 expo2 mant2 rsign rexpo rmant)
    (fresh (expo-diff)  
        (conde
            ((pluso expo-diff expo1 expo2)
             (fp-samesignaddero sign1 expo1 mant1 sign2 expo2 mant2 expo-diff rsign rexpo rmant))
            ((=/= expo1 expo2)
             (pluso expo-diff expo2 expo1)
             (fp-samesignaddero sign2 expo2 mant2 sign1 expo1 mant1 expo-diff rsign rexpo rmant)))))

#|
(fp-pluso f1 f2 r)
    f1: A MKFP number.
    f2: A MKFP number.
    r: A MKFP number that satisfies f1 + f2 = r (under the rules of floating point addition)

    General Floating-Point Addition
|#
(define (fp-pluso f1 f2 r)
    (fresh (sign1 expo1 mant1 sign2 expo2 mant2 rsign rexpo rmant)
        (fp-decompo f1 sign1 expo1 mant1)
        (fp-decompo f2 sign2 expo2 mant2)
        (fp-decompo r rsign rexpo rmant)
        
        (conde
            ((fp-zeroo sign1 expo1 mant1); 0 + 0 = 0
             (fp-zeroo sign2 expo2 mant2)
             (fp-zeroo rsign rexpo rmant))

            ; x + 0 = x (x \in R)
            ((fp-nonzeroo sign1 expo1 mant1) (fp-finiteo sign1 expo1 mant1)
             (fp-zeroo sign2 expo2 mant2)
             (fp-= f1 r))

            ; 0 + y = y (y \in R)
            ((fp-nonzeroo sign2 expo2 mant2) (fp-finiteo sign2 expo2 mant2)
             (fp-zeroo sign1 expo1 mant1)
             (fp-= f2 r))

            ((noto sign1 sign2) ; (x) + (-x) = 0 (x \in R)
             (fp-nonzeroo sign1 expo1 mant1) (fp-finiteo sign1 expo1 mant1)
             (fp-nonzeroo sign2 expo2 mant2) (fp-finiteo sign2 expo2 mant2)
             
             (fp-zeroo rsign rexpo rmant)
             (== expo1 expo2)
             (== mant1 mant2))

            ;(+/- x) + (+/- y) = (+/- z) (x, y \in R) (z \in R \cup {+/- \inf})
            ((fp-nonzeroo sign1 expo1 mant1) (fp-finiteo sign1 expo1 mant1)
             (fp-nonzeroo sign2 expo2 mant2) (fp-finiteo sign2 expo2 mant2)
             
             (fp-nonzeroo rsign rexpo rmant)
             (== sign1 sign2)
             (== sign2 rsign)
             (fp-swapo sign1 expo1 mant1 sign2 expo2 mant2 rsign rexpo rmant))
          
            ;Approach when signs are opposite
            ;When r has the same sign as f1 (+)
            ;f1+(-f2) = r -> f1 = r + f2
            ;fp-pluso (-f2, r, f1)
            ;When r has the same sign as f2 (-)
            ;f1 + (-f2) = -r -> -f2 = -r + (-f1)
            ;fp-pluso (r, -f1, f2)
            ((fp-nonzeroo sign1 expo1 mant1) (fp-finiteo sign1 expo1 mant1)
             (fp-nonzeroo sign2 expo2 mant2) (fp-finiteo sign2 expo2 mant2)
             (fp-nonzeroo rsign rexpo rmant)
             (noto sign1 sign2)
             (== sign1 rsign)
             (fp-swapo sign1 expo2 mant2 rsign rexpo rmant sign1 expo1 mant1))

            ((fp-nonzeroo sign1 expo1 mant1) (fp-finiteo sign1 expo1 mant1)
             (fp-nonzeroo sign2 expo2 mant2) (fp-finiteo sign2 expo2 mant2)
             (fp-nonzeroo rsign rexpo rmant)
             (noto sign1 sign2)
             (== sign2 rsign)
             (fp-swapo sign2 expo1 mant1 rsign rexpo rmant sign2 expo2 mant2))
             
            ((== sign1 sign2)  (== rsign sign2);  (+/- \inf) + (+/- \inf) = (+/- \inf)
             (fp-infiniteo sign1 expo1 mant1)
             (fp-infiniteo sign2 expo2 mant2)
             (fp-infiniteo rsign rexpo rmant))

            ((== sign2 rsign);   c + (+/- \inf) = (+/- \inf) (c \in R)
             (fp-finiteo sign1 expo1 mant1)
             (fp-infiniteo sign2 expo2 mant2)
             (fp-infiniteo rsign rexpo rmant))

            ((== sign1 rsign);   (+/- \inf) + c = (+/- \inf) (c \in R)
             (fp-infiniteo sign1 expo1 mant1)
             (fp-finiteo sign2 expo2 mant2)
             (fp-infiniteo rsign rexpo rmant)))
             
            (expo-lengtho expo1)
            (expo-lengtho expo2)
            (expo-lengtho rexpo)))


#|
(fp-multo f1 f2 r)
    f1: A MKFP number.
    f2: A MKFP number.
    r: A MKFP number that satisfies f1 * f2 = r (under the rules of floating point multiplication)

    General Floating Point Addition
|#
(define (fp-multo f1 f2 r)
    (fresh (sign1 expo1 mant1 sign2 expo2 mant2 rsign rexpo rmant
            expo-sum bias-diff pre-rexpo mant1mant2 pre-mantr template throw-out template-end ls-bits rem)
        (fp-decompo f1 sign1 expo1 mant1)
        (fp-decompo f2 sign2 expo2 mant2)
        (fp-decompo r rsign rexpo rmant)

        (xoro sign1 sign2 rsign)

        (conde 
            ; 0*0 = 0
            ((fp-zeroo rsign rexpo rmant)    
             (fp-zeroo sign1 expo1 mant1)
             (fp-zeroo sign2 expo2 mant2))

            ; 0 * y = 0  (y \in R: y != 0, +/- \inf) 
            ((fp-zeroo rsign rexpo rmant)     
             (fp-zeroo sign1 expo1 mant1)
             (fp-nonzeroo sign2 expo2 mant2) (fp-finiteo sign2 expo2 mant2))

            ; x * 0 = 0 (x \in R: x != 0, +/- \inf) 
            ((fp-zeroo rsign rexpo rmant)    
             (fp-nonzeroo sign1 expo2 mant1) (fp-finiteo sign1 expo1 mant1)
             (fp-zeroo sign2 expo2 mant2))

            ; x * y = z (x,y,z != 0) (x, y \in R: x,y != 0, +/- \inf) 
            ((fp-nonzeroo rsign rexpo rmant)
             (fp-nonzeroo sign1 expo1 mant1) (fp-finiteo sign1 expo1 mant1)
             (fp-nonzeroo sign2 expo2 mant2) (fp-finiteo sign2 expo2 mant2)

             (== bias-diff pre-rexpo)

             (*o mant1 mant2 mant1mant2); pre-mantr  will have either 2*precision - 1 > number of bits.

             (drop-leastsig-bito mant1mant2 pre-mantr ls-bits); need to check if we still round down to 0 if final exponent = 0.

             (mantissa-lengtho template); create a template with 16 bits
             (== template (cons throw-out template-end)); throw out 1 bit to get a 15 bit template

             (appendo template-end rem ls-bits); fit the least significant bits to the template.
             
             (conde 
                 ((=/= rem '())        ; If we add one to the exponent, just set the resultant mantissa to the mantissa with precision bits (pre-mantr)
                  (pluso '(1) pre-rexpo rexpo)
                  (fp-overflowo rexpo pre-mantr rmant))
                 
                 ((== rem '())          ; If we dont add one to the resultant exponent and the exponent is non-zero
                  (=/= pre-rexpo '())
                  (== rexpo pre-rexpo)
                  (fp-overflowo rexpo pre-mantr rmant))

                 ((== rem '())         ; If we dont add one to the exponent and the resultant exponent is zero, round down to zero.
                  (== pre-rexpo '())
                  (== rexpo pre-rexpo)
                  (== rmant (make-list precision 0))))
             
             (pluso expo1 expo2 expo-sum)
             (pluso BIAS bias-diff expo-sum))

            ; x * y = 0 (x, y \in R: x,y != 0, +/- \inf)  underflow
            ((fp-zeroo rsign rexpo rmant) 
             (fp-nonzeroo sign1 expo1 mant1) (fp-finiteo sign1 expo1 mant1)
             (fp-nonzeroo sign2 expo2 mant2) (fp-finiteo sign2 expo2 mant2)
             (=/= bias-diff '())
             (pluso expo1 expo2 expo-sum)
             (pluso expo-sum bias-diff BIAS)) ; expo1 + expo2 < 127
            
            ; (s1 \inf) * (s2 \inf) = ((s1 ^ s2) \inf)
            ((fp-infiniteo rsign rexpo rmant)
             (fp-infiniteo sign1 expo1 mant1)
             (fp-infiniteo sign2 expo2 mant2))

            ; (s1 \inf) * (s2 y) = ((s1 ^ s2) \inf) (y \in R: y != 0, +/- \inf) 
            ((fp-infiniteo rsign rexpo rmant)
             (fp-infiniteo sign1 expo1 mant1)
             (fp-nonzeroo sign2 expo2 mant2) (fp-finiteo sign2 expo2 mant2))

            ; (s1 x) * (s2 \inf) = ((s1 ^ s2) \inf) (x \in R: x != 0, +/- \inf) 
            ((fp-infiniteo rsign rexpo rmant)
             (fp-infiniteo sign1 expo1 mant1)
             (fp-nonzeroo sign2 expo2 mant2) (fp-finiteo sign2 expo2 mant2)))        
              

        (expo-lengtho expo1)
        (expo-lengtho expo2)
        (expo-lengtho rexpo)))

; TODO: add constraint for -zero and +zero to be equal to one another.
#|
(fp-< f1 f2)
    f1: A MKFP number.
    f2: A MKFP number.

    Relation that ensures f1 < f2.
|#
(define (fp-< f1 f2)
    (fresh (sign1 expo1 mant1 sign2 expo2 mant2)
        (fp-decompo f1 sign1 expo1 mant1)
        (fp-decompo f2 sign2 expo2 mant2)
        (conde
            ((fp-zeroo sign1 expo1 mant1)
             (fp-nonzeroo sign2 expo2 mant2)
             (== sign2 0))
    
            ((fp-nonzeroo sign1 expo1 mant1)
             (fp-zeroo sign2 expo2 mant2)
             (== sign1 1))
    
            ((fp-nonzeroo sign1 expo1 mant1)
             (fp-nonzeroo sign2 expo2 mant2)
             (== sign1 sign2)
             (== expo1 expo2)
             (<o mant1 mant2))

            ((fp-nonzeroo sign1 expo1 mant1)
             (fp-nonzeroo sign2 expo2 mant2)
             (== sign1 sign2)
             (<o expo1 expo2))
     
            ((fp-nonzeroo sign1 expo1 mant1)
             (fp-nonzeroo sign2 expo2 mant2)
             (== sign1 1)
             (== sign2 0)))))

#|
(fp-= f1 f2)
    f1: A MKFP number.
    f2: A MKFP number.

    Relation that ensures f1 == f2.
|#
(define (fp-= f1 f2)
    (fresh (sign1 expo1 mant1 sign2 expo2 mant2)
        (fp-decompo f1 sign1 expo1 mant1)
        (fp-decompo f2 sign2 expo2 mant2)
        (conde 
            ((fp-zeroo sign1 expo1 mant1) (fp-zeroo sign2 expo2 mant2)) ; want to make sure -0 = +0
            
            ((fp-nonzeroo sign1 expo1 mant1) (fp-nonzeroo sign2 expo2 mant2)
             (== sign1 sign2) (== expo1 expo2) (== mant1 mant2)))))

#|
(fp-<= f1 f2)
    f1: A MKFP number.
    f2: A MKFP number.

    Relation that ensures f1 <= f2.
|#
(define (fp-<= f1 f2)
    (conde ((fp-= f1 f2)) ((fp-< f1 f2))))

#|
(fp-negateo f negated-f)
    f: A MKFP number
    negated-f: A MKFP number which whas the opposite sign of f.
|#
(define (fp-negateo f negated-f)
    (fresh (signf signnf expo mant)
        (fp-decompo f signf expo mant)
        (fp-decompo negated-f signnf expo mant)
        (noto signf signnf)))