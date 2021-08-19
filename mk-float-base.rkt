#lang racket

(require "mk.rkt")
(require "numbers.rkt")

(provide PRECISION BIAS UNIT-MANTISSA ZERO-MANTISSA FULL-EXP
         expo-lengtho mantissa-lengtho drop-leastsig-bito fp-overflowo
         fp-finiteo fp-infiniteo fp-zeroo fp-nonzeroo fp-decompo
         mantissa-shifto fp-swapo fp-specialpluso fp-specialmulto
         fp-multo-compute-expoo xoro noto
         POS-INFINITY NEG-INFINITY
         get-sign get-exp get-mantissa reify-mantissa reify-exp reify-sign inf? build-truncated-float-helper)

; Constants used for the entire system
(define PRECISION 16) ; CHANGE THIS TO CHANGE THE PRECISION OF THE RELATION SYSTEM

(define ZERO-MANTISSA (make-list PRECISION 0))
(define FULL-EXP '(1 1 1 1  1 1 1 1))

; =====================
; | Reifier Base Code |
; =====================

; Constant for reporting a reification error
(define FP-ERR 'FP-ERR)

; Constants for floats that aren't numbers
(define POS-INFINITY 'positive-infinity)
(define NEG-INFINITY 'negative-infinity)
(define NaN 'NaN)

; Numerical constants used for reification.
(define EXPONENT-SHIFT 127) ; The shifting factor for the exponenet field.
(define SMALLEST-PRECISION-EXP 149) ; The smallest representable exponent for the LSB.

; --------------------------------
; | Representation Decomposition |
; --------------------------------

#|
(get-sign f)
  f: A MKFP number

  Returns the sign bit of f. 
|#
(define (get-sign f) (first f))

#|
(get-exp f)
  f: A MKFP number

  Returns the eight exponent bits of f. 
|#
(define (get-exp f) (second f))

#|
(get-mantissa f)
  f: A MKFP number

  Returns the 23 fractional bits of f. Also known as the significand or mantissa. 
|#
(define (get-mantissa f) (third f))

; -------------------
; | Reify Functions |
; -------------------

#|
(reify-frac mantissa)
  mantissa: The mantissa bits of a MKFP number.

  Returns the Racket float equivallent of what the mantissa is. 
|#
(define (reify-mantissa mantissa)
    (reify-bitlist mantissa (+ (- PRECISION) 1)))

#|
(reify-exp exponent)
  exp: The exponent bits of a MKFP number.

  Returns the value of the shifted exponent. 
|#
(define (reify-exp exponent)
    (- (reify-bitlist exponent 0) EXPONENT-SHIFT)) ; StoredExponent - 127


#|
(reify-sign man)
  sgn: The sign bit of a MKFP number.

  Returns 1 if sgn is zero and -1 if sgn is 1. Otherwise FP-ERR is returned. 
|#
(define/match (reify-sign sign)
  [(0) 1]
  [(1) -1]
  [(_) FP-ERR])

; Function so that the reifier can identify infinities.

#|
(inf? exponent man)
  exponent: The exponent bits of a MKFP number.
  mantissa: The mantissa bits of a MKFP number.

  Returns true if and only if all the exponent bits are 1 and if all the mantissa bits are 0. 
|#
(define (inf? exponent mantissa)
    (let* 
        ([inf-exponent?  (equal? exponent FULL-EXP)]
         [MSB-one?       (equal? 1 (last mantissa))]
         [zero-fraction? (andmap zero? (drop-right mantissa 1))]) ; Is the fraction all zeros after the MSB.

        (and inf-exponent? MSB-one? zero-fraction?)))

; Tail recursive function for turning a list of binary digits (little-endian)
; into a numeric value.

#|
(bitlist-to-int-helper bitlist LSB-exponent acc)
    bitlist: A list of bits in little endian form.
    LSB-exponent: The implicit exponent of the first bit (set to zero to get integers only)

    if bitlist = (b0, b1, b2, ..., bn) and LSB-exponent = s then we return the value of the following
    sum:
    (b0*2^s + b1*2^{s+1} + b2*2^{s+2} + ... + bn*2^{s+n})

    Notice that if LSB-exponent = 0 then we return the integer interpretation of the bitlist.
|#
(define (reify-bitlist bitlist LSB-exponent)
    (reify-bitlist-helper bitlist LSB-exponent 0))

#|
(bitlist-to-int-helper bitlist LSB-exponent acc)
    bitlist: A list of bits in little endian form.
    LSB-exponent: The implicit exponent of the first bit (set to zero to get integers only)
    acc: Accumulator for the sum.

    Tail recursive helper function for converting a list of bits into racket number.
|#
(define/match (reify-bitlist-helper bitlist LSB-exponent acc)
    [((list) _ acc) acc]

    [((cons 0 bitlist-rest) k acc) 
        (reify-bitlist-helper bitlist-rest (+ k 1) acc)]
    
    [((cons 1 bitlist-rest) k acc) 
        (let* 
            ([next-k (+ k 1)]
            [next-acc (+ acc (expt 2 k))])

            (reify-bitlist-helper bitlist-rest next-k next-acc))])


; ---------------------------------------------------------
; | Build A MK Floating-Point Number Function [Truncated] |
; ---------------------------------------------------------

; Functions for turning an integer into a bitlist which represents the same integer in binary.

#|
(int-to-bitlist z)
  z: integer? and positive?

  Returns a list of bits with no leading 0's that represent z in binary.
  Note that the list is in little-endian format, i.e. the least significant bit is first.
  Note that z must be non-negative.
|#
(define (int-to-bitlist z) (int-to-bitlist-helper z '()))

#|
(int-to-bitlist-helper z acc)
  z: integer? and positive?
  acc: An accumulator for the bit list.
  Returns a list of bits with no leading 0's that represent z in binary.
  Note that the list is in little-endian format, i.e. the least significant bit is first.
  Note that 0 is represented as an empty list.
|#
(define (int-to-bitlist-helper z acc)
  (cond
    [(<= z 0) (reverse acc)]
    [else (let*-values ([(q r) (quotient/remainder z 2)])
            (cond
              [(equal? q 0) (int-to-bitlist-helper -1 (cons r acc))]
              [else         (int-to-bitlist-helper q (cons r acc))]))]))


; Function that builds an integer bitlist but one which is capped by a maximum number of bits.

#|
(build-intbitlist z p)
  z: The integer being converted to a magnitude bitlist.
  p: The precision of the bitlist.
  Returns a pair (bitlist . exp)
  bitlist:
    - A bitlist representing the most significant bits of z using only p degrees of precision.
    - 0 <= (length bitlist) <= p
  exp:
    - The exponent of the most significant bit.
|#
(define (build-intbitlist z p) (build-intbitlist-helper z p '() -1))

#|
(build-intbitlist-helper z p bitlist expo)
  z: The integer being built.
  p: The precision of the bitlist.
  bitlist: The bit accumulator for z.
  expo: The exponent accumulator for z.
  Returns the pair (bitlist' . expo') where bitlist is at most 24 bits long and expo'is the expooent
  of the most significant bit.
|#
(define/match (build-intbitlist-helper z p bitlist expo)
  [(z _ bitlist expo) #:when (= z 0) (cons (reverse bitlist) expo)]
  [(z p bitlist expo) (let* ([pair (intbit-generator z)]
                             [residual (car pair)]
                             [bit (cdr pair)]
                             
                             [next-expo (+ 1 expo)]
                             [next-bitlist (if (< next-expo p) ; This causes the list to act like a window that shifts along until the leading bit is found.
                                               (cons bit bitlist)
                                               (cons bit (drop-right bitlist 1)))])

                            (build-intbitlist-helper residual p next-bitlist next-expo))] )


#|
(build-fracbitlist r p)
  r: The real numbered fraction being converted to a magnitude bitlist.
  p: The precision of the bitlist.

  Returns a pair (bitlist . exp)
  bitlist:
    - A bitlist representing the most significant bits of r using only p degrees of precision.
    - (length bitlist) = p
  exp:
    - The exponent of the most significant bit.
|#
(define (build-fracbitlist r p) 
      (let* ([advance-pair (advance-to-leading-bit r fracbit-generator 0 (- SMALLEST-PRECISION-EXP p))]; Advance no more than SMALLEST-PRECISION-EXP - p positions (when single this is 125 = 149-24)
             [advanced-r   (car advance-pair)]; Either the x that will generate the leading 1 or the x that generates the 126th 0.
             [n-calls      (cdr advance-pair)]
             [bitlist      (bitlist-filler '() p fracbit-generator advanced-r)])
            
            (cons bitlist (+ n-calls 1))))

#|
(bitlist-filler bitlist p generate x)
  bitlist: The current bitlist created so far.
  p: The precision.
  generate: A function that takes in a number and spits out a pair (residual . bit)

  Fills the rest of the precision left in the bitlist.
|#
(define/match (bitlist-filler bitlist p generate x) 
  [(bitlist p _ _) #:when (equal? p (length bitlist)) bitlist]
  [(bitlist p generate x) 
                          (let* ([pair     (generate x)]
                                 [residual (car pair)]
                                 [bit      (cdr pair)]

                                 [next-bitlist  (cons bit bitlist)])

                               (bitlist-filler next-bitlist p generate residual))])

#|
(advance-to-leading-bit x generate n cap)
  x: The number to advance.
  generate: the advancing function.
  cap: The max number of times generate will be called.
  n: an accumulator that counts how many times generate was called.

  Returns the pair (x' . n') where x' is the result of calling generate n' times succesively.
  0 <= n' <= cap.
|#
(define/match (advance-to-leading-bit x generate n cap)
  [(x _ z z)          (cons x z)]; When n == cap.
  [(x generate n cap) (let* ([pair     (generate x)]
                             [residual (car pair)]
                             [bit      (cdr pair)])
                            
                            (cond 
                              [(equal? bit 0) (advance-to-leading-bit residual generate (+ n 1) cap)]
                              [(equal? bit 1) (advance-to-leading-bit x generate n n)] ))]; This is the case when x generated a 1 so we must stop here.
)            

#|
(intbit-generator z)
  z: integer?
  Returns the pair (residual . bit). residual, bit and z satisfy: z = 2*(residual) + bit
|#
(define (intbit-generator z) (let*-values ([(q r) (quotient/remainder z 2)])
                                          (cons q (inexact->exact r))))


#|
(fracbit-generator r)
  r: and pos? real? (<= r 1)
  Returns the pair (residual . bit). residual and bit: residual = {2*r} && bit = floor(2 * r) 
|#
(define (fracbit-generator r) (let* ([double-r (* r 2)]
                                     [bit      (if (< double-r 1) 0 1)]
                                     [residual (- double-r bit)]) 
                                    (cons residual bit)))                

#|
(decompose-real r)
  r: A real number
  Return a three element list; (sign, int, frac) where:
    - sign: 0 if non-negative 1 if negative
    - int: The floor of the absolute value of r. (-4.5 -> 4)
    - frac: The fractional part of r. note: 0 <= frac < 1
|#
(define (decompose-real r) 
  (let*
        ([sign  (if (< r 0) 1 0)]
         [abs-r (abs r)]
         [int   (floor abs-r)]
         [frac  (- abs-r int)])
        
        (list sign int frac)))

#|
(build-truncated-float-helper r p)
  r: A number.
  p: The number of bits used in the mantissa.

  Helper function for build-truncated-float. This function builds a floating-point
  representation of r using truncated rounding. The returned representation will use
  one bit for the sign of r, 8 bits for the magnitude of r [exponent], and exactly p
  bits for the mantissa.

  This function breaks the problem into three main cases.
  1) It is a large integer that has no room in the mantissa for anything that would
     follow a decimal point.
  
  2) It is a number in the interval [0, 1) 

  3) It is a number with a non-zero integer part and a non-zero fractional part.

|#
(define (build-truncated-float-helper r p)
    (let* 
        ([sign-int-frac   (decompose-real r)]
         [sign            (first sign-int-frac)]
         [integer-part    (second sign-int-frac)]
         [fractional-part (third sign-int-frac)]
         
         [intbitlist-pair   (build-intbitlist integer-part p)]
         [binary-integer    (car intbitlist-pair)]
         [intMSB-exp        (cdr intbitlist-pair)]
         [integer-bitlength (+ intMSB-exp 1)])
      (cond
      ; Case 1.
      [(>= intMSB-exp p) (let* ([exp-n (+ EXPONENT-SHIFT intMSB-exp)]
                                       [exponent (int-to-bitlist exp-n)])

                                    (list sign exponent binary-integer))] ; We know we are dealing with a pure integer.

      ; Case 2.
      [(equal? intMSB-exp -1) (cond [(equal? fractional-part 0) `(0 () ,ZERO-MANTISSA)]
                                    [else
                                       (let* ([frac-pair   (build-fracbitlist fractional-part p)]; need p bits, Dealing with a pure fraction.
                                              [frac        (car frac-pair)]
                                              [fracMSB-exp (cdr frac-pair)]

                                              [denormal? (and (equal? (last frac) 0) (equal? (+ (- SMALLEST-PRECISION-EXP p) 1) fracMSB-exp))]
                                              [adjusted-fracMSB-exp (if denormal?
                                                                        EXPONENT-SHIFT            ;; Truncate to zero, exponent must be zero (logical form of zero, not stored form)
                                                                        fracMSB-exp)]

                                              [adjusted-frac (if denormal?
                                                                 ZERO-MANTISSA ;; Truncate to zero.
                                                                 frac)]; leave it the same.

                                              [exp-n    (- EXPONENT-SHIFT adjusted-fracMSB-exp)]
                                              [exponent (int-to-bitlist exp-n)])

                                         (list sign exponent adjusted-frac))])]

      ; Case 3.
      [else (let* ([remaining-bits (- p integer-bitlength)]; p - #of integer bits
                   [frac-pair     (build-fracbitlist fractional-part remaining-bits)]; need the remianing-precision bits.
                   [frac          (car frac-pair)]; Extract the computed fraction.
                   [fracMSB-exp   (cdr frac-pair)]; Extract the exponent of the most significant bit from the fractional part.

                   ; Compute the amount of bits taken from frac as we may need to add leading zeros before taking the frac bits.
                   [leading-zeros      (- fracMSB-exp 1)]
                   [zero-bits          (min remaining-bits leading-zeros)]; There maybe to many zeros between the fraction and the integer so we take the min.
                   [zero-bitlist       (make-list zero-bits 0)]
                   [remaining-fracbits (- remaining-bits zero-bits)]
                   [frac-mantissa      (append (take-right frac remaining-fracbits) zero-bitlist)] ;compute the fraction part of the mantissa.

                   [mantissa (append frac-mantissa binary-integer)]
                   [exp-n    (+ EXPONENT-SHIFT intMSB-exp)]
                   [exponent (int-to-bitlist exp-n)])
                  (list sign exponent mantissa))])))


; ==============================================
; | MiniKanren Relational Arithmetic Base Code |
; ==============================================

(define BIAS (build-num 127))
(define UNIT-MANTISSA (append (make-list (- PRECISION 1) 0) '(1)))

; -----------------------
; | Basic bit relations |
; -----------------------

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
    (conde ((== a 1) (== not-a 0))
           ((== a 0) (== not-a 1))))


; ------------------------------------------
; | Length checking/manipulating relations |
; ------------------------------------------


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
            (take (list b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15) PRECISION))))

(define (mantissa-length-minus1o mantissa)
    (fresh (b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15)
        (== mantissa
            (take (list b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15) (- PRECISION 1)))))

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


; ----------------------------------
; | Value Representation relations |
; ----------------------------------


#|
(fp-overflowo expo mant rmant)
    expo: A MKFP oleg exponent
    mant: A MKFP mantissa
    rmant: A MKFP mantissa

    If expo is '(1 1 1 1  1 1 1 1), equate rmant to '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 1)
    otherwise mant == rmant
|#
(define (fp-overflowo expo mant rmant)
    (conde 
        ((=/= expo FULL-EXP) (=/= expo '()) (== mant rmant))
        ((== expo FULL-EXP)
         (== rmant UNIT-MANTISSA))))

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
        (== mantissa ZERO-MANTISSA)))

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
        (=/= mantissa ZERO-MANTISSA)))

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
        (=/= expo FULL-EXP)))

#|
(fp-infiniteo sign expo mantissa)
    sign: The sign bit of a MKFP number.
    expo: The exponent of a MKFP number.
    mantissa: The mantissa of a MKFP number.

Checks if the floating point number given by the arguments -- sign, expo, and mantissa --
represent an infinity.
|#
(define (fp-infiniteo sign expo mantissa)
    (fresh ()
        (conde 
            ((== sign 0))
            ((== sign 1)))
        (== expo FULL-EXP)
        (== mantissa UNIT-MANTISSA)))

;; data structure decomposition relations

#|
Decomposes fp number into sign, exponent, and mantissa
|#
(define (fp-decompo fp sign expo mantissa)
    (fresh (mantissa-head)
        (== fp (list sign expo mantissa))
        (mantissa-lengtho mantissa)
        (conde 
            ((=/= expo '()) (=/= expo FULL-EXP)
             (appendo mantissa-head (list 1) mantissa)) ; These numbers are non-zero but finite.

            ((== expo FULL-EXP) (== mantissa UNIT-MANTISSA)) ; Only allow infinities
            ((== expo '())  (== mantissa ZERO-MANTISSA))))) ; Only allow normalized numbers + 0.


; ---------------------------
; | Base Addition relations |
; ---------------------------


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
(define (fp-samesignaddero expo1 mant1 expo2 mant2 expo-diff rexpo rmant)
    (fresh (shifted-mant1 mant-sum pre-rmant bit)
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
(define (fp-swapo expo1 mant1 expo2 mant2 rexpo rmant)
    (fresh (expo-diff)  
        (conde
            ((pluso expo-diff expo1 expo2)
             (fp-samesignaddero expo1 mant1 expo2 mant2 expo-diff rexpo rmant))
            ((=/= expo1 expo2)
             (pluso expo-diff expo2 expo1)
             (fp-samesignaddero expo2 mant2 expo1 mant1 expo-diff rexpo rmant)))))

#|
(fp-specialpluso sign1 expo1 mant1 sign2 expo2 mant2 rsign rexpo rmant)
    sign1: The sign of the MKFP number
    expo1: The exponent of the MKFP number
    mant1: The mantissa of the MKFP number
    sign2: The sign of the MKFP number
    expo2: The exponent of the MKFP number 
    mant2: The mantissa of the MKFP number
    rsign: The resulting sign of the addition f1 + f2
    rexpo: The resulting exponent of the addition f1 + f2
    rmant: The resulting mantissa of the addition f1 + f2

    Performs the special case additions. i.e. addition with 0 and infinity.
|#
(define (fp-specialpluso sign1 expo1 mant1 sign2 expo2 mant2 rsign rexpo rmant)
    (conde
        ; 0 + 0 = 0
        ((fp-zeroo sign1 expo1 mant1)
         (fp-zeroo sign2 expo2 mant2)
         (fp-zeroo rsign rexpo rmant))

        ; x + 0 = x (x \in R)
        ((fp-nonzeroo sign1 expo1 mant1) (fp-finiteo sign1 expo1 mant1)
         (fp-zeroo sign2 expo2 mant2)
         (== rsign sign1) (== rexpo expo1) (== rmant mant1))

        ; 0 + y = y (y \in R)
        ((fp-nonzeroo sign2 expo2 mant2) (fp-finiteo sign2 expo2 mant2)
         (fp-zeroo sign1 expo1 mant1)
         (== rsign sign2) (== rexpo expo2) (== rmant mant2))

        ((noto sign1 sign2) ; (x) + (-x) = 0 (x \in R)
         (fp-nonzeroo sign1 expo1 mant1) (fp-finiteo sign1 expo1 mant1)
         (fp-nonzeroo sign2 expo2 mant2) (fp-finiteo sign2 expo2 mant2)
         (== expo1 expo2) (== mant1 mant2)
         (fp-zeroo rsign rexpo rmant))

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
         (fp-infiniteo rsign rexpo rmant))))


; ---------------------------------
; | Base Multiplication relations |
; ---------------------------------


#|
(fp-specialmulto sign1 expo1 mant1 sign2 expo2 mant2 rsign rexpo rmant)
    sign1: The sign of the MKFP number
    expo1: The exponent of the MKFP number
    mant1: The mantissa of the MKFP number
    sign2: The sign of the MKFP number
    expo2: The exponent of the MKFP number 
    mant2: The mantissa of the MKFP number
    rsign: The resulting sign of the multiplication f1 * f2
    rexpo: The resulting exponent of the multiplication f1 * f2
    rmant: The resulting mantissa of the multiplication f1 * f2

    Performs the special case multiplications. i.e. multiplication with 0 and infinity.
|#
(define (fp-specialmulto sign1 expo1 mant1 sign2 expo2 mant2 rsign rexpo rmant)
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
         (fp-nonzeroo sign1 expo1 mant1) (fp-finiteo sign1 expo1 mant1)
         (fp-zeroo sign2 expo2 mant2))

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
         (fp-infiniteo sign2 expo2 mant2)
         (fp-nonzeroo sign1 expo1 mant1) (fp-finiteo sign1 expo1 mant1))))

#|
(fp-multo-compute-expoo expo1 expo2 ls-bits rexpo)
    expo1: The exponent of a MKFP number.
    expo2: The exponent of a MKFP number.
    ls-bits: The bits dropped from the products mantissa calculation.
    rexpo: The exponent of a MKFP number which is the product of the
           numbers associated to expo1 and expo2.

    Relates the exponents of the multiplicitands and the dropped bits
    of the computed mantissa to the exponent of the product.
|#
(define (fp-multo-compute-expoo expo1 expo2 ls-bits rexpo)
  (fresh (pre-rexpo expo-sum)
      ; Check if we need to +1 to the exponent
      (conde
          ((mantissa-lengtho ls-bits)  ; 16 bits
           (pluso '(1) pre-rexpo rexpo)) ; add one to exponent    
          
          ((mantissa-length-minus1o ls-bits) ; 15 bits
           (== pre-rexpo rexpo)))

      ; Compute sum of the exponents, to compute rexpo
      (pluso expo1 expo2 expo-sum)
      (pluso BIAS pre-rexpo expo-sum)))
