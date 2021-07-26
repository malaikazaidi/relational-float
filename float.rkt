#lang racket

(provide reify reify-exp reify-mantissa reify-sign
         get-sign get-mantissa get-exp
         POS-INFINITY NEG-INFINITY NaN FP-ERR
         build-truncated-float)

; Representation of a floating point number p=16.
;'(
;  0                                      ; sign     ; positive
;  '(1 1 1 1  1 1 1 1)                    ;exponent  ; first bit is the least significant
;  '(0 0 0 0  0 0 0 0  0 0 0 0   0 0 0 1) ; mantissa ; first bit is the least significant, always contains the leading one.
; )


;Constant for reporting an error
(define FP-ERR 'FP-ERR)

; Constants for floats that aren't numbers
(define POS-INFINITY 'positive-infinity)
(define NEG-INFINITY 'negative-infinity)
(define NaN 'NaN)


(define PRECISION 16) ; number of bits in the mantissa.
(define EXPONENT-SHIFT 127) ; The shifting factor for the exponenet field.
(define SMALLEST-PRECISION-EXP 149) ; The smallest representable exponent for the LSB.

(define FULL-EXP '(1 1 1 1 1 1 1 1)) ; For detecting +/- infinity or NaN
(define ZERO-MANTISSA (make-list PRECISION 0))

; ---------------------------------
; | Representation Deconmposition |
; ---------------------------------

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

#|
(reify mkfp)
  mkfp: A MKFP number.

  Returns the Racket float equivallent of what float mkfp refers to. 
|#
(define (reify mkfp)
  (let*
      ([sign       (reify-sign (get-sign mkfp))]
       [mantissa   (get-mantissa mkfp)]
       [stored-exp (get-exp mkfp)])

    (cond
      [(inf? stored-exp mantissa) (cond
                                    [(equal? 1 sign) POS-INFINITY]
                                    [(equal? -1 sign) NEG-INFINITY])]
      
      [else (let* ([frac        (reify-mantissa mantissa)]
                   [shifted-exp (reify-exp stored-exp)])
              (* sign (expt 2 shifted-exp) frac))])))

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
         [zero-fraction? (andmap zero? (drop-right mantissa 1))]) ; Is the mantissa all zeros after the MSB.

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

#|
(build-truncated-float r)
  r: A racket float.
  Returns a MKFP representation of the floating point number r.
|#
(define (build-truncated-float r) 
    (build-truncated-float-helper r PRECISION))

; Functions for turinging an integer into a bitlist which represents the same integer in binary.

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


