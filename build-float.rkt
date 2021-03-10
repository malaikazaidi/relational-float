#lang racket

(provide build-truncated-float)

(define SINGLE_P 23); + 1 to include hidden bit.

(define SMALLEST_PRECISION_EXP 149)
(define EXP_SHIFT 127)
(define HIDDEN_BIT_INDEX 24)
(define SMALLEST_NORMAL_MAGNITUDE (expt 2 -126))

; These are set to be constant so when we implement a variable precision 
; we can see how to modify the code easier. 

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
    - 0 <= (length bitlist) <= p
  exp:
    - The exponent of the most significant bit.
|#
(define (build-fracbitlist r p) 
      (let* ([advance-pair (advance-to-leading-bit r fracbit-generator 0 (- SMALLEST_PRECISION_EXP 24))]; Advance no more than 125 positions
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
  n: an accumulator that counts how many times gerate was called.

  Returns the pair (x' . n') where x' is the result of calling generate n' times succesively.
  0 <= n' <= cap.
|#
(define/match (advance-to-leading-bit x generate n cap)
  [(x generate z z) (cons x z)]; When n == cap.
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
(build-truncated-float r)
  r: A racket float.

  Returns a MKFP representation of the floating point number r.
|#
(define (build-truncated-float r) 
  (let* 
    ([sign-int-frac     (decompose-real r)]
     [sign              (first sign-int-frac)]
     [integer-part      (second sign-int-frac)]
     [fractional-part   (third sign-int-frac)]

     [intbitlist-pair     (build-intbitlist integer-part (+ SINGLE_P 1))]
     [binary-integer      (car intbitlist-pair)]
     [intMSB-exp          (cdr intbitlist-pair)]
     [integer-bitlength   (+ intMSB-exp 1)]); The number of bits it takes to fully represent the integer part.
     (cond
      [(>= intMSB-exp SINGLE_P) (let* ([mantissa (drop-right binary-integer 1)]; Drop the leading 1.
                                       [exp-n    (+ EXP_SHIFT intMSB-exp)]
                                       [exponent (int-to-bitlist exp-n)])

                                    (list sign exponent mantissa))]; We know we are dealing with a pure integer.

      [(equal? intMSB-exp -1) (let* ([frac-pair   (build-fracbitlist fractional-part (+ SINGLE_P 1))]; need 24 bits.
                                     [frac        (car frac-pair)]
                                     [fracMSB-exp (cdr frac-pair)]

                                     [denormal? (and (equal? (last frac) 0) (equal? (- EXP_SHIFT 1) fracMSB-exp))]
                                     [adjusted-fracMSB-exp (if denormal?
                                                               (+ fracMSB-exp 1)
                                                               fracMSB-exp)]

                                     [mantissa (drop-right frac 1)]; Drop the leading 1.

                                     [exp-n    (- EXP_SHIFT adjusted-fracMSB-exp)]
                                     [exponent (int-to-bitlist exp-n)])

                                    (list sign exponent mantissa))];We know we are dealing with a pure fraction.

      [else (let* ([remaining-bits (- (+ SINGLE_P 1) (+ intMSB-exp 1))]; 24 - #of integer bits
                   [frac-pair     (build-fracbitlist fractional-part remaining-bits)]; need 24 bits.
                   [frac          (car frac-pair)]; Extract the computed fraction.
                   [fracMSB-exp   (cdr frac-pair)]; Extract the exponent of the most significant bit from the fractional part.

                   ; Compute the amount of bits taken from frac as we may need to add leading zeros before taking the frac bits.
                   [leading-zeros      (- fracMSB-exp 1)]
                   [zero-bits          (min remaining-bits leading-zeros)]; There maybe to many zeros between the fraction and the integer so we take the min.
                   [zero-bitlist       (make-list zero-bits 0)]
                   [remaining-fracbits (- remaining-bits zero-bits)]
                   [frac-mantissa      (append (take-right frac remaining-fracbits) zero-bitlist)] ;compute the fraction part of the mantissa.

                   [int-mantissa  (drop-right binary-integer 1)]; Drop the leading 1.
                   [mantissa (append frac-mantissa int-mantissa)]
                   [exp-n    (+ EXP_SHIFT intMSB-exp)]
                   [exponent (int-to-bitlist exp-n)])
                  (list sign exponent mantissa))])))

