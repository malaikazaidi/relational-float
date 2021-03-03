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

(define/match (build-intbitlist-helper z p bitlist expo)
  [(z _ bitlist expo) #:when (= z 0) (cons (reverse bitlist) expo)]
  [(z p bitlist expo) (let* ([pair (intbit-generator z)]
                             [residual (car pair)]
                             [bit (cdr pair)]
                             
                             [next-expo (+ 1 expo)]
                             [next-bitlist (if (< p expo) ; This causes the list to act like a window that shifts along until the leading bit is found.
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
            
            (cons (reverse bitlist) (+ n-calls 1))))

#|
(bitlist-filler bitlist p generate x)
  bitlist: The current bitlist created so far.
  p: The precision.
  generate: A function that takes in a number and spits out a pair (residual . bit)

  Fills the rest of the precision left in the bitlist.
|#
(define/match (bitlist-filler bitlist p generate x) 
  [(p bitlist _ _) #:when (equal? p (length bitlist)) bitlist]
  [(p bitlist generate x) 
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
                                          (cons q r)))


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
(fractional-bitlist r nbits)
  r: A real number less than 1 and non-negative
  nbits: The total number of bits to generate.

  Return, pair (bitlst . zeros) where
    - bitlist is the representation of r in bits. 
    - zeros is the number of leading zeros before the first bit set to 1 in the representation of r.
|#
(define (fractional-bitlist r nbits) 
  (let* 
    ([bitlst-zeros-flag (fractional-bitlist-helper r (list '() nbits 0 #f))]
     [bitlist           (first bitlst-zeros-flag)]
     [zeros             (third bitlst-zeros-flag)]) 
  
    (cons bitlist zeros)))

#|
(fractional-bitlist-helper r nbits acc)
  A helper function that is tail recursive.
  Returns (bitlst nbits zeros flag)
    - bitlist is the representation of r in bits.
    - nbits is an internal intger used to count the number of bits created. 
    - zeros is the number of leading zeros before the first bit set to 1 in the representation of r.
    - flag is an internal flag that is used to determine if a 1 has been found.
|#
(define/match (fractional-bitlist-helper r acc)
  [(r (list bitlist 0 zeros found-one)) (list bitlist 0 zeros found-one)]
  [(r (list bitlist nbits zeros found-one))
     (let* ([double-r (* 2 r)]
            [next-bit (if (< double-r 1) 0 1)]
            [next-r   (if (equal? next-bit 1) (- double-r 1) double-r)]
            [next-acc (next-fractional-acc bitlist nbits zeros found-one next-bit)])

            (fractional-bitlist-helper next-r next-acc))])

#|
(next-fractional-acc bitlist found-one acc-zeros next-bit)

  Creates the next accumulator for the fractional-bitlist-helper function.
|#
(define (next-fractional-acc bitlist nbits zeros found-one next-bit)
  (let* (; If we have already found a 1 start decreasing n-bits
         ; Or, if we are about to fall off precision (leading zeros == (149 - nbits)) 
         ;
         [next-found-one (or found-one 
                             (equal? next-bit 1) 
                             (equal? (- SMALLEST_PRECISION_EXP nbits) zeros))]

         [next-bitlist   (if next-found-one 
                             (cons next-bit bitlist)
                             bitlist)]
                           
         [next-nbits     (if next-found-one  
                             (- nbits 1)
                             nbits)]

         [next-zeros     (if next-found-one  
                             zeros
                             (+ 1 zeros))])
       
        (list next-bitlist next-nbits next-zeros next-found-one)))

#|
(calculate-fractional-nbits bitlength)
  bitlength: The length of the integer part of r in bits which was passed into build-truncated-float.
             pos?
  Returns the number of bits required from the fractional part of r to create a mantissa.
|#
(define (calculate-fractional-nbits bitlength)
  (max 0 (- HIDDEN_BIT_INDEX bitlength))); 24 to account for the hidden bit

#|
(calculate-shifted-exponent-n integer-bitlength leading-zeros)
  integer-bitlength: pos? int?
  leading-zeros: pos? int?

  Calculates the shifted exponent for the mkfp number.
|#
(define/match (calculate-shifted-exponent-n integer-bitlength leading-zeros)
  [(0 leading-zeros)      (- EXP_SHIFT 1 leading-zeros)]; for single-floating point (EXP_SHIFT - 1)=126
  [(integer-bitlength _ ) (+ EXP_SHIFT -1 integer-bitlength)])

#|
(build-mantissa binary-integer fractional-mantissa leading-zeros)
  binary-integer: A binary integer with no leading zeros and in little-endian format.
  fractional-mantissa: A 24 bit mantissa
  leading-zeros: The number of zeros preceeding the bits in the fractional-mantissa.

  Returns the complete mantissa after droping the hidden bit.
|#
(define/match (build-mantissa binary-integer fractional-mantissa leading-zeros denorm?) 
  [('() fractional-mantissa _ #t)            (dropf-right fractional-mantissa zero?)] ; drop the leading zeros.
  [('() fractional-mantissa _ #f)            (drop-right fractional-mantissa 1)] ; drop the leading 1
  [(binary-integer fractional-mantissa leading-zeros _) 
    (let* ([int-length         (length binary-integer)]; The bit length of the integer
           [remaining-bits     (max 0 (- HIDDEN_BIT_INDEX int-length))]; Total number of bits we can take.
           [zero-bits          (min remaining-bits leading-zeros)]; The number of zeros to add.

           [mantissa-bits      (max 0 (- remaining-bits zero-bits ))]; Number of bits from fractional mantissa we keep.

           [with-zeros         (drop-right (append (make-list zero-bits 0) binary-integer) 1)]
           [truncated-mantissa (take-right fractional-mantissa mantissa-bits)])
           
           (append truncated-mantissa with-zeros))])


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

       [binary-integer           (int-to-bitlist integer-part)]
       [integer-bitlength        (length binary-integer)]
       [required-fractional-bits (calculate-fractional-nbits integer-bitlength)]
       
       [frac-zeros          (fractional-bitlist fractional-part required-fractional-bits)]
       [fractional-mantissa (car frac-zeros)]
       [leading-zeros       (cdr frac-zeros)]

       ; Check if denormalized to adjust leading zeros. With denorm 1 leading zero will not be accounted for.
       [denorm? (and (equal? (last fractional-mantissa) 0) 
                     (equal? leading-zeros (- SMALLEST_PRECISION_EXP HIDDEN_BIT_INDEX)); 
                     (equal? integer-bitlength 0))]
      
       [adjusted-leading-zeros (if denorm? (+ leading-zeros 1) leading-zeros)]
       [shifted-exponent-n (calculate-shifted-exponent-n integer-bitlength adjusted-leading-zeros)]

       [exponent (int-to-bitlist shifted-exponent-n)]
       [mantissa (build-mantissa binary-integer fractional-mantissa adjusted-leading-zeros denorm?)]) 

      (list sign exponent mantissa))) 







