#lang racket

(provide build-truncated-float)

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


(define (build-mantissa binary-integer fractional-mantissa leading-zeros) (void))

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
                     (equal? leading-zeros (- SMALLEST_PRECISION_EXP HIDDEN_BIT_INDEX)))]
      
       [adjusted-leading-zeros (if denorm? (+ leading-zeros 1) leading-zeros)]
       [shifted-exponent-n (calculate-shifted-exponent-n integer-bitlength adjusted-leading-zeros)]

       [exponent (int-to-bitlist shifted-exponent-n)]
       [mantissa '()]) 

      (list sign exponent mantissa))) 







