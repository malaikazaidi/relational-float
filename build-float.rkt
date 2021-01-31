#lang racket

(require "special-values.rkt")

(provide build-truncated-float)

#|
(truncate-pad bitlist n)
  bitlist: list? a list of 0's and 1's
  n: integer? and positive? the number of bits in the output list.

  Returns a truncated bitlist with n bits where n of the most significant bits are kept.
  If (< (length bitlist) n) then the bitlist will be padded with 0's on the right. 
|#
(define (truncate-pad bitlist n)
  (let* ([bitlist-length (length bitlist)])
    (cond
      [(> bitlist-length n) (list-tail bitlist (- bitlist-length n))]
      [(< bitlist-length n) (padright bitlist 0 (- n bitlist-length))]
      [else bitlist])))

#|
(drop-leading-bit bitlist)
  bitlist: list? a list of 0's and 1's

 Returns a new bitlist with the same elements but without the last element. 
|#
(define (drop-leading-bit bitlist) (take bitlist (- (length bitlist) 1)))

#|
(truncate bitlist n)
  bitlist: list? a list of 0's and 1's
  n: integer? and positive? the number of bits in the output list.

  Returns a new bitlist with at most n bits. The most significant bits are preserved.
|#
(define (truncate bitlist n)
  (let* ([bitlist-length (length bitlist)])
    (cond
      [(> bitlist-length n) (list-tail bitlist (- bitlist-length n))]
      [else bitlist])))

#|
(padleft bitlist bit n)
  bitlist: list? a list of 0's and 1's
  bit: 0 or 1
  n: integer? and positive? the number of bits we are padding the left of the list with.

  Returns a new bit list padded to the left with n bits specified by bit.
|#
(define/match (padleft bitlist bit n)
  [(bitlist _ 0)   bitlist]
  [(bitlist bit n) (padleft (cons bit bitlist) bit (- n 1))])

#|
(padright bitlist bit n)
  bitlist: list? a list of 0's and 1's
  bit: 0 or 1
  n: integer? and (not negative?) the number of bits we are padding the end of the list with.

  Returns a new bitlist padded to the right with n bits specified by bit.
|#
(define (padright bitlist bit n)
  (let* ([n-bits (make-list n bit)])
    (append bitlist n-bits))
)

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
(get-mantissa-bit fractional-part)
  fractional-part: real? and positive? and less than 1.

  Returns the pair (bit . product) where bit is 0 if (< product 1) and 1 otherwise.
  product = {(* 2 fractional-part)}, {x} is the fractional part of x.
|#
(define (get-mantissa-bit fractional-part)
  (let* ([product (* 2 fractional-part)])
    (cond
      [(< product 1) (cons 0 product)]
      [else (cons 1 (- product 1))])))

#|
(advance-to-first-signifcant-bit fractional-part)
  fractional-part: real? and positive? and less than 1.

  Returns i, where i is the number of doublings of fractional-part that occured
  until the next doubling would be larger than 1.
|#
(define (count-leading-zeros fractional-part)
  (count-leading-zeros-helper fractional-part 0))
  

#|
(count-leading-zeros-helper fractional-part iter)
  fractional-part: real? and positive? and less than 1.
  iter:  The number of doublings performed before being larger than 1.

  Returns the pair i where,
    - 0 <= i <= 126, the min{126, number-of-leading zero's}

  |#
(define (count-leading-zeros-helper fractional-part iter)
  (let* ([doubled (* 2 fractional-part)])
            (cond
              [(> iter 125)     iter] ; this catches 0 / denorms/ underflow.
              [(< doubled 1)    (count-leading-zeros-helper doubled (+ 1 iter)) ]
              [else             iter])))
  
#|
(fractional-to-mantissa fractional-part number-of-bits)
  fractional-part: real? and positive? and less than 1.
  number-of-bits:  integer? and (not negative?)

  Returns a pair (mantissa . n-leading-zeros) where mantissa is a representation of
  the significand of fractional-part represented as a bitlist of length
  number-of-bits, and n-leading-zeros is a integer representing the number of
  leading zeros before the first significant bit. 
|#
(define (fractional-to-mantissa fractional-part number-of-bits)
  (let* ([n-leading-zeros          (count-leading-zeros fractional-part)]; The number of leading zeros
         [advanced-fractional-part (if (< n-leading-zeros 23)
                                       ; This is the case when we don't need to advance the mantissa since it has less than 23 leading zeros
                                       fractional-part
                                       ; We have 23 or more leading zeros so we must advance it.
                                       (* fractional-part (expt 2 n-leading-zeros)))]

         ; a number is denormal if it is preceeded by at least 126 zeros and the remaining portion is not zero.
         
         ) (cons
            (fractional-to-mantissa-helper advanced-fractional-part number-of-bits '()) ;generates the mantissa
            n-leading-zeros)))
             
#|
(fractional-to-mantissa-helper fractional-part number-of-bits acc)
  fractional-part: real? and positive? and less than 1.
  number-of-bits:  integer? and (not negative?)
  acc:             An accumulator for the mantissa

  Returns a mantissa. A mantissa is a representation of the significand of
  fractional-part represented as a bitlist of length number-of-bits. 
|#
(define (fractional-to-mantissa-helper fractional-part number-of-bits acc)
  (cond
    [(< number-of-bits 1) acc]
    [else (let* ([get-mantissa-bit-pair (get-mantissa-bit fractional-part)]
                 [bit                   (car get-mantissa-bit-pair)]
                 [next-fractional-part  (cdr get-mantissa-bit-pair)]
                 [next-number-of-bits   (- number-of-bits 1)]
                 [next-acc              (cons bit acc)])
            (fractional-to-mantissa-helper next-fractional-part next-number-of-bits next-acc))]))

#|
(build-mantissa fractional-mantissa binary-integer denormal?)
  fractional-mantissa: The bitlist obtained after calling (fractional-to-mantissa)
  binary-integer:      The bitlist representation of the integer part of the floating point number we are building.
  denormal?:           A flag indicating if the mantissa should be interpreted as denormal.

  Returns the final stored mantissa.
|#
(define (build-mantissa fractional-mantissa n-leading-mantissa-zeros binary-integer)
  (let* ([no-integer?             (equal? '() binary-integer)]
         [no-fractional-mantissa? (equal? '() fractional-mantissa)]
         [denormal?               (and no-integer? (>= n-leading-mantissa-zeros 126))]
         [mantissa                (append fractional-mantissa (truncate binary-integer 23))])
    (cond
      [denormal?               (truncate mantissa 23)]; drop the last significant bit.
      [no-integer?             (drop-leading-bit mantissa)]; not denormal thus we must drop a leading 1.
      [no-fractional-mantissa? mantissa]; Do nothing since we already truncated the integer.
      [else                    (drop-leading-bit mantissa)])))

#|
(build-exponent binary-integer-length n-leading-zeros)
  binary-integer-length: positive? integer? The number of bits it takes to represent the integer portion of the float.
  n-leading-zeros: positive? integer?
|#
(define/match (build-exponent binary-integer-length n-leading-zeros)
  [(0 126) '(0 0 0 0 0 0 0 0)];denormal case
  
  [(0 n-leading-zeros) (let* ([biased-exponent (+ 127 (- (+ n-leading-zeros 1)))])
                               (truncate-pad (int-to-bitlist biased-exponent) 8))]; less than 1 case

  
  [(binary-integer-length _) (let* ([biased-exponent (+ 127 (- binary-integer-length 1))]); other
                               (truncate-pad (int-to-bitlist biased-exponent) 8))])

#|
(build-truncated-float r)
  r: A racket float.

  Returns a 32-bit MKFP representation of the floating point number r.
|#
(define (build-truncated-float r)
  (let* ([sign-bit                  (if (< r 0) 1 0)]
         [abs-r                     (abs r)]
         [integer-part              (floor abs-r)]; int(r) = floor(|r|)
         [binary-integer            (int-to-bitlist integer-part)] ;in little-edian format,
         [binary-integer-length     (length binary-integer)]
          
         ; We keep an extra bit since we have to drop the leading one in some cases.
         ; If the number is denormal we drop the least significant bit.
         [remaining-mantissa-length (- 24 binary-integer-length)] 
         [fractional-part           (- abs-r integer-part)]; {r} = |r|- floor(|r|)

         
         [fractional-mantissa-pair  (fractional-to-mantissa fractional-part remaining-mantissa-length)]
         ;decompose the pair
         ; This mantissa may still be need to be modified if the integer part is non-zero
         [fractional-mantissa       (car fractional-mantissa-pair)]
         [n-leading-mantissa-zeros  (cdr fractional-mantissa-pair)]


         ; finish building the mantissa
         ; if the number is denormal the mantissa remains unchanged.
         ; If the number is normal we must drop the leading 1.
         [mantissa  (build-mantissa fractional-mantissa n-leading-mantissa-zeros binary-integer)]
         
         
         ; build the exponent
         [exponent                  (build-exponent binary-integer-length n-leading-mantissa-zeros)]; this is the final stored exponent
         
         
         
        ) (append (cons sign-bit (append exponent mantissa)))))







