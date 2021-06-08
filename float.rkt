#lang racket

(provide reify reify-exp reify-mantissa reify-sign get-sign get-mantissa get-exp POS-INFINITY NEG-INFINITY NaN FP-ERR)

(define num '(;sign
              0 ; positive
              '(1 1 1 1 1 1 1 1) ;exponent ; first bit is the least significant
              '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) ; mantissa ; first bit is the least significant, always contains the leading one.
              )
)

;Constant for reporting an error
(define FP-ERR 'FP-ERR)

;Constants for the get-x functions
(define EXP-LEN 8)
(define N_BITS 24)
(define MANTISSA-POS 9)

; Constants for floats that aren't numbers
(define POS-INFINITY 'positive-infinity)
(define NEG-INFINITY 'negative-infinity)
(define NaN 'NaN)


(define PRECISION 16) ; number of bits in the mantissa.\
(define EXPONENT-SHIFT 127)

(define ZERO-EXP '()) ; For detecting denormalized numbers
(define FULL-EXP '(1 1 1 1 1 1 1 1)) ; For detecting +/- infinity or NaN
(define ZERO-MANTISSA (make-list PRECISION 0))

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

#|
(NaN? exponent mantissa)
  exponent: The exponent bits of a MKFP number.
  mantissa: The mantissa bits of a MKFP number.

  Returns true iff all the exponent bits are 1 and the bits after the MSB are non-zero.
|#
(define (NaN? exponent mantissa)
    (let* 
        ([NaN-exponent? (equal? exponent FULL-EXP)]
         [MSB-one?     (equal? 1 (last mantissa))]
         [nonzero-fraction? (ormap 
                                (lambda (int) (not (zero? int)))
                                (drop-right mantissa 1))])

         (and NaN-exponent? MSB-one? nonzero-fraction?))
)


#|
(fp-zero? exponent mantissa)
  exp: The exponent bits of a MKFP number.
  man: The mantissa bits of a MKFP number.
  Returns true if and only if all the exponent bits are 0 and if all the mantissa bits are 0. 
|#
(define/match (fp-zero? exponent mantissa)
    [((list) ZERO-MANTISSA) #t]
    [(_ _) #f])

#|
(reify mkfp)
  mkfp: A MKFP number.
  Returns the Racket float equivallent of what float mkfp refers to. 
|#
(define (reify mkfp)
  (let* ([sign       (reify-sign (get-sign mkfp))]
         [mantissa   (get-mantissa mkfp)]
         [stored-exp (get-exp mkfp)])
    (cond
      [(inf? stored-exp mantissa) (cond
                                    [(equal? 1 sign) POS-INFINITY]
                                    [(equal? -1 sign) NEG-INFINITY])]
      [(NaN? stored-exp mantissa) NaN]
      [else (let* ([frac        (reify-mantissa mantissa)]
                   [shifted-exp (reify-exp stored-exp)])
              (* sign (expt 2 shifted-exp) frac))])))

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
(bitlist-to-int-helper bitlist starting-exponent acc)
    bitlist: A list of bits in little endian form.
    LSB-exponent The implicit exponent of the first bit (set to zero to get integers only)

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
 



