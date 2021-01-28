#lang racket
(define float-format '(
                       b31
                       b23 b24 b25 b26 b27 b28 b29 b30
                       b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22
                     ))
(define num '(;sign
              0 ; positive
              1 1 1 1 1 1 1 1 ;exponent ; first bit is the least significant
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 ; mantissa ; first bit is the least significant
              )
)

;Constant for reporting an error
(define FP-ERR 'FP-ERR)

;Constants for the get-x functions
(define EXP-LEN 8)
(define MANTISSA-POS 9)

; Constants for floats that aren't numbers
(define POS-INFINITY 'positive-infinity)
(define NEG-INFINITY 'negative-infinity)
(define NaN 'NaN)

(define ZERO-EXP '(0 0 0 0 0 0 0 0)) ; For detecting denormalized numbers
(define FULL-EXP '(1 1 1 1 1 1 1 1)) ; For detecting +/- infinity or NaN

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
(define (get-exp f) (take (rest f) EXP-LEN))

#|
(get-frac f)
  f: A MKFP number
  Returns the 23 fractional bits of f. Also known as the significand or mantissa. 
|#
(define (get-frac f) (list-tail f MANTISSA-POS))

#|
(inf? exp man)
  exp: The exponent bits of a MKFP number.
  man: The mantissa bits of a MKFP number.
  Returns true if and only if all the exponent bits are 1 and if all the mantissa bits are 0. 
|#
(define (inf? exp man)       (and (equal? exp FULL-EXP) (equal? 0 (apply + man))))

#|
(NaN? exp man)
  exp: The exponent bits of a MKFP number.
  man: The mantissa bits of a MKFP number.
  Returns true if and only if all the exponent bits are 1 and if not all the mantissa bits are 0. 
|#
(define (NaN? exp man)       (and (equal? exp FULL-EXP) (not (equal? 0 (apply + man)))))

#|
(zero? exp man)
  exp: The exponent bits of a MKFP number.
  man: The mantissa bits of a MKFP number.
  Returns true if and only if all the exponent bits are 0 and if all the mantissa bits are 0. 
|#
(define (zero? exp man)      (and (equal? exp ZERO-EXP) (equal? 0 (apply + man))))

#|
(subnormal? exp man)
  exp: The exponent bits of a MKFP number.
  man: The mantissa bits of a MKFP number.
  Returns true if and only if all the exponent bits are 0 and if not all the mantissa bits are 0. 
|#
(define (subnormal? exp man) (and (equal? exp ZERO-EXP) (not (equal? 0 (apply + man)))))

#|
(reify mkfp)
  mkfp: A MKFP number.
  Returns the Racket float equivallent of what float mkfp refers to. 
|#
(define (reify mkfp)
  (let* (
         [sign       (reify-sign (get-sign mkfp))]
         [man        (get-frac mkfp)]
         [stored-exp (get-exp mkfp)]
        )
    (cond
      [(inf? stored-exp man) (cond
                               [(equal? 1 sign) POS-INFINITY]
                               [(equal? -1 sign) NEG-INFINITY])]
      [(NaN? stored-exp man) NaN]
      [(subnormal? stored-exp man) (* sign (expt 2 -126) (reify-frac man))]
      ((zero? stored-exp man) 0)
      [else (let* (
                   [frac        (+ 1 (reify-frac man))]
                   [shifted-exp (reify-exp stored-exp)]
                  )
              (* sign (expt 2 shited-exp) frac)
            )] 
      )
  )
)

#|
(reify-frac man)
  man: The mantissa bits of a MKFP number.
  Returns the Racket float equivallent of what the mantissa is. 
|#
(define/match (reify-frac man)
  [((list b0 b1 b2 b3 b4 b5 b6 b7 b8 b9
          b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22)) (+  (* b0 (expt 2 -23))  (* b1 (expt 2 -22))
                                                                    (* b2 (expt 2 -21))  (* b3 (expt 2 -20))
                                                                    (* b4 (expt 2 -19))  (* b5 (expt 2 -18))
                                                                    (* b6 (expt 2 -17))  (* b7 (expt 2 -16))
                                                                    (* b8 (expt 2 -15))  (* b9 (expt 2 -14))
                                                                   (* b10 (expt 2 -13)) (* b11 (expt 2 -12))
                                                                   (* b12 (expt 2 -11)) (* b13 (expt 2 -10))
                                                                   (* b14 (expt 2 -9))  (* b15 (expt 2 -8))
                                                                   (* b16 (expt 2 -7))  (* b17 (expt 2 -6))
                                                                   (* b18 (expt 2 -5))  (* b19 (expt 2 -4))
                                                                   (* b20 (expt 2 -3))  (* b21 (expt 2 -2))
                                                                   (* b22 (expt 2 -1)))]
  [(_) FP-ERR]
)

#|
(reify-sign man)
  sgn: The sign bit of a MKFP number.
  Returns 1 if sgn is zero and -1 if sgn is 1. Otherwise FP-ERR is returned. 
|#
(define/match (reify-sign sgn)
  [(0) 1]
  [(1) -1]
  [(_) FP-ERR]
)

#|
(reify-exp exp)
  exp: The exponent bits of a MKFP number.
  Returns the value of the shifted exponent. 
|#
(define/match (reify-exp exp)
  [((list b23 b24 b25 b26 b27 b28 b29 b30)) (+ -127 b23 (* 2 b24) (* 4 b25) (* 8 b26) (* 16 b27) (* 32 b28) (* 64 b29) (* 128 b30))]
  [(_) FP-ERR]
)
