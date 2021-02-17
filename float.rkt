#lang racket

(provide reify reify-exp reify-frac reify-sign get-sign get-frac get-exp POS-INFINITY NEG-INFINITY qNaN sNaN FP-ERR)

(define float-format '(
                       b31
                       'olegexponenntn ; little endian exp
                       'oleg mantissa; little endian mantissa
                      ))
(define num '(;sign
              0 ; positive
              '(1 1 1 1 1 1 1 1) ;exponent ; first bit is the least significant
              '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) ; mantissa ; first bit is the least significant
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
(define qNaN 'qNaN)
(define sNaN 'sNaN)

(define ZERO-EXP '()) ; For detecting denormalized numbers
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
(define (get-exp f) (second f))

#|
(get-frac f)
  f: A MKFP number
  Returns the 23 fractional bits of f. Also known as the significand or mantissa. 
|#
(define (get-frac f) (third f))

#|
(inf? exp man)
  exp: The exponent bits of a MKFP number.
  man: The mantissa bits of a MKFP number.
  Returns true if and only if all the exponent bits are 1 and if all the mantissa bits are 0. 
|#
(define (inf? exp man)       (and (equal? exp FULL-EXP) (equal? 0 (apply + man))))

#|
(qNaN? exp man)
  exp: The exponent bits of a MKFP number.
  man: The mantissa bits of a MKFP number.

  Returns true iff all the exponent bits are 1 and the most significant bit of the mantissa is 1.
|#
(define (qNaN? exp man)       (and (equal? exp FULL-EXP) (equal? 1 (last man))))

#|
(sNaN? exp man)
  exp: The exponent bits of a MKFP number.
  man: The mantissa bits of a MKFP number.

  Returns true iff all the exponent bits are 1 and the most significant bit of the mantissa is 0 and there are other mantissa bits set to 1.
|#
(define (sNaN? exp man)       (and (equal? exp FULL-EXP) (not (equal? 0 (apply + man))) (equal? 0 (last man))))

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
      [(qNaN? stored-exp man) qNaN]
      [(sNaN? stored-exp man) sNaN]
      [(subnormal? stored-exp man) (* sign (expt 2 -126) (reify-frac man))]
      [(zero? stored-exp man) 0]
      [else (let* (
                   [frac        (+ 1 (reify-frac man))]
                   [shifted-exp (reify-exp stored-exp)]
                  )
              (* sign (expt 2 shifted-exp) frac)
            )] 
      )
  )
)

#|
(binary-exponent-folder bit acc-pair)
  bit: A 1 or a 0. int?
  acc-pair: (k, total) pair?

    k: The current exponent.
    total: The current binary sum total.

  Given a bit and an accumulator, we return a new accumulator (k+1, (+ total (* bit (expt 2 k)))).
|#
(define/match (binary-exponent-folder bit acc-pair)
  [(0 (cons k total)) (cons (+ k 1) total)]
  [(1 (cons k total)) (let* ([new-exp (+ 1 k)]
                             [new-total (+ total (expt 2 k))])
                        (cons new-exp new-total))]
  [(_ _) (cons 0 0)])

#|
(make-binary-folder init-exp)
  init-exp: defines the intial exponent to be used to recurse over a list.

  Returns a function that takes in a bit-list and returns the value it represents shifted by init-exp.
|#
(define (make-exp-shifted-binary-folder init-exp)
  (lambda (lst)
    (let* ([acc-pair (foldl binary-exponent-folder (cons init-exp 0) lst)])
      (cdr acc-pair))))

#|
(reify-frac man)
  man: The mantissa bits of a MKFP number.
  Returns the Racket float equivallent of what the mantissa is. 
|#
(define (reify-frac man)
  (let* ([shifted-binary-expander (make-exp-shifted-binary-folder -23)])
    (shifted-binary-expander man)))

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
(define (reify-exp exp)
  (let* ([noshift-binary-expander (make-exp-shifted-binary-folder 0)])
         (+ -127 (noshift-binary-expander exp))))

