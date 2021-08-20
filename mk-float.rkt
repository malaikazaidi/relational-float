#lang racket

(require "mk.rkt")
(require "numbers.rkt")
(require "mk-float-base.rkt")
(require "test-numbers.rkt") ;Un-comment require to play with test-numbers in REPL.
(provide fp-pluso fp-multo fp-< fp-<= fp-= fp-decompo fp-negateo fp-zero? fp-nonzero?
         fp-finite? fp-infinite? reify build-truncated-float)

(define float-format '(sign
                       expo ; Oleg number
                       mantissa)) ; Oleg number

; ===========
; | Reifier |
; ===========

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

; ========================================================
; | MiniKanren Floating Point Number Builder [TRUNCATED] |
; ========================================================

(define (build-truncated-float x)
  (build-truncated-float-helper x PRECISION))


; ==================================================
; | MiniKanren Floating Point Arithmetic Relations |
; ==================================================


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
            ((fp-specialpluso sign1 expo1 mant1 sign2 expo2 mant2 rsign rexpo rmant))
            ;(+/- x) + (+/- y) = (+/- z) (x, y \in R) (z \in R \cup {+/- \inf})
            ((fp-nonzeroo sign1 expo1 mant1) (fp-finiteo sign1 expo1 mant1)
             (fp-nonzeroo sign2 expo2 mant2) (fp-finiteo sign2 expo2 mant2)
             (fp-nonzeroo rsign rexpo rmant)

             (conde 
                ((== sign1 sign2) (== sign2 rsign)
                 (fp-swapo expo1 mant1 expo2 mant2 rexpo rmant)) 
                
                ; Approach when signs are opposite
                ; When r has the same sign as f1 (+)
                ; f1+(-f2) = r -> f1 = r + f2
                ; fp-pluso (-f2, r, f1)
                ; When r has the same sign as f2 (-)
                ; f1 + (-f2) = -r -> -f2 = -r + (-f1)
                ; fp-pluso (r, -f1, f2)
                ((noto sign1 sign2) (== sign1 rsign)
                 (fp-swapo expo2 mant2 rexpo rmant expo1 mant1)) 
                
                ((noto sign1 sign2) (== sign2 rsign)
                 (fp-swapo expo1 mant1 rexpo rmant expo2 mant2)))))
             
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
    (fresh (sign1 expo1 mant1 sign2 expo2 mant2 rsign rexpo rmant)

        (fp-decompo f1 sign1 expo1 mant1)
        (fp-decompo f2 sign2 expo2 mant2)
        (fp-decompo r  rsign rexpo rmant)

        ;(1) relate the signs
        (xoro sign1 sign2 rsign)

        (conde 
            ((fp-specialmulto sign1 expo1 mant1 sign2 expo2 mant2 rsign rexpo rmant))

            ((fp-nonzeroo sign1 expo1 mant1) (fp-finiteo sign1 expo1 mant1)
             (fp-nonzeroo sign2 expo2 mant2) (fp-finiteo sign2 expo2 mant2)
             (fp-nonzeroo rsign rexpo rmant)
             (fresh (mant1mant2 pre-mantr ls-bits pre-rexpo) 
                
                ; mantissa *
                ; (2) Compute the mantissa using Oleg number *o
                (*o mant1 mant2 mant1mant2)
                
                ; round by chopping
                ; (3) Keep only p digits of the mantissa
                (drop-leastsig-bito mant1mant2 pre-mantr ls-bits)
                
                ; (5) Check for overflow
                (fp-overflowo pre-rexpo pre-mantr rexpo rmant)
                
                ; (4) Compute the exponent of the product
                (fp-multo-compute-expoo expo1 expo2 ls-bits pre-rexpo)))) 
        
        (expo-lengtho expo1)
        (expo-lengtho expo2)
        (expo-lengtho rexpo)))

            
            
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
            ;f1=0 and f2>0
            ((fp-zeroo sign1 expo1 mant1)
             (fp-nonzeroo sign2 expo2 mant2)
             (== sign2 0))
            
            ;f1<0 and f2=0
            ((fp-nonzeroo sign1 expo1 mant1)
             (fp-zeroo sign2 expo2 mant2)
             (== sign1 1))

            ;f1<0 and f2>0
            ((fp-nonzeroo sign1 expo1 mant1)
             (fp-nonzeroo sign2 expo2 mant2)
             (== sign1 1)
             (== sign2 0))

            ;f1,f2 >0 and expo of f2 is larger
            ((fp-nonzeroo sign1 expo1 mant1)
             (fp-nonzeroo sign2 expo2 mant2)
             (== sign1 0)
             (== sign2 0)
             (<o expo1 expo2))

            ;f1,f2 <0 and expo of f2 is smaller
            ((fp-nonzeroo sign1 expo1 mant1)
             (fp-nonzeroo sign2 expo2 mant2)
             (== sign1 1)
             (== sign2 1)
             (<o expo2 expo1))

            ;f1,f2 >0 and expo are equal and mantissa of f2 is larger
            ((fp-nonzeroo sign1 expo1 mant1)
             (fp-nonzeroo sign2 expo2 mant2)
             (== sign1 0)
             (== sign2 0)
             (== expo1 expo2)
             (<o mant1 mant2))

            ;f1,f2 <0 and expo are equal and mantissa of f2 is smaller
            ((fp-nonzeroo sign1 expo1 mant1)
             (fp-nonzeroo sign2 expo2 mant2)
             (== sign1 1)
             (== sign2 1)
             (== expo1 expo2)
             (<o mant2 mant1)))

        (expo-lengtho expo1)
        (expo-lengtho expo2)))

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
            ((fp-zeroo sign1 expo1 mant1) (fp-zeroo sign2 expo2 mant2)) ; want to insure -0 = +0
            
            ((fp-nonzeroo sign1 expo1 mant1) (fp-nonzeroo sign2 expo2 mant2)
             (== sign1 sign2) (== expo1 expo2) (== mant1 mant2)))
        (expo-lengtho expo1)
        (expo-lengtho expo2)))

#|
(fp-<= f1 f2)
    f1: A MKFP number.
    f2: A MKFP number.

    Relation that ensures f1 <= f2.
|#
(define (fp-<= f1 f2)
    (conde ((fp-= f1 f2)) ((fp-< f1 f2))))

;; Other potentially usefull relations

#|
(fp-negateo f negated-f)
    f: A MKFP number
    negated-f: A MKFP number which whas the opposite sign of f.

Succeds when -f == negated-f 
|#
(define (fp-negateo f negated-f)
    (fresh (signf signnf expo mant)
        (fp-decompo f signf expo mant)
        (fp-decompo negated-f signnf expo mant)
        (noto signf signnf)))

#|
(fp-zero? fp)
    fp: A MKFP number

This relation succeeds when fp represents +/- 0.
|#
(define (fp-zero? fp)
  (fresh (sign expo mantissa)
         (fp-decompo fp sign expo mantissa)
         (fp-zeroo sign expo mantissa)))
         

#|
(fp-nonzero? fp)
    fp: A MKFP number.

This relation succeds when fp != (+/-0).
|#
(define (fp-nonzero? fp)
  (fresh (sign expo mantissa)
         (fp-decompo fp sign expo mantissa)
         (fp-nonzeroo sign expo mantissa)))

#|
(fp-finite? fp)
    fp: A MKFP number.

This relation succeds when fp != (+/- infinity)
|#
(define (fp-finite? fp)
  (fresh (sign expo mantissa)
         (fp-decompo fp sign expo mantissa)
         (fp-finiteo sign expo mantissa)))

#|
(fp-infinite? fp)
    fp: A MKFP number.

This relation succeds when fp represents +/- infinity
|#
(define (fp-infinite? fp)
  (fresh (sign expo mantissa)
         (fp-decompo fp sign expo mantissa)
         (fp-infiniteo sign expo mantissa)))
