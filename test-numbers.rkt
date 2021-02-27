#lang racket
(require "numbers.rkt")
(provide zero one smallnorm largenorm seventytwo neg421 negseventy fortytwo sixty
         p2049 three n1020.625 n45.125 p12.8 p12.5 n45.2 p0.636018991
         psmallestd plargestd underf p2^23 pinf ninf qnan snan four negone negfour)
                

;defined some fp numbers for testing
(define zero `(0 ,(build-num 0) ()))
(define one `(0 ,(build-num 127) ,(make-list 23 0)))
(define negone `(1 ,(build-num 127) ,(make-list 23 0)))
(define smallnorm `(0 ,(build-num 1) ,(make-list 23 0)))
(define largenorm `(0 ,(build-num 254) ,(make-list 23 1)))
(define four `(0 ,(build-num 129) ,(make-list 23 0)))
(define negfour `(1 ,(build-num 129) ,(make-list 23 0)))
(define seventytwo `(0 ,(build-num 133) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0)))
(define neg421 `(1 ,(build-num 135) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 1 0 1)))
(define negseventy `(1 ,(build-num 133) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0)))
(define fortytwo `(0 ,(build-num 132) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0)))
(define sixty `(0 ,(build-num 132) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1)))
(define p2049 `(0 ,(build-num 138) (0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0)))
(define three `(0 ,(build-num 128) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))
(define p12.8 `(0 ,(build-num 130) (0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0 1)))
(define p12.5 `(0 ,(build-num 130) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1)))
(define n45.2 `(1 ,(build-num 132) (0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 0 1 1 0)))
(define n45.125 `(1 ,(build-num 132) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 1 1 0)))
(define n1020.625 `(1 ,(build-num 136) (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 1 1 1 1 1 1 1)))
(define p0.636018991 `(0 ,(build-num 126) (0 0 1 0 0 1 0 0 0 1 0 0 1 0 1 1 0 1 0 0 0 1 0)))
(define psmallestd `(0 ,(build-num 0) (1)))
(define plargestd `(0 ,(build-num 0) ,(make-list 23 1))); 2^-126 * (1-2^(-23))
(define underf `(0 ,(build-num 103) ,(make-list 23 0) ))
(define p2^23 `(0 ,(build-num 104) ,(make-list 23 0)));-23
(define pinf `(0 (1 1 1 1 1 1 1 1) ,(make-list 23 0)))
(define ninf `(1 (1 1 1 1 1 1 1 1) ,(make-list 23 0)))
(define qnan `(0 (1 1 1 1 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))); Only most sig bit is important for distinguishing nans
(define snan `(0 (1 1 1 1 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0)))


