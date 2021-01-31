#lang racket

(provide ZERO ONE SMALLEST-DENORM LARGEST-DENORM SMALLEST-NORM LARGEST-NORM LARGEST-LESSTHAN-ONE SMALLEST-LARGERTHAN-ONE)

(define ZERO '(0
               0 0 0 0 0 0 0 0
               0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
(define ONE '(0
              1 1 1 1 1 1 1 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
(define SMALLEST-DENORM '(0
                          0 0 0 0 0 0 0 0
                          1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
(define LARGEST-DENORM '(0
                         0 0 0 0 0 0 0 0
                         1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
(define SMALLEST-NORM '(0
                        1 0 0 0 0 0 0 0
                        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
(define LARGEST-NORM '(0
                       0 1 1 1 1 1 1 1
                       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
(define LARGEST-LESSTHAN-ONE '(0
                               0 1 1 1 1 1 1 0
                               1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
(define SMALLEST-LARGERTHAN-ONE '(0
                                  1 1 1 1 1 1 1 0
                                  1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))