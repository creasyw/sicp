#lang racket

(define (make-rat n d)
  (letrec ((np (abs n))
           (dp (abs d))
           (g (gcd np dp))
           (flag (if (> (* n d) 0) 1 -1)))
    (cons (* flag (/ np g)) (/ dp g)))) 