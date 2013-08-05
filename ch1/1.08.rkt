#lang racket

(define (cube-root x error)
  (define (improve y)
    (/ (+ (/ x (* y y)) (* 2 y)) 3))
  (define (cube-iter guess pre)
    (define (good-enough? a)
      (< (abs (- (* a a a) (* pre pre pre))) error))
    (if (good-enough? guess) guess
        (cube-iter (improve guess) guess)))
  (cube-iter 1.0 x))
