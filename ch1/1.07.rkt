#lang racket

(define (my-sqrt x error)
  (define (improve a)
    (/ (+ a (/ x a)) 2)) 
  (define (sqrt-iter guess previous)
    (define (good-enough? a)
      (< (abs (- (* a a) (* previous previous))) error))
    (if (good-enough? guess) guess
        (sqrt-iter (improve guess) guess)))
  (sqrt-iter 1.0 x))