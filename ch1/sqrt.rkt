#lang racket

(define (my-sqrt x error)
  (define (improve a b)
    (/ (+ a (/ b a)) 2))
  (define (good-enough? a b)
    (< (abs (- (* a a) b)) error))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x) guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))