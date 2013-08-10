#lang racket

;; import newtons-method
(require "newtons-method.rkt")

;; approximate zeros of the cubic x3 + ax2 + bx + c
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

;; test with newton's method
(newtons-method (cubic 2 3 4) 1)
