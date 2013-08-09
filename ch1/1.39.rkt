#lang racket

;; import cont-frac
(require "1.37.rkt")

(define (deg x)
  (lambda (n) (if (> n 1) (* x x) x)))

(define (tan-cf x k)
  (cont-frac (lambda (i) (- (* 2 i) 1)) (deg x) k))

;; test
(exact->inexact (tan-cf 45 20))