#lang racket

;; import cont-frac
(require "1.37.rkt")

(define (di n)
  (letrec ((index (quotient (+ n 2) 3))
           (remnd (modulo (+ n 2) 3)))
    (if (= 0 remnd) (* 2 index) 1)))

(define (euler-number round)
  (+ 2 (cont-frac di (lambda (i) 1.0) round)))

;; test
(euler-number 100)