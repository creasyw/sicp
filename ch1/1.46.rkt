#lang racket

;; import iterative-improve
;; import average-damping
(require "newtons-method.rkt")

(define (new-fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve close-enough? f) first-guess))

(define (my-sqrt x)
  (new-fixed-point (average-damping
                      (lambda (y) (exact->inexact (/ x y))))
                     1))

;; testing
(my-sqrt 20)