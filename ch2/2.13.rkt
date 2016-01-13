#lang racket

(require "2.07.rkt")
(require "2.11.rkt")
(require "2.12.rkt")

;; assume all numbers (both upper- and lower- bounds) are positive
(define (mul-percentage a b)
  (letrec ((p1 (percent a))
           (p2 (percent b))
           (c1 (center a))
           (c2 (center b)))
    (make-center-percent (mult (mult c1 c2) (+ 1 (mult p1 p2)))
                         (div (+ p1 p2) (+ 1 (mult p1 p2))))))

(define round-off-interval
  (lambda (x p) (make-interval (round-off (lower-bound x) p)
                               (round-off (upper-bound x) p))))

;; testing
(define x1 (make-interval 3 5))
(define x2 (make-interval 9 15))
(eq-interval? (mul-interval2 x1 x2)
              (round-off-interval (mul-percentage x1 x2) 0))
