#lang racket

(require "2.07.rkt")
(require "2.11.rkt")
(require "2.12.rkt")

(provide (all-defined-out))

;; assume all numbers (both upper- and lower- bounds) are positive
(define (mul-percentage a b)
  (letrec ((p1 (percent a))
           (p2 (percent b))
           (c1 (center a))
           (c2 (center b)))
    (make-center-percent (mult (mult c1 c2) (+ 1 (mult p1 p2)))
                         (div (+ p1 p2) (+ 1 (mult p1 p2))))))

;; the upper-/lower- bounds are "closer" to real-world implementation
;; than the center/ratio representation does, because both values of
;; the latter choice are "relative". That is also the reason that the
;; derivations of `div' and `mul' here are actually from the bounds.
(define (reverse-cp x)
  (letrec ((c (center x))
           (p (percent x)))
    (make-center-percent (div 1.0 (mult (mult (+ 1 p) (- 1 p)) c))
                         p)))

(define (div-percentage a b)
  (if (eq-interval? a b)
      (make-center-percent 1.0 0)
      (mul-percentage a (reverse-cp b))))

(define round-off-interval
  (lambda (x p) (make-interval (round-off (lower-bound x) p)
                               (round-off (upper-bound x) p))))

;; testing
(define x1 (make-interval 3 5))
(define x2 (make-interval 9 15))
(displayln (eq-interval? (mul-interval2 x1 x2)
                         (round-off-interval (mul-percentage x1 x2) 0)))
(displayln (eq-interval? (div-interval x1 x2)
                         (div-percentage x1 x2)))
