#lang racket

(require "2.07.rkt")

(define x (make-interval 6.12 7.48))
(define y (make-interval 9 11))

(define get-width
  (lambda (x)
    (/ (abs (sub (upper-bound x) (lower-bound x))) 2)))

;; Prove that the addition and substraction of two intervals are still
;; a function of the "width"
(define add-num (add-interval x y))
(define add-width (add (get-width x) (get-width y)))
(= add-width (get-width add-num))

(define sub-num (sub-interval x y))
(define sub-width (add (get-width x) (get-width y)))
(= sub-width (get-width sub-num))
