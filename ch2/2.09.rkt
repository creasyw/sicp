#lang racket

(require "2.07.rkt")

(define x (make-interval 6.12 7.48))
(define y (make-interval 9 11))

(define get-width
  (lambda (x)
    (/ (abs (sub (upper-bound x) (lower-bound x))) 2)))

;; Prove that the addition and substraction of two intervals are still
;; a function of the "width".
;; The width of the linear combination of intervals is the sum of each
;; interval.
(define add-num (add-interval x y))
(define add-width (add (get-width x) (get-width y)))
(= add-width (get-width add-num))

(define sub-num (sub-interval x y))
(define sub-width (add (get-width x) (get-width y)))
(= sub-width (get-width sub-num))

;; But mul/div of intervals of the same width, would generate
;; different results of intervals

(define a1 (make-interval 0 10))
(define b1 (make-interval 4 6))

(define a2 (make-interval -5 5))
(define b2 (make-interval -1 1))

;; The example involves two set of intervals of 10 and 2
(define interval1 (get-width (mul-interval a1 b1)))  ;; 30
(define interval2 (get-width (mul-interval a2 b2)))  ;; 5
(not (= interval1 interval2))
