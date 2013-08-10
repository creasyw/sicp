#lang racket

;; import fixed-point-print
(require "newtons-method.rkt")

;; import repeated
(require "../procedure.rkt")

;; related to contents in 1.36

(define (average-damping f)
  (lambda (x) (/ (+ x (f x)) 2)))

(define (square-root x)
  (lambda (y) (exact->inexact (/ x y))))

(define (cubic-root x)
  (lambda (y) (exact->inexact (/ x (* y y)))))

(define (fourth-root x)
  (lambda (y) (exact->inexact (/ x (* y y y)))))

(define (nth-root x n)
  (fixed-point-print ((repeated average-damping (quotient n 2))
                       (lambda (y) (exact->inexact (/ x (expt y (- n 1)))))) 1))

;; testing
;; using 4 steps converging to error less than 0.00001
(fixed-point-print (average-damping (square-root 35)) 1)
;; using 20 steps converging to error less than 0.00001
(fixed-point-print (average-damping (cubic-root 35)) 1)

;; if not repeated, never converge
;; repeated twice, converge in 10 times
(fixed-point-print ((repeated average-damping 2) (fourth-root 35)) 1)

;; testing
(nth-root 35 4)
(nth-root 100 6)
