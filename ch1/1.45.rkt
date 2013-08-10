#lang racket

;; import fixed-point-print
(require "newtons-method.rkt")

;; related to contents in 1.36

(define (average-damping f)
  (lambda (x) (/ (+ x (f x)) 2)))

(define (square-root x)
  (lambda (y) (exact->inexact (/ x y))))

(define (cubic-root x)
  (lambda (y) (exact->inexact (/ x (* y y)))))

(define (fourth-root x)
  (lambda (y) (exact->inexact (/ x (* y y y)))))

;; testing
;; using 4 steps converging to error less than 0.00001
;(fixed-point-print (average-damping (square-root 35)) 1)
;; using 20 steps converging to error less than 0.00001
;(fixed-point-print (average-damping (cubic-root 35)) 1)
;; never converge
;(fixed-point-print (average-damping (fourth-root 35)) 1)