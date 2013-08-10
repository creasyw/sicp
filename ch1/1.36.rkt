#lang racket

;; import fixed-point-print
(require "newtons-method.rkt")

(define question
  (lambda (x) (/ (log 1000) (log x))))

;; test
(fixed-point-print question 2.0)

(define (average-damping f)
  (lambda (x) (/ (+ x (f x)) 2)))

;; find fixed-point with average damping
(displayln "With average damping:")
(fixed-point-print (average-damping question) 2.0)