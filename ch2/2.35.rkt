#lang racket

;; import accumulate
(require "general_func.rkt")

(define (count-leaves t)
  (accumulate (lambda (x y) (+ (if (pair? x) (count-leaves x) 1) y))
              0 t))

;; testing 
(define x (cons (list 1 2) (list 3 4)))
(count-leaves (list x x))