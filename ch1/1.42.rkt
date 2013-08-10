#lang racket

(define (compose f g)
  (lambda (x) (f (g x))))

;; test
(define (square x) (* x x))
(define (inc x) (+ x 1))

((compose square inc) 6)