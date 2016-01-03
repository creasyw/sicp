#lang racket

(define (cons1 x y)
  (lambda (m) (m x y)))

(define (car1 z)
  (z (lambda (p q) p)))

(define (cdr1 z)
  (z (lambda (p q) q)))


;; test
(define x (cons1 1 2))
(eq? (car1 x) 1)
(eq? (cdr1 x) 2)
