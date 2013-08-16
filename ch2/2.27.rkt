#lang racket

(define (deep-reverse lst)
  (reverse (map (lambda (x) (reverse x)) lst)))

;; testing
(define x (list (list 1 2) (list 3 4)))
(displayln x)
(displayln (deep-reverse x))