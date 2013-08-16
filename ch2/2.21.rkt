#lang racket

(define (square-list lst)
  (map (lambda (x) (* x x)) lst))

;; testing
(square-list (list 1 2 3 4))