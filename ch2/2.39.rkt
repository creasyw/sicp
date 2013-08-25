#lang racket

;; import accumulate
(require "general_func.rkt")

(define (my-reverse-v1 seq)
  (foldl (lambda (x y) (cons x y)) '() seq))

(define (my-reverse-v2 seq)
  (accumulate (lambda (x y) (append y (list x))) '() seq))

;; testing
(my-reverse-v1 (list 1 2 3 4 5))
(my-reverse-v2 (list 1 2 3 4 5))