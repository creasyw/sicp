#lang racket

;; import accumulate
(require "general_func.rkt")

(define (my-map proc seq)
  (accumulate (lambda (x y) (cons (proc x) y)) '() seq))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my-length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

;; testing
(accumulate + 0 (list 1 2 3 4 5))
(accumulate cons '() (list 1 2 3 4 5))
(my-map (lambda (x) (* x x)) (list 1 2 3 4 5))
(my-append (list 1 2 3 4 5) (list 3 4 5))
(my-length (list 1 2 3 4 5 6))