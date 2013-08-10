#lang racket

(define (repeated f n)
  (lambda (x)
    (define (helper count)
      (if (= count n) (f x)
          (f (helper (+ count 1)))))
     (helper 1)))

(define (square x) (* x x))
((repeated square 2) 5)