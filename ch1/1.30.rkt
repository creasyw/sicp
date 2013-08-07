#lang racket

(define (sum term a next b)
  (define (helper result acc)
    (if (> acc b) result
        (helper (+ result (term acc)) (next acc))))
  (helper 0 a))