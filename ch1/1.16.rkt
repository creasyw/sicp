#lang racket

(define (fast-expt a n)
  (define (helper acc count)
    (if (= count n) acc
        (helper (* acc a) (+ count 1))))
  (helper 1 0))