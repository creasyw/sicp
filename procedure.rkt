#lang racket

(provide repeated)

(define (repeated f n)
  (lambda (x)
    (define (helper count)
      (if (= count n) (f x)
          (f (helper (+ count 1)))))
     (helper 1)))