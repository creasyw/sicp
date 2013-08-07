#lang racket

(define (my-gcd a b)
  (if (= b 0) a
      (my-gcd b (remainder a b))))