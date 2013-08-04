#lang racket

(define (sum-larger-square a b c)
  (letrec ((lst (sort (list a b c) < ))
           (largest (list-ref lst 2))
           (larger (list-ref lst 1)))
    (+ (* largest largest) (* larger larger))))