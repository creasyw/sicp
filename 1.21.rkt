#lang racket

;; the algo. is still kind of "over-shooting"
;; the i should iterate over all primes no larger than n
;; but this is another story... =D
;; gen-prime can be found in project-euler repo
(define (smallest-divisor n)
  (last (for/list ((i (in-range 2 (+ n 1)))
                   #:final (= 0 (remainder n i))) i)))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)