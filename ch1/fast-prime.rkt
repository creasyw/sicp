#lang racket

(define (fast-prime n)
  ;; has 255 exceptions below 100 million
  (define (fermat-test a)
    (= a (remainder (expt a n) n)))
  ;; improvement based on Fermat Test
  (define (miller-rabin-test a)
    (= 1 (remainder (expt a (- n 1)) n)))
  ;; define how many times implementing the fermat algo.
  (letrec ((all (/ (log n) (log 10))))
    (define (helper times)
      ;; "all" is a floating point number
      (or (> times all)
          (and (miller-rabin-test (+ 1 (random (- n 1))))
               (helper (+ times 1)))))
    (helper 0)))

