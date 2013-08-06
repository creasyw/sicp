#lang racket

(define (fib n)
  (define (fib-iter a b p q acc)
    (cond ((= acc 0) b)
          ((even? acc) (fib-iter a b
                               (+ (* p p) (* q q))
                               (+ (* 2 p q) (* q q))
                               (/ acc 2)))
          (#t (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q)) p q (- acc 1)))))
  (fib-iter 1 0 0 1 n))