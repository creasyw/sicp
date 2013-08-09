#lang racket

;; import prime?
(require "../prime.rkt")

(define (filtered-cumulate combiner null-value pred? term start next terminate)
  (define (helper result acc)
    (cond ((terminate acc) result)
          ((pred? acc) (helper (combiner result (term acc)) (next acc)))
          (#t (helper result (next acc)))))
  (helper null-value start))

(define (prime-square a b)
  (filtered-cumulate + 0 prime? (lambda (x) (* x x)) a
                     (lambda (x) (+ x 1)) (lambda (x) (>= x b))))
(prime-square 1 100)
;; benchmark
(foldl + 0 (map (lambda (x) (* x x)) (filter prime? (range 1 100))))

;; product of all positive integers smaller than and relatively prime to n
(define (product-coprime n)
  (filtered-cumulate * 1 (lambda (x) (= 1 (gcd x n))) (lambda (x) x)
                     2 (lambda (x) (+ x 1)) (lambda (x) (>= x n))))
(product-coprime 100)
;; benchmark
(foldl * 1 (filter (lambda (x) (= 1 (gcd x 100))) (range 2 100)))