#lang racket

;; the last argument using "terminate" rather than b in the book
;; is to find more general terminating criterion.
(define (accumulate combiner null-value term start next terminate)
  (define (helper result acc)
    (if (terminate acc) result
        (helper (combiner result (term acc)) (next acc))))
  (helper null-value start))

(define (sum term a next b)
  (accumulate + 0 term a next (lambda (x) (> x b))))

(define (product term a next b)
  (accumulate * 1 term a next (lambda (x) (> x b))))

;; test sum
(sum (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)

;; test product
(product (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)
