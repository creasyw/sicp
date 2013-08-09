#lang racket

(define (product term a next terminate)
  (define (helper result acc)
    (if (terminate acc) result
        (helper (* result (term acc)) (next acc))))
  (helper 1 a))

(define (factorial n)
  (product (lambda (x) x) 1 (lambda (x) (+ x 1)) (lambda (x) (> x n))))

(define (approximate-pi terms)
  (define (next x)
    (if (> x 1) (/ (numerator x) (+ 2 (denominator x)))
        (/ (+ 2 (numerator x)) (denominator x))))
  (letrec ((b (if (even? terms) (/ (+ terms 2) (+ terms 1))
                  (/ (+ terms 1) (terms 2))))
           (terminate (lambda (x) (> (numerator x) (numerator b)))))
    (exact->inexact (* 4 (product (lambda (x) x) 2/3 next terminate)))))