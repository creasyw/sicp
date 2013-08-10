#lang racket

(define (double f)
  (lambda (x) (f (f x))))

;; test
(define (inc x) (+ x 1))

;; equal to (double (double (double (double inc))))
;; +2 => +4 => +8 => +16
(((double (double double)) inc) 5)