#lang racket

(define (same-parity a . b)
  (letrec ((flag (remainder a 2)))
    (cons a (filter (lambda (x) (= flag (remainder x 2))) b))))

;; testing
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)