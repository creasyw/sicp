#lang racket

;; import accumulate
(require "general_func.rkt")

(define (horner-eval x coeff-seq)
  (accumulate (lambda (this higher) (+ this (* x higher)))
              0 coeff-seq))

;; testing
;; 1 + 3x + 5x**2 + x**5 at x=2
(horner-eval 2 (list 1 3 0 5 0 1))