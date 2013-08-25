#lang racket

;; import accumulate
(require "general_func.rkt")

(provide accumulate-n)

(define (accumulate-n op init seqs)
  (if (null? (car seqs)) '()
      (cons (accumulate op init (accumulate (lambda (x y) (cons (car x) y)) '() seqs))
            (accumulate-n op init (accumulate (lambda (x y) (cons (cdr x) y)) '() seqs)))))

;; testing
(define x (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 x)